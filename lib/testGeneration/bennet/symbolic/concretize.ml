module CF = Cerb_frontend
module C = CF.Ctype
module IT = IndexTerms
module BT = BaseTypes
module LC = LogicalConstraints
module CtA = Fulminate.Cn_to_ail
module Records = Fulminate.Records

module Make (AD : Domain.T) = struct
  module Stage2 = Stage2.Make (AD)
  module Smt = Smt.Make (AD)
  module Ctx = Stage2.Ctx
  module Term = Stage2.Term
  module Def = Stage2.Def

  (** Result of term compilation: statements to emit and final expression *)
  type result =
    { statements : Pp.document list; (* Statements to emit with semicolons *)
      expression : Pp.document (* Final expression to return *)
    }

  (** Convert a generator term to CN-SMT symbolic execution statements and expression *)
  let rec concretize_term (tm : Term.t) : result =
    let open Pp in
    let (GenTerms.Annot (tm_, (), bt, loc)) = tm in
    match tm_ with
    | `Arbitrary | `Symbolic ->
      (* Generate symbolic value of the given base type *)
      { statements = [];
        expression = !^"CN_SMT_CONCRETIZE_SYMBOLIC" ^^ parens (Smt.convert_basetype bt)
      }
    | `Call (fsym, args) ->
      (* Call a defined generator function with arguments *)
      let args_smt = List.map Smt.convert_indexterm args in
      let args_list =
        separate_map (comma ^^^ space) (fun x -> x) (Sym.pp fsym :: args_smt)
      in
      { statements = []; expression = !^"CN_SMT_CONCRETIZE_CALL" ^^ parens args_list }
    | `Asgn ((addr, sct), value, next_term) ->
      (* Assignment: claim ownership and assign value to memory location *)
      let addr_smt = Smt.convert_indexterm addr in
      let value_smt = Smt.convert_indexterm value in
      let next_result = concretize_term next_term in
      let assign_stmt =
        !^"CN_SMT_CONCRETIZE_ASSIGN"
        ^^ parens
             (CF.Pp_ail.(
                with_executable_spec
                  (pp_ctype_human C.no_qualifiers)
                  (Sctypes.to_ctype sct))
              ^^ comma
              ^^^ !^(Option.get (CtA.get_conversion_from_fn_str (Memory.bt_of_sct sct)))
              ^^ comma
              ^^^ addr_smt
              ^^ comma
              ^^^ value_smt)
      in
      { statements = assign_stmt :: next_result.statements;
        expression = next_result.expression
      }
    | `LetStar ((var_sym, Annot ((`Arbitrary | `Symbolic), _, bt_arb, _)), body_term) ->
      (* Let binding with potential backtracking *)
      let body_result = concretize_term body_term in
      { statements =
          (!^"CN_SMT_CONCRETIZE_LET_SYMBOLIC"
           ^^ parens (Sym.pp var_sym ^^ comma ^^^ Smt.convert_basetype bt_arb))
          :: body_result.statements;
        expression = body_result.expression
      }
    | `LetStar ((var_sym, binding_term), body_term) ->
      (* Let binding with potential backtracking *)
      let var_name = Pp.plain (Sym.pp var_sym) in
      let binding_result = concretize_term binding_term in
      let body_result = concretize_term body_term in
      (* Generate let binding as statement *)
      let let_stmt =
        !^"cn_term*" ^^^ !^var_name ^^^ !^"=" ^^^ binding_result.expression
      in
      { statements = binding_result.statements @ (let_stmt :: body_result.statements);
        expression = body_result.expression
      }
    | `Return it ->
      (* Monadic return - just return the expression, no return statement needed *)
      let term_smt = Smt.convert_indexterm it in
      { statements = []; expression = term_smt }
    | `Assert (lc, next_term) ->
      (* Assert logical constraints, backtrack if false *)
      let constraint_smt = Smt.convert_logical_constraint lc in
      let next_result = concretize_term next_term in
      let assert_stmt = !^"CN_SMT_CONCRETIZE_ASSERT" ^^ parens constraint_smt in
      { statements = assert_stmt :: next_result.statements;
        expression = next_result.expression
      }
    | `ITE (condition, then_term, else_term) ->
      (* Convert if-then-else to Pick statement with recursive calls *)
      let then_branch = Term.assert_ (LC.T condition, then_term) () loc in
      let else_branch = Term.assert_ (LC.T (IT.not_ condition loc), else_term) () loc in
      concretize_term (Term.pick_ [ then_branch; else_branch ] () bt loc)
    | `Map ((i_sym, i_bt, perm), inner_term) ->
      (* Map operation over a range - generate a loop-like construct *)
      let var_name = Pp.plain (Sym.pp i_sym) in
      let var_type = Smt.convert_basetype i_bt in
      let perm_smt = Smt.convert_indexterm perm in
      let inner_result = concretize_term inner_term in
      let loop_body =
        inner_result.statements @ [ inner_result.expression ]
        |> List.map (fun stmt -> !^"  " ^^ stmt ^^ !^";")
        |> separate hardline
      in
      let map_stmt =
        !^"/* Map operation over range */"
        ^/^ !^"for ("
        ^^ var_type
        ^^ !^" "
        ^^ !^var_name
        ^^ !^" in range("
        ^^ perm_smt
        ^^ !^")) {"
        ^/^ loop_body
        ^/^ !^"}"
      in
      { statements = [ map_stmt ]; expression = !^"cn_smt_unit()" }
    | `Pick choice_terms ->
      (* Weighted choice selection using CN_SMT_PICK macros *)
      let result_var = Sym.fresh_anon () in
      let tmp_var = Sym.fresh_anon () in
      (* Generate the choices array: weight1, index1, weight2, index2, ... *)
      (* Each choice gets equal weight (1) *)
      let choices =
        choice_terms
        |> List.mapi (fun i _ -> [ !^"1"; int i ])
        |> List.flatten
        |> separate (comma ^^ space)
      in
      (* Generate the pick begin macro call *)
      let pick_begin =
        !^"CN_SMT_CONCRETIZE_PICK_BEGIN"
        ^^ parens (Sym.pp result_var ^^ comma ^^^ Sym.pp tmp_var ^^ comma ^^^ choices)
      in
      (* Generate case statements for each choice *)
      let cases =
        choice_terms
        |> List.mapi (fun i term ->
          let case_begin = !^"CN_SMT_CONCRETIZE_PICK_CASE_BEGIN" ^^ parens (int i) in
          let term_result = concretize_term term in
          let case_stmts =
            term_result.statements
            |> List.map (fun stmt -> stmt ^^ !^";")
            |> separate hardline
          in
          let case_end =
            !^"CN_SMT_CONCRETIZE_PICK_CASE_END"
            ^^ parens (Sym.pp result_var ^^ comma ^^^ term_result.expression)
          in
          case_begin ^/^ case_stmts ^/^ case_end)
        |> separate hardline
      in
      (* Generate the pick end macro call *)
      let pick_end = !^"CN_SMT_CONCRETIZE_PICK_END" ^^ parens !^"()" in
      (* Combine everything as single statement block *)
      let pick_stmt = pick_begin ^/^ cases ^/^ pick_end in
      { statements = [ pick_stmt ]; expression = Sym.pp result_var }


  (** Convert generator definition to complete CN-SMT symbolic execution function *)
  let concretize_def (def : Def.t) : Pp.document =
    let open Pp in
    let term_result = concretize_term def.body in
    let params =
      List.map (fun (sym, _bt) -> !^"cn_term*" ^^^ Sym.pp sym) def.iargs
      |> separate_map (comma ^^^ space) (fun x -> x)
    in
    (* Generate statements with semicolons *)
    let statements =
      term_result.statements
      |> List.map (fun stmt -> !^"  " ^^ stmt ^^ !^";")
      |> separate hardline
    in
    let return_stmt = !^"  return" ^^^ term_result.expression ^^ !^";" in
    let body =
      if List.length term_result.statements > 0 then
        statements ^/^ return_stmt
      else
        return_stmt
    in
    !^"static cn_term*"
    ^^^ !^("cn_smt_concretize_" ^ Pp.plain (Sym.pp def.name))
    ^^ parens (!^"struct cn_smt_solver* smt_solver" ^^ comma ^^^ params)
    ^^^ !^"{"
    ^/^ body
    ^/^ !^"}"


  let concretize_ctx (ctx : Ctx.t) : Pp.document =
    Pp.(separate_map (twice hardline) concretize_def (List.map snd ctx))
end
