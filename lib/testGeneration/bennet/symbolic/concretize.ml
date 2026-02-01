module CF = Cerb_frontend
module A = CF.AilSyntax
module C = CF.Ctype
module IT = IndexTerms
module BT = BaseTypes
module LC = LogicalConstraints
module CtA = Fulminate.Cn_to_ail
module Records = Fulminate.Records

module Make (AD : Domain.T) = struct
  module Stage5 = Stage5.Make (AD)
  module Smt = Smt.Make (AD)
  module Ctx = Stage5.Ctx
  module Term = Stage5.Term
  module Def = Stage5.Def

  let bennet = Sym.fresh "bennet"

  (** Result of term compilation: statements to emit and final expression *)
  type result =
    { statements : Pp.document list; (* Statements to emit with semicolons *)
      expression : Pp.document (* Final expression to return *)
    }

  (** Convert a generator term to CN-SMT symbolic execution statements and expression *)
  let rec concretize_term (sigma : CF.GenTypes.genTypeCategory A.sigma) (tm : Term.t)
    : result
    =
    let open Pp in
    let (GenTerms.Annot (tm_, (), bt, loc)) = tm in
    match tm_ with
    | `ArbitrarySpecialized _ ->
      failwith "ArbitrarySpecialized not supported in symbolic mode"
    | `Arbitrary | `Symbolic ->
      (* Generate symbolic value of the given base type *)
      { statements = [];
        expression = !^"CN_SMT_CONCRETIZE_SYMBOLIC" ^^ parens (Smt.convert_basetype bt)
      }
    | `ArbitraryDomain _ ->
      (* Generate symbolic value from domain - treat like arbitrary for now *)
      { statements = [];
        expression = !^"CN_SMT_CONCRETIZE_SYMBOLIC" ^^ parens (Smt.convert_basetype bt)
      }
    | `Call (fsym, args) | `CallSized (fsym, args, _) ->
      (* Call a defined generator function with arguments *)
      let args_smt = List.map (Smt.convert_indexterm sigma) args in
      let args_list =
        separate_map (comma ^^^ space) (fun x -> x) (Sym.pp fsym :: args_smt)
      in
      let tmp_var = Sym.fresh_make_uniq ("tmp_" ^ Sym.pp_string fsym) in
      let call_stmt =
        !^"cn_term*"
        ^^^ Sym.pp tmp_var
        ^^^ equals
        ^^^ !^"CN_SMT_CONCRETIZE_CALL"
        ^^ parens args_list
      in
      { statements = [ call_stmt ]; expression = Sym.pp tmp_var }
    | `SplitSize (_, next_term) ->
      (* Split size - just process the next term *)
      concretize_term sigma next_term
    | `Asgn ((addr, sct), value, next_term) ->
      (* Assignment: claim ownership and assign value to memory location *)
      let addr_smt = Smt.convert_indexterm sigma addr in
      let value_smt = Smt.convert_indexterm sigma value in
      let next_result = concretize_term sigma next_term in
      let assign_stmt =
        !^"CN_SMT_CONCRETIZE_ASSIGN"
        ^^ parens
             (CF.Pp_ail.(
                with_executable_spec
                  (pp_ctype ~is_human:false C.no_qualifiers)
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
      let body_result = concretize_term sigma body_term in
      { statements =
          (!^"CN_SMT_CONCRETIZE_LET_SYMBOLIC"
           ^^ parens (Sym.pp var_sym ^^ comma ^^^ Smt.convert_basetype bt_arb))
          :: body_result.statements;
        expression = body_result.expression
      }
    | `LetStar ((var_sym, binding_term), body_term) ->
      (* Let binding with potential backtracking *)
      let var_name = Pp.plain (Sym.pp var_sym) in
      let binding_result = concretize_term sigma binding_term in
      let body_result = concretize_term sigma body_term in
      (* Generate let binding as statement *)
      let statements =
        binding_result.statements
        @
        if Sym.Set.mem var_sym (Term.free_vars body_term) then (
          let let_stmt =
            !^"CN_SMT_CONCRETIZE_LET_STAR"
            ^^ parens (!^var_name ^^ comma ^^^ binding_result.expression)
          in
          let_stmt :: body_result.statements)
        else
          body_result.statements
      in
      { statements; expression = body_result.expression }
    | `Return it ->
      (* Monadic return - just return the expression, no return statement needed *)
      let term_smt = Smt.convert_indexterm sigma it in
      { statements = []; expression = term_smt }
    | `Assert (lc, next_term) ->
      (* Assert logical constraints, backtrack if false *)
      let constraint_smt = Smt.convert_logical_constraint sigma lc in
      let next_result = concretize_term sigma next_term in
      let assert_stmt = !^"CN_SMT_CONCRETIZE_ASSERT" ^^ parens constraint_smt in
      { statements = assert_stmt :: next_result.statements;
        expression = next_result.expression
      }
    | `AssertDomain (_, next_term) ->
      (* Assert domain constraints - skip domain for now and continue *)
      concretize_term sigma next_term
    | `Instantiate _ -> failwith ("unreachable @ " ^ __LOC__)
    | `ITE (it_if, then_term, else_term) ->
      (* Convert if-then-else to PickSized statement with recursive calls *)
      let wgts1 =
        match then_term with
        | Annot (`PickSized gts, (), _, _) ->
          List.map (fun (w, gt') -> (w, Term.assert_ (T it_if, gt') () loc)) gts
        | gt' -> [ (Z.one, Term.assert_ (T it_if, gt') () loc) ]
      in
      let wgts2 =
        match else_term with
        | Annot (`PickSized gts, (), _, _) ->
          List.map
            (fun (w, gt') -> (w, Term.assert_ (T (IT.not_ it_if loc), gt') () loc))
            gts
        | gt' -> [ (Z.one, Term.assert_ (T (IT.not_ it_if loc), gt') () loc) ]
      in
      concretize_term sigma (Term.pick_sized_ (wgts1 @ wgts2) () bt loc)
    | `Map
        ( (i_sym, i_bt, it_perm),
          Annot
            ( `LetStar
                ( (x, Annot ((`Arbitrary | `ArbitraryDomain _ | `Symbolic), _, _, _)),
                  Annot
                    ( `Asgn
                        ( (it_addr, sct),
                          IT (Sym x', _, _),
                          Annot (`Return (IT (Sym x'', v_bt, _)), _, _, _) ),
                      _,
                      _,
                      _ ) ),
              _,
              _,
              _ ) )
      when Sym.equal x x' && Sym.equal x' x'' ->
      (* Add constraint to ensure array range doesn't exceed max_array_length *)
      let it_min, it_max = IT.Bounds.get_bounds (i_sym, i_bt) it_perm in
      let max_len_constraint =
        let here = Locations.other __LOC__ in
        let array_len =
          IT.add_ (IT.sub_ (it_max, it_min) here, IT.num_lit_ (Z.of_int 1) i_bt here) here
        in
        let max_len_term =
          IT.num_lit_ (Z.of_int (TestGenConfig.get_max_array_length ())) i_bt here
        in
        LC.T (IT.le_ (array_len, max_len_term) here)
      in
      let assert_stmt =
        !^"CN_SMT_CONCRETIZE_ASSERT"
        ^^ parens (Smt.convert_logical_constraint sigma max_len_constraint)
      in
      (* Array assignment: claim ownership of memory locations *)
      let f = Simplify.IndexTerms.simp (Simplify.default Global.empty) in
      let max_array_length = Smt.get_max_array_length_of (i_sym, i_bt) it_perm in
      let prefix = Printf.sprintf "%s_%d_map_value" (Sym.pp_string x) (Sym.num x) in
      let elem_names =
        max_array_length
        |> Z.to_int
        |> List.range 0
        |> List.map (Printf.sprintf "%s_%d" prefix)
      in
      let elem_docs = List.map Pp.string elem_names in
      let value_bt_doc = Smt.convert_basetype v_bt in
      let values_stmts =
        List.map
          (fun name_doc ->
             !^"CN_SMT_CONCRETIZE_LET_SYMBOLIC"
             ^^ parens (name_doc ^^ comma ^^^ value_bt_doc))
          elem_docs
      in
      let result_ty = Smt.convert_basetype bt in
      let map_var_name = Printf.sprintf "%s_acc" prefix in
      let map_var_doc = Pp.string map_var_name in
      let here = Locations.other __LOC__ in
      let ctype_doc =
        CF.Pp_ail.(
          with_executable_spec
            (pp_ctype ~is_human:false C.no_qualifiers)
            (Sctypes.to_ctype sct))
      in
      let convert_fn_doc =
        !^(Option.get (CtA.get_conversion_from_fn_str (Memory.bt_of_sct sct)))
      in
      let subst_i_in_addr it = f (IT.subst (IT.make_subst [ (i_sym, it) ]) it_addr) in
      let map_init_stmt =
        !^"cn_term*" ^^^ map_var_doc ^^^ !^"=" ^^^ !^"cn_smt_default" ^^ parens result_ty
      in
      let conditional_stmts =
        elem_docs
        |> List.mapi (fun idx value_doc ->
          let idx_it = IT.num_lit_ (Z.of_int idx) i_bt here in
          let guard_it = f (IT.subst (IT.make_subst [ (i_sym, idx_it) ]) it_perm) in
          let cond_doc =
            !^"convert_from_cn_bool"
            ^^ parens
                 (!^"cn_smt_concretize_eval_term"
                  ^^ parens
                       (!^"smt_solver" ^^ comma ^^^ Smt.convert_indexterm sigma guard_it)
                 )
          in
          let addr_doc = Smt.convert_indexterm sigma (subst_i_in_addr idx_it) in
          let assign_doc =
            !^"CN_SMT_CONCRETIZE_ASSIGN"
            ^^ parens
                 (ctype_doc
                  ^^ comma
                  ^^^ convert_fn_doc
                  ^^ comma
                  ^^^ addr_doc
                  ^^ comma
                  ^^^ value_doc)
          in
          let key_doc = Smt.convert_indexterm sigma idx_it in
          let update_doc =
            map_var_doc
            ^^^ !^"="
            ^^^ !^"cn_smt_map_set"
            ^^ parens (separate (comma ^^ space) [ map_var_doc; key_doc; value_doc ])
          in
          !^"if"
          ^^^ parens cond_doc
          ^^^ braces (assign_doc ^^ !^";" ^/^ update_doc ^^ !^";"))
      in
      { statements = (assert_stmt :: values_stmts) @ (map_init_stmt :: conditional_stmts);
        expression = map_var_doc
      }
    | `Map _ -> failwith "TODO: Map"
    | `PickSized choice_terms ->
      (* Weighted choice selection using CN_SMT_PICK macros *)
      let result_var = Sym.fresh_anon () in
      (* Generate the pick begin macro call *)
      let pick_begin =
        !^"CN_SMT_CONCRETIZE_PICK_BEGIN"
        ^^ parens (separate (comma ^^ space) [ Sym.pp result_var ])
      in
      (* Generate case statements for each choice *)
      let cases =
        choice_terms
        |> List.mapi (fun i (_, term) ->
          let case_begin = !^"CN_SMT_CONCRETIZE_PICK_CASE_BEGIN" ^^ parens (int i) in
          let term_result = concretize_term sigma term in
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
      let pick_end = !^"CN_SMT_CONCRETIZE_PICK_END" ^^ parens empty in
      (* Combine everything as single statement block *)
      let pick_stmt = pick_begin ^/^ cases ^/^ pick_end in
      { statements = [ pick_stmt ]; expression = Sym.pp result_var }


  (** Generate forward declaration for a concretize function *)
  let concretize_forward_decl (def : Def.t) : Pp.document =
    let open Pp in
    let params =
      List.map (fun (sym, _bt) -> !^"cn_term*" ^^^ Sym.pp sym) def.iargs
      |> separate_map (comma ^^^ space) (fun x -> x)
    in
    !^"static cn_term*"
    ^^^ !^("cn_smt_concretize_" ^ Pp.plain (Sym.pp def.name))
    ^^ parens
         (!^"struct cn_smt_solver* smt_solver"
          ^^ comma
          ^^^ !^"struct branch_history_queue* branch_hist"
          ^^ comma
          ^^^ params)
    ^^ semi


  (** Convert generator definition to complete CN-SMT symbolic execution function *)
  let concretize_def (sigma : CF.GenTypes.genTypeCategory A.sigma) (def : Def.t)
    : Pp.document
    =
    let open Pp in
    let term_result = concretize_term sigma def.body in
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
    ^^ parens
         (!^"struct cn_smt_solver* smt_solver"
          ^^ comma
          ^^^ !^"struct branch_history_queue* branch_hist"
          ^^ comma
          ^^^ params)
    ^^^ braces body


  let concretize_ctx (sigma : CF.GenTypes.genTypeCategory A.sigma) (ctx : Ctx.t)
    : Pp.document
    =
    Pp.(separate_map (twice hardline) (concretize_def sigma) (List.map snd ctx))
end
