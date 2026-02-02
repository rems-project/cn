module CF = Cerb_frontend
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

  (** Convert a generator term to CN-SMT path selection statements *)
  let rec path_selector_term (ctx : Ctx.t) (last_branch : Sym.t) (tm : Term.t)
    : Pp.document
    =
    let open Pp in
    let (GenTerms.Annot (tm_, (), bt, loc)) = tm in
    match tm_ with
    | `ArbitrarySpecialized _ ->
      failwith "ArbitrarySpecialized not supported in symbolic mode"
    | `Arbitrary | `ArbitraryDomain _ | `Symbolic | `Lazy | `Return _ | `Map _ -> empty
    | `SplitSize (_, next_term) ->
      (* Split size - process the next term *)
      path_selector_term ctx last_branch next_term
    | `Call (fsym, _args) ->
      (* Check if the called function is recursive *)
      let is_recursive =
        match Ctx.find_opt fsym ctx with Some def -> def.Def.recursive | None -> false
      in
      if is_recursive then
        (* For recursive functions, use macro with bennet_get_size() to check failures *)
        !^"CN_SMT_PATH_SELECTOR_CALL"
        ^^ parens
             (separate
                (comma ^^ space)
                [ Sym.pp last_branch; Sym.pp fsym; !^"bennet_get_size()" ])
        ^^ semi
      else
        !^"CN_SMT_PATH_SELECTOR_CALL"
        ^^ parens (separate (comma ^^ space) [ Sym.pp last_branch; Sym.pp fsym ])
        ^^ semi
    | `CallSized (fsym, _args, (n, _sym_size)) ->
      (* CallSized always deals with recursive calls that need size *)
      (* Compute size parameter based on number of recursive calls *)
      let size_expr =
        if n <= 0 then
          failwith "Invalid sized call"
        else if n = 1 then
          !^"(bennet_rec_size - 1)"
        else
          !^("(bennet_rec_size / " ^ string_of_int n ^ ")")
      in
      (* Use macro to ensure failure checking *)
      !^"CN_SMT_PATH_SELECTOR_CALL"
      ^^ parens (separate (comma ^^ space) [ Sym.pp last_branch; Sym.pp fsym; size_expr ])
      ^^ semi
    | `Asgn (_, _, next_term) | `Assert (_, next_term) | `AssertDomain (_, next_term) ->
      path_selector_term ctx last_branch next_term
    | `Instantiate _ -> failwith ("unreachable @ " ^ __LOC__)
    | `LetStar ((_, binding_term), body_term) ->
      let binding_result = path_selector_term ctx last_branch binding_term in
      let body_result = path_selector_term ctx last_branch body_term in
      binding_result ^/^ body_result
    | `ITE (condition, then_term, else_term) ->
      (* Convert if-then-else to PickSized statement with recursive calls *)
      let then_branch = Term.assert_ (LC.T condition, then_term) () loc in
      let else_branch = Term.assert_ (LC.T (IT.not_ condition loc), else_term) () loc in
      path_selector_term
        ctx
        last_branch
        (Term.pick_sized_ [ (Z.one, then_branch); (Z.one, else_branch) ] () bt loc)
    | `PickSized choice_terms ->
      (* Weighted choice selection using CN_SMT_PICK macros *)
      let result_var = Sym.fresh_anon () in
      let tmp_var = Sym.fresh_anon () in
      (* Generate the choices array: weight1, index1, weight2, index2, ... *)
      let choices =
        choice_terms
        |> List.mapi (fun i (weight, _) -> [ z weight; int i ])
        |> List.flatten
        |> separate (comma ^^ space)
      in
      (* Generate the pick begin macro call *)
      let pick_begin =
        !^"CN_SMT_PATH_SELECTOR_PICK_BEGIN"
        ^^ parens
             (separate
                (comma ^^ space)
                [ Sym.pp result_var; Sym.pp tmp_var; Sym.pp last_branch; choices ])
      in
      (* Generate case statements for each choice *)
      let cases =
        choice_terms
        |> List.mapi (fun i (_, term) ->
          let case_begin = !^"CN_SMT_PATH_SELECTOR_PICK_CASE_BEGIN" ^^ parens (int i) in
          let term_result = path_selector_term ctx tmp_var term in
          let case_end = !^"CN_SMT_PATH_SELECTOR_PICK_CASE_END" ^^ parens empty in
          case_begin ^/^ term_result ^/^ case_end)
        |> separate hardline
      in
      (* Generate the pick end macro call *)
      let pick_end = !^"CN_SMT_PATH_SELECTOR_PICK_END" ^^ parens (Sym.pp tmp_var) in
      (* Combine everything as single statement block *)
      let pick_stmt = pick_begin ^/^ cases ^/^ pick_end in
      pick_stmt


  (** Generate forward declaration for a path selector function *)
  let path_selector_forward_decl (def : Def.t) : Pp.document =
    let open Pp in
    let params =
      !^"struct branch_history_queue*"
      ^^^ !^"branch_hist"
      ^^ comma
      ^^^ !^"cn_trie*"
      ^^^ !^"unsat_paths"
      ^^ if def.recursive then comma ^^^ !^"size_t" ^^^ !^"bennet_rec_size" else empty
    in
    !^"static void"
    ^^^ !^("cn_smt_path_selector_" ^ Pp.plain (Sym.pp def.name))
    ^^ parens params
    ^^ semi


  (** Convert generator definition to complete CN-SMT symbolic execution function *)
  let path_selector_def (ctx : Ctx.t) (def : Def.t) : Pp.document =
    let open Pp in
    let params =
      !^"struct branch_history_queue*"
      ^^^ !^"branch_hist"
      ^^ comma
      ^^^ !^"cn_trie*"
      ^^^ !^"unsat_paths"
      ^^ if def.recursive then comma ^^^ !^"size_t" ^^^ !^"bennet_rec_size" else empty
    in
    let init_macro =
      if def.recursive then
        !^"CN_SMT_PATH_SELECTOR_INIT_SIZED();"
      else
        !^"CN_SMT_PATH_SELECTOR_INIT();"
    in
    let body =
      if def.spec then (
        (* For spec functions, generate normal path selector *)
        let term_result = path_selector_term ctx bennet def.body in
        init_macro ^/^ term_result ^/^ !^"return;")
      else (
        let term_result = path_selector_term ctx bennet def.body in
        init_macro ^/^ term_result ^/^ !^"return;")
    in
    !^"static void"
    ^^^ !^("cn_smt_path_selector_" ^ Pp.plain (Sym.pp def.name))
    ^^ parens params
    ^^^ braces body


  let path_selector_ctx (ctx : Ctx.t) : Pp.document =
    Pp.(separate_map (twice hardline) (path_selector_def ctx) (List.map snd ctx))
end
