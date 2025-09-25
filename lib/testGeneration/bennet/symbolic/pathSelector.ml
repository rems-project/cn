module CF = Cerb_frontend
module C = CF.Ctype
module IT = IndexTerms
module BT = BaseTypes
module LC = LogicalConstraints
module CtA = Fulminate.Cn_to_ail
module Records = Fulminate.Records

module Make (AD : Domain.T) = struct
  module Stage3 = Stage3.Make (AD)
  module Smt = Smt.Make (AD)
  module Ctx = Stage3.Ctx
  module Term = Stage3.Term
  module Def = Stage3.Def

  let bennet = Sym.fresh "bennet"

  (** Convert a generator term to CN-SMT path selection statements *)
  let rec path_selector_term (last_branch : Sym.t) (tm : Term.t) : Pp.document =
    let open Pp in
    let (GenTerms.Annot (tm_, (), bt, loc)) = tm in
    match tm_ with
    | `Arbitrary | `ArbitraryDomain _ | `Symbolic | `Return _ | `Map _ -> empty
    | `Call (fsym, _args) ->
      !^"CN_SMT_PATH_SELECTOR_CALL"
      ^^ parens (separate (comma ^^ space) [ Sym.pp last_branch; Sym.pp fsym ])
      ^^ semi
    | `Asgn (_, _, next_term) | `Assert (_, next_term) | `AssertDomain (_, next_term) ->
      path_selector_term last_branch next_term
    | `LetStar ((_, binding_term), body_term) ->
      let binding_result = path_selector_term last_branch binding_term in
      let body_result = path_selector_term last_branch body_term in
      binding_result ^/^ body_result
    | `ITE (condition, then_term, else_term) ->
      (* Convert if-then-else to Pick statement with recursive calls *)
      let then_branch = Term.assert_ (LC.T condition, then_term) () loc in
      let else_branch = Term.assert_ (LC.T (IT.not_ condition loc), else_term) () loc in
      path_selector_term last_branch (Term.pick_ [ then_branch; else_branch ] () bt loc)
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
        !^"CN_SMT_PATH_SELECTOR_PICK_BEGIN"
        ^^ nest
             2
             (parens
                (separate
                   (comma ^^ break 1)
                   [ Sym.pp result_var; Sym.pp tmp_var; Sym.pp last_branch; choices ]))
        ^^ semi
      in
      (* Generate case statements for each choice *)
      let cases =
        choice_terms
        |> List.mapi (fun i term ->
          let case_begin =
            !^"CN_SMT_PATH_SELECTOR_PICK_CASE_BEGIN" ^^ parens (int i) ^^ semi
          in
          let term_result = path_selector_term tmp_var term in
          let case_end = !^"CN_SMT_PATH_SELECTOR_PICK_CASE_END" ^^ parens empty ^^ semi in
          nest 2 (case_begin ^/^ term_result) ^/^ case_end)
        |> separate hardline
      in
      (* Generate the pick end macro call *)
      let pick_end =
        !^"CN_SMT_PATH_SELECTOR_PICK_END" ^^ parens (Sym.pp tmp_var) ^^ semi
      in
      (* Combine everything as single statement block *)
      let pick_stmt = pick_begin ^/^ nest 2 cases ^/^ pick_end in
      pick_stmt


  (** Convert generator definition to complete CN-SMT symbolic execution function *)
  let path_selector_def (def : Def.t) : Pp.document =
    let open Pp in
    let params =
      !^"struct branch_history_queue*"
      ^^^ !^"branch_hist"
      ^^ comma
      ^^ break 1
      ^^ !^"cn_trie*"
      ^^^ !^"unsat_paths"
    in
    let body_content =
      if def.spec then (
        (* For spec functions, generate normal path selector *)
        let term_result = path_selector_term bennet def.body in
        !^"CN_SMT_PATH_SELECTOR_INIT();" ^/^ term_result ^/^ !^"return;")
      else (
        let term_result = path_selector_term bennet def.body in
        !^"CN_SMT_PATH_SELECTOR_INIT();" ^/^ term_result ^/^ !^"return;")
    in
    !^"static void"
    ^^^ !^("cn_smt_path_selector_" ^ Pp.plain (Sym.pp def.name))
    ^^ nest 2 (parens params)
    ^^^ hardline
    ^^ braces (hardline ^^ nest 2 body_content ^^ hardline)


  let path_selector_ctx (ctx : Ctx.t) : Pp.document =
    Pp.(separate_map (twice hardline) path_selector_def (List.map snd ctx))
end
