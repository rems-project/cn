module CF = Cerb_frontend
module A = CF.AilSyntax
module C = CF.Ctype
module BT = BaseTypes
module CtA = Fulminate.Cn_to_ail
module Records = Fulminate.Records

module Make (AD : Domain.T) = struct
  module Stage4 = Stage4.Make (AD)
  module Smt = Smt.Make (AD)
  module Ctx = Stage4.Ctx
  module Def = Stage4.Def

  let arbitrary_of_bt (prog5 : unit Mucore.file) (bt : BT.t) =
    let module Stage1 = Stage1.Make (AD) in
    Stage1.Term.arbitrary_ () bt (Locations.other __LOC__)
    |> Stage1.DestructArbitrary.transform_gt prog5
    |> Stage1.Term.upcast


  let gather_of_bt (sigma : CF.GenTypes.genTypeCategory A.sigma) ((sym, tm) : Sym.t * _)
    : Pp.document
    =
    let module Gather = Gather.Make (AD) in
    let res = tm |> Gather.Term.downcast |> Gather.gather_term sigma in
    let open Pp in
    separate hardline res.statements
    ^/^ !^"cn_term*"
    ^^^ (Sym.pp sym ^^ !^"_var")
    ^^^ equals
    ^^^ parens res.expression
    ^^ semi


  let concretize_of_bt
        (sigma : CF.GenTypes.genTypeCategory A.sigma)
        ((sym, tm) : Sym.t * _)
    : Pp.document
    =
    let module Concretize = Concretize.Make (AD) in
    let res = tm |> Concretize.Term.downcast |> Concretize.concretize_term sigma in
    let open Pp in
    separate hardline res.statements
    ^/^ !^"cn_term*"
    ^^^ (Sym.pp sym ^^ !^"_val")
    ^^^ equals
    ^^^ parens res.expression
    ^^ semi


  (** Convert spec generator to bennet_<generator name> function with symbolic variables *)
  let transform_def
        (sigma : CF.GenTypes.genTypeCategory A.sigma)
        (prog5 : unit Mucore.file)
        (def : Def.t)
    : Pp.document
    =
    let open Pp in
    let generator_name = Sym.pp_string def.name in
    let record_type = !^("cn_test_generator_" ^ generator_name ^ "_record") in
    let state_init =
      !^"assert(gen_state != NULL);"
      ^/^ !^"if"
      ^^^ parens !^"*gen_state == NULL"
      ^^^ braces !^"*(cn_trie**)gen_state = cn_trie_create();"
      ^/^ !^"cn_trie* unsat_paths = *(cn_trie**)gen_state;"
    in
    let vars_decl =
      !^"struct branch_history_queue branch_hist;"
      ^/^ !^"branch_history_init(&branch_hist);"
      ^/^ !^"bennet_rand_checkpoint checkpoint = bennet_rand_save();"
      ^/^ !^"struct cn_smt_solver* smt_solver = cn_smt_new_solver(SOLVER_Z3);"
    in
    let select_path =
      !^"cn_smt_path_selector_"
      ^^ !^generator_name
      ^^ parens !^"&branch_hist, unsat_paths"
      ^^ !^";"
    in
    (* Initialize symbolic execution context *)
    let context_init = !^"cn_smt_gather_init();" in
    let destructed_vars = def.iargs |> List.map_snd (arbitrary_of_bt prog5) in
    (* Generate symbolic variable declarations for each argument *)
    let symbolic_vars =
      destructed_vars |> List.map (gather_of_bt sigma) |> Pp.separate Pp.hardline
    in
    (* Generate call to the corresponding cn_smt_gather_<generator name> function with symbolic variables *)
    let gather_constraints =
      let smt_args =
        def.iargs
        |> List.map (fun (sym, _) -> !^(Sym.pp_string sym ^ "_var"))
        |> Pp.separate_map (!^"," ^^^ Pp.space) (fun x -> x)
      in
      let all_args =
        if List.length def.iargs > 0 then
          !^"&branch_hist" ^^ !^"," ^^^ Pp.space ^^ smt_args
        else
          !^"&branch_hist"
      in
      let reset_or_new_solver =
        if TestGenConfig.is_just_reset_solver () then
          !^"cn_smt_solver_reset(smt_solver);"
        else
          !^"stop_solver(smt_solver);" ^/^ !^"smt_solver = cn_smt_new_solver(SOLVER_Z3);"
      in
      reset_or_new_solver
      ^/^ !^"cn_smt_solver_setup(smt_solver);"
      ^/^ !^"cn_smt_gather_"
      ^^ !^generator_name
      ^^ Pp.parens all_args
      ^^ !^";"
    in
    let check_sat =
      !^"branch_history_rewind(&branch_hist);"
      ^/^ !^"result = cn_smt_gather_model(smt_solver);"
      ^/^ !^"if"
      ^^^ parens !^"result != CN_SOLVER_SAT"
      ^^^ braces
            (!^"assert(result == CN_SOLVER_UNSAT);"
             ^/^ !^"branch_history_update_trie(&branch_hist, unsat_paths);"
             ^/^ !^"branch_history_clear(&branch_hist);"
             ^/^ !^"attempts++;")
    in
    (* Initialize concretization context *)
    let conc_context_init =
      !^"/* Concretize input */" ^/^ !^"cn_smt_concretize_init();"
    in
    (* Generate symbolic variable declarations for each argument *)
    let concrete_vars =
      destructed_vars |> List.map (concretize_of_bt sigma) |> Pp.separate Pp.hardline
    in
    let concretize_model =
      let conc_args =
        def.iargs
        |> List.map (fun (sym, _) -> !^(Sym.pp_string sym ^ "_val"))
        |> Pp.separate_map (!^"," ^^^ Pp.space) (fun x -> x)
      in
      !^"bennet_rand_restore(checkpoint);"
      ^^^ !^"branch_history_rewind(&branch_hist);"
      ^^^ (!^"cn_smt_concretize_"
           ^^ !^generator_name
           ^^ Pp.parens
                (!^"smt_solver" ^^ comma ^^^ !^"&branch_hist" ^^ comma ^^^ conc_args)
           ^^ !^";")
      ^/^ !^"if (bennet_failure_get_failure_type() != BENNET_FAILURE_NONE)"
      ^^^ braces !^"stop_solver(smt_solver); return NULL;"
    in
    (* Generate struct building and return - create default values for all fields *)
    let struct_fields =
      def.iargs
      |> List.map (fun (sym, bt) ->
        dot
        ^^ Sym.pp sym
        ^^^ equals
        ^^^ parens
              CF.Pp_ail.(
                with_executable_spec (pp_ctype C.no_qualifiers) (CtA.bt_to_ail_ctype bt))
        ^^ !^"cn_smt_concretize_eval_term"
        ^^ parens (!^"smt_solver" ^^ comma ^^^ Sym.pp sym ^^ !^"_val")
        ^^ comma)
      |> Pp.separate Pp.hardline
    in
    let stop_solver = !^"stop_solver(smt_solver);" in
    let result_struct =
      (record_type ^^ star)
      ^^^ !^"result_struct"
      ^^^ equals
      ^^^ (!^"malloc" ^^ parens (!^"sizeof" ^^ parens record_type))
      ^^ semi
      ^/^ !^"*result_struct"
      ^^^ equals
      ^^^ parens record_type
      ^/^ braces struct_fields
      ^^ semi
    in
    (* Combine everything into the function *)
    let max_attempts = 10 in
    (record_type ^^ !^"*")
    ^^^ (!^("cn_test_generator_" ^ generator_name) ^^ Pp.parens !^"void** gen_state")
    ^^^ !^"{"
    ^/^ state_init
    ^/^ vars_decl
    ^/^ !^"int attempts = 0;"
    ^/^ !^"enum cn_smt_solver_result result;"
    ^/^ (!^"do"
         ^^^ braces
               (hardline
                ^^ !^"/* Select path */"
                ^/^ select_path
                ^/^ !^"if (bennet_failure_get_failure_type() == BENNET_FAILURE_DEPTH)"
                ^^^ braces
                      (hardline
                       ^^ !^(Printf.sprintf "if (attempts == %d)" max_attempts)
                       ^^^ braces !^"stop_solver(smt_solver); return NULL;"
                       ^/^ !^"branch_history_clear(&branch_hist);"
                       ^/^ !^"bennet_failure_reset();"
                       ^/^ !^"attempts++;"
                       ^/^ !^"continue;")
                ^/^ !^"/* Gather constraints */"
                ^/^ context_init
                ^/^ symbolic_vars
                ^/^ gather_constraints
                ^/^ check_sat)
         ^/^ !^(Printf.sprintf
                  "while (result != CN_SOLVER_SAT && attempts < %d);"
                  max_attempts))
    ^/^ (!^"if (result != CN_SOLVER_SAT)"
         ^^^ braces
               (!^"bennet_failure_set_failure_type(BENNET_FAILURE_UNSAT);"
                ^/^ stop_solver
                ^^^ !^"return NULL;"))
    ^/^ hardline
    ^/^ conc_context_init
    ^/^ concrete_vars
    ^/^ concretize_model
    ^/^ result_struct
    ^/^ stop_solver
    ^/^ !^"return result_struct;"
    ^/^ !^"}"
end
