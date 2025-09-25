module CF = Cerb_frontend
module C = CF.Ctype
module BT = BaseTypes
module CtA = Fulminate.Cn_to_ail
module Records = Fulminate.Records

module Make (AD : Domain.T) = struct
  module Stage3 = Stage3.Make (AD)
  module Smt = Smt.Make (AD)
  module Ctx = Stage3.Ctx
  module Def = Stage3.Def

  (** Convert spec generator to bennet_<generator name> function with symbolic variables *)
  let transform_def (def : Def.t) : Pp.document =
    let open Pp in
    let generator_name = Sym.pp_string def.name in
    let record_type = !^("cn_test_generator_" ^ generator_name ^ "_record") in
    let state_init =
      !^"assert(gen_state != NULL);"
      ^/^ !^"if"
      ^^^ parens !^"*gen_state == NULL"
      ^/^ nest
            2
            (hardline
             ^^ braces
                  (nest
                     2
                     (hardline
                      ^^ !^"*(cn_trie**)gen_state = cn_trie_create();"
                      ^^ hardline)))
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
      ^^ parens (!^"&branch_hist" ^^ comma ^^^ !^"unsat_paths")
      ^^ semi
    in
    (* Initialize symbolic execution context *)
    let context_init = !^"cn_smt_gather_init();" in
    (* Generate symbolic variable declarations for each argument *)
    let symbolic_vars =
      def.iargs
      |> List.map (fun (sym, bt) ->
        let var_name = Sym.pp_string sym in
        let var_type = Smt.convert_basetype bt in
        !^"cn_term*"
        ^^^ Sym.pp sym
        ^^ !^"_var"
        ^^^ equals
        ^^^ parens
              (braces
                 (nest
                    2
                    (hardline
                     ^^ !^"CN_SMT_GATHER_LET_SYMBOLIC"
                     ^^ parens (!^var_name ^^ comma ^^^ var_type)
                     ^^ semi
                     ^/^ Sym.pp sym
                     ^^ semi)
                  ^^ hardline))
        ^^ semi)
      |> separate hardline
    in
    (* Generate call to the corresponding cn_smt_gather_<generator name> function with symbolic variables *)
    let gather_constraints =
      let smt_args =
        def.iargs
        |> List.map (fun (sym, _) -> !^(Sym.pp_string sym ^ "_var"))
        |> Pp.separate_map (comma ^^^ Pp.space) (fun x -> x)
      in
      let all_args =
        if List.length def.iargs > 0 then
          !^"&branch_hist" ^^ comma ^^^ Pp.space ^^ smt_args
        else
          !^"&branch_hist"
      in
      !^"cn_smt_solver_reset(smt_solver);"
      ^/^ !^"cn_smt_solver_setup(smt_solver);"
      ^/^ !^"cn_smt_gather_"
      ^^ !^generator_name
      ^^ Pp.parens all_args
      ^^ semi
    in
    let check_sat =
      !^"branch_history_rewind(&branch_hist);"
      ^/^ !^"result = cn_smt_gather_model(smt_solver);"
      ^/^ !^"if"
      ^^^ parens !^"result != CN_SOLVER_SAT"
      ^/^ nest
            2
            (hardline
             ^^ braces
                  (nest
                     2
                     (hardline
                      ^^ !^"assert(result == CN_SOLVER_UNSAT);"
                      ^/^ !^"branch_history_update_trie(&branch_hist, unsat_paths);"
                      ^/^ !^"branch_history_restore(&branch_hist, NULL);"
                      ^/^ !^"attempts++;")
                   ^^ hardline))
    in
    (* Initialize concretization context *)
    let conc_context_init =
      !^"/* Concretize input */" ^/^ !^"cn_smt_concretize_init();"
    in
    (* Generate symbolic variable declarations for each argument *)
    let concrete_vars =
      def.iargs
      |> List.map (fun (sym, bt) ->
        let var_name = Sym.pp_string sym in
        let var_type = Smt.convert_basetype bt in
        !^"cn_term*"
        ^^^ Sym.pp sym
        ^^ !^"_val"
        ^^^ equals
        ^^^ parens
              (braces
                 (nest
                    2
                    (hardline
                     ^^ !^"CN_SMT_CONCRETIZE_LET_SYMBOLIC"
                     ^^ parens (!^var_name ^^ comma ^^^ var_type)
                     ^^ semi
                     ^/^ Sym.pp sym
                     ^^ semi)
                  ^^ hardline))
        ^^ semi)
      |> separate hardline
    in
    let concretize_model =
      let conc_args =
        def.iargs
        |> List.map (fun (sym, _) -> !^(Sym.pp_string sym ^ "_val"))
        |> separate_map (comma ^^^ space) (fun x -> x)
      in
      !^"bennet_rand_restore(checkpoint);"
      ^/^ (!^"cn_smt_concretize_"
           ^^ !^generator_name
           ^^ parens (!^"smt_solver" ^^ comma ^^^ !^"&branch_hist" ^^ comma ^^^ conc_args)
           ^^ semi)
      ^/^ !^"if (bennet_failure_get_failure_type() != BENNET_FAILURE_NONE)"
      ^/^ nest
            2
            (hardline
             ^^ braces
                  (nest 2 (hardline ^^ !^"stop_solver(smt_solver);" ^/^ !^"return NULL;")
                   ^^ hardline))
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
        ^^ !^"cn_eval_term"
        ^^ parens (Sym.pp sym ^^ !^"_val")
        ^^ comma)
      |> separate hardline
    in
    let stop_solver = !^"stop_solver(smt_solver);" in
    let struct_return =
      (record_type ^^ !^"*")
      ^^^ !^"result_struct"
      ^^^ equals
      ^^^ (!^"malloc" ^^ parens (!^"sizeof" ^^ parens record_type))
      ^^ semi
      ^/^ !^"*result_struct"
      ^^^ equals
      ^^^ parens record_type
      ^/^ nest
            2
            (hardline
             ^^ braces (nest 2 (hardline ^^ struct_fields ^^ hardline) ^^ hardline))
      ^^ semi
      ^/^ !^"return result_struct;"
    in
    (* Combine everything into the function *)
    (record_type ^^ !^"*")
    ^^^ (!^("cn_test_generator_" ^ generator_name) ^^ parens (!^"void**" ^^^ !^"gen_state")
        )
    ^/^ braces
          (nest
             2
             (hardline
              ^^ state_init
              ^/^ hardline
              ^^ vars_decl
              ^/^ !^"int attempts = 0;"
              ^/^ !^"enum cn_smt_solver_result result;"
              ^/^ hardline
              ^^ (!^"do"
                  ^/^ nest
                        2
                        (hardline
                         ^^ braces
                              (nest
                                 2
                                 (hardline
                                  ^^ !^"/* Select path */"
                                  ^/^ select_path
                                  ^/^ !^"/* Gather constraints */"
                                  ^/^ context_init
                                  ^/^ symbolic_vars
                                  ^/^ gather_constraints
                                  ^/^ check_sat)
                               ^^ hardline))
                  ^^^ !^"while (result != CN_SOLVER_SAT && attempts < 10);")
              ^/^ (!^"if (result != CN_SOLVER_SAT)"
                   ^/^ nest
                         2
                         (hardline
                          ^^ braces
                               (nest
                                  2
                                  (hardline
                                   ^^ !^"bennet_failure_set_failure_type(BENNET_FAILURE_UNSAT);"
                                   ^/^ !^"stop_solver(smt_solver);"
                                   ^/^ !^"return NULL;")
                                ^^ hardline)))
              ^/^ hardline
              ^^ conc_context_init
              ^/^ concrete_vars
              ^/^ concretize_model
              ^/^ stop_solver
              ^/^ struct_return)
           ^^ hardline)
end
