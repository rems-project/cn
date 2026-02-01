module CF = Cerb_frontend
module A = CF.AilSyntax
module C = CF.Ctype
module BT = BaseTypes
module CtA = Fulminate.Cn_to_ail
module Records = Fulminate.Records

module Make (AD : Domain.T) = struct
  module Stage5 = Stage5.Make (AD)
  module Smt = Smt.Make (AD)
  module Ctx = Stage5.Ctx
  module Def = Stage5.Def

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
    let solver_extension =
      match TestGenConfig.get_smt_solver () with
      | TestGenConfig.Z3 -> !^"SOLVER_Z3"
      | TestGenConfig.CVC5 -> !^"SOLVER_CVC5"
    in
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
      ^/^ !^"struct cn_smt_solver* smt_solver = cn_smt_new_solver("
      ^^ solver_extension
      ^^ !^");"
    in
    let select_path =
      !^"cn_smt_path_selector_"
      ^^ !^generator_name
      ^^ parens !^"&branch_hist, unsat_paths"
      ^^ !^";"
    in
    let select_path_with_timing =
      !^"bennet_info_timing_start(\"darcy:select_path\");"
      ^/^ select_path
      ^/^ !^"bennet_info_timing_end(\"darcy:select_path\");"
    in
    (* Initialize symbolic execution context *)
    let gather_checkpoint_save =
      !^"cn_bump_frame_id gather_checkpoint = cn_bump_get_frame_id();"
    in
    let context_init = !^"cn_smt_gather_init();" in
    let destructed_vars = def.iargs |> List.map_snd (arbitrary_of_bt prog5) in
    (* Generate symbolic variable declarations for each argument *)
    let symbolic_vars =
      destructed_vars |> List.map (gather_of_bt sigma) |> Pp.separate Pp.hardline
    in
    (* Generate call to the corresponding cn_smt_gather_<generator name> function with symbolic variables *)
    let reset_or_new_solver =
      if TestGenConfig.is_just_reset_solver () then
        !^"cn_smt_solver_reset(smt_solver);"
      else
        !^"stop_solver(smt_solver);"
        ^/^ !^"smt_solver = cn_smt_new_solver("
        ^^ solver_extension
        ^^ !^");"
    in
    let solver_setup = !^"cn_smt_solver_setup(smt_solver);" in
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
      !^"cn_smt_gather_" ^^ !^generator_name ^^ Pp.parens all_args ^^ !^";"
    in
    let query_solver_section =
      !^"/* Query Solver */"
      ^/^ !^"bennet_info_timing_start(\"darcy:query_solver\");"
      ^/^ solver_setup
      ^/^ !^"result = cn_smt_gather_model(smt_solver);"
      ^/^ !^"bennet_info_timing_end(\"darcy:query_solver\");"
      ^/^ !^"branch_history_rewind(&branch_hist);"
    in
    let check_sat =
      !^"if"
      ^^^ parens !^"result != CN_SOLVER_SAT"
      ^^^ braces
            (!^"assert(result == CN_SOLVER_UNSAT);"
             ^/^ !^"branch_history_update_trie(&branch_hist, unsat_paths);"
             ^/^ !^"branch_history_clear(&branch_hist);"
             ^/^ reset_or_new_solver
             ^/^ !^"attempts++;")
    in
    let gather_checkpoint_restore = !^"cn_bump_free_after(gather_checkpoint);" in
    let gather_section_with_timing =
      gather_checkpoint_save
      ^/^ context_init
      ^/^ !^"bennet_info_timing_start(\"darcy:gather_constraints\");"
      ^/^ symbolic_vars
      ^/^ gather_constraints
      ^/^ !^"bennet_info_timing_end(\"darcy:gather_constraints\");"
    in
    (* Initialize concretization context *)
    let concretize_checkpoint_save =
      !^"cn_bump_frame_id concretize_checkpoint = cn_bump_get_frame_id();"
    in
    let conc_context_init = !^"cn_smt_concretize_init();" in
    (* Generate symbolic variable declarations for each argument *)
    let concrete_vars =
      destructed_vars |> List.map (concretize_of_bt sigma) |> Pp.separate Pp.hardline
    in
    let restore_and_rewind =
      !^"bennet_rand_restore(checkpoint);" ^^^ !^"branch_history_rewind(&branch_hist);"
    in
    let concretize_call =
      let conc_args =
        def.iargs
        |> List.map (fun (sym, _) -> !^(Sym.pp_string sym ^ "_val"))
        |> Pp.separate_map (!^"," ^^^ Pp.space) (fun x -> x)
      in
      !^"cn_smt_concretize_"
      ^^ !^generator_name
      ^^ Pp.parens (!^"smt_solver" ^^ comma ^^^ !^"&branch_hist" ^^ comma ^^^ conc_args)
      ^^ !^";"
    in
    (* Generate struct building and return - convert from CN types to C types *)
    let struct_fields =
      def.iargs
      |> List.map (fun (sym, bt) ->
        let cn_value_expr =
          parens
            CF.Pp_ail.(
              with_executable_spec (pp_ctype C.no_qualifiers) (CtA.bt_to_ail_ctype bt))
          ^^ !^"cn_smt_concretize_eval_term"
          ^^ parens (!^"smt_solver" ^^ comma ^^^ Sym.pp sym ^^ !^"_val")
        in
        let converted_expr =
          match CtA.get_conversion_from_fn_str bt with
          | Some conv_fn -> !^conv_fn ^^ parens cn_value_expr
          | None -> cn_value_expr
        in
        dot ^^ Sym.pp sym ^^^ equals ^^^ converted_expr ^^ comma)
      |> Pp.separate Pp.hardline
    in
    let stop_solver = !^"stop_solver(smt_solver);" in
    let result_struct_alloc =
      (record_type ^^ star)
      ^^^ !^"result_struct"
      ^^^ equals
      ^^^ (!^"malloc" ^^ parens (!^"sizeof" ^^ parens record_type))
      ^^ semi
    in
    let result_struct_init =
      !^"*result_struct"
      ^^^ equals
      ^^^ parens record_type
      ^/^ braces struct_fields
      ^^ semi
    in
    let concretize_checkpoint_restore = !^"cn_bump_free_after(concretize_checkpoint);" in
    let concretize_section_with_timing =
      !^"/* Concretize input */"
      ^/^ !^"bennet_info_timing_start(\"darcy:concretize_input\");"
      ^/^ concretize_checkpoint_save
      ^/^ conc_context_init
      ^/^ concrete_vars
      ^/^ restore_and_rewind
      ^/^ result_struct_alloc
      ^/^ concretize_call
      ^/^ result_struct_init
      ^/^ !^"bennet_info_timing_end(\"darcy:concretize_input\");"
      ^/^ stop_solver
      ^/^ concretize_checkpoint_restore
      ^/^ !^"if (bennet_failure_get_failure_type() != BENNET_FAILURE_NONE)"
      ^^^ braces !^"return NULL;"
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
                ^/^ select_path_with_timing
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
                ^/^ gather_section_with_timing
                ^/^ query_solver_section
                ^/^ check_sat
                ^/^ gather_checkpoint_restore)
         ^/^ !^(Printf.sprintf
                  "while (result != CN_SOLVER_SAT && attempts < %d);"
                  max_attempts))
    ^/^ (!^"if (result != CN_SOLVER_SAT)"
         ^^^ braces
               (!^"bennet_failure_set_failure_type(BENNET_FAILURE_UNSAT);"
                ^/^ stop_solver
                ^^^ !^"return NULL;"))
    ^/^ hardline
    ^/^ concretize_section_with_timing
    ^/^ !^"return result_struct;"
    ^/^ !^"}"
end
