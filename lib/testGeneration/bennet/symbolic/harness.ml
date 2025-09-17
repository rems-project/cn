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
    let record_type = !^("bennet_" ^ generator_name ^ "_record") in
    let vars_decl =
      !^"struct cn_smt_solver* smt_solver;"
      ^/^ !^"bennet_rand_checkpoint checkpoint;"
      ^/^ !^"struct branch_history_queue branch_hist;"
      ^/^ !^"branch_history_init(&branch_hist);"
      ^/^ !^"enum cn_smt_solver_result result;"
    in
    let select_path =
      !^"cn_smt_path_selector_" ^^ !^generator_name ^^ Pp.parens !^"&branch_hist" ^^ !^";"
    in
    (* Initialize symbolic execution context *)
    let context_init = !^"cn_smt_gather_init();" in
    (* Generate symbolic variable declarations for each argument *)
    let symbolic_vars =
      def.iargs
      |> List.map (fun (sym, bt) ->
        let var_name = Sym.pp_string sym in
        let var_type = Smt.convert_basetype bt in
        !^"cn_term* "
        ^^ Sym.pp sym
        ^^ !^"_var"
        ^^^ equals
        ^^^ parens
              (braces
                 (!^"CN_SMT_GATHER_LET_SYMBOLIC"
                  ^^ Pp.parens (!^var_name ^^ !^"," ^^^ var_type)
                  ^^ semi
                  ^/^ Sym.pp sym
                  ^^ semi))
        ^^ semi)
      |> Pp.separate Pp.hardline
    in
    (* Generate call to the corresponding cn_smt_gather_<generator name> function with symbolic variables *)
    let collect_constraints =
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
      !^"checkpoint = bennet_rand_save();"
      ^/^ !^"smt_solver = cn_smt_new_solver(SOLVER_Z3);"
      ^/^ !^"cn_smt_solver_setup(smt_solver);"
      ^/^ !^"cn_smt_gather_"
      ^^ !^generator_name
      ^^ Pp.parens all_args
      ^^ !^";"
    in
    let check_sat =
      !^"if (bennet_failure_get_failure_type() != BENNET_FAILURE_NONE)"
      ^^^ braces !^"stop_solver(smt_solver); return NULL;"
      ^/^ !^"result = cn_smt_gather_model(smt_solver);"
    in
    (* Initialize concretization context *)
    let conc_context_init =
      !^"/* Concretize input */"
      ^/^ !^"branch_history_rewind(&branch_hist);"
      ^/^ !^"cn_smt_concretize_init();"
    in
    (* Generate symbolic variable declarations for each argument *)
    let concrete_vars =
      def.iargs
      |> List.map (fun (sym, bt) ->
        let var_name = Sym.pp_string sym in
        let var_type = Smt.convert_basetype bt in
        !^"cn_term* "
        ^^ Sym.pp sym
        ^^ !^"_val"
        ^^^ equals
        ^^^ parens
              (braces
                 (!^"CN_SMT_CONCRETIZE_LET_SYMBOLIC"
                  ^^ Pp.parens (!^var_name ^^ !^"," ^^^ var_type)
                  ^^ semi
                  ^/^ Sym.pp sym
                  ^^ semi))
        ^^ semi)
      |> Pp.separate Pp.hardline
    in
    let concretize_model =
      let conc_args =
        def.iargs
        |> List.map (fun (sym, _) -> !^(Sym.pp_string sym ^ "_val"))
        |> Pp.separate_map (!^"," ^^^ Pp.space) (fun x -> x)
      in
      !^"bennet_rand_restore(checkpoint);"
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
        ^^ !^"cn_eval_term"
        ^^ parens (Sym.pp sym ^^ !^"_val")
        ^^ comma)
      |> Pp.separate Pp.hardline
    in
    let stop_solver = !^"stop_solver(smt_solver);" in
    let struct_return =
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
      ^/^ !^"return result_struct;"
    in
    (* Combine everything into the function *)
    (record_type ^^ !^"*")
    ^^^ (!^("bennet_" ^ generator_name) ^^ Pp.parens !^"void")
    ^^^ !^"{"
    ^/^ vars_decl
    ^/^ !^"/* Select path */"
    ^/^ select_path
    ^/^ !^"/* Gather constraints */"
    ^/^ (context_init ^/^ symbolic_vars ^/^ collect_constraints ^/^ check_sat)
    ^^^ !^"if (result != CN_SOLVER_SAT)"
    ^^^ braces
          (!^"bennet_failure_set_failure_type(BENNET_FAILURE_UNSAT);"
           ^/^ !^"stop_solver(smt_solver); return NULL;")
    ^/^ conc_context_init
    ^/^ concrete_vars
    ^/^ concretize_model
    ^/^ stop_solver
    ^/^ struct_return
    ^/^ !^"}"
end
