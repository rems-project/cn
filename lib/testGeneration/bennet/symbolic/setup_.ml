module CF = Cerb_frontend
module A = CF.AilSyntax
module IT = IndexTerms

module Make (AD : Domain.T) = struct
  module Smt = Smt.Make (AD)
  module Eval = Eval.Make (AD)
  module Stage4 = Stage4.Make (AD)
  module GT = Stage4.Term
  module Ctx = Stage4.Ctx
  module Def = Stage4.Def

  let generate_struct_setup (prog5 : unit Mucore.file) : Pp.document * Pp.document =
    let open Pp in
    (* Get all struct definitions *)
    let all_tag_defs = Pmap.bindings_list prog5.tagDefs in
    let struct_tags_with_defs =
      List.filter_map
        (fun (tag, tag_def) ->
           match tag_def with Mucore.StructDef pieces -> Some (tag, pieces) | _ -> None)
        all_tag_defs
    in
    let struct_tags = List.map fst struct_tags_with_defs in
    (* Generate struct declaration data for cn_structs_declare *)
    let struct_decl_data =
      List.map
        (fun (tag, pieces) ->
           let tag_name = Sym.pp_string_no_nums tag in
           let members =
             pieces
             |> List.filter_map (fun ({ member_or_padding; _ } : Memory.struct_piece) ->
               member_or_padding)
           in
           (tag_name, members))
        struct_tags_with_defs
    in
    (* Generate struct member declarations - these must be initialized at runtime *)
    let struct_decl_code =
      List.fold_left
        (fun acc (tag_name, members) ->
           let member_count = List.length members in
           if member_count = 0 then
             acc
           else
             acc
             ^^ !^"  struct_member_t "
             ^^ !^tag_name
             ^^ !^"_members["
             ^^ int member_count
             ^^ !^"];"
             ^^ hardline
             ^^ !^"  struct_decl_t "
             ^^ !^tag_name
             ^^ !^"_decl;"
             ^^ hardline
             ^^ !^"  bennet_vector(struct_member_t) "
             ^^ !^tag_name
             ^^ !^"_members_vector;"
             ^^ hardline
             ^^ List.fold_left
                  (fun member_acc (idx, (member_id, member_sct)) ->
                     let member_name = Id.get_string member_id in
                     member_acc
                     ^^ !^"  "
                     ^^ !^tag_name
                     ^^ !^"_members["
                     ^^ int idx
                     ^^ !^"].label = \""
                     ^^ !^member_name
                     ^^ !^"\";"
                     ^^ hardline
                     ^^ !^"  "
                     ^^ !^tag_name
                     ^^ !^"_members["
                     ^^ int idx
                     ^^ !^"].base_type = "
                     ^^ Smt.convert_basetype (Memory.bt_of_sct member_sct)
                     ^^ !^";"
                     ^^ hardline)
                  empty
                  (List.mapi (fun i x -> (i, x)) members)
             ^^ !^"  bennet_vector_init(struct_member_t)(&"
             ^^ !^tag_name
             ^^ !^"_members_vector);"
             ^^ hardline
             ^^ List.fold_left
                  (fun push_acc (idx, (_member_id, _member_sct)) ->
                     push_acc
                     ^^ !^"  bennet_vector_push(struct_member_t)(&"
                     ^^ !^tag_name
                     ^^ !^"_members_vector, "
                     ^^ !^tag_name
                     ^^ !^"_members["
                     ^^ int idx
                     ^^ !^"]);"
                     ^^ hardline)
                  empty
                  (List.mapi (fun i x -> (i, x)) members)
             ^^ !^"    "
             ^^ !^tag_name
             ^^ !^"_decl.members = "
             ^^ !^tag_name
             ^^ !^"_members;"
             ^^ hardline
             ^^ !^"    "
             ^^ !^tag_name
             ^^ !^"_decl.member_count = "
             ^^ int member_count
             ^^ !^";"
             ^^ hardline)
        empty
        struct_decl_data
    in
    (* Generate handler functions for all structs *)
    let all_handlers =
      List.fold_left
        (fun acc tag -> acc ^^ Eval.generate_struct_handlers prog5 tag ^^ hardline)
        empty
        struct_tags
    in
    (* Generate struct arrays and registration code *)
    let struct_init_code =
      (if List.length struct_decl_data > 0 then (
         let struct_names_array =
           !^"  const char* struct_names[] = {"
           ^^ hardline
           ^^ List.fold_left
                (fun acc (tag_name, _) ->
                   acc ^^ !^"    \"" ^^ !^tag_name ^^ !^"\"," ^^ hardline)
                empty
                struct_decl_data
           ^^ !^"  };"
           ^^ hardline
         in
         let struct_decls_array =
           !^"  struct_decl_t* struct_decls[] = {"
           ^^ hardline
           ^^ List.fold_left
                (fun acc (tag_name, _) ->
                   acc ^^ !^"    &" ^^ !^tag_name ^^ !^"_decl," ^^ hardline)
                empty
                struct_decl_data
           ^^ !^"  };"
           ^^ hardline
         in
         let cn_structs_declare_call =
           !^"  cn_structs_declare(s, struct_names, struct_decls, "
           ^^ int (List.length struct_decl_data)
           ^^ !^");"
           ^^ hardline
           ^^ hardline
         in
         struct_names_array ^^ struct_decls_array ^^ cn_structs_declare_call)
       else
         empty)
      ^^ List.fold_left
           (fun acc tag ->
              let tag_name = Sym.pp_string_no_nums tag in
              acc
              ^^ !^"  cn_struct_data "
              ^^ !^tag_name
              ^^ !^"_data = {"
              ^^ hardline
              ^^ !^"    .create_struct = create_struct_"
              ^^ !^tag_name
              ^^ !^","
              ^^ hardline
              ^^ !^"    .get_member = get_member_"
              ^^ !^tag_name
              ^^ !^","
              ^^ hardline
              ^^ !^"    .update_member = update_member_"
              ^^ !^tag_name
              ^^ !^","
              ^^ hardline
              ^^ !^"    .default_struct = default_struct_"
              ^^ !^tag_name
              ^^ !^"_cn_smt,"
              ^^ hardline
              ^^ !^"    .members = "
              ^^ !^tag_name
              ^^ !^"_members_vector"
              ^^ hardline
              ^^ !^"  };"
              ^^ hardline
              ^^ !^"  cn_register_struct(\""
              ^^ !^tag_name
              ^^ !^"\", &"
              ^^ !^tag_name
              ^^ !^"_data);"
              ^^ hardline
              ^^ hardline)
           empty
           struct_tags
    in
    (* Return handlers separately from init code *)
    (all_handlers, struct_decl_code ^^ hardline ^^ struct_init_code)


  let generate_record_setup (_prog5 : unit Mucore.file) : Pp.document * Pp.document =
    let open Pp in
    (*
       * Get all record types discovered by Fulminate during compilation.
     * The Fulminate cn_to_ail.ml module maintains a global `records` map
     * that tracks all BaseTypes.Record instances encountered during
     * CN -> AIL transformation.
    *)
    let all_records =
      Fulminate.Cn_to_ail.RecordMap.bindings !Fulminate.Cn_to_ail.records
    in
    if List.length all_records = 0 then
      (empty, empty)
    else (
      (* Generate record setup similar to struct setup *)
      let record_handlers_and_data =
        List.fold_left
          (fun acc (members, sym) ->
             let record_name = Sym.pp_string_no_nums sym in
             (* Generate handler functions for this record type *)
             let create_fn_name = "create_record_" ^ record_name in
             let get_fn_name = "get_member_" ^ record_name in
             let update_fn_name = "update_member_" ^ record_name in
             let default_fn_name = "default_record_" ^ record_name in
             (* Create record handler functions *)
             let create_fn =
               !^"static inline void* "
               ^^ !^create_fn_name
               ^^ !^"(bennet_hash_table(const_char_ptr, void_ptr) * members) {"
               ^^ hardline
               ^^ !^"  struct "
               ^^ !^record_name
               ^^ !^"* record = malloc(sizeof(*record));"
               ^^ hardline
               ^^ List.fold_left
                    (fun acc_inner (id, _) ->
                       let member_name = Id.get_string id in
                       acc_inner
                       ^^ !^"  bennet_optional(void_ptr) "
                       ^^ !^member_name
                       ^^ !^"_opt = "
                       ^^ !^"bennet_hash_table_get(const_char_ptr, void_ptr)(members, \""
                       ^^ !^member_name
                       ^^ !^"\");"
                       ^^ hardline
                       ^^ !^"  if (bennet_optional_is_some("
                       ^^ !^member_name
                       ^^ !^"_opt)) {"
                       ^^ hardline
                       ^^ !^"    record->"
                       ^^ !^member_name
                       ^^ !^" = bennet_optional_unwrap("
                       ^^ !^member_name
                       ^^ !^"_opt);"
                       ^^ hardline
                       ^^ !^"  }"
                       ^^ hardline)
                    empty
                    members
               ^^ !^"  return record;"
               ^^ hardline
               ^^ !^"}"
               ^^ hardline
               ^^ hardline
             in
             let get_fn =
               !^"static inline void* "
               ^^ !^get_fn_name
               ^^ !^"(void* record_val, const char* member_name) {"
               ^^ hardline
               ^^ !^"  struct "
               ^^ !^record_name
               ^^ !^"* record = (struct "
               ^^ !^record_name
               ^^ !^"*)record_val;"
               ^^ hardline
               ^^ List.fold_left
                    (fun acc_inner (id, _) ->
                       let member_name = Id.get_string id in
                       acc_inner
                       ^^ !^"  if (strcmp(member_name, \""
                       ^^ !^member_name
                       ^^ !^"\") == 0) {"
                       ^^ hardline
                       ^^ !^"    return record->"
                       ^^ !^member_name
                       ^^ !^";"
                       ^^ hardline
                       ^^ !^"  }"
                       ^^ hardline)
                    empty
                    members
               ^^ !^"  return NULL;"
               ^^ hardline
               ^^ !^"}"
               ^^ hardline
               ^^ hardline
             in
             let update_fn =
               !^"static inline void* "
               ^^ !^update_fn_name
               ^^ !^"(void* record_val, const char* member_name, void* new_value) {"
               ^^ hardline
               ^^ !^"  struct "
               ^^ !^record_name
               ^^ !^"* old_record = (struct "
               ^^ !^record_name
               ^^ !^"*)record_val;"
               ^^ hardline
               ^^ !^"  struct "
               ^^ !^record_name
               ^^ !^"* new_record = malloc(sizeof(*new_record));"
               ^^ hardline
               ^^ !^"  // Copy all existing members"
               ^^ hardline
               ^^ List.fold_left
                    (fun acc_inner (id, _) ->
                       let member_name_local = Id.get_string id in
                       acc_inner
                       ^^ !^"  new_record->"
                       ^^ !^member_name_local
                       ^^ !^" = old_record->"
                       ^^ !^member_name_local
                       ^^ !^";"
                       ^^ hardline)
                    empty
                    members
               ^^ !^"  // Update the specified member"
               ^^ hardline
               ^^ List.fold_left
                    (fun acc_inner (id, _) ->
                       let member_name_local = Id.get_string id in
                       acc_inner
                       ^^ !^"  if (strcmp(member_name, \""
                       ^^ !^member_name_local
                       ^^ !^"\") == 0) {"
                       ^^ hardline
                       ^^ !^"    new_record->"
                       ^^ !^member_name_local
                       ^^ !^" = new_value;"
                       ^^ hardline
                       ^^ !^"  }"
                       ^^ hardline)
                    empty
                    members
               ^^ !^"  return new_record;"
               ^^ hardline
               ^^ !^"}"
               ^^ hardline
               ^^ hardline
             in
             let default_fn =
               !^"static inline void* "
               ^^ !^default_fn_name
               ^^ !^"(void) {"
               ^^ hardline
               ^^ !^"  struct "
               ^^ !^record_name
               ^^ !^"* record = malloc(sizeof(*record));"
               ^^ hardline
               ^^ List.fold_left
                    (fun acc_inner (id, _) ->
                       let member_name = Id.get_string id in
                       acc_inner
                       ^^ !^"  record->"
                       ^^ !^member_name
                       ^^ !^" = NULL;"
                       ^^ hardline)
                    empty
                    members
               ^^ !^"  return record;"
               ^^ hardline
               ^^ !^"}"
               ^^ hardline
               ^^ hardline
             in
             acc ^^ create_fn ^^ get_fn ^^ update_fn ^^ default_fn)
          empty
          all_records
      in
      (* Generate record registration code *)
      let record_registration =
        List.fold_left
          (fun acc (members, sym) ->
             let record_name = Sym.pp_string_no_nums sym in
             let member_count = List.length members in
             let member_names =
               List.map (fun (id, _) -> "\"" ^ Id.get_string id ^ "\"") members
             in
             let simple_basetype_name bt = Pp.plain (Smt.convert_basetype bt) in
             let member_types =
               List.map (fun (_, bt) -> simple_basetype_name bt) members
             in
             acc
             ^^ !^"  // Record type: "
             ^^ !^record_name
             ^^ hardline
             ^^ !^"  const char* "
             ^^ !^record_name
             ^^ !^"_names[] = { "
             ^^ !^(String.concat ", " member_names)
             ^^ !^" };"
             ^^ hardline
             ^^ !^"  cn_base_type "
             ^^ !^record_name
             ^^ !^"_types[] = { "
             ^^ !^(String.concat ", " member_types)
             ^^ !^" };"
             ^^ hardline
             ^^ !^"  record_hash_t "
             ^^ !^record_name
             ^^ !^"_hash = cn_record_member_hash("
             ^^ int member_count
             ^^ !^", "
             ^^ !^record_name
             ^^ !^"_names, "
             ^^ !^record_name
             ^^ !^"_types);"
             ^^ hardline
             ^^ !^"  struct_member_t "
             ^^ !^record_name
             ^^ !^"_members["
             ^^ int member_count
             ^^ !^"];"
             ^^ hardline
             ^^ !^"  bennet_vector(struct_member_t) "
             ^^ !^record_name
             ^^ !^"_members_vector;"
             ^^ hardline
             ^^ List.fold_left
                  (fun acc_inner ((id, bt), idx) ->
                     let member_name = Id.get_string id in
                     acc_inner
                     ^^ !^"  "
                     ^^ !^record_name
                     ^^ !^"_members["
                     ^^ int idx
                     ^^ !^"].label = \""
                     ^^ !^member_name
                     ^^ !^"\";"
                     ^^ hardline
                     ^^ !^"  "
                     ^^ !^record_name
                     ^^ !^"_members["
                     ^^ int idx
                     ^^ !^"].base_type = "
                     ^^ !^(simple_basetype_name bt)
                     ^^ !^";"
                     ^^ hardline)
                  empty
                  (List.mapi (fun i m -> (m, i)) members)
             ^^ !^"  bennet_vector_init(struct_member_t)(&"
             ^^ !^record_name
             ^^ !^"_members_vector);"
             ^^ hardline
             ^^ List.fold_left
                  (fun acc_inner (_, idx) ->
                     acc_inner
                     ^^ !^"  bennet_vector_push(struct_member_t)(&"
                     ^^ !^record_name
                     ^^ !^"_members_vector, "
                     ^^ !^record_name
                     ^^ !^"_members["
                     ^^ int idx
                     ^^ !^"]);"
                     ^^ hardline)
                  empty
                  (List.mapi (fun i m -> (m, i)) members)
             ^^ !^"  cn_record_data "
             ^^ !^record_name
             ^^ !^"_data = {"
             ^^ hardline
             ^^ !^"    .create_record = create_record_"
             ^^ !^record_name
             ^^ !^","
             ^^ hardline
             ^^ !^"    .get_member = get_member_"
             ^^ !^record_name
             ^^ !^","
             ^^ hardline
             ^^ !^"    .update_member = update_member_"
             ^^ !^record_name
             ^^ !^","
             ^^ hardline
             ^^ !^"    .default_record = default_record_"
             ^^ !^record_name
             ^^ !^","
             ^^ hardline
             ^^ !^"    .members = "
             ^^ !^record_name
             ^^ !^"_members_vector"
             ^^ hardline
             ^^ !^"  };"
             ^^ hardline
             ^^ !^"  cn_register_record("
             ^^ !^record_name
             ^^ !^"_hash, &"
             ^^ !^record_name
             ^^ !^"_data);"
             ^^ hardline
             ^^ hardline)
          empty
          all_records
      in
      (record_handlers_and_data, record_registration))


  let generate_datatype_setup (prog5 : unit Mucore.file) : Pp.document * Pp.document =
    (* Graph modules for datatype dependency analysis *)
    let module G = Graph.Persistent.Digraph.Concrete (Sym) in
    let module Components = Graph.Components.Make (G) in
    let open Pp in
    (* Extract datatypes from prog5 *)
    let datatypes = prog5.datatypes in
    (* Generate datatype handlers *)
    let datatype_handlers = Eval.generate_datatype_handlers prog5 in
    (* Generate forward declarations for destructor functions *)
    let destructor_declarations =
      if List.length datatypes = 0 then
        empty
      else
        !^"// Forward declarations for datatype destructor functions"
        ^^ hardline
        ^^ List.fold_left
             (fun acc (dt_sym, _dt_def) ->
                let dt_name = Sym.pp_string_no_nums dt_sym in
                let destructor_fn_name = "destruct_constructor_" ^ dt_name in
                acc
                ^^ !^"void** "
                ^^ !^destructor_fn_name
                ^^ !^"(const char* ctor_name, void* value);"
                ^^ hardline)
             empty
             datatypes
        ^^ hardline
    in
    if List.length datatypes = 0 then (* Empty arrays for no datatypes *)
      ( empty,
        !^"const char **datatype_order_empty[1] = {NULL};"
        ^^ hardline
        ^^ !^"size_t group_sizes_empty[1] = {0};"
        ^^ hardline
        ^^ !^"dt_info_t *all_datatype_infos_empty[1] = {NULL};"
        ^^ hardline
        ^^ !^"dt_constr_info_t **all_constr_infos_empty[1] = {NULL};"
        ^^ hardline
        ^^ !^"cn_datatypes_declare(s, datatype_order_empty, 0, group_sizes_empty, \
              all_datatype_infos_empty, all_constr_infos_empty);"
        ^^ hardline
        ^^ !^"// No destructor registrations needed for empty datatypes" )
    else (
      (* Build dependency graph *)
      (* Helper: get datatypes referenced in a BaseType *)
      let rec dts_in_bt bt =
        match bt with
        | BaseTypes.Datatype tag -> [ tag ]
        | _ -> List.concat_map dts_in_bt (BaseTypes.contained bt)
      in
      let graph = G.empty in
      let graph =
        List.fold_left (fun graph (dt, _) -> G.add_vertex graph dt) graph datatypes
      in
      let graph =
        List.fold_left
          (fun graph (dt, (dt_def : Mucore.datatype)) ->
             let deps =
               List.concat_map
                 (fun (_ctor, args) -> List.concat_map (fun (_, bt) -> dts_in_bt bt) args)
                 dt_def.cases
             in
             List.fold_left (fun graph dt' -> G.add_edge graph dt dt') graph deps)
          graph
          datatypes
      in
      let sccs = Components.scc_list graph in
      (* Generate C code for each SCC group *)
      let group_count = List.length sccs in
      (* For each group, generate datatype names array *)
      let datatype_order_arrays =
        List.mapi
          (fun group_idx group ->
             let group_size = List.length group in
             !^"const char* datatype_group_"
             ^^ int group_idx
             ^^ !^"_names["
             ^^ int group_size
             ^^ !^"] = {"
             ^^ hardline
             ^^ (List.fold_left
                   (fun acc dt_sym ->
                      acc
                      ^^ !^"  \""
                      ^^ !^(Sym.pp_string_no_nums dt_sym)
                      ^^ !^"\","
                      ^^ hardline)
                   empty
                   group
                 |> nest 2)
             ^^ !^"};")
          sccs
      in
      (* For each group, generate dt_info_t structures *)
      let dt_info_arrays =
        List.mapi
          (fun group_idx group ->
             !^"dt_info_t dt_infos_g"
             ^^ int group_idx
             ^^ !^"[] = {"
             ^^ hardline
             ^^ (List.fold_left
                   (fun acc dt_sym ->
                      let dt_def =
                        List.assoc Sym.equal dt_sym datatypes
                        |> fun (x : Mucore.datatype) -> x
                      in
                      let constr_count = List.length dt_def.cases in
                      let constr_names =
                        List.map
                          (fun (c, _) -> "\"" ^ Sym.pp_string_no_nums c ^ "\"")
                          dt_def.cases
                      in
                      acc
                      ^^ !^"  {"
                      ^^ hardline
                      ^^ !^"    .constrs = (const char*[]){"
                      ^^ !^(String.concat ", " constr_names)
                      ^^ !^"},"
                      ^^ hardline
                      ^^ !^"    .constr_count = "
                      ^^ int constr_count
                      ^^ hardline
                      ^^ !^"  },"
                      ^^ hardline)
                   empty
                   group
                 |> nest 2)
             ^^ !^"};")
          sccs
      in
      (* For each group, generate dt_constr_info_t structures *)
      let constr_info_arrays =
        List.mapi
          (fun group_idx group ->
             (* For each datatype in group *)
             let constr_infos =
               List.mapi
                 (fun dt_idx dt_sym ->
                    let dt_name = Sym.pp_string_no_nums dt_sym in
                    let dt_def =
                      List.assoc Sym.equal dt_sym datatypes
                      |> fun (x : Mucore.datatype) -> x
                    in
                    (* For each constructor *)
                    let constr_structs =
                      List.mapi
                        (fun c_idx (c_sym, params) ->
                           let ctor_name = Sym.pp_string_no_nums c_sym in
                           let fn_name =
                             "create_constructor_" ^ dt_name ^ "_" ^ ctor_name
                           in
                           let param_count = List.length params in
                           if param_count = 0 then
                             !^"dt_constr_info_t constr_g"
                             ^^ int group_idx
                             ^^ !^"_dt"
                             ^^ int dt_idx
                             ^^ !^"_c"
                             ^^ int c_idx
                             ^^ !^" = {.params = NULL, .param_count = 0, .constructor_fn \
                                   = "
                             ^^ !^fn_name
                             ^^ !^"};"
                           else (* Generate dt_param_t array *)
                             !^"dt_param_t constr_params_g"
                             ^^ int group_idx
                             ^^ !^"_dt"
                             ^^ int dt_idx
                             ^^ !^"_c"
                             ^^ int c_idx
                             ^^ !^"["
                             ^^ int param_count
                             ^^ !^"];"
                             ^^ hardline
                             ^^ List.fold_left
                                  (fun acc (p_idx, (param_id, param_bt)) ->
                                     acc
                                     ^^ !^"constr_params_g"
                                     ^^ int group_idx
                                     ^^ !^"_dt"
                                     ^^ int dt_idx
                                     ^^ !^"_c"
                                     ^^ int c_idx
                                     ^^ !^"["
                                     ^^ int p_idx
                                     ^^ !^"].label = \""
                                     ^^ !^(Id.get_string param_id)
                                     ^^ !^"\";"
                                     ^^ hardline
                                     ^^ !^"constr_params_g"
                                     ^^ int group_idx
                                     ^^ !^"_dt"
                                     ^^ int dt_idx
                                     ^^ !^"_c"
                                     ^^ int c_idx
                                     ^^ !^"["
                                     ^^ int p_idx
                                     ^^ !^"].base_type = "
                                     ^^ Smt.convert_basetype param_bt
                                     ^^ !^";"
                                     ^^ hardline)
                                  empty
                                  (List.mapi (fun i x -> (i, x)) params)
                             ^^ !^"dt_constr_info_t constr_g"
                             ^^ int group_idx
                             ^^ !^"_dt"
                             ^^ int dt_idx
                             ^^ !^"_c"
                             ^^ int c_idx
                             ^^ !^" = {.params = constr_params_g"
                             ^^ int group_idx
                             ^^ !^"_dt"
                             ^^ int dt_idx
                             ^^ !^"_c"
                             ^^ int c_idx
                             ^^ !^", .param_count = "
                             ^^ int param_count
                             ^^ !^", .constructor_fn = "
                             ^^ !^fn_name
                             ^^ !^"};")
                        dt_def.cases
                    in
                    separate hardline constr_structs
                    ^^ hardline
                    ^^ !^"dt_constr_info_t* constr_array_g"
                    ^^ int group_idx
                    ^^ !^"_dt"
                    ^^ int dt_idx
                    ^^ !^"["
                    ^^ int (List.length dt_def.cases)
                    ^^ !^"] = {"
                    ^^ hardline
                    ^^ (List.fold_left
                          (fun acc (c_idx, _) ->
                             acc
                             ^^ !^"  &constr_g"
                             ^^ int group_idx
                             ^^ !^"_dt"
                             ^^ int dt_idx
                             ^^ !^"_c"
                             ^^ int c_idx
                             ^^ !^","
                             ^^ hardline)
                          empty
                          (List.mapi (fun i x -> (i, x)) dt_def.cases)
                        |> nest 2)
                    ^^ !^"};")
                 group
             in
             separate hardline constr_infos
             ^^ hardline
             ^^ !^"dt_constr_info_t* all_constr_infos_g"
             ^^ int group_idx
             ^^ !^"[] = {"
             ^^ hardline
             ^^ (List.fold_left
                   (fun acc (dt_idx, dt_sym) ->
                      let dt_def =
                        List.assoc Sym.equal dt_sym datatypes
                        |> fun (x : Mucore.datatype) -> x
                      in
                      List.fold_left
                        (fun inner_acc (c_idx, _) ->
                           inner_acc
                           ^^ !^"  &constr_g"
                           ^^ int group_idx
                           ^^ !^"_dt"
                           ^^ int dt_idx
                           ^^ !^"_c"
                           ^^ int c_idx
                           ^^ !^","
                           ^^ hardline)
                        acc
                        (List.mapi (fun i x -> (i, x)) dt_def.cases))
                   empty
                   (List.mapi (fun i x -> (i, x)) group)
                 |> nest 2)
             ^^ !^"};")
          sccs
      in
      (* Generate top-level arrays *)
      let datatype_order_array =
        !^"const char** datatype_order["
        ^^ int group_count
        ^^ !^"] = {"
        ^^ hardline
        ^^ (List.fold_left
              (fun acc (idx, _) ->
                 acc ^^ !^"  datatype_group_" ^^ int idx ^^ !^"_names," ^^ hardline)
              empty
              (List.mapi (fun i x -> (i, x)) sccs)
            |> nest 2)
        ^^ !^"};"
      in
      let group_sizes_array =
        !^"size_t group_sizes["
        ^^ int group_count
        ^^ !^"] = {"
        ^^ hardline
        ^^ (List.fold_left
              (fun acc group ->
                 acc ^^ !^"  " ^^ int (List.length group) ^^ !^"," ^^ hardline)
              empty
              sccs
            |> nest 2)
        ^^ !^"};"
      in
      let all_dt_infos_array =
        !^"dt_info_t* all_datatype_infos["
        ^^ int group_count
        ^^ !^"] = {"
        ^^ hardline
        ^^ (List.fold_left
              (fun acc (idx, _) ->
                 acc ^^ !^"  dt_infos_g" ^^ int idx ^^ !^"," ^^ hardline)
              empty
              (List.mapi (fun i x -> (i, x)) sccs)
            |> nest 2)
        ^^ !^"};"
      in
      let all_constr_infos_array =
        !^"dt_constr_info_t** all_constr_infos["
        ^^ int group_count
        ^^ !^"] = {"
        ^^ hardline
        ^^ (List.fold_left
              (fun acc (idx, _) ->
                 acc ^^ !^"  all_constr_infos_g" ^^ int idx ^^ !^"," ^^ hardline)
              empty
              (List.mapi (fun i x -> (i, x)) sccs)
            |> nest 2)
        ^^ !^"};"
      in
      (* Combine everything *)
      (* Generate destructor registration calls *)
      let destructor_registrations =
        List.fold_left
          (fun acc (dt_sym, _dt_def) ->
             let dt_name = Sym.pp_string_no_nums dt_sym in
             let destructor_fn_name = "destruct_constructor_" ^ dt_name in
             acc
             ^^ !^"cn_register_datatype_destructor(\""
             ^^ !^dt_name
             ^^ !^"\", "
             ^^ !^destructor_fn_name
             ^^ !^");"
             ^^ hardline)
          (!^"// Register datatype destructors for pattern matching" ^^ hardline)
          datatypes
      in
      ( datatype_handlers ^^ destructor_declarations,
        separate hardline (datatype_order_arrays @ dt_info_arrays @ constr_info_arrays)
        ^^ hardline
        ^^ datatype_order_array
        ^^ hardline
        ^^ group_sizes_array
        ^^ hardline
        ^^ all_dt_infos_array
        ^^ hardline
        ^^ all_constr_infos_array
        ^^ hardline
        ^^ !^"cn_datatypes_declare(s, datatype_order, "
        ^^ int group_count
        ^^ !^", group_sizes, all_datatype_infos, all_constr_infos);"
        ^^ hardline
        ^^ hardline
        ^^ destructor_registrations ))


  let generate_function_setup
        (sigma : CF.GenTypes.genTypeCategory A.sigma)
        (prog5 : unit Mucore.file)
        (ctx : Ctx.t)
    : Pp.document * Pp.document
    =
    let open Pp in
    (* Collect all pure functions used in the generators *)
    let used_functions =
      Ctx.fold
        (fun acc _sym (gd : Def.t) -> Sym.Set.union acc (GT.get_pure_functions gd.body))
        Sym.Set.empty
        ctx
    in
    (* Build function dependency graph to find transitive function calls *)
    let get_function_calls (fn_def : Definition.Function.t) : Sym.Set.t =
      match fn_def.Definition.Function.body with
      | Definition.Function.Def body | Definition.Function.Rec_Def body ->
        IT.preds_of body
      | Definition.Function.Uninterp -> Sym.Set.empty
    in
    let module G = Graph.Persistent.Digraph.Concrete (Sym) in
    let func_graph =
      List.fold_left
        (fun g (fn_sym, fn_def) ->
           Sym.Set.fold
             (fun called_sym g' -> G.add_edge g' fn_sym called_sym)
             (get_function_calls fn_def)
             g)
        G.empty
        prog5.logical_predicates
    in
    (* Compute transitive closure *)
    let module Oper = Graph.Oper.P (G) in
    let closure = Oper.transitive_closure func_graph in
    (* Filter logical_functions to include directly used + transitively called *)
    let logical_functions =
      List.filter
        (fun (fn_sym, _) ->
           (* Include if directly used *)
           Sym.Set.mem fn_sym used_functions
           (* OR if transitively called from any used function *)
           || Sym.Set.exists
                (fun used_sym -> G.mem_edge closure used_sym fn_sym)
                used_functions)
        prog5.logical_predicates
    in
    if List.length logical_functions = 0 then
      (empty, empty)
    else (
      let function_handlers = Eval.generate_function_handlers logical_functions in
      let function_registrations =
        List.fold_left
          (fun acc (fn_sym, fn_def) ->
             let fn_name = Sym.pp_string_no_nums fn_sym in
             let arg_count = List.length fn_def.Definition.Function.args in
             let return_bt = fn_def.Definition.Function.return_bt in
             let handler_name = "cn_func_" ^ fn_name in
             match fn_def.Definition.Function.body with
             | Definition.Function.Def body | Definition.Function.Rec_Def body ->
               let is_recursive =
                 match fn_def.Definition.Function.body with
                 | Definition.Function.Def _ -> false
                 | Definition.Function.Rec_Def _ -> true
                 | Definition.Function.Uninterp -> assert false (* unreachable *)
               in
               (* Generate cn_register_func call for defined functions *)
               let arg_binders_code =
                 if arg_count = 0 then
                   !^"NULL"
                 else
                   !^"(cn_arg_binder[]){"
                   ^^ separate_map
                        (comma ^^ space)
                        (fun (arg_sym, arg_bt) ->
                           braces
                             (!^".sym = "
                              ^^ Smt.convert_sym arg_sym
                              ^^ comma
                              ^^^ !^".bt = "
                              ^^ Smt.convert_basetype arg_bt))
                        fn_def.Definition.Function.args
                   ^^ !^"}"
               in
               (* Generate code block with variable declarations and cn_register_func call *)
               let function_reg_block =
                 if arg_count = 0 then
                   (* No arguments, just call cn_register_func directly *)
                   !^"  // Register function: "
                   ^^ !^fn_name
                   ^^ hardline
                   ^^ !^"  cn_register_func("
                   ^^ Smt.convert_sym fn_sym
                   ^^ comma
                   ^^^ !^handler_name
                   ^^ comma
                   ^^^ !^"NULL, 0"
                   ^^ comma
                   ^^^ Smt.convert_basetype return_bt
                   ^^ comma
                   ^^^ !^(if is_recursive then "true" else "false")
                   ^^ comma
                   ^^^ Smt.convert_indexterm sigma body
                   ^^ comma
                   ^^^ !^"s);"
                   ^^ hardline
                 else (* With arguments, create a scope with variable declarations *)
                   !^"  // Register function: "
                   ^^ !^fn_name
                   ^^ hardline
                   ^^ !^"  {"
                   ^^ hardline
                   ^^ separate_map
                        hardline
                        (fun (arg_sym, arg_bt) ->
                           let arg_name = Sym.pp_string_no_nums arg_sym in
                           !^"    cn_term *"
                           ^^ !^arg_name
                           ^^ !^" = cn_smt_sym"
                           ^^ parens
                                (Smt.convert_sym arg_sym
                                 ^^ comma
                                 ^^^ Smt.convert_basetype arg_bt)
                           ^^ !^";")
                        fn_def.Definition.Function.args
                   ^^ hardline
                   ^^ !^"    cn_term *body = "
                   ^^ Smt.convert_indexterm sigma body
                   ^^ !^";"
                   ^^ hardline
                   ^^ !^"    cn_register_func("
                   ^^ Smt.convert_sym fn_sym
                   ^^ comma
                   ^^^ !^handler_name
                   ^^ comma
                   ^^^ arg_binders_code
                   ^^ comma
                   ^^^ int arg_count
                   ^^ comma
                   ^^^ Smt.convert_basetype return_bt
                   ^^ comma
                   ^^^ !^(if is_recursive then "true" else "false")
                   ^^ !^", body, s);"
                   ^^ hardline
                   ^^ !^"  }"
                   ^^ hardline
               in
               acc ^^ function_reg_block ^^ hardline
             | Definition.Function.Uninterp ->
               (* For uninterpreted functions, still call cn_register_func but with NULL body *)
               (* This will trigger an error message at registration time *)
               acc
               ^^ !^"  // Uninterpreted function (will error): "
               ^^ !^fn_name
               ^^ hardline
               ^^ !^"  cn_register_func("
               ^^ Smt.convert_sym fn_sym
               ^^ comma
               ^^^ !^"NULL" (* NULL handler - will trigger error *)
               ^^ comma
               ^^^ !^"NULL, 0"
               ^^ comma
               ^^^ Smt.convert_basetype return_bt
               ^^ comma
               ^^^ !^"false"
               ^^ comma
               ^^^ !^"NULL" (* NULL body - will trigger error *)
               ^^ comma
               ^^^ !^"s);"
               ^^ hardline
               ^^ hardline)
          empty
          logical_functions
      in
      (function_handlers, function_registrations))


  let generate_smt_setup
        (sigma : CF.GenTypes.genTypeCategory A.sigma)
        (prog5 : unit Mucore.file)
        (ctx : Ctx.t)
    : Pp.document
    =
    let open Pp in
    let struct_handlers, struct_init = generate_struct_setup prog5 in
    let record_handlers, record_init = generate_record_setup prog5 in
    let datatype_handlers, datatype_init = generate_datatype_setup prog5 in
    let function_handlers, function_init = generate_function_setup sigma prog5 ctx in
    let declarations =
      [ !^"cn_tuple_declare(s);";
        !^"cn_option_declare(s);";
        struct_init;
        datatype_init;
        record_init;
        function_init
      ]
    in
    let init_fn =
      !^"void"
      ^^^ !^"cn_smt_solver_setup"
      ^^ parens !^"struct cn_smt_solver *s"
      ^^^ braces (nest 2 (hardline ^^ separate hardline declarations) ^^ hardline)
      ^^ hardline
    in
    struct_handlers
    ^^ hardline
    ^^ record_handlers
    ^^ hardline
    ^^ datatype_handlers
    ^^ hardline
    ^^ function_handlers
    ^^ hardline
    ^^ init_fn
end
