module Make (AD : Domain.T) = struct
  module Smt = Smt.Make (AD)
  module Eval = Eval.Make (AD)

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
             ^^ (List.fold_left
                   (fun (member_acc, idx) (member_id, member_sct) ->
                      let member_name = Id.get_string member_id in
                      ( member_acc
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
                        ^^ hardline,
                        idx + 1 ))
                   (!^"", 0)
                   members
                 |> fst)
             ^^ !^"  bennet_vector_init(struct_member_t)(&"
             ^^ !^tag_name
             ^^ !^"_members_vector);"
             ^^ hardline
             ^^ (List.fold_left
                   (fun (push_acc, idx) (_member_id, _member_sct) ->
                      ( push_acc
                        ^^ !^"  bennet_vector_push(struct_member_t)(&"
                        ^^ !^tag_name
                        ^^ !^"_members_vector, "
                        ^^ !^tag_name
                        ^^ !^"_members["
                        ^^ int idx
                        ^^ !^"]);"
                        ^^ hardline,
                        idx + 1 ))
                   (!^"", 0)
                   members
                 |> fst)
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
        !^""
        struct_decl_data
    in
    (* Generate handler functions for all structs *)
    let all_handlers =
      List.fold_left
        (fun acc tag -> acc ^^ Eval.generate_struct_handlers prog5 tag ^^ hardline)
        !^""
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
                !^""
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
                !^""
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
         !^"")
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
           !^""
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
      (!^"", !^"")
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
               ^^ !^"  bennet_hash_table(const_char_ptr, void_ptr) * record = \
                     malloc(sizeof(*record));"
               ^^ hardline
               ^^ !^"  bennet_hash_table_init(const_char_ptr, void_ptr)(record, \
                     bennet_hash_const_char_ptr, bennet_eq_const_char_ptr);"
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
                       ^^ !^"    bennet_hash_table_set(const_char_ptr, void_ptr)(record, \
                             \""
                       ^^ !^member_name
                       ^^ !^"\", "
                       ^^ !^"bennet_optional_unwrap("
                       ^^ !^member_name
                       ^^ !^"_opt));"
                       ^^ hardline
                       ^^ !^"  }"
                       ^^ hardline)
                    !^""
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
               ^^ !^"  bennet_hash_table(const_char_ptr, void_ptr) * record = "
               ^^ !^"(bennet_hash_table(const_char_ptr, void_ptr) *)record_val;"
               ^^ hardline
               ^^ !^"  bennet_optional(void_ptr) member_opt = \
                     bennet_hash_table_get(const_char_ptr, void_ptr)(record, \
                     member_name);"
               ^^ hardline
               ^^ !^"  return bennet_optional_is_some(member_opt) ? \
                     bennet_optional_unwrap(member_opt) : NULL;"
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
               ^^ !^"  bennet_hash_table(const_char_ptr, void_ptr) * old_record = "
               ^^ !^"(bennet_hash_table(const_char_ptr, void_ptr) *)record_val;"
               ^^ hardline
               ^^ !^"  bennet_hash_table(const_char_ptr, void_ptr) * new_record = \
                     malloc(sizeof(*new_record));"
               ^^ hardline
               ^^ !^"  bennet_hash_table_init(const_char_ptr, void_ptr)(new_record, \
                     bennet_hash_const_char_ptr, bennet_eq_const_char_ptr);"
               ^^ hardline
               ^^ !^"  // Copy all existing members"
               ^^ hardline
               ^^ List.fold_left
                    (fun acc_inner (id, _) ->
                       let member_name = Id.get_string id in
                       acc_inner
                       ^^ !^"  if (strcmp(member_name, \""
                       ^^ !^member_name
                       ^^ !^"\") != 0) {"
                       ^^ hardline
                       ^^ !^"    bennet_optional(void_ptr) "
                       ^^ !^member_name
                       ^^ !^"_opt = "
                       ^^ !^"bennet_hash_table_get(const_char_ptr, void_ptr)(old_record, \
                             \""
                       ^^ !^member_name
                       ^^ !^"\");"
                       ^^ hardline
                       ^^ !^"    if (bennet_optional_is_some("
                       ^^ !^member_name
                       ^^ !^"_opt)) {"
                       ^^ hardline
                       ^^ !^"      bennet_hash_table_set(const_char_ptr, \
                             void_ptr)(new_record, \""
                       ^^ !^member_name
                       ^^ !^"\", "
                       ^^ !^"bennet_optional_unwrap("
                       ^^ !^member_name
                       ^^ !^"_opt));"
                       ^^ hardline
                       ^^ !^"    }"
                       ^^ hardline
                       ^^ !^"  }"
                       ^^ hardline)
                    !^""
                    members
               ^^ !^"  bennet_hash_table_set(const_char_ptr, void_ptr)(new_record, \
                     member_name, new_value);"
               ^^ hardline
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
               ^^ !^"  bennet_hash_table(const_char_ptr, void_ptr) * record = \
                     malloc(sizeof(*record));"
               ^^ hardline
               ^^ !^"  bennet_hash_table_init(const_char_ptr, void_ptr)(record, \
                     bennet_hash_const_char_ptr, bennet_eq_const_char_ptr);"
               ^^ hardline
               ^^ List.fold_left
                    (fun acc_inner (id, _) ->
                       let member_name = Id.get_string id in
                       acc_inner
                       ^^ !^"  bennet_hash_table_set(const_char_ptr, void_ptr)(record, \""
                       ^^ !^member_name
                       ^^ !^"\", NULL);"
                       ^^ hardline)
                    !^""
                    members
               ^^ !^"  return record;"
               ^^ hardline
               ^^ !^"}"
               ^^ hardline
               ^^ hardline
             in
             acc ^^ create_fn ^^ get_fn ^^ update_fn ^^ default_fn)
          !^""
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
             (* For member type arrays, we need proper constructor function calls *)
             let simple_basetype_name bt =
               match bt with
               | BaseTypes.Unit -> "cn_base_type_simple(CN_BASE_UNIT)"
               | BaseTypes.Bool -> "cn_base_type_simple(CN_BASE_BOOL)"
               | BaseTypes.Integer -> "cn_base_type_simple(CN_BASE_INTEGER)"
               | BaseTypes.Bits (BaseTypes.Signed, w) ->
                 "cn_base_type_bits(true, " ^ string_of_int w ^ ")"
               | BaseTypes.Bits (BaseTypes.Unsigned, w) ->
                 "cn_base_type_bits(false, " ^ string_of_int w ^ ")"
               | BaseTypes.Loc _ -> "cn_base_type_simple(CN_BASE_LOC)"
               | BaseTypes.Record _ -> "cn_base_type_simple(CN_BASE_RECORD)"
               | BaseTypes.Struct _ -> "cn_base_type_simple(CN_BASE_STRUCT)"
               | BaseTypes.Datatype _ -> "cn_base_type_simple(CN_BASE_DATATYPE)"
               | BaseTypes.Map _ -> "cn_base_type_simple(CN_BASE_MAP)"
               | BaseTypes.List _ -> "cn_base_type_simple(CN_BASE_LIST)"
               | BaseTypes.Tuple _ -> "cn_base_type_simple(CN_BASE_TUPLE)"
               | _ -> "cn_base_type_simple(CN_BASE_UNKNOWN)"
             in
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
                  !^""
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
                  !^""
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
          !^""
          all_records
      in
      (record_handlers_and_data, record_registration))


  let generate_smt_setup (prog5 : unit Mucore.file) : Pp.document =
    let open Pp in
    let struct_handlers, struct_init = generate_struct_setup prog5 in
    let record_handlers, record_init = generate_record_setup prog5 in
    let init_fn =
      !^"void"
      ^^^ !^"cn_smt_solver_setup"
      ^^ parens !^"struct cn_smt_solver *s"
      ^^^ braces (hardline ^^ struct_init ^^ hardline ^^ record_init)
      ^^ hardline
    in
    struct_handlers ^^ hardline ^^ record_handlers ^^ hardline ^^ init_fn
end
