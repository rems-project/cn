let generate_struct_handlers (prog5 : unit Mucore.file) (tag : Sym.t) : Pp.document =
  match Pmap.find tag prog5.tagDefs with
  | StructDef pieces ->
    let members =
      pieces
      |> List.filter_map (fun ({ member_or_padding; _ } : Memory.struct_piece) ->
        member_or_padding)
    in
    let open Pp in
    let tag_name = Sym.pp_string tag in
    let struct_name = "struct " ^ tag_name ^ "_cn" in
    (* Generate create_struct function *)
    let create_fn_name = "create_struct_" ^ tag_name in
    let create_fn =
      !^"void* "
      ^^ !^create_fn_name
      ^^ !^"(bennet_hash_table(const_char_ptr, void_ptr) * members) {"
      ^^ hardline
      ^^ !^"  "
      ^^ !^struct_name
      ^^ !^"* result = malloc(sizeof("
      ^^ !^struct_name
      ^^ !^"));"
      ^^ hardline
      ^^ List.fold_left
           (fun acc (member_id, _member_sct) ->
              let member_name = Id.get_string member_id in
              acc
              ^^ !^"  bennet_optional(void_ptr) "
              ^^ !^member_name
              ^^ !^"_opt = bennet_hash_table_get(const_char_ptr, void_ptr)(members, \""
              ^^ !^member_name
              ^^ !^"\");"
              ^^ hardline
              ^^ !^"  if (bennet_optional_is_some("
              ^^ !^member_name
              ^^ !^"_opt)) {"
              ^^ hardline
              ^^ !^"    void* "
              ^^ !^member_name
              ^^ !^"_val = bennet_optional_unwrap("
              ^^ !^member_name
              ^^ !^"_opt);"
              ^^ hardline
              ^^ !^"    result->"
              ^^ !^member_name
              ^^ !^" = "
              ^^ !^member_name
              ^^ !^"_val;"
              ^^ hardline
              ^^ !^"  }"
              ^^ hardline)
           !^""
           members
      ^^ !^"  return result;"
      ^^ hardline
      ^^ !^"}"
      ^^ hardline
      ^^ hardline
    in
    (* Generate get_member function *)
    let get_fn_name = "get_member_" ^ tag_name in
    let get_fn =
      !^"void* "
      ^^ !^get_fn_name
      ^^ !^"(void* struct_val, const char* member_name) {"
      ^^ hardline
      ^^ !^"  "
      ^^ !^struct_name
      ^^ !^"* s = ("
      ^^ !^struct_name
      ^^ !^"*)struct_val;"
      ^^ hardline
      ^^ List.fold_left
           (fun acc (member_id, _) ->
              let member_name = Id.get_string member_id in
              acc
              ^^ !^"  if (strcmp(member_name, \""
              ^^ !^member_name
              ^^ !^"\") == 0) {"
              ^^ hardline
              ^^ !^"    return &s->"
              ^^ !^member_name
              ^^ !^";"
              ^^ hardline
              ^^ !^"  }"
              ^^ hardline)
           !^""
           members
      ^^ !^"  return NULL;"
      ^^ hardline
      ^^ !^"}"
      ^^ hardline
      ^^ hardline
    in
    (* Generate update_member function *)
    let update_fn_name = "update_member_" ^ tag_name in
    let update_fn =
      !^"void* "
      ^^ !^update_fn_name
      ^^ !^"(void* struct_val, const char* member_name, void* new_value) {"
      ^^ hardline
      ^^ !^"  "
      ^^ !^struct_name
      ^^ !^"* s = ("
      ^^ !^struct_name
      ^^ !^"*)struct_val;"
      ^^ hardline
      ^^ !^"  "
      ^^ !^struct_name
      ^^ !^"* result = malloc(sizeof("
      ^^ !^struct_name
      ^^ !^"));"
      ^^ hardline
      ^^ !^"  *result = *s; // copy original struct"
      ^^ hardline
      ^^ List.fold_left
           (fun acc (member_id, _member_sct) ->
              let member_name = Id.get_string member_id in
              acc
              ^^ !^"  if (strcmp(member_name, \""
              ^^ !^member_name
              ^^ !^"\") == 0) {"
              ^^ hardline
              ^^ !^"    result->"
              ^^ !^member_name
              ^^ !^" = new_value;"
              ^^ hardline
              ^^ !^"  }"
              ^^ hardline)
           !^""
           members
      ^^ !^"  return result;"
      ^^ hardline
      ^^ !^"}"
      ^^ hardline
    in
    create_fn ^^ get_fn ^^ update_fn
  | _ -> failwith ("no struct " ^ Sym.pp_string tag ^ " found")


let generate_handlers_and_init (prog5 : unit Mucore.file) : Pp.document =
  let open Pp in
  (* Get all struct definitions *)
  let all_tag_defs = Pmap.bindings_list prog5.tagDefs in
  let struct_tags =
    List.filter_map
      (fun (tag, tag_def) ->
         match tag_def with Mucore.StructDef _ -> Some tag | _ -> None)
      all_tag_defs
  in
  (* Generate handler functions for all structs *)
  let all_handlers =
    List.fold_left
      (fun acc tag -> acc ^^ generate_struct_handlers prog5 tag ^^ hardline)
      !^""
      struct_tags
  in
  (* Generate the init function *)
  let init_fn =
    !^"void cn_smt_handlers_init(void) {"
    ^^ hardline
    ^^ !^"  static bool handlers_initialized = false;"
    ^^ hardline
    ^^ !^"  if (handlers_initialized) return;"
    ^^ hardline
    ^^ hardline
    ^^ List.fold_left
         (fun acc tag ->
            let tag_name = Sym.pp_string tag in
            acc
            ^^ !^"  cn_struct_handler "
            ^^ !^tag_name
            ^^ !^"_handler = {"
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
            ^^ hardline
            ^^ !^"  };"
            ^^ hardline
            ^^ !^"  cn_register_struct_handler(\""
            ^^ !^tag_name
            ^^ !^"\", "
            ^^ !^tag_name
            ^^ !^"_handler);"
            ^^ hardline
            ^^ hardline)
         !^""
         struct_tags
    ^^ !^"  handlers_initialized = true;"
    ^^ hardline
    ^^ !^"}"
    ^^ hardline
  in
  all_handlers ^^ hardline ^^ init_fn
