module Make (AD : Domain.T) = struct
  module Smt = Smt.Make (AD)

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
        ^^ !^"* result"
        ^^^ equals
        ^^^ (!^"malloc" ^^ parens (!^"sizeof" ^^ parens !^struct_name))
        ^^ semi
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
                ^^ !^"    return s->"
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
      (* Generate default_struct function *)
      let default_fn_name = "default_struct_" ^ tag_name ^ "_cn" in
      let default_fn =
        !^"void* "
        ^^ !^(default_fn_name ^ "_smt")
        ^^ !^"(void)"
        ^^^ braces (hardline ^^ !^"return" ^^^ !^default_fn_name ^^ parens empty ^^ semi)
      in
      create_fn ^^ get_fn ^^ update_fn ^^ default_fn
    | _ -> failwith ("no struct " ^ Sym.pp_string tag ^ " found")


  let generate_record_handlers (record_hash : int) (members : (string * BaseTypes.t) list)
    : Pp.document
    =
    let open Pp in
    let hash_name = "record_" ^ string_of_int record_hash in
    let struct_name = "struct " ^ hash_name in
    (* Generate create_record function *)
    let create_fn_name = "create_record_" ^ hash_name in
    let create_fn =
      !^"void* "
      ^^ !^create_fn_name
      ^^ !^"(bennet_hash_table(const_char_ptr, void_ptr) * members) {"
      ^^ hardline
      ^^ !^"  "
      ^^ !^struct_name
      ^^ !^"* result"
      ^^^ equals
      ^^^ (!^"malloc" ^^ parens (!^"sizeof" ^^ parens !^struct_name))
      ^^ semi
      ^^ hardline
      ^^ List.fold_left
           (fun acc (member_name, _member_bt) ->
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
    let get_fn_name = "get_member_" ^ hash_name in
    let get_fn =
      !^"void* "
      ^^ !^get_fn_name
      ^^ !^"(void* record_val, const char* member_name) {"
      ^^ hardline
      ^^ !^"  "
      ^^ !^struct_name
      ^^ !^"* r = ("
      ^^ !^struct_name
      ^^ !^"*)record_val;"
      ^^ hardline
      ^^ List.fold_left
           (fun acc (member_name, _) ->
              acc
              ^^ !^"  if (strcmp(member_name, \""
              ^^ !^member_name
              ^^ !^"\") == 0) {"
              ^^ hardline
              ^^ !^"    return r->"
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
    let update_fn_name = "update_member_" ^ hash_name in
    let update_fn =
      !^"void* "
      ^^ !^update_fn_name
      ^^ !^"(void* record_val, const char* member_name, void* new_value) {"
      ^^ hardline
      ^^ !^"  "
      ^^ !^struct_name
      ^^ !^"* r = ("
      ^^ !^struct_name
      ^^ !^"*)record_val;"
      ^^ hardline
      ^^ !^"  "
      ^^ !^struct_name
      ^^ !^"* result = malloc(sizeof("
      ^^ !^struct_name
      ^^ !^"));"
      ^^ hardline
      ^^ !^"  *result = *r; // copy original record"
      ^^ hardline
      ^^ List.fold_left
           (fun acc (member_name, _member_bt) ->
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
    (* Generate default_record function *)
    let default_fn_name = "default_record_" ^ hash_name in
    let default_fn =
      !^"void* "
      ^^ !^default_fn_name
      ^^ !^"(void) {"
      ^^ hardline
      ^^ !^"  "
      ^^ !^struct_name
      ^^ !^"* result = malloc(sizeof("
      ^^ !^struct_name
      ^^ !^"));"
      ^^ hardline
      ^^ List.fold_left
           (fun acc (member_name, _member_bt) ->
              acc ^^ !^"  result->" ^^ !^member_name ^^ !^" = NULL;" ^^ hardline)
           !^""
           members
      ^^ !^"  return result;"
      ^^ hardline
      ^^ !^"}"
      ^^ hardline
      ^^ hardline
    in
    create_fn ^^ get_fn ^^ update_fn ^^ default_fn


  let generate_datatype_handlers (prog5 : unit Mucore.file) : Pp.document =
    let open Pp in
    let datatypes = prog5.datatypes in
    if List.length datatypes = 0 then
      !^""
    else
      List.fold_left
        (fun acc (dt_sym, (dt_def : Mucore.datatype)) ->
           let dt_name = Sym.pp_string_no_nums dt_sym in
           let struct_name = "struct " ^ dt_name in
           (* Generate constructor function for each case *)
           let constructor_fns =
             List.fold_left
               (fun ctor_acc (ctor_sym, params) ->
                  let ctor_name = Sym.pp_string_no_nums ctor_sym in
                  let ctor_name_upper = String.uppercase_ascii ctor_name in
                  let ctor_name_lower = String.lowercase_ascii ctor_name in
                  let fn_name = "create_constructor_" ^ dt_name ^ "_" ^ ctor_name in
                  let param_count = List.length params in
                  (* Function signature *)
                  let fn_sig =
                    !^"void* "
                    ^^ !^fn_name
                    ^^ !^"(bennet_vector(void_ptr)* args)"
                    ^^^ braces
                          (hardline
                           (* Extract arguments from vector *)
                           ^^ (if param_count > 0 then
                                 List.fold_left
                                   (fun (arg_acc, idx) (param_id, _param_bt) ->
                                      let param_name = Id.get_string param_id in
                                      ( arg_acc
                                        ^^ !^"  void* "
                                        ^^ !^param_name
                                        ^^ !^" = *bennet_vector_get(void_ptr)(args, "
                                        ^^ int idx
                                        ^^ !^");"
                                        ^^ hardline,
                                        idx + 1 ))
                                   (!^"", 0)
                                   params
                                 |> fst
                               else
                                 !^"")
                           ^^ hardline
                           (* Allocate datatype struct *)
                           ^^ !^"  "
                           ^^ !^struct_name
                           ^^ !^"* result = malloc(sizeof("
                           ^^ !^struct_name
                           ^^ !^"));"
                           ^^ hardline
                           (* Set tag *)
                           ^^ !^"  result->tag = "
                           ^^ !^ctor_name_upper
                           ^^ !^";"
                           ^^ hardline
                           (* Allocate and populate constructor payload struct *)
                           ^^ !^"  result->u."
                           ^^ !^ctor_name_lower
                           ^^ !^" = malloc(sizeof(struct "
                           ^^ !^ctor_name_lower
                           ^^ !^"));"
                           ^^ hardline
                           (* Set constructor fields *)
                           ^^ (if param_count > 0 then
                                 List.fold_left
                                   (fun field_acc (param_id, _param_bt) ->
                                      let param_name = Id.get_string param_id in
                                      field_acc
                                      ^^ !^"  result->u."
                                      ^^ !^ctor_name_lower
                                      ^^ !^"->"
                                      ^^ !^param_name
                                      ^^ !^" = "
                                      ^^ !^param_name
                                      ^^ !^";"
                                      ^^ hardline)
                                   !^""
                                   params
                               else
                                 !^"")
                           (* Return result *)
                           ^^ !^"  return result;"
                           ^^ hardline)
                    ^^ hardline
                    ^^ hardline
                  in
                  ctor_acc ^^ fn_sig)
               !^""
               dt_def.cases
           in
           acc ^^ constructor_fns)
        !^""
        datatypes
end
