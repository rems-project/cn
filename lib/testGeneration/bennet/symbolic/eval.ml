module Make (AD : Domain.T) = struct
  module Smt = Smt.Make (AD)
  module Stage5 = Stage5.Make (AD)
  module GT = Stage5.Term
  module Ctx = Stage5.Ctx
  module Def = Stage5.Def

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
        ^^^ (!^"cn_bump_malloc" ^^ parens (!^"sizeof" ^^ parens !^struct_name))
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
             empty
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
             empty
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
        ^^ !^"* result = cn_bump_malloc(sizeof("
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
             empty
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
      ^^^ (!^"cn_bump_malloc" ^^ parens (!^"sizeof" ^^ parens !^struct_name))
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
           empty
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
           empty
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
      ^^ !^"* result = cn_bump_malloc(sizeof("
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
           empty
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
      ^^ !^"* result = cn_bump_malloc(sizeof("
      ^^ !^struct_name
      ^^ !^"));"
      ^^ hardline
      ^^ List.fold_left
           (fun acc (member_name, _member_bt) ->
              acc ^^ !^"  result->" ^^ !^member_name ^^ !^" = NULL;" ^^ hardline)
           empty
           members
      ^^ !^"  return result;"
      ^^ hardline
      ^^ !^"}"
      ^^ hardline
      ^^ hardline
    in
    create_fn ^^ get_fn ^^ update_fn ^^ default_fn


  let generate_datatype_destructor (dt_name : string) (dt_def : Mucore.datatype)
    : Pp.document
    =
    let open Pp in
    let fn_name = "destruct_constructor_" ^ dt_name in
    let struct_name = "struct " ^ dt_name in
    (* Generate destructor function signature *)
    !^"void** "
    ^^ !^fn_name
    ^^ !^"(const char* ctor_name, void* value)"
    ^^^ braces
          (hardline
           ^^ !^"  "
           ^^ !^struct_name
           ^^ !^"* dt = ("
           ^^ !^struct_name
           ^^ !^"*)value;"
           ^^ hardline
           ^^ hardline
           (* Generate if-else chain for each constructor *)
           ^^ List.fold_left
                (fun ctor_acc (ctor_sym, params) ->
                   let ctor_name = Sym.pp_string_no_nums ctor_sym in
                   let ctor_name_upper = String.uppercase_ascii ctor_name in
                   let ctor_name_lower = String.lowercase_ascii ctor_name in
                   let param_count = List.length params in
                   ctor_acc
                   ^^ !^"  if (strcmp(ctor_name, "
                   ^^ dquotes !^ctor_name
                   ^^ !^") == 0)"
                   ^^^ braces
                         (hardline
                          ^^ !^"    if (dt->tag != "
                          ^^ !^ctor_name_upper
                          ^^ !^") return NULL;"
                          ^^ hardline
                          ^^
                          if param_count = 0 then
                            !^"    return cn_test_malloc(0);  // No members" ^^ hardline
                          else
                            !^"    void** members = cn_test_malloc(sizeof(void*) * "
                            ^^ int param_count
                            ^^ !^");"
                            ^^ hardline
                            ^^ List.fold_left
                                 (fun member_acc (idx, (param_id, _param_bt)) ->
                                    let param_name = Id.get_string param_id in
                                    member_acc
                                    ^^ !^"    members["
                                    ^^ int idx
                                    ^^ !^"] = dt->u."
                                    ^^ !^ctor_name_lower
                                    ^^ !^"->"
                                    ^^ !^param_name
                                    ^^ !^";"
                                    ^^ hardline)
                                 empty
                                 (List.mapi (fun i x -> (i, x)) params)
                            ^^ !^"    return members;"
                            ^^ hardline)
                   ^^ hardline)
                empty
                dt_def.cases
           ^^ !^"  return NULL;"
           ^^ hardline)
    ^^ hardline
    ^^ hardline


  let generate_datatype_handlers (prog5 : unit Mucore.file) : Pp.document =
    let open Pp in
    let datatypes = prog5.datatypes in
    if List.length datatypes = 0 then
      empty
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
                                   (fun arg_acc (idx, (param_id, _param_bt)) ->
                                      let param_name = Id.get_string param_id in
                                      arg_acc
                                      ^^ !^"  void* "
                                      ^^ !^param_name
                                      ^^ !^" = *bennet_vector_get(void_ptr)(args, "
                                      ^^ int idx
                                      ^^ !^");"
                                      ^^ hardline)
                                   empty
                                   (List.mapi (fun i x -> (i, x)) params)
                               else
                                 empty)
                           ^^ hardline
                           (* Allocate datatype struct *)
                           ^^ !^"  "
                           ^^ !^struct_name
                           ^^ !^"* result = cn_bump_malloc(sizeof("
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
                           ^^ !^" = cn_bump_malloc(sizeof(struct "
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
                                   empty
                                   params
                               else
                                 empty)
                           (* Return result *)
                           ^^ !^"  return result;"
                           ^^ hardline)
                    ^^ hardline
                    ^^ hardline
                  in
                  ctor_acc ^^ fn_sig)
               empty
               dt_def.cases
           in
           (* Generate destructor function *)
           let destructor_fn = generate_datatype_destructor dt_name dt_def in
           acc ^^ constructor_fns ^^ destructor_fn)
        empty
        datatypes


  let generate_function_handlers
        (logical_functions : (Sym.t * Definition.Function.t) list)
    : Pp.document
    =
    let open Pp in
    if List.length logical_functions = 0 then
      empty
    else
      List.fold_left
        (fun acc (fn_sym, (fn_def : Definition.Function.t)) ->
           match fn_def.body with
           | Definition.Function.Uninterp ->
             (* Skip uninterpreted functions - they'll error at registration *)
             acc
           | Definition.Function.Def _ | Definition.Function.Rec_Def _ ->
             let fn_name = Sym.pp_string_no_nums fn_sym in
             let handler_name = "cn_func_" ^ fn_name in
             let arg_count = List.length fn_def.args in
             let is_unit_return =
               match fn_def.return_bt with BaseTypes.Unit -> true | _ -> false
             in
             (* Generate handler function *)
             let handler =
               !^"static void* "
               ^^ !^handler_name
               ^^ !^"(void** args)"
               ^^^ braces
                     (hardline
                      (* Cast arguments from args array *)
                      ^^ (if arg_count > 0 then
                            List.fold_left
                              (fun arg_acc (idx, (arg_sym, _)) ->
                                 let arg_name = Sym.pp_string_no_nums arg_sym in
                                 arg_acc
                                 ^^ !^"  void* "
                                 ^^ !^arg_name
                                 ^^ !^" = args["
                                 ^^ int idx
                                 ^^ !^"];"
                                 ^^ hardline)
                              empty
                              (List.mapi (fun i x -> (i, x)) fn_def.args)
                          else
                            empty)
                      (* Call Fulminate-generated function *)
                      ^^ !^"  "
                      ^^ (if is_unit_return then
                            (* For unit return, call function and return NULL *)
                            !^fn_name
                            ^^ !^"("
                            ^^ separate
                                 (comma ^^ space)
                                 (List.map
                                    (fun (arg_sym, _) ->
                                       !^(Sym.pp_string_no_nums arg_sym))
                                    fn_def.args)
                            ^^ !^");"
                            ^^ hardline
                            ^^ !^"  return NULL;"
                          else (* For non-unit return, return the result *)
                            !^"return "
                            ^^ !^fn_name
                            ^^ !^"("
                            ^^ separate
                                 (comma ^^ space)
                                 (List.map
                                    (fun (arg_sym, _) ->
                                       !^(Sym.pp_string_no_nums arg_sym))
                                    fn_def.args)
                            ^^ !^");")
                      ^^ hardline)
               ^^ hardline
               ^^ hardline
             in
             acc ^^ handler)
        empty
        logical_functions
end
