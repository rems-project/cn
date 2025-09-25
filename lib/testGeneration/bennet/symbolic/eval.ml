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
        !^"void*"
        ^^^ !^create_fn_name
        ^^ parens !^"bennet_hash_table(const_char_ptr, void_ptr) * members"
        ^^ space
        ^^ braces
             (nest
                2
                (hardline
                 ^^ !^struct_name
                 ^^ !^"* result ="
                 ^^^ !^"malloc"
                 ^^ parens (!^"sizeof" ^^ parens !^struct_name)
                 ^^ semi
                 ^^ List.fold_left
                      (fun acc (member_id, _member_sct) ->
                         let member_name = Id.get_string member_id in
                         acc
                         ^^ hardline
                         ^^ !^"bennet_optional(void_ptr)"
                         ^^^ !^member_name
                         ^^ !^"_opt = bennet_hash_table_get(const_char_ptr, void_ptr)"
                         ^^ parens (!^"members, \"" ^^ !^member_name ^^ !^"\"")
                         ^^ semi
                         ^^ hardline
                         ^^ !^"if"
                         ^^^ parens
                               (!^"bennet_optional_is_some"
                                ^^ parens (!^member_name ^^ !^"_opt"))
                         ^^ space
                         ^^ braces
                              (nest
                                 2
                                 (hardline
                                  ^^ !^"void*"
                                  ^^^ !^member_name
                                  ^^ !^"_val = bennet_optional_unwrap"
                                  ^^ parens (!^member_name ^^ !^"_opt")
                                  ^^ semi
                                  ^^ hardline
                                  ^^ !^"result->"
                                  ^^ !^member_name
                                  ^^^ equals
                                  ^^^ !^member_name
                                  ^^ !^"_val"
                                  ^^ semi)))
                      empty
                      members
                 ^^ hardline
                 ^^ !^"return result"
                 ^^ semi))
        ^^ twice hardline
      in
      (* Generate get_member function *)
      let get_fn_name = "get_member_" ^ tag_name in
      let get_fn =
        !^"void*"
        ^^^ !^get_fn_name
        ^^ parens !^"void* struct_val, const char* member_name"
        ^^ space
        ^^ braces
             (nest
                2
                (hardline
                 ^^ !^struct_name
                 ^^ !^"* s ="
                 ^^^ parens (!^struct_name ^^ !^"*")
                 ^^ !^"struct_val"
                 ^^ semi
                 ^^ List.fold_left
                      (fun acc (member_id, _) ->
                         let member_name = Id.get_string member_id in
                         acc
                         ^^ hardline
                         ^^ !^"if"
                         ^^^ parens
                               (!^"strcmp"
                                ^^ parens (!^"member_name, \"" ^^ !^member_name ^^ !^"\"")
                                ^^^ !^"== 0")
                         ^^ space
                         ^^ braces
                              (nest
                                 2
                                 (hardline ^^ !^"return s->" ^^ !^member_name ^^ semi)))
                      empty
                      members
                 ^^ hardline
                 ^^ !^"return NULL"
                 ^^ semi))
        ^^ twice hardline
      in
      (* Generate update_member function *)
      let update_fn_name = "update_member_" ^ tag_name in
      let update_fn =
        !^"void*"
        ^^^ !^update_fn_name
        ^^ parens !^"void* struct_val, const char* member_name, void* new_value"
        ^^ space
        ^^ braces
             (nest
                2
                (hardline
                 ^^ !^struct_name
                 ^^ !^"* s ="
                 ^^^ parens (!^struct_name ^^ !^"*")
                 ^^ !^"struct_val"
                 ^^ semi
                 ^^ hardline
                 ^^ !^struct_name
                 ^^ !^"* result = malloc"
                 ^^ parens (!^"sizeof" ^^ parens !^struct_name)
                 ^^ semi
                 ^^ hardline
                 ^^ !^"*result = *s; // copy original struct"
                 ^^ List.fold_left
                      (fun acc (member_id, _member_sct) ->
                         let member_name = Id.get_string member_id in
                         acc
                         ^^ hardline
                         ^^ !^"if"
                         ^^^ parens
                               (!^"strcmp"
                                ^^ parens (!^"member_name, \"" ^^ !^member_name ^^ !^"\"")
                                ^^^ !^"== 0")
                         ^^ space
                         ^^ braces
                              (nest
                                 2
                                 (hardline
                                  ^^ !^"result->"
                                  ^^ !^member_name
                                  ^^^ !^"= new_value"
                                  ^^ semi)))
                      empty
                      members
                 ^^ hardline
                 ^^ !^"return result"
                 ^^ semi))
        ^^ twice hardline
      in
      (* Generate default_struct function *)
      let default_fn_name = "default_struct_" ^ tag_name ^ "_cn" in
      let default_fn =
        !^"void*"
        ^^^ !^(default_fn_name ^ "_smt")
        ^^ parens !^"void"
        ^^ space
        ^^ braces
             (nest
                2
                (hardline ^^ !^"return" ^^^ !^default_fn_name ^^ parens empty ^^ semi))
        ^^ hardline
      in
      create_fn ^^ get_fn ^^ update_fn ^^ default_fn
    | _ -> failwith ("no struct " ^ Sym.pp_string tag ^ " found")
end
