module Make (AD : Domain.T) = struct
  module Smt = Smt.Make (AD)
  module Eval = Eval.Make (AD)

  let generate_smt_setup (prog5 : unit Mucore.file) : Pp.document =
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
    (* Generate the init function *)
    let init_fn =
      !^"void cn_smt_solver_setup(struct cn_smt_solver *s) {"
      ^^ hardline
      ^^ struct_decl_code
      ^^ hardline
      ^^ (if List.length struct_decl_data > 0 then (
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
      ^^ !^"}"
      ^^ hardline
    in
    all_handlers ^^ hardline ^^ init_fn
end
