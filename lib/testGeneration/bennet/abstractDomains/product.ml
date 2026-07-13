module T = Terms.Normal
module MT = MakeTerm
module LC = LogicalConstraints
module Sym = Sym
module StringSet = Set.Make (String)
module StringSetSet = Set.Make (StringSet)

(** Use a GADT to safely store a domain and its state. *)
type domain_component =
  | DPack : (module Domain.T with type t = 'a) * 'a -> domain_component

type relative_component =
  | RPack : (module Domain.RELATIVE_VIEW with type t = 'a) * 'a -> relative_component

let specialized =
  StringSetSet.of_list
    [ StringSet.of_list [ Ownership.Inner.CInt.name; Interval_.Inner.CInt.name ];
      StringSet.of_list [ Congruence.Inner.CInt.name; Ownership.Inner.CInt.name ];
      StringSet.of_list [ Ownership.Inner.CInt.name; TNum.Inner.CInt.name ];
      StringSet.of_list [ Congruence.Inner.CInt.name; Interval_.Inner.CInt.name ];
      StringSet.of_list
        [ Congruence.Inner.CInt.name;
          Ownership.Inner.CInt.name;
          Interval_.Inner.CInt.name
        ]
    ]


let product_domains (domains : (module Domain.T) list) =
  match domains with
  | [] -> failwith "Cannot create product of empty domain list"
  | _ ->
    let domains =
      List.sort_uniq
        (fun (module D1 : Domain.T) (module D2 : Domain.T) ->
           String.compare D1.CInt.name D2.CInt.name)
        domains
    in
    let name =
      "product_"
      ^ String.concat "_x_" (List.map (fun (module D : Domain.T) -> D.CInt.name) domains)
    in
    let module ProductDomain = struct
      module CInt = struct
        let name = "product"

        open Pp

        (* Helper functions for C code generation *)
        let generate_macro_call ?op name =
          let op = match op with Some op -> "_" ^ op | None -> "" in
          !^(Printf.sprintf "bennet_domain_%s%s_##ty" name op)


        let c_prefix = "bennet_domain_" ^ name

        let func_prefix = c_prefix ^ "_"

        let struct_type = c_prefix ^ "(ty)"

        let generate_domain_calls op_name =
          domains
          |> List.mapi (fun i (module D : Domain.T) ->
            !^"  result->element_"
            ^^ int i
            ^^ !^" = *"
            ^^ !^"bennet_domain_"
            ^^ !^D.CInt.name
            ^^ !^"_"
            ^^ !^op_name
            ^^ !^"_##ty"
            ^^ !^"();")
          |> separate hardline


        let generate_condition_calls op_name joiner =
          domains
          |> List.mapi (fun i (module D : Domain.T) ->
            generate_macro_call ~op:op_name D.CInt.name
            ^^ !^"(&ptr->element_"
            ^^ int i
            ^^ !^")")
          |> separate (!^" " ^^ !^joiner ^^ !^" ")


        let generate_binary_calls op_name =
          domains
          |> List.mapi (fun i (module D : Domain.T) ->
            !^"  result->element_"
            ^^ int i
            ^^ !^" = *"
            ^^ generate_macro_call ~op:op_name D.CInt.name
            ^^ !^"(&ptr1->element_"
            ^^ int i
            ^^ !^", &ptr2->element_"
            ^^ int i
            ^^ !^");")
          |> separate hardline


        let generate_binary_conditions op_name joiner =
          domains
          |> List.mapi (fun i (module D : Domain.T) ->
            generate_macro_call ~op:op_name D.CInt.name
            ^^ !^"(&ptr1->element_"
            ^^ int i
            ^^ !^", &ptr2->element_"
            ^^ int i
            ^^ !^")")
          |> separate (!^" " ^^ !^joiner ^^ !^" ")


        let generate_unary_op op_name =
          !^"static inline"
          ^^^ !^struct_type
          ^^ !^"*"
          ^^^ !^(func_prefix ^ op_name ^ "_##ty")
          ^^ !^"(void) {"
          ^/^ !^"  "
          ^^ !^struct_type
          ^^ !^"* result = std_malloc(sizeof("
          ^^ !^struct_type
          ^^ !^"));"
          ^/^ generate_domain_calls op_name
          ^/^ !^"  return result;"
          ^/^ !^"}"


        let generate_unary_predicate op_name joiner =
          !^"static inline bool"
          ^^^ !^(func_prefix ^ op_name ^ "_##ty")
          ^^ parens (!^struct_type ^^ !^"* ptr")
          ^/^ braces (!^"  return " ^^ generate_condition_calls op_name joiner ^^ !^";")


        let generate_binary_op op_name =
          !^"static inline"
          ^^^ !^struct_type
          ^^ !^"*"
          ^^^ !^(func_prefix ^ op_name ^ "_##ty")
          ^^ parens (!^struct_type ^^ !^"* ptr1, " ^^ !^struct_type ^^ !^"* ptr2")
          ^/^ braces
                (!^"  "
                 ^^ !^(Printf.sprintf
                         "%s* result = std_malloc(sizeof(%s));"
                         struct_type
                         struct_type)
                 ^/^ generate_binary_calls op_name
                 ^/^ !^"  return result;")


        let generate_binary_predicate op_name joiner =
          !^"static inline bool"
          ^^^ !^(func_prefix ^ op_name ^ "_##ty")
          ^^ parens (!^struct_type ^^ !^"* ptr1, " ^^ !^struct_type ^^ !^"* ptr2")
          ^/^ braces (!^"  return " ^^ generate_binary_conditions op_name joiner ^^ !^";")


        let generate_copy_function =
          !^"static inline"
          ^^^ !^struct_type
          ^^ !^"*"
          ^^^ !^(func_prefix ^ "copy_##ty")
          ^^ parens (!^struct_type ^^ !^"* ptr")
          ^/^ braces
                (!^"  "
                 ^^ !^struct_type
                 ^^ !^"* result = std_malloc(sizeof("
                 ^^ !^struct_type
                 ^^ !^"));"
                 ^/^ (domains
                      |> List.mapi (fun i (module _ : Domain.T) ->
                        !^"  result->element_"
                        ^^ int i
                        ^^ !^" = ptr->element_"
                        ^^ int i
                        ^^ !^";")
                      |> separate hardline)
                 ^/^ !^"  return result;")


        let generate_arbitrary_function =
          let domain_index (module D : Domain.T) =
            Option.get
              (List.find_index
                 (fun (module D' : Domain.T) -> String.equal D.CInt.name D'.CInt.name)
                 domains)
          in
          let domain_index' (name : string) =
            Option.get
              (List.find_index
                 (fun (module D : Domain.T) -> String.equal name D.CInt.name)
                 domains)
          in
          (* Get set of domain names in this product *)
          let domain_names =
            domains
            |> List.map (fun (module D : Domain.T) -> D.CInt.name)
            |> StringSet.of_list
          in
          let arb_funcs =
            domains
            |> List.map (fun (module D : Domain.T) ->
              let res =
                StringSetSet.fold
                  (fun g acc ->
                     (* Only include specialized group if ALL its members are in our product *)
                     if StringSet.mem D.CInt.name g && StringSet.subset g domain_names
                     then (
                       match StringSetSet.elements acc with
                       | h :: _ when StringSet.cardinal h = StringSet.cardinal g ->
                         StringSetSet.add g acc
                       | h :: _ when StringSet.cardinal h < StringSet.cardinal g ->
                         StringSetSet.singleton g
                       | _ :: _ -> acc
                       | [] -> StringSetSet.singleton g)
                     else
                       acc)
                  specialized
                  StringSetSet.empty
              in
              if StringSetSet.is_empty res then
                StringSetSet.singleton (StringSet.singleton D.CInt.name)
              else
                res)
            |> List.fold_left StringSetSet.union StringSetSet.empty
          in
          !^"static inline ty"
          ^^^ !^func_prefix
          ^^ !^"arbitrary_##ty"
          ^^ parens (!^struct_type ^^ !^"* ptr")
          ^^ braces
               (!^"int which = 0;"
                ^/^ !^"for (int attempt = 0; attempt < 10; attempt++)"
                ^/^ braces
                      (!^"ty res;"
                       ^/^ !^"switch(which)"
                       ^^ braces
                            (separate
                               hardline
                               (arb_funcs
                                |> StringSetSet.elements
                                |> List.mapi (fun i dset ->
                                  let supported_domains =
                                    dset
                                    |> StringSet.elements
                                    |> List.fast_sort String.compare
                                  in
                                  let combined_name =
                                    String.concat "_" supported_domains
                                  in
                                  let args =
                                    supported_domains
                                    |> List.map (fun name ->
                                      Printf.sprintf
                                        "&ptr->element_%d"
                                        (domain_index' name))
                                    |> String.concat ", "
                                  in
                                  let checks =
                                    List.map
                                      (fun (module D' : Domain.T) ->
                                         !^(Printf.sprintf
                                              "bennet_domain_%s_check_##ty(res, \
                                               &ptr->element_%d)"
                                              D'.CInt.name
                                              (domain_index (module D'))))
                                      (List.filter
                                         (fun (module D' : Domain.T) ->
                                            not (StringSet.mem D'.CInt.name dset))
                                         domains)
                                  in
                                  let checks =
                                    if List.is_empty checks then
                                      !^"return res;"
                                    else
                                      !^"if"
                                      ^/^ parens (separate !^"&&" checks)
                                      ^^^ braces !^"return res;"
                                      ^^^ !^"else"
                                      ^^^ braces
                                            (!^(Printf.sprintf
                                                  "which = (which + 1) %% %d;"
                                                  (StringSetSet.cardinal arb_funcs))
                                             ^/^ !^"continue;")
                                  in
                                  !^(Printf.sprintf
                                       "case %d:; res = \
                                        bennet_domain_%s_arbitrary_##ty(%s);"
                                       i
                                       combined_name
                                       args)
                                  ^^ checks))
                             ^^ !^"default: assert(false); break;"))
                ^^ !^"assert(false); return 0;")


        let generate_from_assignment_function =
          !^"static inline"
          ^^^ !^struct_type
          ^^ !^"*"
          ^^^ !^func_prefix
          ^^ !^"from_assignment_"
          ^^ !^"##ty"
          ^^ !^"(void *base_ptr, void *addr, size_t bytes) {"
          ^/^ !^"  "
          ^^ !^struct_type
          ^^ !^"* result = std_malloc(sizeof("
          ^^ !^struct_type
          ^^ !^"));"
          ^/^ (domains
               |> List.mapi (fun i (module D : Domain.T) ->
                 !^"  result->element_"
                 ^^ int i
                 ^^ !^" = *bennet_domain_"
                 ^^ !^D.CInt.name
                 ^^ !^"_from_assignment_"
                 ^^ !^"##ty"
                 ^^ !^"(base_ptr, addr, bytes);")
               |> separate hardline)
          ^/^ !^(Printf.sprintf "  %sreduce_##ty(result);" func_prefix)
          ^/^ !^"  return result;"
          ^/^ !^"}"


        let generate_of_constructor_function =
          let generate_param_list () =
            separate_map
              (comma ^^ space)
              (fun (module D : Domain.T) -> !^(D.pp_params ()))
              domains
          in
          let generate_assignments () =
            domains
            |> List.mapi (fun i (module D : Domain.T) ->
              !^"  result->element_"
              ^^ int i
              ^^ !^" = *bennet_domain_"
              ^^ !^D.CInt.name
              ^^ !^"_of_"
              ^^ !^"##ty"
              ^^ parens !^(D.pp_args ())
              ^^ !^";")
            |> separate hardline
          in
          !^"static inline"
          ^^^ !^struct_type
          ^^ !^"*"
          ^^^ !^func_prefix
          ^^ !^"of_"
          ^^ !^"##ty"
          ^^ !^"("
          ^^ generate_param_list ()
          ^^ !^") {"
          ^/^ !^"  "
          ^^ !^struct_type
          ^^ !^"* result = std_malloc(sizeof("
          ^^ !^struct_type
          ^^ !^"));"
          ^/^ generate_assignments ()
          ^/^ !^(Printf.sprintf "  %sreduce_##ty(result);" func_prefix)
          ^/^ !^"  return result;"
          ^/^ !^"}"


        let generate_check_function =
          let generate_check_calls () =
            domains
            |> List.mapi (fun i (module D : Domain.T) ->
              !^"bennet_domain_"
              ^^ !^D.CInt.name
              ^^ !^"_check_##ty(val, &ptr->element_"
              ^^ int i
              ^^ !^")")
            |> separate !^" && "
          in
          !^"static inline bool"
          ^^^ !^(func_prefix ^ "check_##ty")
          ^^ parens (!^"ty val, " ^^ !^struct_type ^^ !^"* ptr")
          ^/^ braces (!^"  return " ^^ generate_check_calls () ^^ !^";")


        let generate_check_ownership_function =
          let ownership_index =
            List.find_index
              (fun (module D : Domain.T) -> String.equal D.CInt.name "ownership")
              domains
          in
          match ownership_index with
          | Some idx ->
            !^"static inline bool"
            ^^^ !^(func_prefix ^ "check_ownership_##ty")
            ^^ parens (!^"ty val, " ^^ !^struct_type ^^ !^"* ptr")
            ^/^ braces
                  (!^"  return bennet_domain_ownership_check_##ty(val, &ptr->element_"
                   ^^ int idx
                   ^^ !^");")
          | None ->
            !^"static inline bool"
            ^^^ !^(func_prefix ^ "check_ownership_##ty")
            ^^ parens (!^"ty val, " ^^ !^struct_type ^^ !^"* ptr")
            ^/^ braces !^"  (void)val; (void)ptr; return true;"


        let generate_top_except_ownership_function =
          let ownership_index =
            List.find_index
              (fun (module D : Domain.T) -> String.equal D.CInt.name "ownership")
              domains
          in
          match ownership_index with
          | Some _ ->
            !^"static inline"
            ^^^ !^struct_type
            ^^ !^"*"
            ^^^ !^(func_prefix ^ "top_except_ownership_##ty")
            ^^ parens (!^struct_type ^^ !^"* ptr")
            ^/^ braces
                  (!^"  "
                   ^^ !^struct_type
                   ^^ !^"* result = std_malloc(sizeof("
                   ^^ !^struct_type
                   ^^ !^"));"
                   ^/^ (domains
                        |> List.mapi (fun i (module D : Domain.T) ->
                          if String.equal D.CInt.name "ownership" then
                            !^"  result->element_"
                            ^^ int i
                            ^^ !^" = ptr->element_"
                            ^^ int i
                            ^^ !^";"
                          else
                            !^"  result->element_"
                            ^^ int i
                            ^^ !^" = *"
                            ^^ !^"bennet_domain_"
                            ^^ !^D.CInt.name
                            ^^ !^"_top_##ty"
                            ^^ !^"();")
                        |> separate hardline)
                   ^/^ !^(Printf.sprintf "  %sreduce_##ty(result);" func_prefix)
                   ^/^ !^"  return result;")
          | None ->
            (* No ownership domain - just return top *)
            !^"static inline"
            ^^^ !^struct_type
            ^^ !^"*"
            ^^^ !^(func_prefix ^ "top_except_ownership_##ty")
            ^^ parens (!^struct_type ^^ !^"* ptr")
            ^/^ braces
                  (!^"  (void)ptr; return " ^^ !^(func_prefix ^ "top_##ty") ^^ !^"();")


        (* Lift a bare ownership element into the product: top except the
           ownership component (the blame-path constructor).
           Mirrors top_except_ownership,
           including the reduce. *)
        let generate_from_ownership_function =
          let ownership_index =
            List.find_index
              (fun (module D : Domain.T) -> String.equal D.CInt.name "ownership")
              domains
          in
          match ownership_index with
          | Some _ ->
            !^"static inline"
            ^^^ !^struct_type
            ^^ !^"*"
            ^^^ !^(func_prefix ^ "from_ownership_##ty")
            ^^ parens !^"bennet_domain_ownership(ty)* own"
            ^/^ braces
                  (!^"  "
                   ^^ !^struct_type
                   ^^ !^"* result = std_malloc(sizeof("
                   ^^ !^struct_type
                   ^^ !^"));"
                   ^/^ (domains
                        |> List.mapi (fun i (module D : Domain.T) ->
                          if String.equal D.CInt.name "ownership" then
                            !^"  result->element_" ^^ int i ^^ !^" = *own;"
                          else
                            !^"  result->element_"
                            ^^ int i
                            ^^ !^" = *"
                            ^^ !^"bennet_domain_"
                            ^^ !^D.CInt.name
                            ^^ !^"_top_##ty"
                            ^^ !^"();")
                        |> separate hardline)
                   ^/^ !^(Printf.sprintf "  %sreduce_##ty(result);" func_prefix)
                   ^/^ !^"  return result;")
          | None ->
            (* No ownership domain - the bare element adds no information *)
            !^"static inline"
            ^^^ !^struct_type
            ^^ !^"*"
            ^^^ !^(func_prefix ^ "from_ownership_##ty")
            ^^ parens !^"bennet_domain_ownership(ty)* own"
            ^/^ braces
                  (!^"  (void)own; return " ^^ !^(func_prefix ^ "top_##ty") ^^ !^"();")


        let generate_to_interval_function =
          let n = List.length domains in
          if n = 0 then
            !^"static inline bool"
            ^^^ !^(func_prefix ^ "to_interval_##ty")
            ^^ parens (!^struct_type ^^ !^"* ptr, ty* lo_out, ty* hi_out")
            ^/^ braces !^"  (void)ptr; (void)lo_out; (void)hi_out; return false;"
          else
            !^"static inline bool"
            ^^^ !^(func_prefix ^ "to_interval_##ty")
            ^^ parens (!^struct_type ^^ !^"* ptr, ty* lo_out, ty* hi_out")
            ^/^ braces
                  (!^"  ty _lo = 0, _hi = 0;"
                   ^/^ !^"  bool _valid = false;"
                   ^/^ (domains
                        |> List.mapi (fun i (module D : Domain.T) ->
                          let dname = D.CInt.name in
                          !^"  {"
                          ^/^ !^"    ty _tlo, _thi;"
                          ^/^ !^(Printf.sprintf
                                   "    if \
                                    (bennet_domain_%s_to_interval_##ty(&ptr->element_%d, \
                                    &_tlo, &_thi)) {"
                                   dname
                                   i)
                          ^/^ !^"      if (_valid) {"
                          ^/^ !^"        if (_tlo > _lo) _lo = _tlo;"
                          ^/^ !^"        if (_thi < _hi) _hi = _thi;"
                          ^/^ !^"      } else {"
                          ^/^ !^"        _lo = _tlo;"
                          ^/^ !^"        _hi = _thi;"
                          ^/^ !^"        _valid = true;"
                          ^/^ !^"      }"
                          ^/^ !^"    }"
                          ^/^ !^"  }")
                        |> separate hardline)
                   ^/^ !^"  if (_valid) {"
                   ^/^ !^"    *lo_out = _lo;"
                   ^/^ !^"    *hi_out = _hi;"
                   ^/^ !^"  }"
                   ^/^ !^"  return _valid;")


        let generate_of_interval_function =
          !^"static inline"
          ^^^ !^struct_type
          ^^ !^"*"
          ^^^ !^(func_prefix ^ "of_interval_##ty")
          ^^ parens !^"ty lo, ty hi"
          ^/^ braces
                (!^"  "
                 ^^ !^struct_type
                 ^^ !^"* result = std_malloc(sizeof("
                 ^^ !^struct_type
                 ^^ !^"));"
                 ^/^ (domains
                      |> List.mapi (fun i (module D : Domain.T) ->
                        let dname = D.CInt.name in
                        !^"  result->element_"
                        ^^ int i
                        ^^ !^" = *bennet_domain_"
                        ^^ !^dname
                        ^^ !^"_of_interval_##ty(lo, hi);")
                      |> separate hardline)
                 ^/^ !^"  return result;")


        let generate_reduce_function =
          let domain_index' (name : string) =
            Option.get
              (List.find_index
                 (fun (module D : Domain.T) -> String.equal name D.CInt.name)
                 domains)
          in
          let domain_names =
            domains
            |> List.map (fun (module D : Domain.T) -> D.CInt.name)
            |> StringSet.of_list
          in
          let applicable_groups =
            let applicable =
              StringSetSet.filter (fun g -> StringSet.subset g domain_names) specialized
            in
            (* Inclusion-maximal groups only: a group strictly contained in
               another applicable group is redundant, since the larger
               combined reduce already chains the smaller ones. *)
            applicable
            |> StringSetSet.filter (fun g ->
              not
                (StringSetSet.exists
                   (fun g' ->
                      StringSet.cardinal g < StringSet.cardinal g'
                      && StringSet.subset g g')
                   applicable))
            |> StringSetSet.elements
          in
          let specialized_body =
            applicable_groups
            |> List.map (fun group ->
              let combined_name =
                group
                |> StringSet.elements
                |> List.fast_sort String.compare
                |> String.concat "_"
              in
              let args =
                group
                |> StringSet.elements
                |> List.fast_sort String.compare
                |> List.map (fun name ->
                  Printf.sprintf "&ptr->element_%d" (domain_index' name))
                |> String.concat ", "
              in
              !^(Printf.sprintf "  bennet_domain_%s_reduce_##ty(%s);" combined_name args))
            |> separate hardline
          in
          let n = List.length domains in
          let general_body =
            if n < 2 then
              empty
            else (
              let extract_block i (module D : Domain.T) =
                let dname = D.CInt.name in
                let elem = Printf.sprintf "&ptr->element_%d" i in
                if i = 0 then (* Initial extract from first domain *)
                  !^(Printf.sprintf
                       "    if (bennet_domain_%s_to_interval_##ty(%s, &_r_lo, &_r_hi)) {"
                       dname
                       elem)
                  ^/^ !^"      _r_valid = true;"
                  ^/^ !^"    }"
                else (* Meet interval into this domain, then extract *)
                  !^(Printf.sprintf "    if (_r_valid) {")
                  ^/^ !^(Printf.sprintf
                           "      bennet_domain_%s(ty)* _r_tmp_%d = \
                            bennet_domain_%s_of_interval_##ty(_r_lo, _r_hi);"
                           dname
                           i
                           dname)
                  ^/^ !^(Printf.sprintf
                           "      ptr->element_%d = \
                            *bennet_domain_%s_meet_##ty(&ptr->element_%d, _r_tmp_%d);"
                           i
                           dname
                           i
                           i)
                  ^/^ !^"    }"
                  ^/^ !^"    {"
                  ^/^ !^(Printf.sprintf "      ty _r_lo2, _r_hi2;")
                  ^/^ !^(Printf.sprintf
                           "      if (bennet_domain_%s_to_interval_##ty(%s, &_r_lo2, \
                            &_r_hi2)) {"
                           dname
                           elem)
                  ^/^ !^"        if (_r_valid) {"
                  ^/^ !^"          if (_r_lo2 > _r_lo) _r_lo = _r_lo2;"
                  ^/^ !^"          if (_r_hi2 < _r_hi) _r_hi = _r_hi2;"
                  ^/^ !^"        } else {"
                  ^/^ !^"          _r_lo = _r_lo2;"
                  ^/^ !^"          _r_hi = _r_hi2;"
                  ^/^ !^"          _r_valid = true;"
                  ^/^ !^"        }"
                  ^/^ !^"      }"
                  ^/^ !^"    }"
              in
              let first_domain = List.hd domains in
              let first_dname =
                let (module D : Domain.T) = first_domain in
                D.CInt.name
              in
              let closeback =
                !^"    if (_r_valid) {"
                ^/^ !^(Printf.sprintf
                         "      bennet_domain_%s(ty)* _r_tmp_0 = \
                          bennet_domain_%s_of_interval_##ty(_r_lo, _r_hi);"
                         first_dname
                         first_dname)
                ^/^ !^(Printf.sprintf
                         "      ptr->element_0 = \
                          *bennet_domain_%s_meet_##ty(&ptr->element_0, _r_tmp_0);"
                         first_dname)
                ^/^ !^"    }"
              in
              !^"  for (int _ri = 0; _ri < 2; _ri++) {"
              ^/^ !^"    ty _r_lo = 0, _r_hi = 0;"
              ^/^ !^"    bool _r_valid = false;"
              ^/^ (domains |> List.mapi extract_block |> separate hardline)
              ^/^ closeback
              ^/^ !^"  }")
          in
          let bottom_propagation =
            if n < 2 then
              empty
            else (
              let bottom_check =
                domains
                |> List.mapi (fun i (module D : Domain.T) ->
                  !^(Printf.sprintf
                       "bennet_domain_%s_is_bottom_##ty(&ptr->element_%d)"
                       D.CInt.name
                       i))
                |> separate !^" || "
              in
              let set_all_bottom =
                domains
                |> List.mapi (fun i (module D : Domain.T) ->
                  !^(Printf.sprintf
                       "    ptr->element_%d = *bennet_domain_%s_bottom_##ty();"
                       i
                       D.CInt.name))
                |> separate hardline
              in
              !^"  if (" ^^ bottom_check ^^ !^") {" ^/^ set_all_bottom ^/^ !^"  }")
          in
          let body =
            if List.is_empty applicable_groups && n < 2 then
              !^"  (void)ptr;"
            else
              (if List.is_empty applicable_groups then
                 empty
               else
                 specialized_body)
              ^^
              if n >= 2 then
                (if not (List.is_empty applicable_groups) then
                   hardline
                 else
                   empty)
                ^^ general_body
                ^^ hardline
                ^^ bottom_propagation
              else
                empty
          in
          !^"static inline void"
          ^^^ !^(func_prefix ^ "reduce_##ty")
          ^^ parens (!^struct_type ^^ !^"* ptr")
          ^/^ braces body


        let generate_meet_function =
          !^"static inline"
          ^^^ !^struct_type
          ^^ !^"*"
          ^^^ !^(func_prefix ^ "meet_##ty")
          ^^ parens (!^struct_type ^^ !^"* ptr1, " ^^ !^struct_type ^^ !^"* ptr2")
          ^/^ braces
                (!^(Printf.sprintf
                      "  %s* result = std_malloc(sizeof(%s));"
                      struct_type
                      struct_type)
                 ^/^ generate_binary_calls "meet"
                 ^/^ !^(Printf.sprintf "  %sreduce_##ty(result);" func_prefix)
                 ^/^ !^"  return result;")


        let generate_refine_function =
          !^"static inline"
          ^^^ !^struct_type
          ^^ !^"*"
          ^^^ !^(func_prefix ^ "refine_##ty")
          ^^ parens
               (!^struct_type
                ^^ !^"* ptr, bennet_absint_sym x_sym, cn_base_type* x_bt, cn_term* \
                      constraint_term, bool* is_bottom_out")
          ^/^
          let per_domain_blocks =
            domains
            |> List.mapi (fun i (module D : Domain.T) ->
              let dname = D.CInt.name in
              !^(Printf.sprintf "  {")
              ^/^ !^(Printf.sprintf
                       "    bennet_absint_state* _state_%s = \
                        bennet_absint_state_create();"
                       dname)
              ^/^ !^(Printf.sprintf
                       "    _state_%s = bennet_absint_state_set_%s(_state_%s, x_sym, \
                        bennet_tagged_domain_create(x_bt, &ptr->element_%d));"
                       dname
                       dname
                       dname
                       i)
              ^/^ !^(Printf.sprintf
                       "    _state_%s = \
                        bennet_%s_transform_backward_assume(constraint_term, true, \
                        _state_%s);"
                       dname
                       dname
                       dname)
              ^/^ !^(Printf.sprintf
                       "    if (bennet_absint_state_is_bottom_%s(_state_%s)) {"
                       dname
                       dname)
              ^/^ !^"      *is_bottom_out = true;"
              ^/^ !^"      return ptr;"
              ^/^ !^"    }"
              ^/^ !^(Printf.sprintf
                       "    bennet_tagged_domain _new_%s = \
                        bennet_absint_state_get_%s(_state_%s, x_sym, x_bt);"
                       dname
                       dname
                       dname)
              ^/^ !^(Printf.sprintf
                       "    result->element_%d = *(bennet_domain_%s(ty)*)_new_%s.domain;"
                       i
                       dname
                       dname)
              ^/^ !^"  }")
            |> separate hardline
          in
          braces
            (!^(Printf.sprintf
                  "  %s* result = std_malloc(sizeof(%s));"
                  struct_type
                  struct_type)
             ^/^ per_domain_blocks
             ^/^ !^(Printf.sprintf "  %sreduce_##ty(result);" func_prefix)
             ^/^ !^(Printf.sprintf "  if (%sis_bottom_##ty(result)) {" func_prefix)
             ^/^ !^"    *is_bottom_out = true;"
             ^/^ !^"    std_free(result);"
             ^/^ !^"    return ptr;"
             ^/^ !^"  }"
             ^/^ !^"  *is_bottom_out = false;"
             ^/^ !^"  return result;")


        let generate_refine_with_state_function =
          !^"static inline"
          ^^^ !^struct_type
          ^^ !^"*"
          ^^^ !^(func_prefix ^ "refine_with_state_##ty")
          ^^ parens
               (!^struct_type
                ^^ !^"* ptr, bennet_absint_sym x_sym, cn_base_type* x_bt, cn_term* \
                      constraint_term, bool* is_bottom_out, bennet_absint_sym extra_sym, \
                      bennet_tagged_domain extra_domain")
          ^/^
          let per_domain_blocks =
            domains
            |> List.mapi (fun i (module D : Domain.T) ->
              let dname = D.CInt.name in
              !^(Printf.sprintf "  {")
              ^/^ !^(Printf.sprintf
                       "    bennet_absint_state* _init_%s = bennet_absint_state_create();"
                       dname)
              ^/^ !^(Printf.sprintf
                       "    _init_%s = bennet_absint_state_set_%s(_init_%s, extra_sym, \
                        bennet_tagged_domain_create(extra_domain.type, \
                        %sget_element_%d(extra_domain.type, extra_domain.domain)));"
                       dname
                       dname
                       dname
                       func_prefix
                       i)
              ^/^ !^(Printf.sprintf
                       "    bennet_absint_state* _state_%s = _init_%s;"
                       dname
                       dname)
              ^/^ !^(Printf.sprintf
                       "    _state_%s = bennet_absint_state_set_%s(_state_%s, x_sym, \
                        bennet_tagged_domain_create(x_bt, &ptr->element_%d));"
                       dname
                       dname
                       dname
                       i)
              ^/^ !^(Printf.sprintf
                       "    _state_%s = \
                        bennet_%s_transform_backward_assume(constraint_term, true, \
                        _state_%s);"
                       dname
                       dname
                       dname)
              ^/^ !^(Printf.sprintf
                       "    if (bennet_absint_state_is_bottom_%s(_state_%s)) {"
                       dname
                       dname)
              ^/^ !^"      *is_bottom_out = true;"
              ^/^ !^"      return ptr;"
              ^/^ !^"    }"
              ^/^ !^(Printf.sprintf
                       "    bennet_tagged_domain _new_%s = \
                        bennet_absint_state_get_%s(_state_%s, x_sym, x_bt);"
                       dname
                       dname
                       dname)
              ^/^ !^(Printf.sprintf
                       "    result->element_%d = *(bennet_domain_%s(ty)*)_new_%s.domain;"
                       i
                       dname
                       dname)
              ^/^ !^"  }")
            |> separate hardline
          in
          braces
            (!^(Printf.sprintf
                  "  %s* result = std_malloc(sizeof(%s));"
                  struct_type
                  struct_type)
             ^/^ per_domain_blocks
             ^/^ !^(Printf.sprintf "  %sreduce_##ty(result);" func_prefix)
             ^/^ !^(Printf.sprintf "  if (%sis_bottom_##ty(result)) {" func_prefix)
             ^/^ !^"    *is_bottom_out = true;"
             ^/^ !^"    std_free(result);"
             ^/^ !^"    return ptr;"
             ^/^ !^"  }"
             ^/^ !^"  *is_bottom_out = false;"
             ^/^ !^"  return result;")


        let generate_transform_backward_function =
          let per_domain_blocks =
            domains
            |> List.mapi (fun i (module D : Domain.T) ->
              let dname = D.CInt.name in
              !^"  {"
              ^/^ !^"    bennet_absint_state* _state = bennet_absint_state_create();"
              ^/^ !^(Printf.sprintf
                       "    bennet_tagged_domain _out = \
                        bennet_tagged_domain_create(output_bt, \
                        &output_domain->element_%d);"
                       i)
              ^/^ !^(Printf.sprintf
                       "    _state = bennet_absint_state_set_%s(_state, target_sym, \
                        bennet_tagged_domain_top_%s(output_bt));"
                       dname
                       dname)
              ^/^ !^(Printf.sprintf
                       "    _state = bennet_%s_transform_backward(term, target_sym, \
                        _out, _state);"
                       dname)
              ^/^ !^(Printf.sprintf
                       "    if (bennet_absint_state_is_bottom_%s(_state)) {"
                       dname)
              ^/^ !^(Printf.sprintf "      return %sbottom_##ty();" func_prefix)
              ^/^ !^"    }"
              ^/^ !^(Printf.sprintf
                       "    bennet_tagged_domain _new = \
                        bennet_absint_state_get_%s(_state, target_sym, output_bt);"
                       dname)
              ^/^ !^(Printf.sprintf
                       "    result->element_%d = *(bennet_domain_%s(ty)*)_new.domain;"
                       i
                       dname)
              ^/^ !^"  }")
            |> separate hardline
          in
          !^"static inline"
          ^^^ !^struct_type
          ^^ !^"*"
          ^^^ !^(func_prefix ^ "transform_backward_##ty")
          ^^ parens
               (!^"cn_term* term, bennet_absint_sym target_sym, cn_base_type* output_bt, \
                   cn_base_type* target_bt, "
                ^^ !^struct_type
                ^^ !^"* output_domain")
          ^/^ braces
                (!^(Printf.sprintf
                      "  %s* result = std_malloc(sizeof(%s));"
                      struct_type
                      struct_type)
                 ^/^ per_domain_blocks
                 ^/^ !^(Printf.sprintf "  %sreduce_##ty(result);" func_prefix)
                 ^/^ !^"  return result;")


        let macro_dispatchers =
          separate
            hardline
            [ !^"#define"
              ^^^ !^func_prefix
              ^^ !^"top(ty)"
              ^^^ !^func_prefix
              ^^ !^"top_##ty()";
              !^"#define"
              ^^^ !^func_prefix
              ^^ !^"is_top(ty, ptr)"
              ^^^ !^func_prefix
              ^^ !^"is_top_##ty(ptr)";
              !^"#define"
              ^^^ !^func_prefix
              ^^ !^"bottom(ty)"
              ^^^ !^func_prefix
              ^^ !^"bottom_##ty()";
              !^"#define"
              ^^^ !^func_prefix
              ^^ !^"is_bottom(ty, ptr)"
              ^^^ !^func_prefix
              ^^ !^"is_bottom_##ty(ptr)";
              !^"#define"
              ^^^ !^func_prefix
              ^^ !^"of(ty, ...)"
              ^^^ !^func_prefix
              ^^ !^"of_##ty(__VA_ARGS__)";
              !^"#define"
              ^^^ !^func_prefix
              ^^ !^"refine(ty, ptr, sym, bt, constraint, out)"
              ^^^ !^func_prefix
              ^^ !^"refine_##ty(ptr, sym, bt, constraint, out)";
              !^"#define"
              ^^^ !^func_prefix
              ^^ !^"refine_with_state(ty, ptr, sym, bt, constraint, out, extra_sym, \
                    extra_domain)"
              ^^^ !^func_prefix
              ^^ !^"refine_with_state_##ty(ptr, sym, bt, constraint, out, extra_sym, \
                    extra_domain)";
              !^"#define"
              ^^^ !^func_prefix
              ^^ !^"top_except_ownership(ty, ptr)"
              ^^^ !^func_prefix
              ^^ !^"top_except_ownership_##ty(ptr)";
              !^"#define"
              ^^^ !^func_prefix
              ^^ !^"from_ownership(ty, own)"
              ^^^ !^func_prefix
              ^^ !^"from_ownership_##ty(own)";
              !^"#define"
              ^^^ !^func_prefix
              ^^ !^"transform_backward(ty, term, sym, out_bt, tgt_bt, out)"
              ^^^ !^func_prefix
              ^^ !^"transform_backward_##ty(term, sym, out_bt, tgt_bt, out)"
            ]


        let definitions () =
          let open Pp in
          let domain_defs =
            domains
            |> List.map (fun (module D : Domain.T) -> D.CInt.definitions ())
            |> List.fold_left ( ^^ ) empty
          in
          let type_macro =
            !^"#define"
            ^^^ !^c_prefix
            ^^ parens !^"ty"
            ^^^ !^"struct"
            ^^^ !^c_prefix
            ^^ underscore
            ^^ !^"##ty"
          in
          let decl_macro_name = !^(String.uppercase_ascii c_prefix ^ "_DECL") in
          let ty = decl_macro_name ^^ parens !^"ty" in
          let element_accessors_for_type =
            domains
            |> List.mapi (fun i _dom ->
              !^(Printf.sprintf
                   "static inline void* %sget_element_%d_##ty(void* domain)"
                   func_prefix
                   i)
              ^/^ braces
                    !^(Printf.sprintf
                         "  return &((%s*)domain)->element_%d;"
                         struct_type
                         i))
            |> separate hardline
          in
          let functions_for_type =
            separate
              hardline
              [ generate_unary_op "top";
                generate_unary_predicate "is_top" "&&";
                generate_unary_op "bottom";
                generate_unary_predicate "is_bottom" "||";
                generate_binary_predicate "leq" "&&";
                generate_binary_predicate "equal" "&&";
                generate_binary_op "join";
                generate_reduce_function;
                generate_meet_function;
                generate_copy_function;
                generate_arbitrary_function;
                generate_from_assignment_function;
                generate_of_constructor_function;
                generate_check_function;
                generate_check_ownership_function;
                generate_top_except_ownership_function;
                generate_from_ownership_function;
                generate_to_interval_function;
                generate_of_interval_function;
                generate_refine_function;
                generate_transform_backward_function;
                element_accessors_for_type
              ]
          in
          let definitions_macro =
            escape_lines
              (!^"#define"
               ^^^ ty
               ^/^ !^c_prefix
               ^^ parens !^"ty"
               ^^^ braces
                     (separate
                        hardline
                        (List.mapi
                           (fun i (module D : Domain.T) ->
                              !^"bennet_domain_"
                              ^^ !^D.CInt.name
                              ^^ parens !^"ty"
                              ^^^ !^"element_"
                              ^^ int i
                              ^^ semi)
                           domains))
               ^^ semi
               ^/^ hardline
               ^^ functions_for_type)
            ^/^ hardline
          in
          let types =
            [ "int8_t";
              "uint8_t";
              "int16_t";
              "uint16_t";
              "int32_t";
              "uint32_t";
              "int64_t";
              "uint64_t";
              "uintptr_t"
            ]
          in
          let instantiations =
            separate_map hardline (fun ty -> decl_macro_name ^^ parens !^ty) types
          in
          (* Generate concrete type-dispatch functions for element access.
             These are defined after all type instantiations so that all
             type-specific accessors are available. *)
          let element_dispatch_functions =
            domains
            |> List.mapi (fun i _dom ->
              let fn_name = Printf.sprintf "%sget_element_%d" func_prefix i in
              !^(Printf.sprintf
                   "static inline void* %s(cn_base_type* type, void* domain)"
                   fn_name)
              ^/^ braces
                    (!^"  if (type->tag == CN_BASE_LOC)"
                     ^/^ !^(Printf.sprintf "    return %s_uintptr_t(domain);" fn_name)
                     ^/^ !^"  assert(type->tag == CN_BASE_BITS);"
                     ^/^ !^"  if (type->data.bits.is_signed) {"
                     ^/^ !^"    switch (type->data.bits.size_bits) {"
                     ^/^ !^(Printf.sprintf
                              "      case 8: return %s_int8_t(domain);"
                              fn_name)
                     ^/^ !^(Printf.sprintf
                              "      case 16: return %s_int16_t(domain);"
                              fn_name)
                     ^/^ !^(Printf.sprintf
                              "      case 32: return %s_int32_t(domain);"
                              fn_name)
                     ^/^ !^(Printf.sprintf
                              "      case 64: return %s_int64_t(domain);"
                              fn_name)
                     ^/^ !^"      default: assert(0); return NULL;"
                     ^/^ !^"    }"
                     ^/^ !^"  } else {"
                     ^/^ !^"    switch (type->data.bits.size_bits) {"
                     ^/^ !^(Printf.sprintf
                              "      case 8: return %s_uint8_t(domain);"
                              fn_name)
                     ^/^ !^(Printf.sprintf
                              "      case 16: return %s_uint16_t(domain);"
                              fn_name)
                     ^/^ !^(Printf.sprintf
                              "      case 32: return %s_uint32_t(domain);"
                              fn_name)
                     ^/^ !^(Printf.sprintf
                              "      case 64: return %s_uint64_t(domain);"
                              fn_name)
                     ^/^ !^"      default: assert(0); return NULL;"
                     ^/^ !^"    }"
                     ^/^ !^"  }"))
            |> separate (twice hardline)
          in
          (* Generate the refine_with_state macro separately, instantiated
             after dispatch functions are available *)
          let rws_decl_macro_name = !^(String.uppercase_ascii c_prefix ^ "_RWS_DECL") in
          let rws_ty = rws_decl_macro_name ^^ parens !^"ty" in
          let rws_definitions_macro =
            escape_lines (!^"#define" ^^^ rws_ty ^/^ generate_refine_with_state_function)
            ^/^ hardline
          in
          let rws_instantiations =
            separate_map hardline (fun ty -> rws_decl_macro_name ^^ parens !^ty) types
          in
          domain_defs
          ^/^ hardline
          ^^ type_macro
          ^/^ hardline
          ^^ definitions_macro
          ^/^ hardline
          ^^ instantiations
          ^/^ twice hardline
          ^^ element_dispatch_functions
          ^/^ twice hardline
          ^^ rws_definitions_macro
          ^/^ hardline
          ^^ rws_instantiations
          ^/^ hardline
          ^^ macro_dispatchers
          ^^ hardline
      end

      module Relative = struct
        type t = relative_component array

        let equal p1 p2 =
          if Array.length p1 = 0 || Array.length p2 = 0 then
            failwith "Cannot compare empty product domains";
          if Array.length p1 <> Array.length p2 then
            failwith "Comparing products of different lengths";
          Array.for_all2
            (fun c1 c2 ->
               match (c1, c2) with
               | RPack ((module R1), s1), RPack ((module R2), s2) ->
                 if String.equal R1.name R2.name then
                   (* Unsafe, but unavoidable without changing Domain.T to include a
                    type ID. The string comparison is a safeguard. The real safety
                    comes from the invariant that product domains are always
                    constructed from the same list of base domains in the same order. *)
                   R1.equal s1 (Obj.magic s2)
                 else
                   failwith
                     ("Comparing products of different domains: "
                      ^ R1.name
                      ^ " vs "
                      ^ R2.name))
            p1
            p2


        let compare p1 p2 =
          if Array.length p1 = 0 || Array.length p2 = 0 then
            failwith "Cannot compare empty product domains";
          let len1, len2 = (Array.length p1, Array.length p2) in
          if len1 <> len2 then failwith "Comparing products of different lengths";
          let rec compare_arrays i =
            if i >= len1 then
              0
            else (
              let c1, c2 = (p1.(i), p2.(i)) in
              match (c1, c2) with
              | RPack ((module R1), s1), RPack ((module R2), s2) ->
                if not (String.equal R1.name R2.name) then
                  failwith
                    ("Comparing products of different domains: "
                     ^ R1.name
                     ^ " vs "
                     ^ R2.name);
                let state_cmp = R1.compare s1 (Obj.magic s2) in
                if state_cmp <> 0 then
                  state_cmp
                else
                  compare_arrays (i + 1))
          in
          compare_arrays 0


        let name = name

        open Pp

        let is_top p = Array.for_all (fun (RPack ((module R), r)) -> R.is_top r) p

        let is_bottom p = Array.exists (fun (RPack ((module R), r)) -> R.is_bottom r) p

        let pp s =
          parens
            (separate_map
               (comma ^^ space)
               (fun (RPack ((module R), r)) -> R.pp r)
               (Array.to_list s))


        let pp_args s =
          s
          |> Array.map (fun (RPack ((module R), r)) -> R.pp_args r)
          |> Array.to_list
          |> String.concat ", "


        let to_lc (s : t) (sym : Sym.t) : LC.t =
          let loc = Locations.other __LOC__ in
          let constraints =
            s
            |> Array.to_list
            |> List.filter_map (fun (RPack ((module R), r)) ->
              match R.to_lc r sym with LC.T it -> Some it | _ -> failwith "TODO")
          in
          LC.T (MT.and_ constraints loc)
      end

      let name = name

      type t = domain_component array

      let bottom =
        Array.of_list
          (List.map (fun (module D : Domain.T) -> DPack ((module D), D.bottom)) domains)


      let top =
        Array.of_list
          (List.map (fun (module D : Domain.T) -> DPack ((module D), D.top)) domains)


      let equal p1 p2 =
        if Array.length p1 = 0 || Array.length p2 = 0 then
          failwith "Cannot compare empty product domains";
        if Array.length p1 <> Array.length p2 then
          failwith "Comparing products of different lengths";
        Array.for_all2
          (fun c1 c2 ->
             match (c1, c2) with
             | DPack ((module D1), s1), DPack ((module D2), s2) ->
               if String.equal D1.name D2.name then
                 (* Unsafe, but unavoidable without changing Domain.T to include a
                    type ID. The string comparison is a safeguard. The real safety
                    comes from the invariant that product domains are always
                    constructed from the same list of base domains in the same order. *)
                 D1.equal s1 (Obj.magic s2)
               else
                 failwith
                   ("Comparing products of different domains: "
                    ^ D1.name
                    ^ " vs "
                    ^ D2.name))
          p1
          p2


      let compare p1 p2 =
        if Array.length p1 = 0 || Array.length p2 = 0 then
          failwith "Cannot compare empty product domains";
        let len1, len2 = (Array.length p1, Array.length p2) in
        if len1 <> len2 then failwith "Comparing products of different lengths";
        let rec compare_arrays i =
          if i >= len1 then
            0
          else (
            let c1, c2 = (p1.(i), p2.(i)) in
            match (c1, c2) with
            | DPack ((module D1), s1), DPack ((module D2), s2) ->
              if not (String.equal D1.name D2.name) then
                failwith
                  ("Comparing products of different domains: "
                   ^ D1.name
                   ^ " vs "
                   ^ D2.name);
              let state_cmp = D1.compare s1 (Obj.magic s2) in
              if state_cmp <> 0 then
                state_cmp
              else
                compare_arrays (i + 1))
        in
        compare_arrays 0


      (** Cross-pollinate constraints between all domain components via
          interval extraction. Each domain i extracts its per-symbol interval
          bounds and meets them into every other domain j (Gauss-Seidel style). *)
      let reduce (product : t) : t =
        let n = Array.length product in
        let rec fixpoint product =
          let result = Array.copy product in
          for i = 0 to n - 1 do
            let (DPack ((module Di), si)) = result.(i) in
            let intervals = Di.to_interval si in
            if not (List.is_empty intervals) then
              for j = 0 to n - 1 do
                if i <> j then (
                  let (DPack ((module Dj), sj)) = result.(j) in
                  let sj' =
                    List.fold_left
                      (fun s (sym, bt, lo, hi) -> Dj.meet s (Dj.of_interval sym bt lo hi))
                      sj
                      intervals
                  in
                  result.(j) <- DPack ((module Dj), sj'))
              done
          done;
          if equal product result then result else fixpoint result
        in
        let result = fixpoint product in
        if Array.exists (fun (DPack ((module D), s)) -> D.equal s D.bottom) result then
          bottom
        else
          result


      let leq p1 p2 =
        if Array.length p1 = 0 || Array.length p2 = 0 then
          failwith "Cannot compare empty product domains";
        if Array.length p1 <> Array.length p2 then
          false
        else
          Array.for_all2
            (fun c1 c2 ->
               match (c1, c2) with
               | DPack ((module D1), s1), DPack ((module D2), s2) ->
                 if String.equal D1.name D2.name then
                   D1.leq s1 (Obj.magic s2)
                 else
                   failwith
                     ("Comparing products of different domains: "
                      ^ D1.name
                      ^ " vs "
                      ^ D2.name))
            p1
            p2


      let join p1 p2 =
        if Array.length p1 = 0 || Array.length p2 = 0 then
          failwith "Cannot join empty product domains";
        if Array.length p1 <> Array.length p2 then
          failwith "Product domain array length mismatch";
        let result =
          Array.map2
            (fun c1 c2 ->
               match (c1, c2) with
               | DPack ((module D1), s1), DPack ((module D2), s2) ->
                 if String.equal D1.name D2.name then (
                   let result = D1.join s1 (Obj.magic s2) in
                   DPack ((module D1), result))
                 else
                   failwith
                     ("Joining products of different domains: "
                      ^ D1.name
                      ^ " vs "
                      ^ D2.name))
            p1
            p2
        in
        reduce result


      let meet p1 p2 =
        if Array.length p1 = 0 || Array.length p2 = 0 then
          failwith "Cannot meet empty product domains";
        if Array.length p1 <> Array.length p2 then
          failwith "Product domain array length mismatch";
        let result =
          Array.map2
            (fun c1 c2 ->
               match (c1, c2) with
               | DPack ((module D1), s1), DPack ((module D2), s2) ->
                 if String.equal D1.name D2.name then (
                   let result = D1.meet s1 (Obj.magic s2) in
                   DPack ((module D1), result))
                 else
                   failwith
                     ("Meeting products of different domains: "
                      ^ D1.name
                      ^ " vs "
                      ^ D2.name))
            p1
            p2
        in
        let result = reduce result in
        (* If any component is bottom, the whole product is unsatisfiable *)
        let any_component_bottom =
          Array.exists (fun (DPack ((module D), s)) -> D.equal s D.bottom) result
        in
        if any_component_bottom then bottom else result


      let join_many products = reduce (List.fold_left join bottom products)

      let meet_many products = List.fold_left meet top products

      let rename ~from ~to_ product =
        Array.map
          (fun comp ->
             match comp with
             | DPack ((module D), s) ->
               let result = D.rename ~from ~to_ s in
               DPack ((module D), result))
          product


      let remove sym product =
        Array.map
          (fun comp ->
             match comp with
             | DPack ((module D), s) ->
               let result = D.remove sym s in
               DPack ((module D), result))
          product


      let retain syms product =
        Array.map
          (fun comp ->
             match comp with
             | DPack ((module D), s) ->
               let result = D.retain syms s in
               DPack ((module D), result))
          product


      let relative_to sym bt (product : t) : Relative.t =
        let relative_strings =
          Array.map
            (fun comp ->
               match comp with
               | DPack ((module D), s) ->
                 let rel = D.relative_to sym bt s in
                 RPack ((module D.Relative), rel))
            product
        in
        relative_strings


      let free_vars product =
        product
        |> Array.map (fun comp ->
          match comp with DPack ((module D), s) -> D.free_vars s)
        |> Array.fold_left Sym.Set.union Sym.Set.empty


      let free_vars_bts product =
        product
        |> Array.fold_left
             (fun acc comp ->
                match comp with
                | DPack ((module D), s) ->
                  Sym.Map.union
                    (fun _ bt1 bt2 ->
                       assert (BaseTypes.equal bt1 bt2);
                       Some bt1)
                    acc
                    (D.free_vars_bts s))
             Sym.Map.empty


      let pp product =
        let open Pp in
        parens
          (separate_map
             (comma ^^ space)
             (fun comp -> match comp with DPack ((module D), s) -> D.pp s)
             (Array.to_list product))


      let abs_assert (lc : LC.t) (product : t) : t =
        let result =
          Array.map
            (fun comp ->
               match comp with
               | DPack ((module D), s) -> DPack ((module D), D.abs_assert lc s))
            product
        in
        reduce result


      let abs_assign (assign_info : (T.t * Sctypes.t) * T.t) (product : t) : t =
        let result =
          Array.map
            (fun comp ->
               match comp with
               | DPack ((module D), s) ->
                 let result = D.abs_assign assign_info s in
                 DPack ((module D), result))
            product
        in
        reduce result


      let pp_params () : string =
        domains
        |> List.map (fun (module D : Domain.T) -> D.pp_params ())
        |> String.concat ", "


      let pp_args () : string =
        domains
        |> List.map (fun (module D : Domain.T) -> D.pp_args ())
        |> String.concat ", "


      let to_lc (product : t) : LC.t =
        let loc = Locations.other __LOC__ in
        let constraints =
          Array.to_list product
          |> List.map (fun comp ->
            match comp with
            | DPack ((module D), s) ->
              (match D.to_lc s with LC.T it -> it | _ -> failwith "TODO"))
        in
        LC.T (MT.and_ constraints loc)


      let to_interval product =
        Array.to_list product
        |> List.concat_map (fun (DPack ((module D), s)) -> D.to_interval s)


      let of_interval sym bt lo hi =
        Array.map
          (fun (DPack ((module D), _)) -> DPack ((module D), D.of_interval sym bt lo hi))
          top


      let is_meet_assoc =
        List.for_all (fun (module D : Domain.T) -> D.is_meet_assoc) domains


      let is_join_assoc =
        List.for_all (fun (module D : Domain.T) -> D.is_join_assoc) domains
    end
    in
    (module ProductDomain : Domain.T)
