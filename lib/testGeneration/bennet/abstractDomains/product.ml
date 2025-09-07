module IT = IndexTerms
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
    [ StringSet.of_list [ Ownership.Inner.CInt.name; Interval_.Inner.CInt.name ] ]


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
          ^^ !^"* result = malloc(sizeof("
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
                         "%s* result = malloc(sizeof(%s));"
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
                 ^^ !^"* result = malloc(sizeof("
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
          let arb_funcs =
            domains
            |> List.map (fun (module D : Domain.T) ->
              let res =
                StringSetSet.fold
                  (fun g acc ->
                     if StringSet.mem D.CInt.name g then (
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
          ^^ !^"* result = malloc(sizeof("
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
          ^^ !^"* result = malloc(sizeof("
          ^^ !^struct_type
          ^^ !^"));"
          ^/^ generate_assignments ()
          ^/^ !^"  return result;"
          ^/^ !^"}"


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
              ^^ !^"of_##ty(__VA_ARGS__)"
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
          let functions_for_type =
            separate
              hardline
              [ generate_unary_op "top";
                generate_unary_predicate "is_top" "||";
                generate_unary_op "bottom";
                generate_unary_predicate "is_bottom" "||";
                generate_binary_predicate "leq" "&&";
                generate_binary_predicate "equal" "&&";
                generate_binary_op "join";
                generate_binary_op "meet";
                generate_copy_function;
                generate_arbitrary_function;
                generate_from_assignment_function;
                generate_of_constructor_function
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
          domain_defs
          ^/^ hardline
          ^^ type_macro
          ^/^ hardline
          ^^ definitions_macro
          ^/^ hardline
          ^^ instantiations
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

        let is_top p = Array.exists (fun (RPack ((module R), r)) -> R.is_top r) p

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
        Array.map2
          (fun c1 c2 ->
             match (c1, c2) with
             | DPack ((module D1), s1), DPack ((module D2), s2) ->
               if String.equal D1.name D2.name then (
                 let result = D1.join s1 (Obj.magic s2) in
                 DPack ((module D1), result))
               else
                 failwith
                   ("Joining products of different domains: " ^ D1.name ^ " vs " ^ D2.name))
          p1
          p2


      let meet p1 p2 =
        if Array.length p1 = 0 || Array.length p2 = 0 then
          failwith "Cannot meet empty product domains";
        if Array.length p1 <> Array.length p2 then
          failwith "Product domain array length mismatch";
        Array.map2
          (fun c1 c2 ->
             match (c1, c2) with
             | DPack ((module D1), s1), DPack ((module D2), s2) ->
               if String.equal D1.name D2.name then (
                 let result = D1.meet s1 (Obj.magic s2) in
                 DPack ((module D1), result))
               else
                 failwith
                   ("Meeting products of different domains: " ^ D1.name ^ " vs " ^ D2.name))
          p1
          p2


      let join_many products = List.fold_left join bottom products

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


      let pp product =
        let open Pp in
        parens
          (separate_map
             (comma ^^ space)
             (fun comp -> match comp with DPack ((module D), s) -> D.pp s)
             (Array.to_list product))


      let abs_assert (lc : LC.t) (product : t) : t =
        Array.map
          (fun comp ->
             match comp with
             | DPack ((module D), s) ->
               let result = D.abs_assert lc s in
               DPack ((module D), result))
          product


      let abs_assign (assign_info : (IT.t * Sctypes.t) * IT.t) (product : t) : t =
        Array.map
          (fun comp ->
             match comp with
             | DPack ((module D), s) ->
               let result = D.abs_assign assign_info s in
               DPack ((module D), result))
          product


      let pp_params () : string =
        domains
        |> List.map (fun (module D : Domain.T) -> D.pp_params ())
        |> String.concat ", "


      let pp_args () : string =
        domains
        |> List.map (fun (module D : Domain.T) -> D.pp_args ())
        |> String.concat ", "
    end
    in
    (module ProductDomain : Domain.T)
