module CF = Cerb_frontend
module C = CF.Ctype
module A = CF.AilSyntax
module IT = IndexTerms
module BT = BaseTypes
module LC = LogicalConstraints
module CtA = Fulminate.Cn_to_ail
module Records = Fulminate.Records

module Make (AD : Domain.T) = struct
  module Stage4 = Stage4.Make (AD)
  module Ctx = Stage4.Ctx
  module Term = Stage4.Term
  module Def = Stage4.Def
  module StringMap = Map.Make (String)
  module StringSet = Set.Make (String)
  module IntMap = Map.Make (Int)

  (** Convert BaseTypes to CN-SMT base type creation expressions *)
  let rec convert_basetype (bt : BT.t) : Pp.document =
    let open Pp in
    match bt with
    | Unit -> !^"cn_base_type_simple(CN_BASE_UNIT)"
    | Bool -> !^"cn_base_type_simple(CN_BASE_BOOL)"
    | Integer -> !^"cn_base_type_simple(CN_BASE_INTEGER)"
    | Real -> !^"cn_base_type_simple(CN_BASE_REAL)"
    | Bits (sign, width) ->
      let sign_bool = match sign with BT.Signed -> "true" | BT.Unsigned -> "false" in
      !^"cn_base_type_bits(" ^^ !^sign_bool ^^ comma ^^^ int width ^^ !^")"
    | Loc _ -> !^"cn_base_type_simple(CN_BASE_LOC)"
    | Alloc_id ->
      !^"cn_base_type_simple(CN_BASE_INTEGER)"
      (* Alloc_id is represented as integer in SMT *)
    | CType -> !^"cn_base_type_simple(CN_BASE_CTYPE)"
    | Struct tag ->
      let tag_name = Sym.pp tag in
      !^"cn_base_type_struct" ^^ parens (dquotes tag_name)
    | Datatype tag ->
      let tag_name = Sym.pp_string_no_nums tag in
      !^"cn_base_type_datatype" ^^ parens (dquotes !^tag_name)
    | List _element_bt ->
      (* TODO: Implement proper recursive type creation for List types *)
      !^"cn_base_type_simple(CN_BASE_LIST) /* TODO: use cn_base_type_list with proper \
         element type */"
    | Map (key_bt, value_bt) ->
      let key_doc = convert_basetype key_bt in
      let value_doc = convert_basetype value_bt in
      !^"create_map_type" ^^ parens (separate (comma ^^ space) [ key_doc; value_doc ])
    | Tuple _element_bts ->
      (* TODO: Implement proper recursive type creation for Tuple types *)
      !^"cn_base_type_simple(CN_BASE_TUPLE) /* TODO: use cn_base_type_tuple with proper \
         element types */"
    | Record member_types ->
      let field_count = List.length member_types in
      if field_count = 0 then
        !^"cn_base_type_record(NULL, NULL, 0)"
      else (
        (* Generate call to helper function that handles record creation *)
        let field_names = List.map (fun (id, _) -> dquotes (Id.pp id)) member_types in
        let field_types = List.map (fun (_, bt) -> convert_basetype bt) member_types in
        let names_args = separate_map (comma ^^ space) (fun x -> x) field_names in
        let types_args = separate_map (comma ^^ space) (fun x -> x) field_types in
        !^"create_record_type"
        ^^ parens
             (int field_count
              ^^ comma
              ^^^ parens !^"const char*[]"
              ^^ braces names_args
              ^^ comma
              ^^^ parens !^"cn_base_type[]"
              ^^ braces types_args))
    | Set _element_bt ->
      (* TODO: Implement proper recursive type creation for Set types *)
      !^"cn_base_type_simple(CN_BASE_SET) /* TODO: use cn_base_type_set with proper \
         element type */"
    | Option _element_bt ->
      (* TODO: Implement proper recursive type creation for Option types *)
      !^"cn_base_type_simple(CN_BASE_OPTION) /* TODO: use cn_base_type_option with \
         proper element type */"
    | _ -> failwith ("TODO (" ^ Pp.plain (BT.pp bt) ^ ")@ " ^ __LOC__)


  (** Convert Sym.t to a cn_sym structure initialization *)
  let convert_sym (sym : Sym.t) : Pp.document =
    let open Pp in
    let name = Sym.pp sym in
    let id = Sym.num sym in
    parens !^"cn_sym" ^^ braces (dquotes name ^^ comma ^^^ int id)


  (** Assert that a pattern is not nested (only top-level constructor with PSym/PWild fields) *)
  let assert_pattern_not_nested (pat : BT.t Terms.pattern) : unit =
    let open Terms in
    match pat with
    | Pat (PConstructor (_ctor, fields), _bt, _loc) ->
      List.iter
        (fun (_id, field_pat) ->
           match field_pat with
           | Pat (PSym _, _, _) | Pat (PWild, _, _) -> ()
           | Pat (PConstructor _, _, _) ->
             failwith
               ("Nested pattern constructors are not supported in Match expressions @ "
                ^ __LOC__))
        fields
    | Pat (PSym _, _, _) | Pat (PWild, _, _) ->
      failwith ("Match pattern must be a constructor @ " ^ __LOC__)


  (** Extract constructor name from a pattern *)
  let extract_constructor_name (pat : BT.t Terms.pattern) : string =
    let open Terms in
    match pat with
    | Pat (PConstructor (ctor, _fields), _bt, _loc) -> Sym.pp_string_no_nums ctor
    | _ -> failwith ("Pattern must be a constructor @ " ^ __LOC__)


  (** Reorder a list of (Id.t * 'a) pairs to match the datatype constructor field
      ordering in sigma. This is used to ensure that constructor arguments and pattern
      fields match the SMT datatype declaration order. *)
  let reorder_constructor_fields
        (type a)
        (sigma : CF.GenTypes.genTypeCategory A.sigma)
        (dt_tag : Sym.t)
        (ctor : Sym.t)
        (fields : (Id.t * a) list)
    : (Id.t * a) list
    =
    (* Look up the datatype definition in sigma *)
    match
      List.find_opt
        (fun (dt : A.sigma_cn_datatype) -> Sym.equal dt.cn_dt_name dt_tag)
        sigma.cn_datatypes
    with
    | Some dt_def ->
      (* Find the constructor within the datatype *)
      (match
         List.find_opt (fun (ctor_sym, _) -> Sym.equal ctor_sym ctor) dt_def.cn_dt_cases
       with
       | Some (_, sigma_fields) ->
         (* Reorder fields to match sigma_fields ordering *)
         List.map
           (fun (sigma_field_id, _sigma_field_bt) ->
              match
                List.find_opt
                  (fun (field_id, _) -> Id.equal field_id sigma_field_id)
                  fields
              with
              | Some field -> field
              | None ->
                failwith
                  ("Constructor field "
                   ^ Id.get_string sigma_field_id
                   ^ " not found in arguments @ "
                   ^ __LOC__))
           sigma_fields
       | None ->
         failwith
           ("Constructor "
            ^ Sym.pp_string ctor
            ^ " not found in datatype "
            ^ Sym.pp_string dt_tag
            ^ " @ "
            ^ __LOC__))
    | None ->
      failwith
        ("Datatype "
         ^ Sym.pp_string dt_tag
         ^ " not found in sigma.cn_datatypes @ "
         ^ __LOC__)


  (** Extract pattern variables (symbol, type pairs) from a constructor pattern *)
  let extract_pattern_vars (pat : BT.t Terms.pattern) : (Sym.t * BT.t) list =
    let open Terms in
    match pat with
    | Pat (PConstructor (_ctor, fields), _bt, _loc) ->
      List.filter_map
        (fun (_id, field_pat) ->
           match field_pat with
           | Pat (PSym sym, field_bt, _loc) -> Some (sym, field_bt)
           | Pat (PWild, _, _) -> None
           | _ -> failwith ("Unexpected nested pattern @ " ^ __LOC__))
        fields
    | _ -> failwith ("Pattern must be a constructor @ " ^ __LOC__)


  (** Extract all pattern fields (including wildcards) from a constructor pattern *)
  type pattern_field =
    | PField_Sym of Sym.t * BT.t
    | PField_Wild

  let extract_pattern_fields (pat : BT.t Terms.pattern) : pattern_field list =
    let open Terms in
    match pat with
    | Pat (PConstructor (_ctor, fields), _bt, _loc) ->
      List.map
        (fun (_id, field_pat) ->
           match field_pat with
           | Pat (PSym sym, field_bt, _loc) -> PField_Sym (sym, field_bt)
           | Pat (PWild, _, _) -> PField_Wild
           | _ -> failwith ("Unexpected nested pattern @ " ^ __LOC__))
        fields
    | _ -> failwith ("Pattern must be a constructor @ " ^ __LOC__)


  (** Reorder pattern fields to match the datatype constructor field ordering in sigma.
      This is necessary because the SMT datatype declaration uses the sigma ordering,
      but the user's pattern may have fields in a different order. *)
  let reorder_pattern_fields
        (sigma : CF.GenTypes.genTypeCategory A.sigma)
        (pat : BT.t Terms.pattern)
    : pattern_field list
    =
    let open Terms in
    match pat with
    | Pat (PConstructor (ctor, fields), bt, _loc) ->
      (match bt with
       | BT.Datatype dt_tag ->
         let reordered_fields = reorder_constructor_fields sigma dt_tag ctor fields in
         List.map
           (fun (_, field_pat) ->
              match field_pat with
              | Pat (PSym sym, field_bt, _loc) -> PField_Sym (sym, field_bt)
              | Pat (PWild, _, _) -> PField_Wild
              | _ -> failwith ("Unexpected nested pattern @ " ^ __LOC__))
           reordered_fields
       | _ ->
         failwith
           ("Pattern constructor "
            ^ Sym.pp_string ctor
            ^ " has non-datatype base type @ "
            ^ __LOC__))
    | _ -> failwith ("Pattern must be a constructor @ " ^ __LOC__)


  (** Rename pattern variables according to the given mapping *)
  let rename_pattern (renaming : Sym.t Sym.Map.t) (pat : BT.t Terms.pattern)
    : BT.t Terms.pattern
    =
    let open Terms in
    match pat with
    | Pat (PConstructor (ctor, fields), bt, loc) ->
      let renamed_fields =
        List.map
          (fun (id, field_pat) ->
             match field_pat with
             | Pat (PSym sym, field_bt, field_loc) ->
               let new_sym =
                 match Sym.Map.find_opt sym renaming with Some s -> s | None -> sym
               in
               (id, Pat (PSym new_sym, field_bt, field_loc))
             | _ -> (id, field_pat))
          fields
      in
      Pat (PConstructor (ctor, renamed_fields), bt, loc)
    | _ -> pat


  (** Compute free variables in an index term, excluding the given bound variables *)
  let rec free_vars_it (bound : Sym.Set.t) (it : IT.t) : (Sym.t * BT.t) list =
    let (IT (term, bt, _)) = it in
    match term with
    | Const _ -> []
    | Sym sym ->
      if Sym.Set.mem sym bound then
        []
      else
        [ (sym, bt) ]
    | Unop (_, t) -> free_vars_it bound t
    | Binop (_, t1, t2) -> free_vars_it bound t1 @ free_vars_it bound t2
    | ITE (cond, then_term, else_term) ->
      free_vars_it bound cond
      @ free_vars_it bound then_term
      @ free_vars_it bound else_term
    | EachI ((_start, (var_sym, _var_bt), _end), body) ->
      let bound' = Sym.Set.add var_sym bound in
      free_vars_it bound' body
    | Tuple terms -> List.concat_map (free_vars_it bound) terms
    | NthTuple (_, tuple_term) -> free_vars_it bound tuple_term
    | Struct (_, members) -> List.concat_map (fun (_, t) -> free_vars_it bound t) members
    | StructMember (struct_term, _) -> free_vars_it bound struct_term
    | StructUpdate ((struct_term, _), value) ->
      free_vars_it bound struct_term @ free_vars_it bound value
    | Record members -> List.concat_map (fun (_, t) -> free_vars_it bound t) members
    | RecordMember (record_term, _) -> free_vars_it bound record_term
    | RecordUpdate ((record_term, _), value) ->
      free_vars_it bound record_term @ free_vars_it bound value
    | Constructor (_, args) -> List.concat_map (fun (_, t) -> free_vars_it bound t) args
    | MemberShift (term, _, _) -> free_vars_it bound term
    | ArrayShift { base; index; _ } -> free_vars_it bound base @ free_vars_it bound index
    | CopyAllocId { addr; loc } -> free_vars_it bound addr @ free_vars_it bound loc
    | HasAllocId loc -> free_vars_it bound loc
    | SizeOf _ | OffsetOf _ -> []
    | Aligned { t; align } -> free_vars_it bound t @ free_vars_it bound align
    | WrapI (_, term) -> free_vars_it bound term
    | MapConst (_, term) -> free_vars_it bound term
    | MapSet (map_term, key_term, value_term) ->
      free_vars_it bound map_term
      @ free_vars_it bound key_term
      @ free_vars_it bound value_term
    | MapGet (map_term, key_term) ->
      free_vars_it bound map_term @ free_vars_it bound key_term
    | MapDef ((var_sym, _var_bt), body) ->
      let bound' = Sym.Set.add var_sym bound in
      free_vars_it bound' body
    | Apply (_, args) -> List.concat_map (free_vars_it bound) args
    | Let ((var_sym, binding), body) ->
      let bound' = Sym.Set.add var_sym bound in
      free_vars_it bound binding @ free_vars_it bound' body
    | Match (scrutinee, cases) ->
      let scrutinee_fvs = free_vars_it bound scrutinee in
      let cases_fvs =
        List.concat_map
          (fun (pat, body) ->
             (* Extract pattern variables and add them to bound set *)
             let pat_vars = extract_pattern_vars pat in
             let bound' =
               List.fold_left (fun acc (sym, _) -> Sym.Set.add sym acc) bound pat_vars
             in
             free_vars_it bound' body)
          cases
      in
      scrutinee_fvs @ cases_fvs
    | Cast (_, term) -> free_vars_it bound term
    | CN_None _ -> []
    | CN_Some term -> free_vars_it bound term
    | Good _ | Representable _ -> []
    | _ -> []


  (** Remove duplicates from free variable list, keeping first occurrence *)
  let unique_free_vars (fvs : (Sym.t * BT.t) list) : (Sym.t * BT.t) list =
    let seen = ref Sym.Set.empty in
    List.filter
      (fun (sym, _bt) ->
         if Sym.Set.mem sym !seen then
           false
         else (
           seen := Sym.Set.add sym !seen;
           true))
      fvs


  (** Generate CN-SMT term creation code for IndexTerms.t *)
  let rec convert_indexterm (sigma : CF.GenTypes.genTypeCategory A.sigma) (it : IT.t)
    : Pp.document
    =
    let (IT (term, bt, loc)) = it in
    match term with
    | Const c -> convert_const c
    | Sym sym -> Sym.pp sym
    | Unop (op, t) -> convert_unop sigma op t
    | Binop (op, t1, t2) -> convert_binop sigma op t1 t2
    | ITE (cond, then_term, else_term) -> convert_ite sigma cond then_term else_term
    | EachI ((start, (var_sym, var_bt), end_i), body) ->
      convert_eachi sigma start var_sym var_bt end_i body
    | Tuple terms -> convert_tuple sigma terms
    | NthTuple (n, tuple_term) -> convert_nthtuple sigma n tuple_term
    | Struct (tag, members) -> convert_struct sigma tag members
    | StructMember (struct_term, member) ->
      convert_structmember sigma struct_term member bt
    | StructUpdate ((struct_term, member), value) ->
      convert_structupdate sigma struct_term member value
    | Record members -> convert_record sigma members
    | RecordMember (record_term, member) -> convert_recordmember sigma record_term member
    | RecordUpdate ((record_term, member), value) ->
      convert_recordupdate sigma record_term member value
    | Constructor (constr, args) -> convert_constructor sigma bt constr args
    | MemberShift (term, tag, member) -> convert_membershift sigma term tag member
    | ArrayShift { base; ct; index } -> convert_arrayshift sigma base ct index
    | CopyAllocId { addr; loc } -> convert_copyallocid sigma addr loc
    | HasAllocId loc -> convert_hasallocid sigma loc
    | SizeOf ct -> convert_sizeof ct
    | OffsetOf (tag, member) -> convert_offsetof tag member
    | Aligned { t; align } -> convert_aligned sigma t align
    | WrapI (int_type, term) -> convert_wrapi sigma int_type term
    | MapConst (base_type, term) -> convert_mapconst sigma base_type term
    | MapSet (map_term, key_term, value_term) ->
      convert_mapset sigma map_term key_term value_term
    | MapGet (map_term, key_term) -> convert_mapget sigma map_term key_term
    | MapDef ((var_sym, var_bt), body) -> convert_mapdef sigma var_sym var_bt body
    | Apply (func_sym, args) ->
      (* Try to expand builtin functions inline *)
      let expanded =
        match
          ( Builtins.apply_builtin_fun_defs func_sym args loc,
            Builtins.apply_builtin_funs func_sym (List.map IT.Surface.inj args) loc )
        with
        | Some it, _ -> Some it
        | None, Ok (Some it_surface) -> Some (IT.Surface.proj it_surface)
        | _ -> None
      in
      (match expanded with
       | Some it_expanded -> convert_indexterm sigma it_expanded
       | None -> convert_apply sigma func_sym args bt)
    | Let ((var_sym, binding), body) -> convert_let sigma var_sym binding body
    | Match (scrutinee, cases) -> convert_match sigma scrutinee cases
    | Cast (target_bt, term) -> convert_cast sigma target_bt term
    | CN_None bt -> convert_cn_none bt
    | CN_Some term -> convert_cn_some sigma term
    | Good _ | Representable _ ->
      Pp.string "cn_smt_bool(true)" (* FIXME: Fulminate also doesn't enforce *)
    | _ -> failwith ("TODO (" ^ Pp.plain (IT.pp it) ^ ") @ " ^ __LOC__)


  and convert_const (c : Terms.const) : Pp.document =
    let open Pp in
    match c with
    | Z n -> !^"cn_smt_z" ^^ parens (z n)
    | Q q -> !^"cn_smt_q" ^^ parens !^(Q.to_string q)
    | Bits ((sign, width), value) ->
      let sign_str =
        match sign with BaseTypes.Signed -> "true" | BaseTypes.Unsigned -> "false"
      in
      let z_min, _ = BT.bits_range (sign, width) in
      let suffix =
        let size_of = Memory.size_of_integer_type in
        match sign with
        | Unsigned ->
          if width <= size_of (Unsigned Int_) then
            Some A.U
          else if width <= size_of (Unsigned Long) then
            Some A.UL
          else
            Some A.ULL
        | Signed ->
          if width <= size_of (Signed Int_) then
            None
          else if width <= size_of (Signed Long) then
            Some A.L
          else
            Some A.LL
      in
      let ail_const =
        Fulminate.Utils.mk_expr
          (let k a = A.(AilEconst (ConstantInteger (IConstant (a, Decimal, suffix)))) in
           if Z.equal value z_min && BT.equal_sign sign BT.Signed then
             A.(
               AilEbinary
                 ( Fulminate.Utils.mk_expr (k (Z.neg (Z.sub (Z.neg value) Z.one))),
                   Arithmetic Sub,
                   Fulminate.Utils.mk_expr (k Z.one) ))
           else
             k value)
      in
      !^"cn_smt_bits"
      ^^ parens
           (!^sign_str
            ^^ comma
            ^^^ int width
            ^^ comma
            ^^^ CF.Pp_ail.pp_expression ail_const)
    | MemByte _ | Pointer _ -> failwith ("Unsupported @" ^ __LOC__)
    | Bool true -> !^"cn_smt_bool(true)"
    | Bool false -> !^"cn_smt_bool(false)"
    | Unit -> !^"cn_smt_unit()"
    | Null -> !^"cn_smt_null()"
    | CType_const ct -> !^"cn_smt_ctype_const" ^^ parens (Sctypes.pp ct)
    | Default bt -> !^"cn_smt_default" ^^ parens (convert_basetype bt)
    | Alloc_id id -> !^"cn_smt_alloc_id" ^^ parens (z id)


  and convert_unop
        (sigma : CF.GenTypes.genTypeCategory A.sigma)
        (op : Terms.unop)
        (t : IT.t)
    : Pp.document
    =
    let open Pp in
    let operand = convert_indexterm sigma t in
    match op with
    | Not -> !^"cn_smt_not" ^^ parens operand
    | Negate -> !^"cn_smt_negate" ^^ parens operand
    | BW_CLZ_NoSMT -> !^"cn_smt_bw_clz" ^^ parens operand
    | BW_CTZ_NoSMT -> !^"cn_smt_bw_ctz" ^^ parens operand
    | BW_FFS_NoSMT -> !^"cn_smt_bw_ffs" ^^ parens operand
    | BW_FLS_NoSMT -> !^"cn_smt_bw_fls" ^^ parens operand
    | BW_Compl -> !^"cn_smt_bw_compl" ^^ parens operand


  and convert_binop
        (sigma : CF.GenTypes.genTypeCategory A.sigma)
        (op : Terms.binop)
        (t1 : IT.t)
        (t2 : IT.t)
    : Pp.document
    =
    let open Pp in
    let left = convert_indexterm sigma t1 in
    let right = convert_indexterm sigma t2 in
    let args = parens (left ^^ comma ^^^ right) in
    match op with
    | And -> !^"cn_smt_and" ^^ args
    | Or -> !^"cn_smt_or" ^^ args
    | Implies -> !^"cn_smt_implies" ^^ args
    | Add -> !^"cn_smt_add" ^^ args
    | Sub -> !^"cn_smt_sub" ^^ args
    | Mul -> !^"cn_smt_mul" ^^ args
    | MulNoSMT -> !^"cn_smt_mul_uf" ^^ args
    | Div -> !^"cn_smt_div" ^^ args
    | DivNoSMT -> !^"cn_smt_div_uf" ^^ args
    | Exp -> !^"cn_smt_exp" ^^ args
    | ExpNoSMT -> !^"cn_smt_exp_uf" ^^ args
    | Mod -> !^"cn_smt_mod" ^^ args
    | ModNoSMT -> !^"cn_smt_mod_uf" ^^ args
    | Rem -> !^"cn_smt_rem" ^^ args
    | RemNoSMT -> !^"cn_smt_rem_uf" ^^ args
    | BW_And -> !^"cn_smt_bw_and" ^^ args
    | BW_Or -> !^"cn_smt_bw_or" ^^ args
    | BW_Xor -> !^"cn_smt_bw_xor" ^^ args
    | ShiftLeft -> !^"cn_smt_shift_left" ^^ args
    | ShiftRight -> !^"cn_smt_shift_right" ^^ args
    | LT | LTPointer -> !^"cn_smt_lt" ^^ args
    | LE | LEPointer -> !^"cn_smt_le" ^^ args
    | EQ -> !^"cn_smt_eq" ^^ args
    | Min -> !^"cn_smt_min" ^^ args
    | Max -> !^"cn_smt_max" ^^ args
    | SetUnion -> !^"cn_smt_set_union" ^^ args
    | SetIntersection -> !^"cn_smt_set_intersect" ^^ args
    | SetDifference -> !^"cn_smt_set_diff" ^^ args
    | SetMember -> !^"cn_smt_set_member" ^^ args
    | Subset -> !^"cn_smt_subset" ^^ args


  and convert_ite
        (sigma : CF.GenTypes.genTypeCategory A.sigma)
        (cond : IT.t)
        (then_term : IT.t)
        (else_term : IT.t)
    : Pp.document
    =
    let cond_smt = convert_indexterm sigma cond in
    let then_smt = convert_indexterm sigma then_term in
    let else_smt = convert_indexterm sigma else_term in
    Pp.(!^"cn_smt_ite" ^^ parens (cond_smt ^^ comma ^^^ then_smt ^^ comma ^^^ else_smt))


  and convert_eachi
        (sigma : CF.GenTypes.genTypeCategory A.sigma)
        (start : int)
        (var_sym : Sym.t)
        (var_bt : BT.t)
        (end_i : int)
        (body : IT.t)
    : Pp.document
    =
    let body_smt = convert_indexterm sigma body in
    let var_name = Sym.pp var_sym in
    let var_type = convert_basetype var_bt in
    Pp.(
      !^"cn_smt_eachi"
      ^^ parens
           (int start
            ^^ comma
            ^^^ dquotes var_name
            ^^ comma
            ^^^ var_type
            ^^ comma
            ^^^ int end_i
            ^^ comma
            ^^^ body_smt))


  and convert_tuple (sigma : CF.GenTypes.genTypeCategory A.sigma) (terms : IT.t list)
    : Pp.document
    =
    let terms_smt = List.map (convert_indexterm sigma) terms in
    let args_array =
      Pp.(braces (separate_map (comma ^^ Pp.space) (fun x -> x) terms_smt))
    in
    Pp.(!^"cn_smt_tuple" ^^ parens (int (List.length terms) ^^ comma ^^^ args_array))


  and convert_nthtuple
        (sigma : CF.GenTypes.genTypeCategory A.sigma)
        (n : int)
        (tuple_term : IT.t)
    : Pp.document
    =
    let tuple_smt = convert_indexterm sigma tuple_term in
    Pp.(!^"cn_smt_nthtuple" ^^ parens (int n ^^ comma ^^^ tuple_smt))


  and convert_struct
        (sigma : CF.GenTypes.genTypeCategory A.sigma)
        (tag : Sym.t)
        (members : (Id.t * IT.t) list)
    : Pp.document
    =
    let open Pp in
    let tag_name = Sym.pp tag in
    let member_count = List.length members in
    if member_count = 0 then
      !^"cn_smt_struct" ^^ parens (dquotes tag_name ^^ comma ^^^ !^"0, NULL, NULL")
    else (
      let member_names = List.map (fun (id, _) -> dquotes (Id.pp id)) members in
      let member_values =
        List.map (fun (_, term) -> convert_indexterm sigma term) members
      in
      (* Use compound literals for the arrays *)
      let names_array =
        parens !^"const char*[]"
        ^^ braces (separate_map (comma ^^ space) (fun x -> x) member_names)
      in
      let values_array =
        parens !^"cn_term*[]"
        ^^ braces (separate_map (comma ^^ space) (fun x -> x) member_values)
      in
      !^"cn_smt_struct"
      ^^ parens
           (dquotes tag_name
            ^^ comma
            ^^^ int member_count
            ^^ comma
            ^^^ names_array
            ^^ comma
            ^^^ values_array))


  and convert_structmember
        (sigma : CF.GenTypes.genTypeCategory A.sigma)
        (struct_term : IT.t)
        (member : Id.t)
        (bt : BT.t)
    : Pp.document
    =
    let open Pp in
    let struct_smt = convert_indexterm sigma struct_term in
    !^"cn_smt_struct_member"
    ^^ parens
         (struct_smt ^^ comma ^^^ dquotes (Id.pp member) ^^ comma ^^^ convert_basetype bt)


  and convert_structupdate
        (sigma : CF.GenTypes.genTypeCategory A.sigma)
        (struct_term : IT.t)
        (member : Id.t)
        (value : IT.t)
    : Pp.document
    =
    let open Pp in
    let struct_smt = convert_indexterm sigma struct_term in
    let value_smt = convert_indexterm sigma value in
    !^"cn_smt_struct_update"
    ^^ parens (struct_smt ^^ comma ^^^ dquotes (Id.pp member) ^^ comma ^^^ value_smt)


  and convert_record
        (sigma : CF.GenTypes.genTypeCategory A.sigma)
        (members : (Id.t * IT.t) list)
    : Pp.document
    =
    let open Pp in
    let member_count = List.length members in
    let member_names = List.map (fun (id, _) -> dquotes (Id.pp id)) members in
    let member_values =
      List.map (fun (_, term) -> convert_indexterm sigma term) members
    in
    let names_array =
      parens !^"const char*[]" ^^ braces (separate (comma ^^ space) member_names)
    in
    let values_array =
      parens !^"cn_term*[]"
      ^^ braces (separate_map (comma ^^ space) (fun x -> x) member_values)
    in
    !^"cn_smt_record"
    ^^ parens (int member_count ^^ comma ^^^ names_array ^^ comma ^^^ values_array)


  and convert_recordmember
        (sigma : CF.GenTypes.genTypeCategory A.sigma)
        (record_term : IT.t)
        (member : Id.t)
    : Pp.document
    =
    let record_smt = convert_indexterm sigma record_term in
    Pp.(
      !^"cn_smt_record_member" ^^ parens (record_smt ^^ comma ^^^ dquotes (Id.pp member)))


  and convert_recordupdate
        (sigma : CF.GenTypes.genTypeCategory A.sigma)
        (record_term : IT.t)
        (member : Id.t)
        (value : IT.t)
    : Pp.document
    =
    let record_smt = convert_indexterm sigma record_term in
    let value_smt = convert_indexterm sigma value in
    Pp.(
      !^"cn_smt_record_update"
      ^^ parens (record_smt ^^ comma ^^^ dquotes (Id.pp member) ^^ comma ^^^ value_smt))


  and convert_constructor
        (sigma : CF.GenTypes.genTypeCategory A.sigma)
        (bt : BT.t)
        (constr : Sym.t)
        (args : (Id.t * IT.t) list)
    : Pp.document
    =
    let constr_name = Sym.pp constr in
    (* Reorder args to match the datatype definition ordering in sigma *)
    let reordered_args =
      match bt with
      | BT.Datatype dt_tag -> reorder_constructor_fields sigma dt_tag constr args
      | _ ->
        failwith
          ("Constructor "
           ^ Sym.pp_string constr
           ^ " has non-datatype base type @ "
           ^ __LOC__)
    in
    let arg_count = List.length reordered_args in
    let arg_names = List.map (fun (id, _) -> Pp.dquotes (Id.pp id)) reordered_args in
    let arg_values =
      List.map (fun (_, term) -> convert_indexterm sigma term) reordered_args
    in
    let bt_smt = convert_basetype bt in
    let names_array =
      Pp.(
        parens !^"const char*[]"
        ^^ braces (separate_map (comma ^^ Pp.space) (fun x -> x) arg_names))
    in
    let values_array =
      Pp.(
        parens !^"cn_term*[]"
        ^^ braces (separate_map (comma ^^ Pp.space) (fun x -> x) arg_values))
    in
    Pp.(
      !^"cn_smt_constructor"
      ^^ parens
           (bt_smt
            ^^ comma
            ^^^ dquotes constr_name
            ^^ comma
            ^^^ int arg_count
            ^^ comma
            ^^^ names_array
            ^^ comma
            ^^^ values_array))


  and convert_membershift
        (sigma : CF.GenTypes.genTypeCategory A.sigma)
        (term : IT.t)
        (tag : Sym.t)
        (member : Id.t)
    : Pp.document
    =
    let term_smt = convert_indexterm sigma term in
    Pp.(
      !^"cn_smt_member_shift"
      ^^ parens
           (term_smt
            ^^ comma
            ^^^ !^"offsetof"
            ^^ parens (!^"struct" ^^^ Sym.pp tag ^^ comma ^^^ Id.pp member)))


  and convert_arrayshift
        (sigma : CF.GenTypes.genTypeCategory A.sigma)
        (base : IT.t)
        (ct : Sctypes.t)
        (index : IT.t)
    : Pp.document
    =
    let base_smt = convert_indexterm sigma base in
    let index_smt = convert_indexterm sigma index in
    let ctype_str =
      Pp.plain (CF.Pp_ail.pp_ctype ~is_human:false C.no_qualifiers (Sctypes.to_ctype ct))
    in
    Pp.(
      !^"cn_smt_array_shift"
      ^^ parens
           (base_smt
            ^^ comma
            ^^^ (!^"sizeof" ^^ parens !^ctype_str)
            ^^ comma
            ^^^ index_smt))


  and convert_copyallocid
        (sigma : CF.GenTypes.genTypeCategory A.sigma)
        (addr : IT.t)
        (loc : IT.t)
    : Pp.document
    =
    let addr_smt = convert_indexterm sigma addr in
    let loc_smt = convert_indexterm sigma loc in
    Pp.(!^"cn_smt_copy_alloc_id" ^^ parens (addr_smt ^^ comma ^^^ loc_smt))


  and convert_hasallocid (sigma : CF.GenTypes.genTypeCategory A.sigma) (loc : IT.t)
    : Pp.document
    =
    let loc_smt = convert_indexterm sigma loc in
    Pp.(!^"cn_smt_has_alloc_id" ^^ parens loc_smt)


  and convert_sizeof (sct : Sctypes.t) : Pp.document =
    let open Pp in
    let sign, width = Option.get (BT.is_bits_bt Memory.size_bt) in
    let sign_str =
      match sign with BaseTypes.Signed -> "true" | BaseTypes.Unsigned -> "false"
    in
    !^"cn_smt_bits"
    ^^ parens
         (!^sign_str
          ^^ comma
          ^^^ int width
          ^^ comma
          ^^^ !^"sizeof"
          ^^ parens
               (CF.Pp_ail.pp_ctype ~is_human:false C.no_qualifiers (Sctypes.to_ctype sct))
         )


  and convert_offsetof (tag : Sym.t) (member : Id.t) : Pp.document =
    let open Pp in
    let sign, width = Option.get (BT.is_bits_bt Memory.size_bt) in
    let sign_str =
      match sign with BaseTypes.Signed -> "true" | BaseTypes.Unsigned -> "false"
    in
    !^"cn_smt_bits"
    ^^ parens
         (!^sign_str
          ^^ comma
          ^^^ int width
          ^^ comma
          ^^^ !^"offsetof"
          ^^ parens (!^"struct" ^^^ Sym.pp tag ^^ comma ^^^ Id.pp member))


  and convert_aligned
        (sigma : CF.GenTypes.genTypeCategory A.sigma)
        (t : IT.t)
        (align : IT.t)
    : Pp.document
    =
    let t_smt = convert_indexterm sigma t in
    let align_smt = convert_indexterm sigma align in
    Pp.(!^"cn_smt_aligned" ^^ parens (t_smt ^^ comma ^^^ align_smt))


  and convert_wrapi
        (sigma : CF.GenTypes.genTypeCategory A.sigma)
        (int_type : Sctypes.IntegerTypes.t)
        (term : IT.t)
    : Pp.document
    =
    let open Pp in
    let term_smt = convert_indexterm sigma term in
    let int_type_str = plain (Sctypes.IntegerTypes.pp int_type) in
    !^"cn_smt_wrapi" ^^ parens (dquotes !^int_type_str ^^ comma ^^^ term_smt)


  and convert_mapconst
        (sigma : CF.GenTypes.genTypeCategory A.sigma)
        (base_type : BT.t)
        (term : IT.t)
    : Pp.document
    =
    let open Pp in
    let term_smt = convert_indexterm sigma term in
    let bt_smt = convert_basetype base_type in
    !^"cn_smt_map_const" ^^ parens (bt_smt ^^ comma ^^^ term_smt)


  and convert_mapset
        (sigma : CF.GenTypes.genTypeCategory A.sigma)
        (map_term : IT.t)
        (key_term : IT.t)
        (value_term : IT.t)
    : Pp.document
    =
    let open Pp in
    let map_smt = convert_indexterm sigma map_term in
    let key_smt = convert_indexterm sigma key_term in
    let value_smt = convert_indexterm sigma value_term in
    !^"cn_smt_map_set" ^^ parens (map_smt ^^ comma ^^^ key_smt ^^ comma ^^^ value_smt)


  and convert_mapget
        (sigma : CF.GenTypes.genTypeCategory A.sigma)
        (map_term : IT.t)
        (key_term : IT.t)
    : Pp.document
    =
    let open Pp in
    let map_smt = convert_indexterm sigma map_term in
    let key_smt = convert_indexterm sigma key_term in
    !^"cn_smt_map_get" ^^ parens (map_smt ^^ comma ^^^ key_smt)


  and convert_mapdef
        (sigma : CF.GenTypes.genTypeCategory A.sigma)
        (var_sym : Sym.t)
        (var_bt : BT.t)
        (body : IT.t)
    : Pp.document
    =
    let open Pp in
    let body_smt = convert_indexterm sigma body in
    let var_name = Sym.pp var_sym in
    let var_type = convert_basetype var_bt in
    !^"cn_smt_map_def"
    ^^ parens (dquotes var_name ^^ comma ^^^ var_type ^^ comma ^^^ body_smt)


  and convert_apply
        (sigma : CF.GenTypes.genTypeCategory A.sigma)
        (func_sym : Sym.t)
        (args : IT.t list)
        (result_bt : BT.t)
    : Pp.document
    =
    let open Pp in
    let func_name = Sym.pp func_sym in
    let args_smt = List.map (convert_indexterm sigma) args in
    let result_type = convert_basetype result_bt in
    let args_array =
      if List.length args = 0 then
        !^"NULL"
      else
        parens !^"cn_term*[]"
        ^^ braces (separate_map (comma ^^ space) (fun x -> x) args_smt)
    in
    !^"cn_smt_apply"
    ^^ parens
         (dquotes func_name
          ^^ comma
          ^^^ result_type
          ^^ comma
          ^^^ args_array
          ^^ comma
          ^^^ int (List.length args))


  and convert_let
        (sigma : CF.GenTypes.genTypeCategory A.sigma)
        (var_sym : Sym.t)
        (binding : IT.t)
        (body : IT.t)
    : Pp.document
    =
    let open Pp in
    let binding_smt = convert_indexterm sigma binding in
    let body_smt = convert_indexterm sigma body in
    (* Extract base type from binding *)
    let (IT (_, binding_bt, _)) = binding in
    let bt_smt = convert_basetype binding_bt in
    (* Get symbol info *)
    let var_name_doc = Sym.pp var_sym in
    let var_id = Sym.num var_sym in
    let var_name_c = Sym.pp_string_no_nums var_sym in
    (* Generate block statement with symbol declaration and cn_smt_let *)
    !^"({"
    ^^ nest
         2
         (hardline
          ^^ !^"cn_term* "
          ^^ !^var_name_c
          ^^ !^" = cn_smt_sym("
          ^^ parens !^"cn_sym"
          ^^ braces
               (!^".name = " ^^ dquotes var_name_doc ^^ comma ^^^ !^".id = " ^^ int var_id)
          ^^ comma
          ^^^ bt_smt
          ^^ !^");"
          ^^ hardline
          ^^ !^"cn_smt_let"
          ^^ parens
               (!^var_name_c
                ^^ !^"->data.sym"
                ^^ comma
                ^^^ binding_smt
                ^^ comma
                ^^^ body_smt)
          ^^ !^";")
    ^^ hardline
    ^^ !^"})"


  and convert_match
        (sigma : CF.GenTypes.genTypeCategory A.sigma)
        (scrutinee : IT.t)
        (cases : (BT.t Terms.pattern * IT.t) list)
    : Pp.document
    =
    let open Pp in
    let scrutinee_smt = convert_indexterm sigma scrutinee in
    let case_count = List.length cases in
    (* Step 1: Validate all patterns *)
    List.iter (fun (pat, _) -> assert_pattern_not_nested pat) cases;
    (* Step 2: Collect all pattern variables with their case indices *)
    let indexed_vars =
      List.mapi
        (fun case_idx (pat, _body) ->
           let vars = extract_pattern_vars pat in
           List.map (fun (sym, bt) -> (case_idx, sym, bt)) vars)
        cases
      |> List.concat
    in
    (* Step 2.5: Collect free variable names from scrutinee and case bodies to avoid shadowing *)
    let free_var_names =
      let scrutinee_fvs = IT.free_vars_bts scrutinee in
      let body_fvs =
        List.fold_left
          (fun acc (_pat, body) ->
             let body_free_vars = IT.free_vars_bts body in
             Sym.Map.union (fun _k v1 _v2 -> Some v1) acc body_free_vars)
          Sym.Map.empty
          cases
      in
      let all_free_vars =
        Sym.Map.union (fun _k v1 _v2 -> Some v1) scrutinee_fvs body_fvs
      in
      (* Convert to a set of strings for easy lookup *)
      Sym.Map.fold
        (fun sym _bt acc -> StringSet.add (Sym.pp_string_no_nums sym) acc)
        all_free_vars
        StringSet.empty
    in
    (* Step 3: Group variables by base name to detect duplicates *)
    let vars_by_name =
      List.fold_left
        (fun acc (case_idx, sym, bt) ->
           let name = Sym.pp_string_no_nums sym in
           let existing =
             match StringMap.find_opt name acc with Some lst -> lst | None -> []
           in
           StringMap.add name ((case_idx, sym, bt) :: existing) acc)
        StringMap.empty
        indexed_vars
    in
    (* Step 4: Assign suffixes and create renaming info *)
    (* Returns: list of (case_idx, old_sym, new_sym, orig_name, orig_id, bt) *)
    let all_renamings =
      StringMap.bindings vars_by_name
      |> List.concat_map (fun ((base_name : string), occurrences) ->
        let count = List.length occurrences in
        (* Check if this name conflicts with a free variable in scope *)
        let conflicts_with_free_var = StringSet.mem base_name free_var_names in
        if count = 1 && not conflicts_with_free_var then (* No suffix needed *)
          List.map
            (fun (case_idx, sym, bt) -> (case_idx, sym, sym, base_name, Sym.num sym, bt))
            occurrences
        else (
          (* Assign suffixes _1, _2, _3, ... in order of appearance *)
          let sorted_occurrences =
            List.sort (fun (i, _, _) (j, _, _) -> compare i j) occurrences
          in
          List.mapi
            (fun i (case_idx, sym, bt) ->
               let suffix = i + 1 in
               let new_name = base_name ^ "_" ^ string_of_int suffix in
               let new_sym = Sym.fresh_make_uniq new_name in
               (case_idx, sym, new_sym, base_name, Sym.num sym, bt))
            sorted_occurrences))
    in
    (* Step 5: Create renaming map for each case *)
    let case_renaming_maps =
      List.fold_left
        (fun acc (case_idx, old_sym, new_sym, _, _, _) ->
           let existing =
             match IntMap.find_opt case_idx acc with
             | Some map -> map
             | None -> Sym.Map.empty
           in
           IntMap.add case_idx (Sym.Map.add old_sym new_sym existing) acc)
        IntMap.empty
        all_renamings
    in
    (* Step 6: Process each case with renaming *)
    let processed_cases =
      List.mapi
        (fun case_idx (pattern, body) ->
           let ctor_name = extract_constructor_name pattern in
           let renaming_map =
             match IntMap.find_opt case_idx case_renaming_maps with
             | Some map -> map
             | None -> Sym.Map.empty
           in
           let renamed_pattern = rename_pattern renaming_map pattern in
           (* Apply multiple renamings by folding over the map *)
           let renamed_body =
             Sym.Map.fold
               (fun old_sym new_sym acc_body ->
                  IT.subst (IT.make_rename ~from:old_sym ~to_:new_sym) acc_body)
               renaming_map
               body
           in
           (ctor_name, renamed_pattern, renamed_body))
        cases
    in
    (* Step 7: Collect all unique renamed variables for declarations *)
    let unique_renamed_vars =
      List.sort_uniq
        (fun (_, _, new_sym1, _, _, _) (_, _, new_sym2, _, _, _) ->
           Sym.compare new_sym1 new_sym2)
        all_renamings
      |> List.map (fun (_, _, new_sym, orig_name, orig_id, bt) ->
        (new_sym, orig_name, orig_id, bt))
    in
    (* Step 8: Generate variable declarations *)
    let var_declarations =
      if List.length unique_renamed_vars = 0 then
        empty
      else
        separate_map
          hardline
          (fun (sym, orig_name, orig_id, bt) ->
             let var_name = Sym.pp_string_no_nums sym in
             let bt_smt = convert_basetype bt in
             !^"cn_term* "
             ^^ !^var_name
             ^^ !^" = cn_smt_sym("
             ^^ parens !^"cn_sym"
             ^^ braces
                  (!^".name = "
                   ^^ dquotes !^orig_name
                   ^^ comma
                   ^^^ !^".id = "
                   ^^ int orig_id)
             ^^ comma
             ^^^ bt_smt
             ^^ !^");")
          unique_renamed_vars
        ^^ hardline
    in
    (* Step 9: Generate pattern arrays *)
    let constructor_tags =
      parens !^"const char*[]"
      ^^ braces
           (separate_map
              (comma ^^ space)
              (fun (ctor_name, _, _) -> dquotes !^ctor_name)
              processed_cases)
    in
    (* For each case, generate array of cn_sym* for pattern variables *)
    let pattern_syms_arrays =
      parens !^"cn_sym*[]"
      ^^ braces
           (separate_map
              (comma ^^ space)
              (fun (_, renamed_pattern, _) ->
                 let fields = reorder_pattern_fields sigma renamed_pattern in
                 if List.length fields = 0 then
                   !^"NULL"
                 else
                   parens !^"cn_sym[]"
                   ^^ braces
                        (separate_map
                           (comma ^^ space)
                           (fun field ->
                              match field with
                              | PField_Sym (sym, _bt) ->
                                let var_name = Sym.pp_string_no_nums sym in
                                !^var_name ^^ !^"->data.sym"
                              | PField_Wild ->
                                parens !^"cn_sym"
                                ^^ braces (!^".name = NULL" ^^ comma ^^^ !^".id = -1"))
                           fields))
              processed_cases)
    in
    let pattern_sym_counts =
      parens !^"size_t[]"
      ^^ braces
           (separate_map
              (comma ^^ space)
              (fun (_, renamed_pattern, _) ->
                 let fields = reorder_pattern_fields sigma renamed_pattern in
                 int (List.length fields))
              processed_cases)
    in
    (* Step 10: Generate body terms array *)
    let body_terms =
      parens !^"cn_term*[]"
      ^^ braces
           (separate_map
              (comma ^^ space)
              (fun (_, _, body) -> convert_indexterm sigma body)
              processed_cases)
    in
    (* Step 11: Generate the complete block statement with cn_smt_match call *)
    parens
      (braces
         (nest
            2
            (hardline
             ^^ var_declarations
             ^^ !^"cn_smt_match"
             ^^ parens
                  (scrutinee_smt
                   ^^ comma
                   ^^^ int case_count
                   ^^ comma
                   ^^^ constructor_tags
                   ^^ comma
                   ^^^ pattern_syms_arrays
                   ^^ comma
                   ^^^ pattern_sym_counts
                   ^^ comma
                   ^^^ body_terms)
             ^^ !^";")
          ^^ hardline))


  and convert_cast
        (sigma : CF.GenTypes.genTypeCategory A.sigma)
        (target_bt : BT.t)
        (term : IT.t)
    : Pp.document
    =
    let open Pp in
    let term_smt = convert_indexterm sigma term in
    let target_bt_smt = convert_basetype target_bt in
    !^"cn_smt_cast" ^^ parens (target_bt_smt ^^ comma ^^^ term_smt)


  and convert_cn_none (bt : BT.t) : Pp.document =
    let open Pp in
    let bt_smt = convert_basetype bt in
    !^"cn_smt_none" ^^ parens bt_smt


  and convert_cn_some (sigma : CF.GenTypes.genTypeCategory A.sigma) (term : IT.t)
    : Pp.document
    =
    let open Pp in
    let term_smt = convert_indexterm sigma term in
    !^"cn_smt_some" ^^ parens term_smt


  (** Convert LogicalConstraints.t to CN-SMT logical constraint creation code *)
  let convert_logical_constraint (sigma : CF.GenTypes.genTypeCategory A.sigma) (lc : LC.t)
    : Pp.document
    =
    let open Pp in
    match lc with
    | LC.T it -> convert_indexterm sigma it
    | LC.Forall ((sym, bt), body) ->
      let var_name = convert_sym sym in
      let var_type = convert_basetype bt in
      let body_term = convert_indexterm sigma body in
      !^"cn_logical_constraint_create_forall"
      ^^ parens (var_name ^^ comma ^^^ var_type ^^ comma ^^^ body_term)


  let get_max_array_length_of (i_sym, i_bt) it_perm =
    let max_array_length_setting = Z.of_int (TestGenConfig.get_max_array_length ()) in
    let f = Simplify.IndexTerms.simp (Simplify.default Global.empty) in
    let it_min, it_max = IT.Bounds.get_bounds (i_sym, i_bt) it_perm in
    let it_min, it_max = (f it_min, f it_max) in
    let basic_length =
      match (it_min, it_max) with
      | IT (Const (Bits (_, min)), _, _), IT (Const (Bits (_, max)), _, _) ->
        Z.add (Z.sub max min) Z.one
      | _, IT (Const (Bits ((Unsigned, _), max)), _, _) -> Z.add max Z.one
      | _ -> max_array_length_setting
    in
    Z.max Z.zero (Z.min basic_length max_array_length_setting)
end
