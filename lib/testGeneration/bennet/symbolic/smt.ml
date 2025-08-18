module CF = Cerb_frontend
module C = CF.Ctype
module IT = IndexTerms
module BT = BaseTypes
module LC = LogicalConstraints
module CtA = Fulminate.Cn_to_ail
module Records = Fulminate.Records

module Make (AD : Domain.T) = struct
  module Stage2 = Stage2.Make (AD)
  module Ctx = Stage2.Ctx
  module Term = Stage2.Term
  module Def = Stage2.Def

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
    | CType -> !^"cn_base_type_simple(CN_BASE_CTYPE)"
    | Struct tag ->
      let tag_name = Sym.pp tag in
      !^"cn_base_type_struct" ^^ parens (dquotes tag_name)
    | List _element_bt ->
      (* TODO: Implement proper recursive type creation for List types *)
      !^"cn_base_type_simple(CN_BASE_LIST) /* TODO: use cn_base_type_list with proper \
         element type */"
    | Map (_key_bt, _value_bt) ->
      (* TODO: Implement proper recursive type creation for Map types *)
      !^"cn_base_type_simple(CN_BASE_MAP) /* TODO: use cn_base_type_map with proper \
         key/value types */"
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
              ^^^ braces !^"const char*[]"
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
    braces (dquotes name ^^ comma ^^^ int id)


  (** Generate CN-SMT term creation code for IndexTerms.t *)
  let rec convert_indexterm (it : IT.t) : Pp.document =
    let (IT (term, _bt, _)) = it in
    match term with
    | Const c -> convert_const c
    | Sym sym -> Sym.pp sym
    | Unop (op, t) -> convert_unop op t
    | Binop (op, t1, t2) -> convert_binop op t1 t2
    | ITE (cond, then_term, else_term) -> convert_ite cond then_term else_term
    | EachI ((start, (var_sym, var_bt), end_i), body) ->
      convert_eachi start var_sym var_bt end_i body
    | Tuple terms -> convert_tuple terms
    | NthTuple (n, tuple_term) -> convert_nthtuple n tuple_term
    | Struct (tag, members) -> convert_struct tag members
    | StructMember (struct_term, member) -> convert_structmember struct_term member
    | StructUpdate ((struct_term, member), value) ->
      convert_structupdate struct_term member value
    | Record members -> convert_record members
    | RecordMember (record_term, member) -> convert_recordmember record_term member
    | RecordUpdate ((record_term, member), value) ->
      convert_recordupdate record_term member value
    | Constructor (constr, args) -> convert_constructor constr args
    | MemberShift (term, tag, member) -> convert_membershift term tag member
    | ArrayShift { base; ct; index } -> convert_arrayshift base ct index
    | CopyAllocId { addr; loc } -> convert_copyallocid addr loc
    | HasAllocId loc -> convert_hasallocid loc
    | SizeOf ct -> convert_sizeof ct
    | OffsetOf (tag, member) -> convert_offsetof tag member
    | Aligned { t; align } -> convert_aligned t align
    | WrapI (int_type, term) -> convert_wrapi int_type term
    | MapConst (base_type, term) -> convert_mapconst base_type term
    | MapSet (map_term, key_term, value_term) ->
      convert_mapset map_term key_term value_term
    | MapGet (map_term, key_term) -> convert_mapget map_term key_term
    | MapDef ((var_sym, var_bt), body) -> convert_mapdef var_sym var_bt body
    | Apply (func_sym, args) -> convert_apply func_sym args
    | Let ((var_sym, binding), body) -> convert_let var_sym binding body
    | Match (scrutinee, cases) -> convert_match scrutinee cases
    | Cast (target_bt, term) -> convert_cast target_bt term
    | CN_None bt -> convert_cn_none bt
    | CN_Some term -> convert_cn_some term
    | Good _ | Representable _ ->
      Pp.string "cn_smt_bool(true)" (* FIXME: Fulminate also doesn't enforce *)
    | _ -> failwith ("TODO (" ^ Pp.plain (IT.pp it) ^ ") @ " ^ __LOC__)


  and convert_const (c : Terms.const) : Pp.document =
    let open Pp in
    match c with
    | Z n -> !^"cn_smt_z" ^^ parens !^(Z.to_string n)
    | Q q -> !^"cn_smt_q" ^^ parens !^(Q.to_string q)
    | Bits ((sign, width), value) ->
      let sign_str =
        match sign with BaseTypes.Signed -> "true" | BaseTypes.Unsigned -> "false"
      in
      !^"cn_smt_bits"
      ^^ parens (!^sign_str ^^ comma ^^^ int width ^^ comma ^^^ !^(Z.to_string value))
    | MemByte _ | Pointer _ -> failwith ("Unsupported @" ^ __LOC__)
    | Bool true -> !^"cn_smt_bool(true)"
    | Bool false -> !^"cn_smt_bool(false)"
    | Unit -> !^"cn_smt_unit()"
    | Null -> !^"cn_smt_null()"
    | CType_const ct -> !^"cn_smt_ctype_const" ^^ parens (Sctypes.pp ct)
    | Default bt -> !^"cn_smt_default" ^^ parens (convert_basetype bt)
    | Alloc_id id -> !^"cn_smt_alloc_id" ^^ parens !^(Z.to_string id)


  and convert_unop (op : Terms.unop) (t : IT.t) : Pp.document =
    let open Pp in
    let operand = convert_indexterm t in
    match op with
    | Not -> !^"cn_smt_not" ^^ parens operand
    | Negate -> !^"cn_smt_negate" ^^ parens operand
    | BW_CLZ_NoSMT -> !^"cn_smt_bw_clz" ^^ parens operand
    | BW_CTZ_NoSMT -> !^"cn_smt_bw_ctz" ^^ parens operand
    | BW_FFS_NoSMT -> !^"cn_smt_bw_ffs" ^^ parens operand
    | BW_FLS_NoSMT -> !^"cn_smt_bw_fls" ^^ parens operand
    | BW_Compl -> !^"cn_smt_bw_compl" ^^ parens operand


  and convert_binop (op : Terms.binop) (t1 : IT.t) (t2 : IT.t) : Pp.document =
    let open Pp in
    let left = convert_indexterm t1 in
    let right = convert_indexterm t2 in
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
    | LT -> !^"cn_smt_lt" ^^ args
    | LE -> !^"cn_smt_le" ^^ args
    | EQ -> !^"cn_smt_eq" ^^ args
    | LTPointer -> !^"cn_smt_lt_pointer" ^^ args
    | LEPointer -> !^"cn_smt_le_pointer" ^^ args
    | Min -> !^"cn_smt_min" ^^ args
    | Max -> !^"cn_smt_max" ^^ args
    | SetUnion -> !^"cn_smt_set_union" ^^ args
    | SetIntersection -> !^"cn_smt_set_intersect" ^^ args
    | SetDifference -> !^"cn_smt_set_diff" ^^ args
    | SetMember -> !^"cn_smt_set_member" ^^ args
    | Subset -> !^"cn_smt_subset" ^^ args


  and convert_ite (cond : IT.t) (then_term : IT.t) (else_term : IT.t) : Pp.document =
    let cond_smt = convert_indexterm cond in
    let then_smt = convert_indexterm then_term in
    let else_smt = convert_indexterm else_term in
    Pp.(!^"cn_smt_ite" ^^ parens (cond_smt ^^ comma ^^^ then_smt ^^ comma ^^^ else_smt))


  and convert_eachi
        (start : int)
        (var_sym : Sym.t)
        (var_bt : BT.t)
        (end_i : int)
        (body : IT.t)
    : Pp.document
    =
    let body_smt = convert_indexterm body in
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


  and convert_tuple (terms : IT.t list) : Pp.document =
    let terms_smt = List.map convert_indexterm terms in
    let args_array =
      Pp.(braces (separate_map (comma ^^ Pp.space) (fun x -> x) terms_smt))
    in
    Pp.(!^"cn_smt_tuple" ^^ parens (int (List.length terms) ^^ comma ^^^ args_array))


  and convert_nthtuple (n : int) (tuple_term : IT.t) : Pp.document =
    let tuple_smt = convert_indexterm tuple_term in
    Pp.(!^"cn_smt_nthtuple" ^^ parens (int n ^^ comma ^^^ tuple_smt))


  and convert_struct (tag : Sym.t) (members : (Id.t * IT.t) list) : Pp.document =
    let tag_name = Sym.pp tag in
    let member_count = List.length members in
    let member_names = List.map (fun (id, _) -> Pp.dquotes (Id.pp id)) members in
    let member_values = List.map (fun (_, term) -> convert_indexterm term) members in
    let names_array =
      Pp.(braces (separate_map (comma ^^ Pp.space) (fun x -> x) member_names))
    in
    let values_array =
      Pp.(braces (separate_map (comma ^^ Pp.space) (fun x -> x) member_values))
    in
    Pp.(
      !^"cn_smt_struct"
      ^^ parens
           (dquotes tag_name
            ^^ comma
            ^^^ int member_count
            ^^ comma
            ^^^ names_array
            ^^ comma
            ^^^ values_array))


  and convert_structmember (struct_term : IT.t) (member : Id.t) : Pp.document =
    let open Pp in
    let struct_smt = convert_indexterm struct_term in
    !^"cn_smt_struct_member" ^^ parens (struct_smt ^^ comma ^^^ dquotes (Id.pp member))


  and convert_structupdate (struct_term : IT.t) (member : Id.t) (value : IT.t)
    : Pp.document
    =
    let open Pp in
    let struct_smt = convert_indexterm struct_term in
    let value_smt = convert_indexterm value in
    !^"cn_smt_struct_update"
    ^^ parens (struct_smt ^^ comma ^^^ dquotes (Id.pp member) ^^ comma ^^^ value_smt)


  and convert_record (members : (Id.t * IT.t) list) : Pp.document =
    let open Pp in
    let member_count = List.length members in
    if member_count = 0 then
      !^"cn_smt_record" ^^ parens !^"0, NULL, NULL"
    else (
      let member_names = List.map (fun (id, _) -> dquotes (Id.pp id)) members in
      let member_values = List.map (fun (_, term) -> convert_indexterm term) members in
      let names_array = braces (separate (comma ^^ space) member_names) in
      let values_array =
        braces (separate_map (comma ^^ space) (fun x -> x) member_values)
      in
      !^"cn_smt_record"
      ^^ parens (int member_count ^^ comma ^^^ names_array ^^ comma ^^^ values_array))


  and convert_recordmember (record_term : IT.t) (member : Id.t) : Pp.document =
    let record_smt = convert_indexterm record_term in
    Pp.(
      !^"cn_smt_record_member" ^^ parens (record_smt ^^ comma ^^^ dquotes (Id.pp member)))


  and convert_recordupdate (record_term : IT.t) (member : Id.t) (value : IT.t)
    : Pp.document
    =
    let record_smt = convert_indexterm record_term in
    let value_smt = convert_indexterm value in
    Pp.(
      !^"cn_smt_record_update"
      ^^ parens (record_smt ^^ comma ^^^ dquotes (Id.pp member) ^^ comma ^^^ value_smt))


  and convert_constructor (constr : Sym.t) (args : (Id.t * IT.t) list) : Pp.document =
    let constr_name = Sym.pp constr in
    let arg_count = List.length args in
    let arg_names = List.map (fun (id, _) -> Pp.dquotes (Id.pp id)) args in
    let arg_values = List.map (fun (_, term) -> convert_indexterm term) args in
    let names_array =
      Pp.(braces (separate_map (comma ^^ Pp.space) (fun x -> x) arg_names))
    in
    let values_array =
      Pp.(braces (separate_map (comma ^^ Pp.space) (fun x -> x) arg_values))
    in
    Pp.(
      !^"cn_smt_constructor"
      ^^ parens
           (dquotes constr_name
            ^^ comma
            ^^^ int arg_count
            ^^ comma
            ^^^ names_array
            ^^ comma
            ^^^ values_array))


  and convert_membershift (term : IT.t) (tag : Sym.t) (member : Id.t) : Pp.document =
    let term_smt = convert_indexterm term in
    Pp.(
      !^"cn_smt_member_shift"
      ^^ parens
           (term_smt
            ^^ comma
            ^^^ !^"offsetof"
            ^^ parens (!^"struct" ^^^ Sym.pp tag ^^ comma ^^^ Id.pp member)))


  and convert_arrayshift (base : IT.t) (ct : Sctypes.t) (index : IT.t) : Pp.document =
    let base_smt = convert_indexterm base in
    let index_smt = convert_indexterm index in
    let ctype_str = Pp.plain (Sctypes.pp ct) in
    Pp.(
      !^"cn_smt_array_shift"
      ^^ parens (base_smt ^^ comma ^^^ dquotes !^ctype_str ^^ comma ^^^ index_smt))


  and convert_copyallocid (addr : IT.t) (loc : IT.t) : Pp.document =
    let addr_smt = convert_indexterm addr in
    let loc_smt = convert_indexterm loc in
    Pp.(!^"cn_smt_copy_alloc_id" ^^ parens (addr_smt ^^ comma ^^^ loc_smt))


  and convert_hasallocid (loc : IT.t) : Pp.document =
    let loc_smt = convert_indexterm loc in
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
          ^^ parens (CF.Pp_ail.pp_ctype_human C.no_qualifiers (Sctypes.to_ctype sct)))


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


  and convert_aligned (t : IT.t) (align : IT.t) : Pp.document =
    let t_smt = convert_indexterm t in
    let align_smt = convert_indexterm align in
    Pp.(!^"cn_smt_aligned" ^^ parens (t_smt ^^ comma ^^^ align_smt))


  and convert_wrapi (int_type : Sctypes.IntegerTypes.t) (term : IT.t) : Pp.document =
    let open Pp in
    let term_smt = convert_indexterm term in
    let int_type_str = plain (Sctypes.IntegerTypes.pp int_type) in
    !^"cn_smt_wrapi" ^^ parens (dquotes !^int_type_str ^^ comma ^^^ term_smt)


  and convert_mapconst (base_type : BT.t) (term : IT.t) : Pp.document =
    let open Pp in
    let term_smt = convert_indexterm term in
    let bt_smt = convert_basetype base_type in
    !^"cn_smt_map_const" ^^ parens (bt_smt ^^ comma ^^^ term_smt)


  and convert_mapset (map_term : IT.t) (key_term : IT.t) (value_term : IT.t) : Pp.document
    =
    let open Pp in
    let map_smt = convert_indexterm map_term in
    let key_smt = convert_indexterm key_term in
    let value_smt = convert_indexterm value_term in
    !^"cn_smt_map_set" ^^ parens (map_smt ^^ comma ^^^ key_smt ^^ comma ^^^ value_smt)


  and convert_mapget (map_term : IT.t) (key_term : IT.t) : Pp.document =
    let open Pp in
    let map_smt = convert_indexterm map_term in
    let key_smt = convert_indexterm key_term in
    !^"cn_smt_map_get" ^^ parens (map_smt ^^ comma ^^^ key_smt)


  and convert_mapdef (var_sym : Sym.t) (var_bt : BT.t) (body : IT.t) : Pp.document =
    let open Pp in
    let body_smt = convert_indexterm body in
    let var_name = Sym.pp var_sym in
    let var_type = convert_basetype var_bt in
    !^"cn_smt_map_def"
    ^^ parens (dquotes var_name ^^ comma ^^^ var_type ^^ comma ^^^ body_smt)


  and convert_apply (func_sym : Sym.t) (args : IT.t list) : Pp.document =
    let open Pp in
    let func_name = Sym.pp func_sym in
    let args_smt = List.map convert_indexterm args in
    let args_array =
      !^"{" ^^ separate_map (comma ^^ space) (fun x -> x) args_smt ^^ !^"}"
    in
    !^"cn_smt_apply"
    ^^ parens
         (dquotes func_name ^^ comma ^^^ int (List.length args) ^^ comma ^^^ args_array)


  and convert_let (var_sym : Sym.t) (binding : IT.t) (body : IT.t) : Pp.document =
    let open Pp in
    let binding_smt = convert_indexterm binding in
    let body_smt = convert_indexterm body in
    let var_name = Sym.pp var_sym in
    !^"cn_smt_let"
    ^^ parens (dquotes var_name ^^ comma ^^^ binding_smt ^^ comma ^^^ body_smt)


  and convert_match (scrutinee : IT.t) (cases : (BT.t Terms.pattern * IT.t) list)
    : Pp.document
    =
    let open Pp in
    let scrutinee_smt = convert_indexterm scrutinee in
    (* For simplicity, match expressions will generate a simplified representation *)
    let case_count = List.length cases in
    !^"cn_smt_match"
    ^^ parens
         (scrutinee_smt ^^ comma ^^^ int case_count ^^ comma ^^^ !^"/* cases omitted */")


  and convert_cast (target_bt : BT.t) (term : IT.t) : Pp.document =
    let open Pp in
    let term_smt = convert_indexterm term in
    let target_bt_smt = convert_basetype target_bt in
    !^"cn_smt_cast" ^^ parens (target_bt_smt ^^ comma ^^^ term_smt)


  and convert_cn_none (bt : BT.t) : Pp.document =
    let open Pp in
    let bt_smt = convert_basetype bt in
    !^"cn_smt_none" ^^ parens bt_smt


  and convert_cn_some (term : IT.t) : Pp.document =
    let open Pp in
    let term_smt = convert_indexterm term in
    !^"cn_smt_some" ^^ parens term_smt


  (** Convert LogicalConstraints.t to CN-SMT logical constraint creation code *)
  let convert_logical_constraint (lc : LC.t) : Pp.document =
    let open Pp in
    match lc with
    | LC.T it -> convert_indexterm it
    | LC.Forall ((sym, bt), body) ->
      let var_name = convert_sym sym in
      let var_type = convert_basetype bt in
      let body_term = convert_indexterm body in
      !^"cn_logical_constraint_create_forall"
      ^^ parens (var_name ^^ comma ^^^ var_type ^^ comma ^^^ body_term)
end
