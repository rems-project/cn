type act =
  { loc : Locations.t;
    annot : Cerb_frontend.Annot.annot list;
    ct : Sctypes.t
  }

type 'TY object_value_ =
  | OVinteger of Cerb_frontend.Impl_mem.integer_value
  | OVfloating of Cerb_frontend.Impl_mem.floating_value
  | OVpointer of Cerb_frontend.Impl_mem.pointer_value
  | OVarray of 'TY loaded_value list
  | OVstruct of Sym.t * (Id.t * Sctypes.t * Cerb_frontend.Impl_mem.mem_value) list
  | OVunion of Sym.t * Id.t * Cerb_frontend.Impl_mem.mem_value

and 'TY object_value = OV of 'TY * 'TY object_value_

and 'TY loaded_value =
  | LVspecified of 'TY object_value
  | LVunspecified of Cerb_frontend.Ctype.ctype

and 'TY value_ =
  | Vobject of 'TY object_value
  | Vloaded of 'TY loaded_value
  | Vctype of Cerb_frontend.Ctype.ctype
  | Vunit
  | Vtrue
  | Vfalse
  | Vlist of Cerb_frontend.Core.core_base_type * 'TY value list
  | Vtuple of 'TY value list

and 'TY value = V of 'TY * 'TY value_

let bt_of_value (V (bty, _)) = bty

let bt_of_object_value (OV (bty, _)) = bty

let bt_of_loaded_value = function
  | LVspecified ov -> bt_of_object_value ov
  | LVunspecified _ -> assert false


type ctor = Cerb_frontend.Core.ctor

type 'TY pattern_ =
  | CaseBase of (Sym.t option * Cerb_frontend.Core.core_base_type)
  | CaseCtor of ctor * 'TY pattern list

and 'TY pattern =
  | Pattern of Locations.t * Cerb_frontend.Annot.annot list * 'TY * 'TY pattern_

let bt_of_pattern (Pattern (_, _, bty, _)) = bty

let loc_of_pattern (Pattern (loc, _, _, _)) = loc

type 'sym generic_name = 'sym Cerb_frontend.Core.generic_name

type integerType = Cerb_frontend.Ctype.integerType

type 'TY pexpr_ =
  | PEsym of Sym.t
  | PEval of 'TY value
  | PEconstrained of (Cerb_frontend.Mem.mem_iv_constraint * 'TY pexpr) list
  | PEundef of Locations.t * Cerb_frontend.Undefined.undefined_behaviour
  | PEerror of string * 'TY pexpr
  | PEctor of ctor * 'TY pexpr list
  | PEmember_shift of 'TY pexpr * Sym.t * Id.t
  | PEarray_shift of 'TY pexpr * Sctypes.t * 'TY pexpr
  | PEcatch_exceptional_condition of integerType * Cerb_frontend.Core.iop * 'TY pexpr * 'TY pexpr
  | PEwrapI of integerType * Cerb_frontend.Core.iop * 'TY pexpr * 'TY pexpr
  | PEmemop of Cerb_frontend.Mem_common.pure_memop * 'TY pexpr
  | PEnot of 'TY pexpr
  | PEop of Cerb_frontend.Core.binop * 'TY pexpr * 'TY pexpr
  | PEconv_int of 'TY pexpr * 'TY pexpr
  | PEstruct of Sym.t * (Id.t * 'TY pexpr) list
  | PEunion of Sym.t * Id.t * 'TY pexpr
  | PEcfunction of 'TY pexpr
  | PEmemberof of Sym.t * Id.t * 'TY pexpr
  | PEcall of Sym.t generic_name * 'TY pexpr list
  | PElet of 'TY pattern * 'TY pexpr * 'TY pexpr
  | PEif of 'TY pexpr * 'TY pexpr * 'TY pexpr
  | PEare_compatible of 'TY pexpr * 'TY pexpr

and 'TY pexpr = Pexpr of Locations.t * Cerb_frontend.Annot.annot list * 'TY * 'TY pexpr_

let loc_of_pexpr (Pexpr (loc, _, _, _)) = loc

let bt_of_pexpr : 'TY. 'TY pexpr -> 'TY = fun (Pexpr (_loc, _annots, bty, _e)) -> bty

let is_undef_or_error_pexpr (Pexpr (_, _, _, pe)) =
  match pe with PEundef _ | PEerror _ -> true | _ -> false


let is_ctype_const (Pexpr (_loc, _, _, pe)) =
  match pe with PEval (V (_, Vctype ct)) -> Some ct | _ -> None


type m_kill_kind =
  | Dynamic
  | Static of Sctypes.t

type 'TY action_ =
  | Create of 'TY pexpr * act * Cerb_frontend.Symbol.prefix
  | CreateReadOnly of 'TY pexpr * act * 'TY pexpr * Cerb_frontend.Symbol.prefix
  | Alloc of 'TY pexpr * 'TY pexpr * Cerb_frontend.Symbol.prefix
  | Kill of m_kill_kind * 'TY pexpr
  | Store of bool * act * 'TY pexpr * 'TY pexpr * Cerb_frontend.Cmm_csem.memory_order
  | Load of act * 'TY pexpr * Cerb_frontend.Cmm_csem.memory_order
  | RMW of
      act
      * 'TY pexpr
      * 'TY pexpr
      * 'TY pexpr
      * Cerb_frontend.Cmm_csem.memory_order
      * Cerb_frontend.Cmm_csem.memory_order
  | Fence of Cerb_frontend.Cmm_csem.memory_order
  | CompareExchangeStrong of
      act
      * 'TY pexpr
      * 'TY pexpr
      * 'TY pexpr
      * Cerb_frontend.Cmm_csem.memory_order
      * Cerb_frontend.Cmm_csem.memory_order
  | CompareExchangeWeak of
      act
      * 'TY pexpr
      * 'TY pexpr
      * 'TY pexpr
      * Cerb_frontend.Cmm_csem.memory_order
      * Cerb_frontend.Cmm_csem.memory_order
  | LinuxFence of Cerb_frontend.Linux.linux_memory_order
  | LinuxLoad of act * 'TY pexpr * Cerb_frontend.Linux.linux_memory_order
  | LinuxStore of act * 'TY pexpr * 'TY pexpr * Cerb_frontend.Linux.linux_memory_order
  | LinuxRMW of act * 'TY pexpr * 'TY pexpr * Cerb_frontend.Linux.linux_memory_order

type 'TY action = Action of Locations.t * 'TY action_

type 'TY paction = Paction of Cerb_frontend.Core.polarity * 'TY action

(* type 'TY memop = *)
(*   | PtrEq of ('TY pexpr * 'TY pexpr) *)
(*   | PtrNe of ('TY pexpr * 'TY pexpr) *)
(*   | PtrLt of ('TY pexpr * 'TY pexpr) *)
(*   | PtrGt of ('TY pexpr * 'TY pexpr) *)
(*   | PtrLe of ('TY pexpr * 'TY pexpr) *)
(*   | PtrGe of ('TY pexpr * 'TY pexpr) *)
(*   | Ptrdiff of (act * 'TY pexpr * 'TY pexpr) *)
(*   | IntFromPtr of (act * act * 'TY pexpr) *)
(*   | PtrFromInt of (act * act * 'TY pexpr) *)
(*   | PtrValidForDeref of (act * 'TY pexpr) *)
(*   | PtrWellAligned of (act * 'TY pexpr) *)
(*   | PtrArrayShift of ('TY pexpr * act * 'TY pexpr) *)
(*   | PtrMemberShift of (Sym.t * Id.t * 'TY pexpr) *)
(*   | Memcpy of ('TY pexpr * 'TY pexpr * 'TY pexpr) *)
(*   | Memcmp of ('TY pexpr * 'TY pexpr * 'TY pexpr) *)
(*   | Realloc of ('TY pexpr * 'TY pexpr * 'TY pexpr) *)
(*   | Va_start of ('TY pexpr * 'TY pexpr) *)
(*   | Va_copy of 'TY pexpr *)
(*   | Va_arg of ('TY pexpr * act) *)
(*   | Va_end of 'TY pexpr *)
(*   | CopyAllocId of ('TY pexpr * 'TY pexpr) *)

type 'TY expr_ =
  | Epure of 'TY pexpr
  | Ememop of Sym.t Cerb_frontend.Mem_common.generic_memop * 'TY pexpr list
  | Eaction of 'TY paction
  | Eskip
  | Eccall of
      act * 'TY pexpr * 'TY pexpr list * (Locations.t * IndexTerms.t Cnprog.t list) option
  | Eproc of Sym.t generic_name * 'TY pexpr list
  | Elet of 'TY pattern * 'TY pexpr * 'TY expr
  | Eunseq of 'TY expr list
  | Ewseq of 'TY pattern * 'TY expr * 'TY expr
  | Esseq of 'TY pattern * 'TY expr * 'TY expr
  | Eif of 'TY pexpr * 'TY expr * 'TY expr
  | Ebound of 'TY expr
  | End of 'TY expr list
  | Erun of Sym.t * 'TY pexpr list
  | CN_progs of
      (Sym.t, Cerb_frontend.Ctype.ctype) Cerb_frontend.Cn.cn_statement list
      * Cnstatement.statement Cnprog.t list

and 'TY expr = Expr of Locations.t * Cerb_frontend.Annot.annot list * 'TY * 'TY expr_

let is_undef_or_error_expr (Expr (_, _, _, e)) =
  match e with Epure pe -> is_undef_or_error_pexpr pe | _ -> false


let bt_of_expr : 'TY. 'TY expr -> 'TY = fun (Expr (_loc, _annots, bty, _e)) -> bty

let loc_of_expr (Expr (loc, _, _, _)) = loc

type 'TY globs =
  | GlobalDef of Sctypes.t * 'TY expr
  | GlobalDecl of Sctypes.t

type 'i arguments_l =
  | Define of (Sym.t * IndexTerms.t) * Locations.info * 'i arguments_l
  | Resource of (Sym.t * (Request.t * BaseTypes.t)) * Locations.info * 'i arguments_l
  | Constraint of LogicalConstraints.t * Locations.info * 'i arguments_l
  | I of 'i

let mDefine (bound, info) t = Define (bound, info, t)

let mResource (bound, info) t = Resource (bound, info, t)

let mConstraint (lc, info) t = Constraint (lc, info, t)

let mConstraints lcs t = List.fold_right mConstraint lcs t

let mResources res t = List.fold_right mResource res t

type 'i arguments =
  | Computational of (Sym.t * BaseTypes.t) * Locations.info * 'i arguments
  | Ghost of (Sym.t * BaseTypes.t) * Locations.info * 'i arguments
  | L of 'i arguments_l

let mComputational (bound, info) t = Computational (bound, info, t)

let dtree_of_arguments_l dtree_i =
  let module IT = IndexTerms in
  let open Cerb_frontend.Pp_ast in
  let rec aux = function
    | Define ((s, it), _, t) ->
      Dnode (pp_ctor "Define", [ Dleaf (Sym.pp s); IT.dtree it; aux t ])
    | Resource ((s, (rt, bt)), _, t) ->
      Dnode
        ( pp_ctor "Resource",
          [ Dleaf (Sym.pp s); Request.dtree rt; Dleaf (BaseTypes.pp bt); aux t ] )
    | Constraint (lc, _, t) ->
      Dnode (pp_ctor "Constraint", [ LogicalConstraints.dtree lc; aux t ])
    | I i -> Dnode (pp_ctor "I", [ dtree_i i ])
  in
  aux


let dtree_of_arguments dtree_i =
  let open Cerb_frontend.Pp_ast in
  let rec aux = function
    | Computational ((s, _bt), _, lat) ->
      Dnode (pp_ctor "Computational", [ Dleaf (Sym.pp s); aux lat ])
    | Ghost ((s, _bt), _, lat) -> Dnode (pp_ctor "Ghost", [ Dleaf (Sym.pp s); aux lat ])
    | L l -> dtree_of_arguments_l dtree_i l
  in
  aux


type parse_ast_label_spec =
  { label_spec : (Sym.t, Cerb_frontend.Ctype.ctype) Cerb_frontend.Cn.cn_condition list }

type 'TY label_def =
  | Non_inlined of Locations.t * Sym.t * Cerb_frontend.Annot.label_annot * unit arguments
  | Return of Locations.t
  | Loop of
      Locations.t
      * 'TY expr arguments
      * Cerb_frontend.Annot.annot list
      * parse_ast_label_spec
      * [ `Aux_info of Locations.t * Locations.t * bool ]
(* first loc is condition, second is whole loop *)
(* loop condition location, for executable checking *)
(* bool signifies whether any loop invariant was provided by the user, for executable checking *)

type trusted =
  | Trusted of Locations.t
  | Checked

type 'TY args_and_body =
  ('TY expr * (Sym.t, 'TY label_def) Pmap.map * ReturnTypes.t) arguments

type 'TY fun_map_decl =
  | Proc of
      { loc : Locations.t;
        args_and_body : 'TY args_and_body;
        trusted : trusted
      }
  | ProcDecl of Locations.t * ArgumentTypes.ft option

type tag_definition =
  | StructDef of Memory.struct_layout
  | UnionDef

type function_to_convert =
  { loc : Locations.t;
    c_fun_sym : Sym.t;
    l_fun_sym : Sym.t
  }

type datatype =
  { loc : Locations.t;
    cases : (Sym.t * (Id.t * BaseTypes.t) list) list
  }

type 'TY file =
  { main : Sym.t option;
    tagDefs : (Sym.t, tag_definition) Pmap.map;
    globs : (Sym.t * 'TY globs) list;
    funs : (Sym.t, 'TY fun_map_decl) Pmap.map;
    extern : Cerb_frontend.Core.extern_map;
    stdlib_syms : Sym.Set.t;
    mk_functions : function_to_convert list;
    resource_predicates : (Sym.t * Definition.Predicate.t) list;
    logical_predicates : (Sym.t * Definition.Function.t) list;
    datatypes : (Sym.t * datatype) list;
    lemmata : (Sym.t * (Locations.t * ArgumentTypes.lemmat)) list;
    call_funinfo : (Sym.t, Sctypes.c_concrete_sig) Pmap.map
  }

let empty_file : 'TY file =
  { main = None;
    tagDefs = Pmap.empty Sym.compare;
    globs = [];
    funs = Pmap.empty Sym.compare;
    extern = Pmap.empty Id.compare;
    stdlib_syms = Sym.Set.empty;
    mk_functions = [];
    resource_predicates = [];
    logical_predicates = [];
    datatypes = [];
    lemmata = [];
    call_funinfo = Pmap.empty Sym.compare
  }
