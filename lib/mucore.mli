(* Module Mucore - CN-specific variant of Cerberus Core

   A more specialized version of Core – this is what CN actually uses. (Among a few other
   differences, Core can pass around C types as values – CN is more restrictive, for
   simplicity.) *)

(** Annotated C type.  The annotations are typically an explanation of
    something that might go wrong (e.g., overflow on an integer type). *)
type act =
  { loc : Locations.t; (** Source location *)
    annot : Cerb_frontend.Annot.annot list; (** Annotations *)
    ct : Sctypes.t (** Affected type *)
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

val bt_of_value : 'a value -> 'a

val bt_of_object_value : 'a object_value -> 'a

val bt_of_loaded_value : 'a loaded_value -> 'a

type ctor = Cerb_frontend.Core.ctor

type 'TY pattern_ =
  | CaseBase of (Sym.t option * Cerb_frontend.Core.core_base_type)
  | CaseCtor of ctor * 'TY pattern list

and 'TY pattern =
  | Pattern of Locations.t * Cerb_frontend.Annot.annot list * 'TY * 'TY pattern_

val bt_of_pattern : 'a pattern -> 'a

val loc_of_pattern : 'a pattern -> Locations.t

(** What to do on out of bounds.
    The annotated C type is the result type of the operation. *)
type bound_kind =
  | Bound_Wrap of act (** Wrap around (used for unsigned types) *)
  | Bound_Except of act (** Report an exception, for signed types *)

val bound_kind_act : bound_kind -> act

type 'sym generic_name = 'sym Cerb_frontend.Core.generic_name

type 'TY pexpr_ =
  | PEsym of Sym.t
  | PEval of 'TY value
  | PEconstrained of (Cerb_frontend.Mem.mem_iv_constraint * 'TY pexpr) list
  | PEundef of Locations.t * Cerb_frontend.Undefined.undefined_behaviour
  | PEerror of string * 'TY pexpr
  | PEctor of ctor * 'TY pexpr list
  | PEmember_shift of 'TY pexpr * Sym.t * Id.t
  | PEarray_shift of 'TY pexpr * Sctypes.t * 'TY pexpr
  | PEbounded_binop of bound_kind * Cerb_frontend.Core.iop * 'TY pexpr * 'TY pexpr
  | PEmemop of Cerb_frontend.Mem_common.pure_memop * 'TY pexpr
  | PEnot of 'TY pexpr
  | PEop of Cerb_frontend.Core.binop * 'TY pexpr * 'TY pexpr
  | PEconv_int of 'TY pexpr * 'TY pexpr
  | PEcatch_exceptional_condition of act * 'TY pexpr
  | PEstruct of Sym.t * (Id.t * 'TY pexpr) list
  | PEunion of Sym.t * Id.t * 'TY pexpr
  | PEcfunction of 'TY pexpr
  | PEmemberof of Sym.t * Id.t * 'TY pexpr
  | PEcall of Sym.t generic_name * 'TY pexpr list
  | PElet of 'TY pattern * 'TY pexpr * 'TY pexpr
  | PEif of 'TY pexpr * 'TY pexpr * 'TY pexpr
  | PEis_representable_integer of 'TY pexpr * act
  | PEare_compatible of 'TY pexpr * 'TY pexpr

and 'TY pexpr = Pexpr of Locations.t * Cerb_frontend.Annot.annot list * 'TY * 'TY pexpr_

val loc_of_pexpr : 'a pexpr -> Locations.t

val bt_of_pexpr : 'TY pexpr -> 'TY

val is_undef_or_error_pexpr : 'a pexpr -> bool

val is_ctype_const : 'a pexpr -> Cerb_frontend.Ctype.ctype option

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

val is_undef_or_error_expr : 'a expr -> bool

val bt_of_expr : 'TY expr -> 'TY

val loc_of_expr : 'a expr -> Locations.t

type 'TY globs =
  | GlobalDef of Sctypes.t * 'TY expr
  | GlobalDecl of Sctypes.t

type 'i arguments_l =
  | Define of (Sym.t * IndexTerms.t) * Locations.info * 'i arguments_l
  | Resource of (Sym.t * (Request.t * BaseTypes.t)) * Locations.info * 'i arguments_l
  | Constraint of LogicalConstraints.t * Locations.info * 'i arguments_l
  | I of 'i

val mDefine : (Sym.t * IndexTerms.t) * Locations.info -> 'a arguments_l -> 'a arguments_l

val mConstraint
  :  LogicalConstraints.t * Locations.info ->
  'a arguments_l ->
  'a arguments_l

val mConstraints
  :  (LogicalConstraints.t * Locations.info) list ->
  'a arguments_l ->
  'a arguments_l

val mResource
  :  (Sym.t * (Request.t * BaseTypes.t)) * Locations.info ->
  'a arguments_l ->
  'a arguments_l

val mResources
  :  ((Sym.t * (Request.t * BaseTypes.t)) * Locations.info) list ->
  'a arguments_l ->
  'a arguments_l

type 'i arguments =
  | Computational of (Sym.t * BaseTypes.t) * Locations.info * 'i arguments
  | Ghost of (Sym.t * BaseTypes.t) * Locations.info * 'i arguments
  | L of 'i arguments_l

val mComputational
  :  (Sym.t * BaseTypes.t) * Locations.info ->
  'a arguments ->
  'a arguments

val dtree_of_arguments
  :  ('a -> Cerb_frontend.Pp_ast.doc_tree) ->
  'a arguments ->
  Cerb_frontend.Pp_ast.doc_tree

type parse_ast_label_spec =
  { label_spec : (Sym.t, Cerb_frontend.Ctype.ctype) Cerb_frontend.Cn.cn_condition list }

type 'TY label_def =
  | Non_inlined of Locations.t * Sym.t * Cerb_frontend.Annot.label_annot * unit arguments
  (** This constructor is used when skipping label inlining, to
                  make CN testing usable on programs with switches. *)
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

val empty_file : 'TY file
