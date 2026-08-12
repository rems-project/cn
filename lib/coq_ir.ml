module BT = BaseTypes
module AT = ArgumentTypes

(* CN primitives *)

type itp_sym = ITP_sym of Sym.t

type itp_id = ITP_id of Id.t

type itp_sign =
  | ITP_Signed
  | ITP_Unsigned

(* CN BaseTypes*)

type itp_bt =
  | ITP_Bool
  | ITP_Integer
  | ITP_Bits of itp_sign * int
  | ITP_Map of itp_bt * itp_bt
  | ITP_Struct of itp_sym * itp_bt list
  | ITP_Record of itp_bt list
  | ITP_Loc
  | ITP_Datatype of itp_sym
  | ITP_List of itp_bt
  | ITP_Unit
  | ITP_Membyte
  | ITP_Real
  | ITP_Alloc_id
  | ITP_CType
  | ITP_Tuple of itp_bt list
  | ITP_Set of itp_bt

(* CN IndexTerms *)

type itp_pat =
  | ITP_pSym of itp_sym
  | ITP_pWild
  | ITP_pConstructor of itp_sym * itp_pat list

type itp_const =
  | ITP_bool_prop of bool
  | ITP_Z of Z.t
  | ITP_bits of Z.t

type itp_unop =
  | ITP_neg_prop
  | ITP_BW_FFS_NoSMT
  | ITP_BW_CTZ_NoSMT

type itp_binop =
  | ITP_add
  | ITP_sub
  | ITP_mul
  | ITP_div
  | ITP_mod
  | ITP_rem
  | ITP_lt_prop
  | ITP_le_prop
  | ITP_exp
  | ITP_bwxor
  | ITP_bwand
  | ITP_bwor
  | ITP_eq_prop
  | ITP_and_prop
  | ITP_or_prop
  | ITP_impl_prop

type itp_term =
  | ITP_sym_term of itp_sym
  | ITP_const of itp_const
  | ITP_unop of itp_unop * itp_term * itp_bt
  | ITP_binop of itp_binop * itp_term * itp_term * itp_bt
  | ITP_match of itp_term * (itp_pat * itp_term) list
  | ITP_ite of itp_term * itp_term * itp_term
  | ITP_eachI of (int * (itp_sym * itp_bt) * int) * itp_term
  | ITP_mapset of itp_term * itp_term * itp_term
  | ITP_mapget of itp_term * itp_term
    (* the (int * int) gives the position of an element *)
  | ITP_recordmember of itp_term * itp_id * (int * int)
  | ITP_recordupdate of (itp_term * itp_id) * itp_term * (int * int)
  | ITP_record of itp_term list
  | ITP_structmember of itp_term * itp_id * (int * int)
  | ITP_structupdate of (itp_term * itp_id) * itp_term * (int * int)
  | ITP_cast of itp_bt * itp_term
  | ITP_apply_prop of itp_sym * itp_term list
  | ITP_good of itp_term (*| ITP_good of itp_sym * itp_bt * itp_term*)
  | ITP_representable of itp_sym * itp_bt * itp_term
  | ITP_constructor of itp_sym * itp_term list
  | ITP_nthlist of itp_term * itp_term * itp_term
  | ITP_arraytolist of itp_term * itp_term * itp_term
  | ITP_let of itp_sym * itp_term * itp_term
  | ITP_wrapI of Z.t * Z.t * itp_term
  | ITP_arrayshift of itp_term * Z.t * itp_term
  | ITP_unsupported of string
  | ITP_forall of itp_sym * itp_bt * itp_term
  | ITP_Define of itp_sym * itp_term * itp_term
  | ITP_Constraint_LRT of itp_term * itp_term
  | ITP_Constraint_LAT of itp_term * itp_term
  | ITP_LAT_I of itp_term
  | ITP_LRT_I
  | ITP_Owned_LRT of itp_sym * itp_bt * iris_term * itp_term * itp_term list
  | ITP_Block_LRT of itp_sym * itp_bt * iris_term * itp_term
  | ITP_Owned_LAT of itp_sym * itp_bt * iris_term * itp_term * itp_term list
  | ITP_Block_LAT of itp_sym * itp_bt * iris_term * itp_term
  | ITP_PName_LAT of itp_sym * itp_sym * itp_bt * itp_term * itp_term list * itp_term
  | ITP_PName_LRT of itp_sym * itp_sym * itp_bt * itp_term * itp_term list * itp_term
  | ITP_Each_LAT of itp_sym * itp_sym * itp_bt * itp_term * itp_term * itp_term * itp_term
  | ITP_Each_LRT of itp_sym * itp_sym * itp_bt * itp_term * itp_term * itp_term * itp_term
  | ITP_pure of itp_term

and iris_term = Iris_term of itp_term

(* CN datatypes *)
(* note: this is different from ITP_Datatype in itp_term *)
type itp_constr = ITP_constr of itp_sym * itp_bt list

type itp_dt =
  (* parameters: name, list of argument types, list of constructors *)
  | ITP_dt of itp_sym * itp_bt list * itp_constr list

(* CN logical functions *)
type itp_uninterp =
  | ITP_uninterp_prop

type itp_def =
  | ITP_def of itp_term
  | ITP_recdef of itp_term

type itp_fun =
  (* parameters: function name, function body, argument typess, return type*)
  | ITP_fun_uninterp of itp_sym * itp_uninterp * (itp_sym * itp_bt) list * itp_bt
  | ITP_fun_def of itp_sym * itp_def * (itp_sym * itp_bt) list * itp_bt

(* CN resource predicates *)
type itp_clause =
  (* parameters : guard, clause body *)
  | ITP_clause of itp_term list * itp_term

type itp_resource_pred =
  { name : itp_sym;
    ptr : itp_sym;
    args : (itp_sym * itp_bt) list;
    ret_bt : itp_bt;
    clauses : itp_clause list
  }

(* Group of mutually recursive resource predicates *)
type itp_resource_pred_group = itp_resource_pred list

type itp_uinterp_resource_pred = itp_sym * itp_sym * (itp_sym * itp_bt) list * itp_bt

(* CN lemmas *)
type itp_lemma =
  (* parameters: lemma name, lemma body *)
  | ITP_lemma of itp_sym * itp_term

(* The entire CN global typing context, plus lemma statements *)
type itp_gl =
  | ITP_gl of
      itp_dt list list
      (* uninterpreted functions and defined functions*)
      * (itp_fun list list * itp_fun list list)
      * itp_resource_pred_group list
      * itp_uinterp_resource_pred list
      * itp_lemma list
