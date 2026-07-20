module BT = BaseTypes
module AT = ArgumentTypes

(* CN primitives *)

type coq_sym = Coq_sym of Sym.t

type coq_id = Coq_id of Id.t

type coq_sign =
  | Coq_Signed
  | Coq_Unsigned

(* CN BaseTypes*)

type coq_bt =
  | Coq_Bool
  | Coq_Integer
  | Coq_Bits of coq_sign * int
  | Coq_Map of coq_bt * coq_bt
  | Coq_Struct of coq_sym * coq_bt list
  | Coq_Record of coq_bt list
  | Coq_Loc
  | Coq_Datatype of coq_sym
  | Coq_List of coq_bt
  | Coq_Unit
  | Coq_Membyte
  | Coq_Real
  | Coq_Alloc_id
  | Coq_CType
  | Coq_Tuple of coq_bt list
  | Coq_Set of coq_bt

(* CN IndexTerms *)

type coq_pat =
  | Coq_pSym of coq_sym
  | Coq_pWild
  | Coq_pConstructor of coq_sym * coq_pat list

type coq_const =
  | Coq_bool of bool
  | Coq_bool_prop of bool
  | Coq_Z of Z.t
  | Coq_bits of Z.t

type coq_unop =
  | Coq_neg
  | Coq_neg_prop
  | Coq_BW_FFS_NoSMT
  | Coq_BW_CTZ_NoSMT

type coq_binop =
  | Coq_add
  | Coq_sub
  | Coq_mul
  | Coq_div
  | Coq_mod
  | Coq_rem
  | Coq_lt
  | Coq_lt_prop
  | Coq_le
  | Coq_le_prop
  | Coq_exp
  | Coq_bwxor
  | Coq_bwand
  | Coq_bwor
  | Coq_eq
  | Coq_eq_prop
  | Coq_and
  | Coq_and_prop
  | Coq_or
  | Coq_or_prop
  | Coq_impl
  | Coq_impl_prop

type coq_term =
  | Coq_sym_term of coq_sym
  | Coq_const of coq_const
  | Coq_unop of coq_unop * coq_term * coq_bt
  | Coq_binop of coq_binop * coq_term * coq_term * coq_bt
  | Coq_match of coq_term * (coq_pat * coq_term) list
  | Coq_ite of coq_term * coq_term * coq_term
  | Coq_eachI of (int * (coq_sym * coq_bt) * int) * coq_term
  | Coq_mapset of coq_term * coq_term * coq_term
  | Coq_mapget of coq_term * coq_term
    (* the (int * int) gives the position of an element *)
  | Coq_recordmember of coq_term * coq_id * (int * int)
  | Coq_recordupdate of (coq_term * coq_id) * coq_term * (int * int)
  | Coq_record of coq_term list
  | Coq_structmember of coq_term * coq_id * (int * int)
  | Coq_structupdate of (coq_term * coq_id) * coq_term * (int * int)
  | Coq_cast of coq_bt * coq_term
  | Coq_apply of coq_sym * coq_term list
  | Coq_apply_prop of coq_sym * coq_term list
  | Coq_good of coq_term (*| Coq_good of coq_sym * coq_bt * coq_term*)
  | Coq_representable of coq_sym * coq_bt * coq_term
  | Coq_constructor of coq_sym * coq_term list
  | Coq_nthlist of coq_term * coq_term * coq_term
  | Coq_arraytolist of coq_term * coq_term * coq_term
  | Coq_let of coq_sym * coq_term * coq_term
  | Coq_wrapI of Z.t * Z.t * coq_term
  | Coq_arrayshift of coq_term * Z.t * coq_term
  | Coq_unsupported of string
  | Coq_forall of coq_sym * coq_bt * coq_term
  | Coq_Define of coq_sym * coq_term * coq_term
  | Coq_Constraint_LRT of coq_term * coq_term
  | Coq_Constraint_LAT of coq_term * coq_term
  | Coq_LAT_I of coq_term
  | Coq_LRT_I
  | Coq_Owned_LRT of coq_sym * coq_bt * iris_term * coq_term * coq_term list
  | Coq_Block_LRT of coq_sym * coq_bt * iris_term * coq_term
  | Coq_Owned_LAT of coq_sym * coq_bt * iris_term * coq_term * coq_term list
  | Coq_Block_LAT of coq_sym * coq_bt * iris_term * coq_term
  | Coq_PName_LAT of coq_sym * coq_sym * coq_bt * coq_term * coq_term list * coq_term
  | Coq_PName_LRT of coq_sym * coq_sym * coq_bt * coq_term * coq_term list * coq_term
  | Coq_Each_LAT of coq_sym * coq_sym * coq_bt * coq_term * coq_term * coq_term * coq_term
  | Coq_Each_LRT of coq_sym * coq_sym * coq_bt * coq_term * coq_term * coq_term * coq_term
  | Coq_pure of coq_term

and iris_term = Iris_term of coq_term

(* CN datatypes *)
(* note: this is different from Coq_Datatype in coq_term *)
type coq_constr = Coq_constr of coq_sym * coq_bt list

type coq_dt =
  (* parameters: name, list of argument types, list of constructors *)
  | Coq_dt of coq_sym * coq_bt list * coq_constr list

(* CN logical functions *)
type coq_uninterp =
  | Coq_uninterp
  | Coq_uninterp_prop

type coq_def =
  | Coq_def of coq_term
  | Coq_recdef of coq_term

type coq_fun =
  (* parameters: function name, function body, argument typess, return type*)
  | Coq_fun_uninterp of coq_sym * coq_uninterp * (coq_sym * coq_bt) list * coq_bt
  | Coq_fun_def of coq_sym * coq_def * (coq_sym * coq_bt) list * coq_bt

(* CN resource predicates *)
type coq_clause =
  (* parameters : guard, clause body *)
  | Coq_clause of coq_term list * coq_term

type coq_resource_pred =
  (* parameters: pred name, input pointer name, input arguments and types,
               output type, clauses (None -> uninterpreted pred) *)
  | Coq_rpred of coq_sym * coq_sym * (coq_sym * coq_bt) list * coq_bt * coq_clause list
  | Coq_rpred_uninterp of coq_sym * coq_sym * (coq_sym * coq_bt) list * coq_bt

(* CN lemmas *)
type coq_lemma =
  (* parameters: lemma name, lemma body *)
  | Coq_lemma of coq_sym * coq_term

(* The entire CN global typing context, plus lemma statements *)
type coq_gl =
  | Coq_gl of
      coq_dt list list
      (* uninterpreted functions and defined functions*)
      * (coq_fun list list * coq_fun list list)
      * coq_resource_pred list list
      * coq_lemma list
