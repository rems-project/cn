Require Import Coq.Lists.List.
Require Import Coq.ZArith.ZArith.
Require Import Coq.FSets.FSetInterface.

From Cerberus Require Import IntegerType Ctype.
From Cn Require Import Prooflog Request Resource Context Sym IndexTerms BaseTypes SCtypes Global LogicalConstraints.

Import ListNotations.


(* This is a placeholder for the actual definition of `provable`.
   The actual definition should be using a proof witness from
   the SMT solver, which will be checked via SMTCoq plugin.
*)
Inductive provable (g:Global.t): LCSet.t -> LogicalConstraints.t -> Prop :=
| solvable_SMT: forall lc it, provable g lc it.

(* Helper functoin to get a set of resources from the contex *)

Definition ctx_resources_set (l:((list (Resource.t * Z)) * Z)) : ResSet.t
  :=
  Resource.set_from_list (List.map fst (fst l)).

(* Relation between Sctypes.t and BaseTypes.t *)
Inductive bt_of_sct_rel : SCtypes.t -> BaseTypes.t -> Prop :=
| bt_of_sct_void : bt_of_sct_rel SCtypes.Void (BaseTypes.Unit unit)
| bt_of_sct_integer : forall ity,
    bt_of_sct_rel (SCtypes.Integer ity) 
      (BaseTypes.Bits _ (if Memory.is_signed_integer_type ity then BaseTypes.Signed else BaseTypes.Unsigned)
                      (Memory.sizeof_ity ity * 8))
| bt_of_sct_array : forall sct bt,
    bt_of_sct_rel sct bt ->
    bt_of_sct_rel (SCtypes.Array (sct, None)) 
      (BaseTypes.Map _ 
        (BaseTypes.Bits _ BaseTypes.Unsigned (Memory.sizeof_ity (IntegerType.Signed Intptr_t) * 8)) 
        bt)
| bt_of_sct_pointer : forall sct,
    bt_of_sct_rel (SCtypes.Pointer sct) (BaseTypes.Loc unit tt)
| bt_of_sct_struct : forall tag,
    bt_of_sct_rel (SCtypes.Struct tag) (BaseTypes.Struct _ tag)
(* TODO function types *).

(* Defines when a term represents a cast of another term to a specific type *)
Inductive cast_ (loc: Locations.t) : BaseTypes.t -> IndexTerms.t -> IndexTerms.t -> Prop :=
| cast_same: forall bt t' bt' l',
    bt = bt' ->
    cast_ loc bt (Terms.IT _ t' bt' l') (Terms.IT _ t' bt' l')
| cast_diff: forall bt t' bt' l',
    bt <> bt' ->
    cast_ loc bt (Terms.IT _ t' bt' l') (Terms.IT _ (Terms.Cast _ bt (Terms.IT _ t' bt' l')) bt loc).

Inductive allocId_ (loc: Locations.t) : IndexTerms.t -> IndexTerms.t -> Prop :=
| allocId_intro: forall t' bt' l' result,
    cast_ loc (BaseTypes.Alloc_id _) (Terms.IT _ t' bt' l') result ->
    allocId_ loc (Terms.IT _ t' bt' l') result.

(* Defines when a term represents the address of another term *)
Inductive addr_ (loc: Locations.t) : IndexTerms.t -> IndexTerms.t -> Prop :=
| addr_intro: forall pt t' bt' l' result,
    bt_of_sct_rel (SCtypes.Integer (IntegerType.Signed Intptr_t)) pt ->
    cast_ loc pt (Terms.IT _ t' bt' l') result ->
    addr_ loc (Terms.IT _ t' bt' l') result.

(* Helper predicate to relate struct piece to resource *)
Inductive struct_piece_to_resource 
  (piece: Memory.struct_piece) 
  (iinit: init) 
  (ipointer: IndexTerms.t) 
  (iargs: list IndexTerms.t) 
  (tag: Sym.t) 
  (loc: Location.t)
  : output -> Resource.t -> Prop :=
| struct_piece_to_resource_intro:
    forall pid pty field_bt fields field_it struct_loc,
    Memory.piece_member_or_padding piece = Some (pid, pty) ->
    let field_pointer := Terms.IT _ (Terms.MemberShift _ ipointer tag pid) (BaseTypes.Loc _ tt) loc in
    let field_out := Resource.O field_it in
    (* The field's type maps to its base type *)
    bt_of_sct_rel pty field_bt ->
    (* field_out is the IT corresponding to pid in iout's field list *)
    List.In (pid, field_it) fields 
    
    ->

    struct_piece_to_resource piece iinit ipointer iargs tag loc 
      (Resource.O (Terms.IT _ (Terms.Struct _ tag fields) (BaseTypes.Struct _ tag) struct_loc))
      (Request.P {| Predicate.name := Request.Owned pty iinit;
                    Predicate.pointer := field_pointer; 
                    Predicate.iargs := iargs |},
       field_out).

Inductive resource_unfold (globals:Global.t): Resource.t -> ResSet.t -> Prop :=
(* non-struct "Owned" resources unfold to themselves *)
| resource_unfold_nonstruct:
    forall ipointer iargs iout iinit ity,
    not (SCtypes.is_struct ity) ->

    resource_unfold globals
      (Request.P 
        {| 
          Predicate.name := Request.Owned ity iinit; 
          Predicate.pointer := ipointer; 
          Predicate.iargs := iargs 
        |}, 
        iout) 
      (ResSet.singleton 
        (Request.P 
        {| 
          Predicate.name := Request.Owned ity iinit; 
          Predicate.pointer := ipointer; 
          Predicate.iargs := iargs 
        |}, 
        iout)         
      ) 

| resource_unfold_struct:
  forall out_res ipointer iargs iout iinit isym sdecl loc,

  (* lookup struct declaration in global environment *)
  SymMap.MapsTo isym sdecl globals.(Global.struct_decls) ->

  (* Resources are related to struct pieces *)
  (forall r, 
    ResSet.In r out_res <->
      exists piece, List.In piece sdecl /\
        struct_piece_to_resource piece iinit ipointer iargs isym loc iout r) 
  
  ->

  resource_unfold globals
    (Request.P 
      {| 
        Predicate.name := Request.Owned (SCtypes.Struct isym) iinit; 
        Predicate.pointer := ipointer; 
        Predicate.iargs := iargs 
      |}, 
      iout) 
    out_res.
  
(** Inductive predicate which defines correctness of resource unfolding step *)
Inductive unfold_step : Context.t -> Context.t -> Prop :=
| simple_unfold_step:
    forall 
    icomputational ilogical iresources iconstraints iglobal
    ocomputational ological oresources oconstraints oglobal,
    
  (* The following parts of context are not changed *)
    icomputational = ocomputational ->
    iglobal = oglobal ->
    ilogical = ological ->
    iconstraints = oconstraints ->

    let in_res := ctx_resources_set iresources in
    let out_res := ctx_resources_set oresources in

    (* The `out_res` is union of `resource_unfold` of all resources in `in_res` *)
    (forall r', ResSet.In r' out_res <-> 
      exists r, ResSet.In r in_res /\ exists s, 
      resource_unfold iglobal r s /\ ResSet.In r' s
    ) 
    
    ->

    unfold_step
    (* input context *)
    {|
      Context.computational := icomputational;
      Context.logical := ilogical;
      Context.resources := iresources;
      Context.constraints := iconstraints;
      Context.global := iglobal
    |}

    (* output context *)
    {|
      Context.computational := ocomputational;
      Context.logical := ological;
      Context.resources := oresources;
      Context.constraints := oconstraints;
      Context.global := oglobal
    |}.
  

(** Inductive predicate which defines correctess of log inference step *)
Inductive log_entry_valid : log_entry -> Prop :=
| unfold_resources_step:
  forall loc c c',
  unfold_step c c' ->
  log_entry_valid (ResourceInferenceStep c (UnfoldResources loc) c')

| array_resource_inference_step:
  forall ity isize iinit ipointer iargs
    oname opointer oargs
    err out lines oloc
    icomputational ilogical iresources iconstraints iglobal
    ocomputational ological oresources oconstraints oglobal,

  log_entry_valid
    (ResourceInferenceStep
    (* input context *)
      {|
        Context.computational := icomputational;
        Context.logical := ilogical;
        Context.resources := iresources;
        Context.constraints := iconstraints;
        Context.global := iglobal
      |}

      (* request type *)
      (PredicateRequest
        err (* unused *)
        (* input predicate *)
        {| Predicate.name := Request.Owned (SCtypes.Array (SCtypes.Integer ity, isize)) iinit;
          Predicate.pointer := ipointer;
          Predicate.iargs := iargs |}
        oloc (* unused *)
        ((
          (* output predicate *)
          {| Predicate.name:=oname; Predicate.pointer:=opointer; Predicate.iargs:=oargs |},
          out
          ), lines (* unused *)
        )
     )

     (* output context *)
     {|
        Context.computational := ocomputational;
        Context.logical := ological;
        Context.resources := oresources;
        Context.constraints := oconstraints;
        Context.global := oglobal
     |}
   )
  
 (* struct case: struct resource is removed from input context *)
| struct_resource_inference_step:
  forall isym iinit ipointer iargs
    oname opointer oargs
    err out lines oloc
    icomputational ilogical iresources iconstraints iglobal
    ocomputational ological oresources oconstraints oglobal,

  (* The following parts of context are not changed *)
  icomputational = ocomputational ->
  iglobal = oglobal ->
  ilogical = ological ->
  iconstraints = oconstraints ->

  let in_res := ctx_resources_set iresources in
  let out_res := ctx_resources_set oresources in

  let iname := Request.Owned (SCtypes.Struct isym) iinit in
  
  iname = oname ->
  ipointer = opointer ->
  iargs = oargs ->

  (* Find the struct resource and its fields *)
  (exists (field_res: ResSet.t),
    (* The struct unfolds to field resources *)
    resource_unfold iglobal 
      (Request.P {| Predicate.name := iname;
                   Predicate.pointer := ipointer;
                   Predicate.iargs := iargs |}, out)
       field_res /\
      (* Output context is input context minus all field resources *)
      ResSet.Equal out_res (ResSet.diff in_res field_res)
  ) ->

  log_entry_valid
    (ResourceInferenceStep
    (* input context *)
      {|
        Context.computational := icomputational;
        Context.logical := ilogical;
        Context.resources := iresources;
        Context.constraints := iconstraints;
        Context.global := iglobal
      |}

      (* request type *)
      (PredicateRequest
        err (* unused *)
        (* input predicate *)
        {| Predicate.name := Request.Owned (SCtypes.Struct isym) iinit;
          Predicate.pointer := ipointer;
          Predicate.iargs := iargs |}
        oloc (* unused *)
        ((
          (* output predicate *)
          {| Predicate.name:=oname; Predicate.pointer:=opointer; Predicate.iargs:=oargs |},
          out
          ), lines (* unused *)
        )
     )

     (* output context *)
     {|
        Context.computational := ocomputational;
        Context.logical := ological;
        Context.resources := oresources;
        Context.constraints := oconstraints;
        Context.global := oglobal
     |}
   )

(* simple case: non-recursive request, no packing *)
| simple_resource_inference_step:
  forall iname  ipointer  iargs
    oname  opointer  oargs
    err out lines oloc
    icomputational ilogical iresources iconstraints iglobal
    ocomputational ological oresources oconstraints oglobal,

  (* The following parts of context are not changed *)
  icomputational = ocomputational ->
  iglobal = oglobal ->
  ilogical = ological ->
  iconstraints = oconstraints ->

  let in_res := ctx_resources_set iresources in
  let out_res := ctx_resources_set oresources in


  iname = oname ->
  ipointer = opointer ->
  iargs = oargs ->

  (* alt. definition sketch:
  let res_diff := Resource.ResSet.diff in_res out_res in
  Resource.ResSet.cardinal res_diff = 1 /
  ...
   *)

  (* [out_res] is a subset of [in_res] with exactly one element [used] removed. *)
  (exists (upred: Request.Predicate.t),
    ResSet.Equal (Resource.ResSet.add (P upred, out) out_res) in_res /\
    (* name matches *)
    Request.subsumed iname upred.(Request.Predicate.name) /\
    (* alloc_id matches *)
    (exists alloc_id_eq loc alloc alloc',
      allocId_ loc ipointer alloc /\
      allocId_ loc upred.(Request.Predicate.pointer) alloc' /\
      IndexTerms.eq_ loc alloc alloc' alloc_id_eq /\
      provable iglobal iconstraints (LogicalConstraints.T alloc_id_eq)) /\
    (* pointer and arguments match *)
    (exists pointer_eq loc, 
      IndexTerms.eq_ loc upred.(Request.Predicate.pointer) ipointer pointer_eq /\
      provable iglobal iconstraints (LogicalConstraints.T pointer_eq) /\
     (* arguments match *)
     (exists addr_iargs_match addr addr',
      addr_ loc ipointer addr /\
      addr_ loc upred.(Request.Predicate.pointer) addr' /\
      IndexTerms.eq_ loc addr addr' pointer_eq /\
      IndexTerms.eq_and_list_rel Location.Loc_unknown upred.(Request.Predicate.iargs) iargs addr_iargs_match /\
      provable iglobal iconstraints 
        (* we add pointer address equality to the constraint as it might be needed for proving the argument equality *)
        (LogicalConstraints.T (IndexTerms.and2_ Location.Loc_unknown pointer_eq addr_iargs_match))
      )
    )
  )

  
  ->

    log_entry_valid
      (ResourceInferenceStep
         (* input context *)
         {|
           Context.computational := icomputational;
           Context.logical := ilogical;
           Context.resources := iresources;
           Context.constraints := iconstraints;
           Context.global := iglobal
         |}

         (* request type *)
         (PredicateRequest
            err (* unused *)
            (* input predicate *)
            {| Predicate.name:=iname; Predicate.pointer:=ipointer; Predicate.iargs:=iargs |}
            oloc (* unused *)
            ((
                (* output predicate *)
                {| Predicate.name:=oname; Predicate.pointer:=opointer; Predicate.iargs:=oargs |},
                  out
              ), lines (* unused *)
         ))

         (* output context *)
         {|
           Context.computational := ocomputational;
           Context.logical := ological;
           Context.resources := oresources;
           Context.constraints := oconstraints;
           Context.global := oglobal
         |}
      )

.



(** Proof log is valid if all entries are valid *)
Definition prooflog_valid (l:Prooflog.log) := List.Forall log_entry_valid l.

