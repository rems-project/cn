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

Inductive term_is_struct: Terms.term BaseTypes.t -> Prop :=
| term_is_struct_intro: forall tag fields,
  term_is_struct (Terms.Struct _ tag fields).

Lemma term_is_struct_dec: forall t, (term_is_struct t) + (~ term_is_struct t).
Proof.
  intros t.
  destruct t.
  all: try (left; constructor).
  all: try (right; intros H; inversion H).
Qed.

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

Definition bool_of_sum {A : Type} {x y : A} (dec : sumbool (x = y) (x <> y)) : bool :=
  match dec with
  | left _ => true
  | right _ => false
  end.

Fixpoint bt_of_sct_fun (sct : SCtypes.t) (bt : BaseTypes.t): bool :=
  match sct with
  | SCtypes.Void =>
    match bt with
    | BaseTypes.Unit _ => true
    | _ => false
    end
  | SCtypes.Integer ity =>
      bool_of_sum
        (BasetTypes_t_as_MiniDecidableType.eq_dec bt
          (BaseTypes.Bits _ (if Memory.is_signed_integer_type ity then BaseTypes.Signed else BaseTypes.Unsigned)
                            (Memory.sizeof_ity ity * 8)))
  | SCtypes.Array (sct, None) =>
    match bt with
    | BaseTypes.Map _ bt1 bt2 =>
      bt_of_sct_fun sct bt2 &&
      bool_of_sum
        (BasetTypes_t_as_MiniDecidableType.eq_dec bt1
          (BaseTypes.Bits _ BaseTypes.Unsigned (Memory.sizeof_ity (IntegerType.Signed Intptr_t) * 8)))
    | _ => false
    end
  | SCtypes.Pointer sct =>
    match bt with
    | BaseTypes.Loc _ tt => true
    | _ => false
    end
  | SCtypes.Struct tag1 =>
    match bt with
    | BaseTypes.Struct _ tag2 => 
      if Sym_t_as_MiniDecidableType.eq_dec tag1 tag2
      then true else false
    | _ => false
    end
  | _ => false
  end.

Lemma eq_dec_refl_l {A : Type} (x : A) (eq_dec : forall x y : A, {x = y} + {x <> y}) :
  bool_of_sum (eq_dec x x) = true.
Proof.
  destruct (eq_dec x x) as [H | H]; auto.
Qed.

Lemma eq_dec_refl_r {A : Type} (x y : A) (eq_dec : forall x y : A, {x = y} + {x <> y}) :
  bool_of_sum (eq_dec x y) = true -> x = y.
Proof.
  destruct (eq_dec x y) as [H | H]; auto.
  intros ?; discriminate.
Qed.

Lemma bt_of_sct_rel_fun_eq: forall sct bt,
  bt_of_sct_rel sct bt <-> bt_of_sct_fun sct bt = true.
Proof.
  intros sct bt.
  split; intros H.
  - induction H; try reflexivity.
    + unfold bt_of_sct_fun.
      apply eq_dec_refl_l.
    + simpl.
      rewrite IHbt_of_sct_rel.
      reflexivity.
    + simpl.
      apply eq_dec_refl_l.
  - revert bt H.
    apply SCtypes.ctype_ind_set with (P := fun sct => forall bt : BaseTypes.t, bt_of_sct_fun sct bt = true -> bt_of_sct_rel sct bt).
    + intros bt H.
      destruct bt; try discriminate.
      constructor.
    + intros i bt H.
      destruct bt; try discriminate.
      unfold bt_of_sct_fun in H.
      destruct BasetTypes_t_as_MiniDecidableType.eq_dec as [E | ?]; try discriminate.
      rewrite E.
      constructor.
    + intros [sct' o] IH bt H.
      destruct o; try discriminate.
      destruct bt; try discriminate.
      simpl in IH, H.
      destruct (bt_of_sct_fun sct' bt2) eqn:H'; try discriminate.
      destruct BasetTypes_t_as_MiniDecidableType.eq_dec as [E | ?]; try discriminate.
      clear H.
      rewrite E.
      constructor.
      apply IH, H'.
    + intros sct' IH bt H.
      destruct bt; try discriminate.
      destruct u.
      constructor.
    + intros s bt H.
      destruct bt; try discriminate.
      simpl in H.
      destruct Sym_t_as_MiniDecidableType.eq_dec as [E | ?]; try discriminate.
      rewrite E.
      constructor.
    + intros ? ? ? ? H.
      inversion H.
Qed.

(* Defines when a term represents a cast of another term to a specific type *)
Inductive cast_ (loc: Locations.t) : BaseTypes.t -> IndexTerms.t -> IndexTerms.t -> Prop :=
| cast_same: forall bt t' l',
  cast_ loc bt (Terms.IT _ t' bt l') (Terms.IT _ t' bt l')
| cast_diff: forall bt t' bt' l',
  bt <> bt' ->
  cast_ loc bt (Terms.IT _ t' bt l') (Terms.IT _ (Terms.Cast _ bt (Terms.IT _ t' bt' l')) bt loc).

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
  (iinit: init) (ipointer: IndexTerms.t) (iargs: list IndexTerms.t) (* lhs predicate*)
  (tag: Sym.t)
  (loc: Location.t)
  : output -> Resource.t -> Prop :=
| struct_piece_to_resource_struct:
  forall pid pty fields field_it struct_loc,
  Memory.piece_member_or_padding piece = Some (pid, pty) ->
  let field_pointer := Terms.IT _ (Terms.MemberShift _ ipointer tag pid) (BaseTypes.Loc _ tt) loc in
  (* field_out is the IT corresponding to pid in iout's field list *)
  List.In (pid, field_it) fields

  ->

    struct_piece_to_resource piece iinit ipointer iargs tag loc
      (Resource.O (Terms.IT _ (Terms.Struct _ tag fields) (BaseTypes.Struct _ tag) struct_loc))
      (Request.P {| Predicate.name := Request.Owned pty iinit;
                   Predicate.pointer := field_pointer;
                   Predicate.iargs := iargs |},
       (Resource.O field_it))

| struct_piece_to_resource_opaque:
  forall pid pty field_bt struct_loc struct_term,
  Memory.piece_member_or_padding piece = Some (pid, pty) ->
  let field_pointer := Terms.IT _ (Terms.MemberShift _ ipointer tag pid) (BaseTypes.Loc _ tt) loc in
  (* The field's type maps to its base type *)
  bt_of_sct_rel pty field_bt ->
  let struct_type := (BaseTypes.Struct _ tag) in
  ~ term_is_struct struct_term

  ->

    struct_piece_to_resource piece iinit ipointer iargs tag loc
      (Resource.O (Terms.IT _ struct_term struct_type struct_loc))
      (Request.P {| Predicate.name := Request.Owned pty iinit;
                    Predicate.pointer := field_pointer;
                    Predicate.iargs := iargs |},
       (Resource.O
         (Terms.IT _ 
           (Terms.StructMember _
             (Terms.IT _ struct_term struct_type struct_loc)
             pid)
           field_bt
           loc))).

Definition struct_piece_to_resource_fun
  (piece: Memory.struct_piece)
  (iinit: init) (ipointer: IndexTerms.t) (iargs: list IndexTerms.t) (* lhs predicate*)
  (tag: Sym.t)
  (loc: Location.t)
  (out : output) (res : Resource.t) : bool :=
  match res with
  | (req, Resource.O field_it) =>
    match out with
    | Resource.O (Terms.IT _ struct_term struct_type struct_loc) =>
      match struct_term with
      | Terms.Struct _ tag' fields =>
        match Memory.piece_member_or_padding piece with
        | Some (pid, pty) =>
          let field_pointer := Terms.IT _ (Terms.MemberShift _ ipointer tag pid) (BaseTypes.Loc _ tt) loc in
          List.existsb (fun '(pid', field_it') =>
                          bool_of_sum (Symbol_identifier_as_MiniDecidableType.eq_dec pid pid') &&
                          bool_of_sum (IndexTerm_as_MiniDecidableType.eq_dec field_it field_it'))
                        fields &&
          bool_of_sum (BasetTypes_t_as_MiniDecidableType.eq_dec struct_type (BaseTypes.Struct _ tag)) &&
          bool_of_sum (Sym_t_as_MiniDecidableType.eq_dec tag tag') &&
          bool_of_sum (Request_as_MiniDecidableType.eq_dec req
            (Request.P {| Predicate.name := Request.Owned pty iinit;
                          Predicate.pointer := field_pointer;
                          Predicate.iargs := iargs |}))
        | _ => false
        end
      | _ =>
        match Memory.piece_member_or_padding piece with
        | Some (pid, pty) =>
          let field_pointer := Terms.IT _ (Terms.MemberShift _ ipointer tag pid) (BaseTypes.Loc _ tt) loc in
          match field_it with
          | Terms.IT _  output_term  field_bt loc' =>
            bt_of_sct_fun pty field_bt &&
            bool_of_sum (BasetTypes_t_as_MiniDecidableType.eq_dec struct_type (BaseTypes.Struct _ tag)) &&
            bool_of_sum (Request_as_MiniDecidableType.eq_dec req
              (Request.P {| Predicate.name := Request.Owned pty iinit;
                            Predicate.pointer := field_pointer;
                            Predicate.iargs := iargs |})) &&
            bool_of_sum (IndexTerm_as_MiniDecidableType.eq_dec
              (Terms.IT _  output_term  field_bt loc')
              (Terms.IT _ (Terms.StructMember _ (Terms.IT _ struct_term struct_type struct_loc) pid) field_bt loc))
          end
        | _ => false
        end
      end
    end
  end.

Lemma struct_piece_to_resource_fun_eq:
  forall piece iinit ipointer iargs tag loc out res,
  struct_piece_to_resource piece iinit ipointer iargs tag loc out res <->
  struct_piece_to_resource_fun piece iinit ipointer iargs tag loc out res = true.
Proof.
  intros piece iinit ipointer iargs tag loc out res.
  split.
  - intros H.
    unfold struct_piece_to_resource_fun.
    destruct res as [req [field_it]].
    destruct out as [[]].
    inversion H; subst.
    + rewrite H2.
      epose proof (existsb_exists _ _) as He.
      destruct He as [_ He].
      rewrite He; revgoals.
      { exists (pid, field_it).
        split; auto.
        apply andb_true_intro; split; apply eq_dec_refl_l. }
      repeat (apply andb_true_intro; split); auto.
      all: apply eq_dec_refl_l.
    + rewrite H4.
      apply bt_of_sct_rel_fun_eq in H6.
      rewrite H6.
      unfold struct_type.
      rewrite 3 eq_dec_refl_l.
      destruct t; auto.
      exfalso.
      apply H7.
      constructor.
  - intros H.
    unfold struct_piece_to_resource_fun in H.
    destruct res as [req [field_it]].
    destruct out as [[]].
    destruct (term_is_struct_dec t) as [Ht | Ht].
    + inversion Ht; subst; clear Ht.
      destruct Memory.piece_member_or_padding as [[pid pty]|] eqn:E; try discriminate.
      apply andb_prop in H; destruct H as [H1 H2].
      apply andb_prop in H1; destruct H1 as [H1 H3].
      apply andb_prop in H1; destruct H1 as [H1 H4].
      epose proof (existsb_exists _ _) as He.
      destruct He as [He _].
      apply He in H1; clear He.
      destruct H1 as [[pid' field_it'] [H0 H1]].
      apply andb_prop in H1; destruct H1 as [H1 H5].
      apply eq_dec_refl_r in H1, H2, H3, H4, H5; subst.
      apply struct_piece_to_resource_struct.
      * apply E.
      * apply H0.
    + destruct t; try (exfalso; apply Ht; constructor).
      all: destruct Memory.piece_member_or_padding as [[]|] eqn:E; try discriminate.
      all: destruct field_it; try discriminate.
      all: apply andb_prop in H; destruct H as [H1 H2].
      all: apply andb_prop in H1; destruct H1 as [H1 H3].
      all: apply andb_prop in H1; destruct H1 as [H1 H4].
      all: apply eq_dec_refl_r in H2, H3, H4; subst.
      all: inversion H2; subst; clear H2.
      all: apply bt_of_sct_rel_fun_eq in H1.
      all: apply struct_piece_to_resource_opaque.
      all: assumption.
Qed.


Definition subsumed_fun (p1 p2 : Request.name) : bool :=
  orb
    (bool_of_sum (Name_as_MiniDecidableType.eq_dec p1 p2))
    (match p1, p2 with
     | Request.Owned ct Uninit, Request.Owned ct' Init
         => bool_of_sum (SCtypes_as_MiniDecidableType.eq_dec ct ct')
     | _, _ => false
     end).

Lemma subsumed_fun_eq:
  forall p1 p2,
  subsumed p1 p2 <-> subsumed_fun p1 p2 = true.
Proof.
  intros p1 p2.
  split.
  - intros H.
    unfold subsumed_fun.
    apply orb_true_intro.
    inversion H; subst.
    + left.
      apply eq_dec_refl_l.
    + right.
      apply eq_dec_refl_l.
  - intros H.
    unfold subsumed_fun in H.
    apply orb_prop in H; destruct H as [H | H].
    + apply Subsumed_equal.
      eapply eq_dec_refl_r, H.
    + destruct p1; try discriminate.
      destruct i; try discriminate.
      destruct p2; try discriminate.
      destruct i; try discriminate.
      apply Subsumed_owned.
      eapply eq_dec_refl_r, H.
Qed.

Inductive resource_unfold (globals:Global.t): Resource.t -> ResSet.t -> Prop :=
| resource_unfold_struct:
  forall out_res ipointer iargs iout iinit iinit' isym sdecl loc,

  let iname := Request.Owned (SCtypes.Struct isym) iinit in
  let iname' := Request.Owned (SCtypes.Struct isym) iinit' in

  subsumed iname iname' ->
  
  (* lookup struct declaration in global environment *)
  SymMap.MapsTo isym sdecl globals.(Global.struct_decls) ->

  (* Resources are related to struct pieces *)
  (forall r,
     ResSet.In r out_res <->
       exists piece, List.In piece sdecl /\
                struct_piece_to_resource piece iinit' ipointer iargs isym loc iout r)

  ->

    resource_unfold globals
      (Request.P
         {|
           Predicate.name := iname;
           Predicate.pointer := ipointer;
           Predicate.iargs := iargs
         |},
         iout)
      out_res.


(* quick and dirty implementation. TODO: move to utils module somewhere *)      
Fixpoint list_map2 {A B C} (f: A -> B -> C) (m : list A) (n : list B) : option (list C) :=
  match m, n with
  | [], [] => Some []
  | _::_, [] => None
  | [], _::_ => None
  | x::l, x'::l' => 
        match list_map2 f l l' with
        | Some l'' => Some ((f x x')::l'')
        | None => None
        end
  end.

 (* This is a temporary workaround until `loc` argument is removed from
 `struct_piece_to_resource` and `struct_piece_to_resource_fun` *) 
Local Definition get_resource_pointer_loc (r : Resource.t) : option Location.t :=
  match r with
  | ((Request.P {| Predicate.name := Request.Owned pty iinit;
                   Predicate.pointer := Terms.IT _ _ _ loc ;
                   Predicate.iargs := _ |}), _) => 
                   Some loc
  | _ => None
  end.

(* This function will be simplified when `loc` argument is removed from
 `struct_piece_to_resource` and `struct_piece_to_resource_fun` *) 
Definition resource_unfold_fun (globals:Global.t) (r : Resource.t) (out_res: list Resource.t) : bool :=
  match r with
  | (Request.P {|
      Predicate.name := Request.Owned (SCtypes.Struct isym) iinit;
      Predicate.pointer := ipointer;
      Predicate.iargs := iargs
      |} , iout) =>
        match SymMap.find isym globals.(Global.struct_decls) with
        | Some sdecl =>
          let tmp := list_map2 (fun piece r =>
            match get_resource_pointer_loc r with
            | Some loc =>
              struct_piece_to_resource_fun piece iinit ipointer iargs isym loc iout r
            | None => false
            end
          ) sdecl out_res in 
          match tmp with
          | None => false
          | Some tmp' => List.fold_left andb tmp' true
          end
        | None => false
        end
  | _ => false
  end.

Lemma resource_unfold_fun_eq:
  forall globals r out_res,
  resource_unfold globals r (Resource.set_from_list out_res) <-> resource_unfold_fun globals r out_res = true.
Proof.
  admit.
Admitted.

Inductive resource_unfold_full (globals:Global.t): ResSet.t -> ResSet.t -> Prop :=
| resource_unfold_full_step:
    forall input input' output r unfolded_r,
    (* Find a resource that can be unfolded *)
    ResSet.In r input ->
    resource_unfold globals r unfolded_r ->
    (* Continue unfolding with the resource replaced by its unfolded components *)
    ResSet.Equal input' (ResSet.union (ResSet.remove r input) unfolded_r) ->
    resource_unfold_full globals input' output ->
    (* This is the result of unfolding the input *)
    resource_unfold_full globals input output
| resource_unfold_full_fixpoint:
    forall input,
    (* A fixpoint is reached when no resource can be unfolded further *)
    ~ ResSet.Exists
        (fun r => exists unfolded_r, resource_unfold globals r unfolded_r)
        input ->
    resource_unfold_full globals input input.

Inductive resource_unfold_full_explicit (globals:Global.t):
  list (Resource.t * unpack_result) -> ResSet.t -> ResSet.t -> Prop :=
| resource_unfold_full_explicit_step:
    forall input input' output r unfolded_r_list unfold_changed,
    let unfolded_r_set := Resource.set_from_list unfolded_r_list in
    (* Find a resource that can be unfolded *)
    ResSet.In r input ->
    resource_unfold globals r unfolded_r_set ->
    (* Continue unfolding with the resource replaced by its unfolded components *)
    ResSet.Equal input' (ResSet.union (ResSet.remove r input) unfolded_r_set) ->
    resource_unfold_full_explicit globals unfold_changed input' output ->
    (* This is the result of unfolding the input *)
    resource_unfold_full_explicit globals ((r, UnpackRES unfolded_r_list) :: unfold_changed) input output
| resource_unfold_full_explicit_fixpoint:
    forall input,
    (* A fixpoint is reached when no resource can be unfolded further *)
    ~ ResSet.Exists
        (fun r => exists unfolded_r, resource_unfold globals r unfolded_r)
        input ->
    resource_unfold_full_explicit globals [] input input.

Lemma resource_unfold_full_explicit_eq:
  forall globals input output unfold_changed,
  resource_unfold_full_explicit globals unfold_changed input output ->
  resource_unfold_full globals input output.
Proof.
  intros globals input output unfold_changed H.
  induction H.
  - econstructor; eassumption.
  - econstructor; eassumption.
Qed.

Definition resource_unfold_full_explicit_fun
  (globals:Global.t)
  (unfold_changed : list (Resource.t * unpack_result))
  (input output: list Resource.t): bool.
  admit.
Admitted.

Lemma resource_unfold_full_explicit_fun_eq:
  forall globals input output unfold_changed,
  resource_unfold_full_explicit globals unfold_changed (Resource.set_from_list input) (Resource.set_from_list output) ->
  resource_unfold_full_explicit_fun globals unfold_changed input output = true.
Proof.
  admit.
Admitted.

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
  resource_unfold_full iglobal in_res out_res ->

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

(* Helper definition, used in [log_entry_valid] *)
Local Definition ptr_alloc_id_eq (g: Global.t) (c: LCSet.t) (p p': IndexTerms.t) :=
  exists loc alloc_id_eq alloc alloc',
    allocId_ loc p alloc /\
    allocId_ loc p' alloc' /\
    IndexTerms.eq_ loc alloc alloc' alloc_id_eq /\
    provable g c (LogicalConstraints.T alloc_id_eq).

(* Helper definition, used in [log_entry_valid] *)
Local Definition ptr_addr_and_args_eq (g: Global.t) (c: LCSet.t) (p p': IndexTerms.t) (a a': list IndexTerms.t) :=
  exists pointer_eq loc,
    IndexTerms.eq_ loc p p' pointer_eq /\
    provable g c (LogicalConstraints.T pointer_eq) /\
    (* arguments match *)
    (exists addr_iargs_match addr addr',
        addr_ loc p addr /\
        addr_ loc p' addr' /\
        IndexTerms.eq_ loc addr addr' pointer_eq /\
        IndexTerms.eq_and_list_rel Location.Loc_unknown a a' addr_iargs_match /\
        provable g c
          (* we add pointer address equality to the constraint as it might be needed for proving the argument equality *)
          (LogicalConstraints.T (IndexTerms.and2_ Location.Loc_unknown pointer_eq addr_iargs_match))
    ).

(** Inductive predicate which defines correctess of log inference step *)
Inductive log_entry_valid : log_entry -> Prop :=
| unfold_resources_step:
  forall loc c c' steps,
  unfold_step c c' ->
  log_entry_valid (UnfoldResources c loc steps c')

| array_resource_inference_step:
  forall ity isize iinit ipointer iargs
    oname opointer oargs
    err out
    icomputational ilogical iresources iconstraints iglobal
    ocomputational ological oresources oconstraints oglobal
    steps,

  log_entry_valid
    (PredicateRequest
      (* input context *)
      {|
        Context.computational := icomputational;
        Context.logical := ilogical;
        Context.resources := iresources;
        Context.constraints := iconstraints;
        Context.global := iglobal
      |}
      (* sutuation (unused) *)
      err 
      (* input predicate *)
      {| Predicate.name := Request.Owned (SCtypes.Array (SCtypes.Integer ity, isize)) iinit;
        Predicate.pointer := ipointer;
        Predicate.iargs := iargs 
      |}
      (* output predicate *)
      ({| Predicate.name:=oname; Predicate.pointer:=opointer; Predicate.iargs:=oargs |}, out)
      (* steps *)
      steps
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
    err out
    icomputational ilogical iresources iconstraints iglobal
    ocomputational ological oresources oconstraints oglobal
    steps,

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
      resource_unfold_full iglobal
        (ResSet.singleton
          (Request.P {| Predicate.name := iname;
                       Predicate.pointer := ipointer;
                       Predicate.iargs := iargs |}, out))
        field_res /\
      (* Output context is input context minus all field resources *)
      ResSet.Equal out_res (ResSet.diff in_res field_res)
  ) ->
 
  log_entry_valid
    (PredicateRequest
      (* input context *)
      {|
        Context.computational := icomputational;
        Context.logical := ilogical;
        Context.resources := iresources;
        Context.constraints := iconstraints;
        Context.global := iglobal
      |}  
      (* sutuation (unused) *)
      err 
      (* input predicate *)
      {| Predicate.name := Request.Owned (SCtypes.Struct isym) iinit;
        Predicate.pointer := ipointer;
        Predicate.iargs := iargs 
      |}
      (* output predicate *)
      ({| Predicate.name:=oname; Predicate.pointer:=opointer; Predicate.iargs:=oargs |}, out)
      (* steps *)
      steps
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
    err out
    icomputational ilogical iresources iconstraints iglobal
    ocomputational ological oresources oconstraints oglobal
    steps,

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

  (* [out_res] is a subset of [in_res] with exactly one element [used] removed. *)
  (exists (upred: Request.Predicate.t),
      ResSet.Equal (Resource.ResSet.add (P upred, out) out_res) in_res /\
      (* name matches *)
      Request.subsumed iname upred.(Request.Predicate.name) /\
      (* alloc_id matches *)
      (ptr_alloc_id_eq iglobal iconstraints ipointer upred.(Request.Predicate.pointer)) /\
      (* pointer and arguments match *)
      (ptr_addr_and_args_eq iglobal iconstraints ipointer upred.(Request.Predicate.pointer) iargs upred.(Request.Predicate.iargs))
  )

  ->

    log_entry_valid
      (PredicateRequest
        (* input context *)
        {|
          Context.computational := icomputational;
          Context.logical := ilogical;
          Context.resources := iresources;
          Context.constraints := iconstraints;
          Context.global := iglobal
        |}
        (* sutuation (unused) *)
        err 
        (* input predicate *)
        {| Predicate.name:=iname; Predicate.pointer:=ipointer; Predicate.iargs:=iargs |}
        (* output predicate *)
        ({| Predicate.name:=oname; Predicate.pointer:=opointer; Predicate.iargs:=oargs |}, out)
        (* steps *)
        steps
        (* output context *)
        {|
          Context.computational := ocomputational;
          Context.logical := ological;
          Context.resources := oresources;
          Context.constraints := oconstraints;
          Context.global := oglobal
        |}
      ).

(** Proof log is valid if all entries are valid *)
Definition prooflog_valid (l:Prooflog.log) := List.Forall log_entry_valid l.

