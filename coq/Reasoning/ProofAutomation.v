From Ltac2 Require Import Ltac1 Ltac2 Notations Std Constr Env Ident FSet FMap.
 
From Cn Require Import Prooflog Request Resource Context.
Require Import Ltac2Utils ResourceInference.


Ltac2 verbose: bool := true.

Ltac2 verbose_msg msg :=
  if verbose then
    Message.print msg
  else
    ().

Ltac2 verbose_print (s : string) :=
  verbose_msg (Message.of_string s).

Ltac2 verbose_print_constr (msg:string) (c : constr) :=
  verbose_msg (Message.concat (Message.of_string msg) (Message.of_constr c)).

(* Sample usage for the proof log extracted from CN:

Theorem resource_inference_steps_valid: prooflog_valid _cn_ResourceInferenceSteps.
Proof.
  unfold prooflog_valid.
  unfold _cn_ResourceInferenceSteps.
  ltac2:(prove_log_entry_list_valid ()).
Qed.

*)
 
 Ltac2 predicate_of_request (t : constr) :=
 match Constr.Unsafe.kind t with
 | Constr.Unsafe.App f args =>
     let r_constr := constr:(@Request.P) in
     let r_constr_name := get_constructor_name r_constr in
     let f_name := get_constructor_name f in
     if Constr.equal f_name r_constr_name then
       if Int.equal (Array.length args) 1 then
         Array.get args 0 
       else
         Control.throw (Tactic_failure (Some (Message.of_string "Term is not a fully applied P")))
     else
       Control.throw (Tactic_failure (Some (Message.of_string "Term is not a P")))
 | _ =>
     Control.throw (Tactic_failure (Some (Message.of_string "Term is not an application (and thus not a P)")))
 end.
  
 Ltac2 prove_simple_resource_inference_step () :=
   match! goal with
   | [ |- exists upred,
       ResSet.Equal (ResSet.add (P upred, ?out) (set_from_list ?out_res)) (set_from_list ?in_res) /\ subsumed _ (Predicate.name upred) /\ _ /\ _ ] =>
     (* break down goal into components *)
     let outname   := Fresh.in_goal @out in
     let inresname := Fresh.in_goal @in_res in
     let outresname:= Fresh.in_goal @out_res in
     let clause := { on_hyps := None; on_concl := AllOccurrences } in
     Std.remember false (Some inresname) (fun _ => in_res) None clause;
     Std.remember false (Some outresname) (fun _ => out_res) None clause;
     Std.remember false (Some outname) (fun _ => out) None clause;
     (* now try to compare lists *)
     let list_of_constr l := destruct_list (constr:(Resource.t)) l in
     let in_res_list  := list_of_constr  in_res in
     let out_res_list := list_of_constr out_res in
     let diff := const_list_subtract in_res_list out_res_list in
     match diff with
     | [res] =>
         (* single resource [res] matched, extract request as [req] *)
         let (req,out') := destruct_pair res in
         let p := predicate_of_request req in
         exists $p;
         Std.split false NoBindings;
         (* ResSet subset proof *)
         Control.focus 1 1 (fun () => ltac1:(subst;cbn;ResSetDecide.fsetdec));
         (* subsumed/\pointereq/\iargseq *)
         Control.focus 1 1 (fun () => 
          Std.split false NoBindings;         
          (* subsumed *)
          Control.focus 1 1 (fun () =>  Std.constructor false; Std.reflexivity ());
          (* pointereq/\iargseq *)
          (Control.focus 1 1 (fun () =>
           Std.split false NoBindings;         
           Control.focus 1 1 (Control.shelve);  (* TODO: prove alloc_id equality (via provable) *)
           Control.focus 1 1 (Control.shelve)  (* TODO: prove pointer address and arguments equality (via provable) *)
         )))
     | [] =>
         Control.throw (Tactic_failure (Some (Message.of_string "No resource change between the input and output")))
     | _ =>
         Control.throw (Tactic_failure (Some (Message.of_string "More than one resource change between the input and output")))
     end
   | [ |- _ ] => Control.throw (Tactic_failure (Some (Message.of_string "prove_simple_resource_inference_step: match failed")))
   end.

(* [struct_resource_inference_step constructor] proof *)
 Ltac2 prove_struct_resource_inference_step () :=
 match! goal with
 | [ |- exists field_res,
        resource_unfold ?iglobal ?res field_res /\
        ResSet.Equal (set_from_list ?out_res) (ResSet.diff (set_from_list ?in_res) field_res) ] =>

   (* now try to compute field_res from in_res and out_res *)
   let list_of_constr l := destruct_list (constr:(Resource.t)) l in
   let in_res_list  := list_of_constr  in_res in
   let out_res_list := list_of_constr out_res in
   let field_res := const_list_subtract in_res_list out_res_list in
   if List.is_empty field_res then
    Control.throw (Tactic_failure (Some (Message.of_string "No resource change between the input and output")))
   else
     let cfield_res := recons_list (constr:(Resource.t)) field_res in
     let field_res_set := constr:(set_from_list $cfield_res) in
     exists $field_res_set;
     Std.split false NoBindings;
     Control.focus 1 1 (fun () =>
      (* WIP:
      let s_decls := constr:($iglobal.(Global.struct_decls)) in
      let empty_map:((int, constr) FMap.t) := FMap.empty (Tags.int_tag) in
      let lc := destruct_list (constr:(Sym.t * Memory.struct_decls)) s_decls in
      let lcc_pairs := List.map destruct_pair lc in 
      *)
      (* Now lc_pars have elements of constr:(Sym.t) * constr:Memory.struct_decls) type,
         For stroring in FMap, we need to convert first element to int.
       *)
      (*
      let global_map :=List.fold_left (fun m (k, v) => FMap.add k v m) empty_map lic_pairs in
      *)
      Control.shelve ()
     );  (* unfold predicte *)
     Control.focus 1 1 (Control.shelve);  (* TODO: prove pointer address and arguments equality (via provable) *)
     verbose_print "TODO: verify the rest of of `struct_resource_inference_step` premises";
     Control.shelve ()
 | [ |- _ ] => Control.throw (Tactic_failure (Some (Message.of_string "prove_struct_resource_inference_step: match failed")))
end.


Ltac2 prove_unfold_step () :=
  match! goal with
  | [ |- unfold_step ?c ?c' ] =>
      Std.constructor false;
      Control.focus 1 1 (fun () => Std.reflexivity ());
      Control.focus 1 1 (fun () => Std.reflexivity ());
      Control.focus 1 1 (fun () => Std.reflexivity ());
      Control.focus 1 1 (fun () => Std.reflexivity ());

      Message.print (Message.of_string "TODO: Shelving unfold step pre-condition.");
      Control.shelve ()
  | [ |- _ ] => Control.throw (Tactic_failure (Some (Message.of_string "prove_unfold_step: match failed")))
  end.

 Ltac2 prove_log_entry_valid n :=
 let smsg s := Message.concat (Message.concat (Message.of_string "Step #") (Message.of_int n)) (Message.concat (Message.of_string ": ") (Message.of_string s)) in
 let msg m := Message.concat (Message.concat (Message.of_string "Step #") (Message.of_int n)) (Message.concat (Message.of_string ": ") m) in
 match! goal with
  | [ |- log_entry_valid (ResourceInferenceStep _ (PredicateRequest _ 
      {| 
        Predicate.name := Request.Owned (SCtypes.Array ?p) ?iinit;
        Predicate.pointer := ?ipointer; Predicate.iargs := ?iargs 
      |}
      _ _) _) ] =>
        Message.print (msg (Message.of_string "Arrays are not supported yet"));
        Std.constructor_n false 2 NoBindings (* apply array_resource_inference_step *)
  | [ |- log_entry_valid (ResourceInferenceStep _ (PredicateRequest _ 
      {| 
        Predicate.name := Request.Owned (SCtypes.Struct ?isym) ?iinit;
        Predicate.pointer := ?ipointer; Predicate.iargs := ?iargs 
      |}
      _ _) _) ] =>
       (* PredicateRequest case *)
       verbose_msg (smsg "Checking PredicateRequest for Struct");
       verbose_print_constr "    Predicate symbol name: " isym;
       Std.constructor_n false 3 NoBindings; (* apply struct_resource_inference_step *)
       Control.focus 1 1 (fun () => Std.reflexivity ());
       Control.focus 1 1 (fun () => Std.reflexivity ());
       Control.focus 1 1 (fun () => Std.reflexivity ());
       Control.focus 1 1 (fun () => Std.reflexivity ());
       Control.focus 1 1 (fun () => Std.reflexivity ());
       Control.focus 1 1 (fun () => Std.reflexivity ());
       Control.focus 1 1 (fun () => Std.reflexivity ());
       Control.focus 1 1 (fun () => 
        let clause := { on_hyps := None; on_concl := AllOccurrences } in
          Std.unfold [(const_to_const_reference constr:(@ctx_resources_set), AllOccurrences)] clause;
          Std.cbn 
          { rStrength := Std.Norm;
            rBeta := true;
            rMatch := true;
            rFix := true;
            rCofix := true;
            rZeta := true;
            rDelta := true;
            rConst := [const_to_const_reference constr:(@set_from_list); const_to_const_reference constr:(@Sym.map_from_list)]
          } clause ;
          prove_struct_resource_inference_step ()
       )
  | [ |- log_entry_valid (ResourceInferenceStep _ (PredicateRequest _ ?p _ _) _) ] =>
       (* PredicateRequest case *)
       verbose_msg (smsg "Checking PredicateRequest for non-struct");
       verbose_print_constr "    Predicate: " p;
       Std.constructor false; (* apply simple_resource_inference_step*)
       Control.focus 1 1 (fun () => Std.reflexivity ());
       Control.focus 1 1 (fun () => Std.reflexivity ());
       Control.focus 1 1 (fun () => Std.reflexivity ());
       Control.focus 1 1 (fun () => Std.reflexivity ());
       Control.focus 1 1 (fun () => Std.reflexivity ());
       Control.focus 1 1 (fun () => Std.reflexivity ());
       Control.focus 1 1 (fun () => Std.reflexivity ());
       Control.focus 1 1 (fun () => 
       let clause := { on_hyps := None; on_concl := AllOccurrences } in
         Std.unfold [(const_to_const_reference constr:(@ctx_resources_set), AllOccurrences)] clause;
         Std.cbn 
           { rStrength := Std.Norm;
             rBeta := true;
             rMatch := true;
             rFix := true;
             rCofix := true;
             rZeta := true;
             rDelta := true;
             rConst := [const_to_const_reference  constr:(@set_from_list)]
           } clause ;
           prove_simple_resource_inference_step ()
       )
  | [ |- log_entry_valid (ResourceInferenceStep _ (UnfoldResources _) _) ] =>
      (* UnfoldResources case *)
      verbose_msg (smsg "Checking UnfoldResources");
      Std.constructor false;
      prove_unfold_step ()
  | [ |- _ ] => 
      Control.throw (Tactic_failure (Some (msg (Message.of_string "prove_log_entry_valid: match failed"))))
  end.

 Ltac2 prove_log_entry_list_valid () :=
   match! goal with
   | [ |- List.Forall log_entry_valid ?l ] =>
       let rec aux n l :=
         let nil_constr := constr:(@nil log_entry) in
         let cons_constr := constr:(@cons log_entry) in
         match Constr.Unsafe.kind l with
         | Constr.Unsafe.App f args =>
             let f_name := get_constructor_name f in
             let nil_name := get_constructor_name nil_constr in
             let cons_name := get_constructor_name cons_constr in
             if Constr.equal f_name nil_name then
               (* nil case *)
               Std.constructor false
             else if Constr.equal f_name cons_name then
               (* cons case *)
               let head := Array.get args 1 in
               let tail := Array.get args 2 in
               Std.constructor false;
               Control.focus 1 1 (fun () => prove_log_entry_valid n);
               Control.focus 1 1 (fun () => aux (Int.add n 1) tail)
             else
               Control.throw (Tactic_failure (Some (Message.of_string "Not a list")))
         | _ =>
             Control.throw (Tactic_failure (Some (Message.of_string "Not a list")))
         end
       in aux 0 l
   end.
 
 