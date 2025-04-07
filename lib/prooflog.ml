(* Add a mutable flag to control whether proof logging is enabled *)
let proof_log_enabled = ref false

(* Function to set the proof log enabled flag *)
let set_enabled flag = proof_log_enabled := flag

(* Function to check if proof logging is enabled *)
let is_enabled () = !proof_log_enabled

type unpack_result =
  | UnpackLRT of LogicalReturnTypes.t
  | UnpackRES of Resource.t list

type unfold_changed = (Resource.t * unpack_result) list

type extract_changed = Resource.t list

type unfold_step = unfold_changed * extract_changed

type log_entry =
  | PredicateRequest of
      Context.t
      * Error_common.situation
      * Request.Predicate.t
      * Resource.predicate
      * log_entry list
      * Context.t
  | UnfoldResources of Context.t * Cerb_location.t * unfold_step list * Context.t

type log = log_entry list (* most recent first *)

(* list of log entries *)
let proof_log = ref []

let add_log_entry entry =
  if !proof_log_enabled then
    proof_log := entry :: !proof_log
  else
    () (* No logging if disabled *)


let get_proof_log () = !proof_log

let record_resource_inference_step entry = add_log_entry entry

let rec simplify_proof_log_entry simp_ctxt log_entry =
  match log_entry with
  | PredicateRequest (ic, s, req, (p, Resource.O o), log, oc) ->
    let p_simp = Simplify.Request.Predicate.simp simp_ctxt p in
    let o_simp = Simplify.IndexTerms.simp ~inline_symbols:false simp_ctxt o in
    let log_simp = simplify_proof_log simp_ctxt log in
    PredicateRequest (ic, s, req, (p_simp, Resource.O o_simp), log_simp, oc)
  | _ -> log_entry


and simplify_proof_log simp_ctxt log = List.map (simplify_proof_log_entry simp_ctxt) log

(* these versions of functions simplify only the inner proof log steps, which are used for hints *)
let simplify_hints_proof_log_entry simp_ctxt log_entry =
  match log_entry with
  | PredicateRequest (ic, s, req, r, log, oc) ->
    let log_simp = simplify_proof_log simp_ctxt log in
    PredicateRequest (ic, s, req, r, log_simp, oc)
  | _ -> log_entry


let simplify_hints_proof_log simp_ctxt log =
  List.map (simplify_hints_proof_log_entry simp_ctxt) log
