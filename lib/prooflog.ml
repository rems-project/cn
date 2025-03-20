(* Add a mutable flag to control whether proof logging is enabled *)
let proof_log_enabled = ref false

(* Function to set the proof log enabled flag *)
let set_enabled flag = proof_log_enabled := flag

type unpack_result =
  | UnpackLRT of LogicalReturnTypes.t
  | UnpackRES of Resource.t list

type unfold_changed = (Resource.t * unpack_result) list

type extract_changed = Resource.t list

type unfold_step = unfold_changed * extract_changed

type resource_inference_type =
  | PredicateRequest of
      Error_common.situation * Request.Predicate.t * (Resource.predicate * int list)
  | UnfoldResources of Cerb_location.t * unfold_step list

(** Info about what happened *)
type log_entry =
  | ResourceInferenceStep of (Context.t * resource_inference_type * Context.t)

type log = log_entry list (* most recent first *)

(* list of log entries *)
let proof_log = ref []

let add_log_entry entry =
  if !proof_log_enabled then
    proof_log := entry :: !proof_log
  else
    () (* No logging if disabled *)


let get_proof_log () = !proof_log

let record_resource_inference_step
      (c : Context.t)
      (c' : Context.t)
      (ri : resource_inference_type)
  : unit
  =
  add_log_entry (ResourceInferenceStep (c, ri, c'))
