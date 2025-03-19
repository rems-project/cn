type unpack_result =
  | UnpackLRT of LogicalReturnTypes.t
  | UnpackRES of Resource.t list

type unfold_changed = (int * Resource.t * unpack_result) list

type extract_changed = Resource.t list

type unfold_step = unfold_changed * extract_changed

type resource_inference_type =
  | PredicateRequest of
      Error_common.situation
      * Request.Predicate.t
      * (Locations.t * string) option
      * (Resource.predicate * int list)
  | UnfoldResources of Cerb_location.t * unfold_step list

type log_entry =
  | ResourceInferenceStep of (Context.t * resource_inference_type * Context.t)

type log = log_entry list

val set_enabled : bool -> unit

val add_log_entry : log_entry -> unit

val get_proof_log : unit -> log_entry list

val record_resource_inference_step
  :  Context.t ->
  Context.t ->
  resource_inference_type ->
  unit
