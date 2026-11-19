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

type log = log_entry list

val set_enabled : bool -> unit

val is_enabled : unit -> bool

val add_log_entry : log_entry -> unit

val get_proof_log : unit -> log_entry list

val record_resource_inference_step : log_entry -> unit
