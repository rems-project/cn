(** Manipulate a resource *)
type action =
  | Read of IndexTerms.t * IndexTerms.t
  | Write of IndexTerms.t * IndexTerms.t
  | Create of IndexTerms.t
  | Kill of IndexTerms.t
  | Call of
      { fsym : Sym.t;
        args : IndexTerms.t list;
        gargs : IndexTerms.t list
      }
  | Return of
      { arg : IndexTerms.t;
        gargs : IndexTerms.t list
      }

(** Info about what happened *)
type log_entry =
  | Action of action * Locations.t (** We did this. *)
  | State of Context.t (** Various things we know about. *)

(** Steps we took to get here, most recent first *)
type log = log_entry list

(** Additional information about what went wrong. *)
type state_extras =
  { request : Request.t option; (** Requested resource *)
    unproven_constraint : LogicalConstraints.t option (** Unproven constraint *)
  }

(** No additional information *)
val no_ex : state_extras

(** Generate a report describing what went wrong. *)
val trace : Context.t * log -> Solver.model_with_q -> state_extras -> Report.report
