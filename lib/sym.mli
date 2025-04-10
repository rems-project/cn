val print_nums : bool ref

val executable_spec_enabled : bool ref

type t = Cerb_frontend.Symbol.sym

val compare : t -> t -> int

val equal : t -> t -> bool

module Set : Set.S with type elt = t

module Map : Map.S with type key = t

val description : t -> Cerb_frontend.Symbol.symbol_description

val pp_string : t -> string

val pp_string_no_nums : t -> string

val pp : t -> PPrint.document

val pp_debug : t -> PPrint.document

val num : t -> int

val fresh_anon : unit -> t

val fresh : string -> t

val fresh_same : t -> t

val has_id : t -> string option

val has_id_with : (string -> bool) -> t -> bool

val has_cn_id_with : (string -> bool) -> t -> bool

val fresh_make_uniq : string -> t

val fresh_make_uniq_kind : prefix:string -> string -> t

val json : t -> Yojson.Safe.t

val hash : t -> int
