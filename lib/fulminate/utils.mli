module CF = Cerb_frontend
module A = CF.AilSyntax
module C = CF.Ctype
module Cn = CF.Cn

val empty_attributes : CF.Annot.attributes

val mk_ctype : ?annots:Cerb_frontend.Annot.annot list -> C.ctype_ -> C.ctype

val rm_ctype : C.ctype -> C.ctype_

val get_typedef_string : C.ctype -> string option

val mk_expr
  :  ?loc:Cerb_location.t ->
  CF.GenTypes.genTypeCategory A.expression_ ->
  CF.GenTypes.genTypeCategory A.expression

val mk_stmt : 'a A.statement_ -> 'a A.statement

val rm_expr : 'a A.expression -> 'a A.expression_

val empty_ail_expr : 'a A.expression_

val generate_sym_with_suffix
  :  ?suffix:string ->
  ?uppercase:bool ->
  ?lowercase:bool ->
  C.union_tag ->
  C.union_tag

val list_split_three : ('a * 'b * 'c) list -> 'a list * 'b list * 'c list

val ifndef_wrap : string -> string -> string

val generate_include_header : string * bool -> string

val get_ctype_without_ptr : C.ctype -> C.ctype

val str_of_ctype : C.ctype -> string

val execCtypeEqual : C.ctype -> C.ctype -> bool

val create_binding
  :  'a ->
  'b ->
  'a * ((Cerb_location.t * A.storageDuration * bool) * 'c option * C.qualifiers * 'b)

val find_ctype_from_bindings : (Sym.t * ('a * 'b * 'c * 'd)) list -> Sym.t -> 'd

val get_start_loc : ?offset:int -> Cerb_location.t -> Cerb_location.t

val get_end_loc : ?offset:int -> Cerb_location.t -> Cerb_location.t

val concat_map_newline : Pp.document list -> Pp.document
