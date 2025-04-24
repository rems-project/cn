open Cerb_frontend

val empty_attributes : Annot.attributes

val mk_ctype : ?annots:Annot.annot list -> Ctype.ctype_ -> Ctype.ctype

val rm_ctype : Ctype.ctype -> Ctype.ctype_

val get_typedef_string : Ctype.ctype -> string option

val mk_expr
  :  ?loc:Cerb_location.t ->
  GenTypes.genTypeCategory AilSyntax.expression_ ->
  GenTypes.genTypeCategory AilSyntax.expression

val mk_stmt : 'a AilSyntax.statement_ -> 'a AilSyntax.statement

val rm_expr : 'a AilSyntax.expression -> 'a AilSyntax.expression_

val empty_ail_expr : 'a AilSyntax.expression_

val generate_sym_with_suffix
  :  ?suffix:string ->
  ?uppercase:bool ->
  ?lowercase:bool ->
  Ctype.union_tag ->
  Ctype.union_tag

val list_split_three : ('a * 'b * 'c) list -> 'a list * 'b list * 'c list

val ifndef_wrap : string -> string -> string

val generate_include_header : string * bool -> string

val get_ctype_without_ptr : Ctype.ctype -> Ctype.ctype

val str_of_ctype : Ctype.ctype -> string

val execCtypeEqual : Ctype.ctype -> Ctype.ctype -> bool

val create_binding
  :  'a ->
  'b ->
  'a
  * ((Cerb_location.t * AilSyntax.storageDuration * bool)
    * 'c option
    * Ctype.qualifiers
    * 'b)

val find_ctype_from_bindings : (Sym.t * ('a * 'b * 'c * 'd)) list -> Sym.t -> 'd

val get_start_loc : ?offset:int -> Cerb_location.t -> Cerb_location.t

val get_end_loc : ?offset:int -> Cerb_location.t -> Cerb_location.t

val concat_map_newline : PPrint.document list -> PPrint.document
