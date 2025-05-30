module CF = Cerb_frontend
module A = CF.AilSyntax
module BT = BaseTypes
module IT = IndexTerms
module LC = LogicalConstraints

type definition =
  { filename : string;
    sized : bool;
    name : Sym.t;
    iargs : (Sym.t * BT.t) list;
    oargs : (Sym.t * BT.t) list;
    body : GenElaboratedTerms.t
  }
[@@deriving eq, ord]

val pp_definition : definition -> Pp.document

type context = (A.ail_identifier * definition) list

val pp : context -> Pp.document

val elaborate : GenContext.Make(GenTerms).t -> context
