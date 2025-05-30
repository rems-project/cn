module CF = Cerb_frontend
module A = CF.AilSyntax
module BT = BaseTypes
module IT = IndexTerms
module LC = LogicalConstraints

type context = (A.ail_identifier * GenDefinitions.Make(GenElaboratedTerms).t) list

val pp : context -> Pp.document

val elaborate : GenContext.Make(GenTerms).t -> context
