module type Interpreter = sig
  module AD : GenTerms.Domain.T

  val abs_stmt
    :  AD.t Sym.Map.t ->
    ('tag, [< ('tag, 'recur) GenTerms.Make(AD).Inner.ast ]) GenTerms.annot ->
    AD.t ->
    AD.t
end
