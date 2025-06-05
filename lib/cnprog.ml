module IT = IndexTerms
module Loc = Locations

type load =
  { ct : Sctypes.t;
    pointer : IndexTerms.t
  }

type 'a t =
  | Let of (Sym.t * load) * 'a t
  | Pure of 'a
[@@deriving map]

let rec subst f substitution = function
  | Let ((name, { ct; pointer }), prog) ->
    let pointer = IT.subst substitution pointer in
    let name, prog = suitably_alpha_rename f substitution.relevant name prog in
    Let ((name, { ct; pointer }), subst f substitution prog)
  | Pure x -> Pure (f substitution x)


and alpha_rename_ f ~from ~to_ prog = (to_, subst f (IT.make_rename ~from ~to_) prog)

and alpha_rename f from prog =
  let to_ = Sym.fresh_same from in
  alpha_rename_ f ~from ~to_ prog


and suitably_alpha_rename f syms s prog =
  if Sym.Set.mem s syms then
    alpha_rename f s prog
  else
    (s, prog)


let rec dtree f =
  let open Cerb_frontend.Pp_ast in
  function
  | Let ((s, load), prog) ->
    Dnode
      ( pp_ctor "LetLoad",
        [ Dleaf (Sym.pp s);
          IT.dtree load.pointer;
          Dleaf (Sctypes.pp load.ct);
          dtree f prog
        ] )
  | Pure x -> f x
