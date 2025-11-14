module IT = IndexTerms
module Loc = Locations

type load =
  { ct : Sctypes.t;
    pointer : IndexTerms.t
  }

(* Cnprog.t should be removed eventually
  since adding CN loads is sound and so can be done on the frontend *)
type 'a t =
  | Let of Loc.t * (Sym.t * load) * 'a t
  | Pure of Loc.t * 'a
[@@deriving map]

let rec subst f substitution = function
  | Let (loc, (name, { ct; pointer }), prog) ->
    let pointer = IT.subst substitution pointer in
    let name, prog = suitably_alpha_rename f substitution.relevant name prog in
    Let (loc, (name, { ct; pointer }), subst f substitution prog)
  | Pure (loc, x) -> Pure (loc, f substitution x)


and alpha_rename_ f ~from ~to_ prog = (to_, subst f (IT.make_rename ~from ~to_) prog)

and alpha_rename f from prog =
  let to_ = Sym.fresh_same from in
  alpha_rename_ f ~from ~to_ prog


and suitably_alpha_rename f syms s prog =
  if Sym.Set.mem s syms then
    alpha_rename f s prog
  else
    (s, prog)


let rec free_vars f = function
  | Let (_, (name, { pointer; _ }), prog) ->
    let pointer_fvs = IT.free_vars pointer in
    let prog_fvs = free_vars f prog in
    Sym.Set.union pointer_fvs (Sym.Set.remove name prog_fvs)
  | Pure (_, x) -> f x


let free_vars_list pure_free_vars progs =
  progs
  |> List.map (free_vars pure_free_vars)
  |> List.fold_left Sym.Set.union Sym.Set.empty


let rec dtree f =
  let open Cerb_frontend.Pp_ast in
  function
  | Let (_loc, (s, load), prog) ->
    Dnode
      ( pp_ctor "LetLoad",
        [ Dleaf (Sym.pp s);
          IT.dtree load.pointer;
          Dleaf (Sctypes.pp load.ct);
          dtree f prog
        ] )
  | Pure (_loc, x) -> f x


let rec get_bt (cnprog_it : IT.t t) =
  match cnprog_it with
  | Let (_, _, cnprog_it) -> get_bt cnprog_it
  | Pure (_, it) -> IT.get_bt it
