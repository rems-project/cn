module T = Terms.Normal
module MT = MakeTerm
module BT = BaseTypes
open Pp

type t =
  | T of T.t
  | Forall of (Sym.t * BT.t) * T.t
[@@deriving eq, ord]

module Set = Set.Make (struct
    type nonrec t = t

    let compare = compare
  end)

let pp = function
  | T it -> T.pp it
  | Forall ((s, bt), it) -> Pp.c_app !^"forall" [ Sym.pp s; BT.pp bt ] ^^ dot ^^^ T.pp it


let json c : Yojson.Safe.t = `String (Pp.plain (pp c))

let subst su c =
  match c with
  | T it -> T (T.subst su it)
  | Forall ((s, bt), body) ->
    let s, body = T.suitably_alpha_rename su.relevant s body in
    Forall ((s, bt), T.subst su body)


let subst_ su c = subst (T.make_subst su) c

let free_vars_bts = function
  | T c -> T.free_vars_bts c
  | Forall ((s, _), body) -> Sym.Map.remove s (T.free_vars_bts body)


let free_vars = function
  | T c -> T.free_vars c
  | Forall ((s, _), body) -> Sym.Set.remove s (T.free_vars body)


let alpha_equivalent lc lc' =
  match (lc, lc') with
  | T c, T c' -> T.equal c c'
  | Forall ((s, bt), c), Forall ((s', bt'), c') ->
    BT.equal bt bt'
    &&
    if Sym.equal s s' then
      T.equal c c'
    else (
      let new_s = Sym.fresh_same s in
      let c = T.subst (T.make_rename ~from:s ~to_:new_s) c in
      let c' = T.subst (T.make_rename ~from:s' ~to_:new_s) c' in
      T.equal c c')
  | _ -> false


let forall_ (s, bt) it = Forall ((s, bt), it)

let is_sym_lhs_equality = function
  | T t ->
    (match Terms.is_eq t with
     | Some (lhs, rhs) ->
       (match Terms.is_sym lhs with Some (s, _) -> Some (s, rhs) | _ -> None)
     | _ -> None)
  | _ -> None


let is_sym_equality t =
  match is_sym_lhs_equality t with
  | Some (s, rhs) ->
    (match Terms.is_sym rhs with Some (s', _) -> Some (s, s') | _ -> None)
  | _ -> None


let is_equality = function
  | T it ->
    (match it with
     | IT (Binop (EQ, a, b), _, _) -> Some ((a, b), true)
     | IT (Unop (Not, IT (Binop (EQ, a, b), _, _)), _, _) -> Some ((a, b), false)
     | _ -> None)
  | _ -> None


let equates_to it2 = function
  | T it ->
    (match it with
     | IT (Binop (EQ, a, b), _, _) when T.equal a it2 -> Some b
     | IT (Binop (EQ, a, b), _, _) when T.equal b it2 -> Some a
     | _ -> None)
  | _ -> None


let dtree =
  let open Cerb_frontend.Pp_ast in
  function
  | T it -> Dnode (pp_ctor "T", [ T.dtree it ])
  | Forall ((s, _bt), t) -> Dnode (pp_ctor "Forall", [ Dleaf (Sym.pp s); T.dtree t ])


let is_forall : t -> bool = fun c -> match c with T _ -> false | Forall _ -> true

let is_interesting : t -> bool =
  fun c ->
  match c with
  (* | LC.T (IT (Aligned _, _, _)) -> false *)
  | T (IT (Representable _, _, _)) -> false
  | T (IT (Good _, _, _)) -> false
  | _ -> true


(** make `lc` conditional on `it` *)
let impl loc (it : T.t) (lc : t) : t =
  match lc with
  | T t -> T (MT.impl_ (it, t) loc)
  | Forall ((s, bt), t) ->
    let s, t = T.alpha_rename s t in
    Forall ((s, bt), MT.impl_ (it, t) loc)


let preds_of (lc : t) : Sym.Set.t =
  match lc with T it | Forall (_, it) -> Terms.preds_of it
