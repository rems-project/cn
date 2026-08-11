module T = Terms.Normal
module Req = Request

type output = O of T.t [@@ocaml.unboxed] [@@deriving eq]

let pp_output (O t) = T.pp t

type predicate = Req.Predicate.t * output

type qpredicate = Req.QPredicate.t * output

type t = Req.t * output [@@deriving eq]

let pp (r, O output) = Req.pp_aux r (Some output)

let json re : Yojson.Safe.t = `String (Pp.plain (pp re))

let subst substitution ((r, O oargs) : t) =
  (Req.subst substitution r, O (T.subst substitution oargs))


let free_vars (r, O oargs) = Sym.Set.union (Req.free_vars r) (T.free_vars oargs)

