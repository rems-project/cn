(* module SizedDomain : Domain.T = struct
  type t =
    | Bot
    | Size of Int64.t
  [@@deriving eq, ord]

  (* let is_top d = match d with Size sz -> Int64.equal sz Int64.zero | Bot -> false *)

  (* let is_bot d = match d with Bot -> true | _ -> false *)

  let join d1 d2 =
    match (d1, d2) with
    | Size sz1, Size sz2 -> Size (Int64.max sz1 sz2)
    | Bot, d | d, Bot -> d


  let meet d1 d2 =
    match (d1, d2) with
    | Size sz1, Size sz2 -> Size (Int64.max sz1 sz2)
    | Bot, _ | _, Bot -> Bot


  let update_it = failwith ""

  let abstract_it = failwith ""

  let pp d =
    let open Pp in
    !^"sized"
    ^^ parens
         (match d with
          | Bot -> !^"⊥"
          | Size sz -> if Int64.equal sz Int64.zero then !^"⊤" else int64 sz)
end *)
