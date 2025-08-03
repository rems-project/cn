(* module WIntDomain : Domain.T = struct
  type b =
    { start : int64;
      stop : int64
    }
  [@@deriving eq, ord]

  type t = b Sym.Map.t option [@@deriving eq, ord]

  let top () = Some Sym.Map.empty

  (* let is_top d = match d with Top -> true | _ -> false *)

  (* let is_bot d = match d with Bot -> true | _ -> false *)

  let join = failwith ""

  let meet = failwith ""

  let rec forward_abstract_op (it : IndexTerms.t) (d : t) : b = failwith ""

  let rec backward_abstract_op (it : IndexTerms.t) (inputs : b list) (output : b) : b list
    =
    failwith ""


  let forward_abstract_it (it : IndexTerms.t) (d : t) : t =
    let rec aux (it : IndexTerms.t) (d : t) : t =
      let (IT (it_, _, _)) = it in
      match it_ with _ -> d
    in
    aux it d


  let backward_abstract_it (it : IndexTerms.t) (d : t) : t =
    let rec aux (it : IndexTerms.t) (d : t) : t =
      let (IT (it_, _, _)) = it in
      match it_ with _ -> d
    in
    aux it d


  let pp d =
    let open Pp in
    !^"wint"
    ^^ parens
         (match d with
          | None -> !^"⊥"
          | Some d' ->
            braces
              (separate_map
                 (semi ^^ break 1)
                 (fun (x, { start; stop }) ->
                    Sym.pp x
                    ^^^ !^"∈"
                    ^^^ parens
                          (assert (not (Int64.equal (Int64.sub start stop) Int64.one));
                           braces (int64 start ^^ comma ^^^ int64 stop)))
                 (Sym.Map.bindings d')))
end *)
