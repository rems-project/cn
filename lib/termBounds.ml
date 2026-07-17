open Terms
open Terms.Normal
open IndexTerms

let get_lower_bound_opt ((x, bt) : Sym.t * BaseTypes.t) (it : t) : t option =
  let rec aux (it : t) : t option =
    match it with
    | IT (Binop (EQ, IT (Sym x', _, _), tm2), _, _)
    | IT (Binop (EQ, tm2, IT (Sym x', _, _)), _, _) ->
      if Sym.equal x x' then Some tm2 else None
    | IT (Binop (LE, it', IT (Sym x', _, _)), _, _) when Sym.equal x x' -> Some it'
    | IT (Binop (LT, it', IT (Sym x', _, _)), _, _) when Sym.equal x x' ->
      Some
	(IT
	   ( Binop (Add, it', num_lit_ Z.one bt Cerb_location.unknown),
	     bt,
	     Cerb_location.unknown ))
    | IT (Binop (And, tm1, tm2), _, _) ->
      (match (aux tm1, aux tm2) with
       | None, None -> None
       | None, it' | it', None -> it'
       | Some tm1, Some tm2 ->
	 Some (IT (Binop (Max, tm1, tm2), bt, Cerb_location.unknown)))
    | IT (Binop (Or, tm1, tm2), _, _) ->
      (match (aux tm1, aux tm2) with
       | None, None | None, _ | _, None -> None
       | Some tm1, Some tm2 ->
	 Some (IT (Binop (Min, tm1, tm2), bt, Cerb_location.unknown)))
    | _ -> None
  in
  aux it


let get_lower_bound ((x, bt) : Sym.t * BaseTypes.t) (it : t) : t =
  let min =
    match bt with
    | Bits (sign, sz) -> fst (BaseTypes.bits_range (sign, sz))
    | _ ->
      Cerb_colour.with_colour
	(fun () ->
	   print_endline
	     Pp.(
	       plain
		 (!^"unsupported type"
		  ^^^ squotes (BaseTypes.pp bt)
		  ^^^ !^"in permission"
		  ^^^ squotes (pp it)
		  ^^^ !^"at"
		  ^^^ Locations.pp (get_loc it))))
	();
      exit 2
  in
  get_lower_bound_opt (x, bt) it
  |> Option.value ~default:(num_lit_ min bt Cerb_location.unknown)


let get_upper_bound_opt ((x, bt) : Sym.t * BaseTypes.t) (it : t) : t option =
  let rec aux (it : t) : t option =
    match it with
    | IT (Binop (EQ, IT (Sym x', _, _), tm2), _, _)
    | IT (Binop (EQ, tm2, IT (Sym x', _, _)), _, _) ->
      if Sym.equal x x' then Some tm2 else None
    | IT (Binop (LE, IT (Sym x', _, _), it'), _, _) when Sym.equal x x' -> Some it'
    | IT (Binop (LT, IT (Sym x', _, _), it'), _, _) when Sym.equal x x' ->
      Some
	(IT
	   ( Binop (Sub, it', num_lit_ Z.one bt Cerb_location.unknown),
	     bt,
	     Cerb_location.unknown ))
    | IT (Binop (And, tm1, tm2), _, _) ->
      (match (aux tm1, aux tm2) with
       | None, None -> None
       | None, it' | it', None -> it'
       | Some tm1, Some tm2 ->
	 Some (IT (Binop (Min, tm1, tm2), bt, Cerb_location.unknown)))
    | IT (Binop (Or, tm1, tm2), _, _) ->
      (match (aux tm1, aux tm2) with
       | None, None | None, _ | _, None -> None
       | Some tm1, Some tm2 ->
	 Some (IT (Binop (Max, tm1, tm2), bt, Cerb_location.unknown)))
    | _ -> None
  in
  aux it


let get_upper_bound ((x, bt) : Sym.t * BaseTypes.t) (it : t) : t =
  let max =
    match bt with
    | Bits (sign, sz) -> snd (BaseTypes.bits_range (sign, sz))
    | _ ->
      Cerb_colour.with_colour
	(fun () ->
	   print_endline
	     Pp.(
	       plain
		 (!^"unsupported type"
		  ^^^ squotes (BaseTypes.pp bt)
		  ^^^ !^"in permission"
		  ^^^ squotes (pp it)
		  ^^^ !^"at"
		  ^^^ Locations.pp (get_loc it))))
	();
      exit 2
  in
  get_upper_bound_opt (x, bt) it
  |> Option.value ~default:(num_lit_ max bt Cerb_location.unknown)


(* used for Fulminate optimisation *)
let is_eq_bound ((x_sym, _) : Sym.t * BaseTypes.t) (it : t) : bool =
  match it with
  | IT (Binop (EQ, IT (Sym x_sym', _, _), _), _, _)
  | IT (Binop (EQ, _, IT (Sym x_sym', _, _)), _, _) ->
    Sym.equal x_sym x_sym'
  | _ -> false


let is_lower_bound ((x, _) : Sym.t * BaseTypes.t) (it : t) : bool =
  match it with
  | IT (Binop (LE, _, IT (Sym x', _, _)), _, _)
  | IT (Binop (LT, _, IT (Sym x', _, _)), _, _) ->
    Sym.equal x x'
  | _ -> false


let is_upper_bound ((x, _) : Sym.t * BaseTypes.t) (it : t) : bool =
  match it with
  | IT (Binop (LE, IT (Sym x', _, _), _), _, _)
  | IT (Binop (LT, IT (Sym x', _, _), _), _, _) ->
    Sym.equal x x'
  | _ -> false


let is_bound (x : Sym.t * BaseTypes.t) (it : t) : bool =
  is_eq_bound x it || is_lower_bound x it || is_upper_bound x it


let it_only_bounds (x : Sym.t * BaseTypes.t) (it : t) : bool =
  match it with
  | IT (Binop (EQ, IT (Sym _, _, _), _), _, _) -> is_eq_bound x it
  | IT (Binop (And, tm1, tm2), _, _) -> is_lower_bound x tm1 && is_upper_bound x tm2
  | _ -> false


let it_non_bounds (x : Sym.t * BaseTypes.t) (it : t) : t =
  let true_const = bool_ true Cerb_location.unknown in
  let rec aux it =
    match it with
    | IT (Binop (And, tm1, tm2), bt, loc) ->
      if is_bound x tm1 && is_bound x tm2 then
	true_const
      else if is_bound x tm1 then
	aux tm2
      else if is_bound x tm2 then
	aux tm1
      else
	IT (Binop (And, aux tm1, aux tm2), bt, loc)
    | _ -> it
  in
  aux it


let get_bounds_opt ((x, bt) : Sym.t * BaseTypes.t) (it : t) : t option * t option =
  (get_lower_bound_opt (x, bt) it, get_upper_bound_opt (x, bt) it)


let get_bounds ((x, bt) : Sym.t * BaseTypes.t) (it : t) : t * t =
  (get_lower_bound (x, bt) it, get_upper_bound (x, bt) it)
