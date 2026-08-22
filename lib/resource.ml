module T = Terms.Normal
module MT = MakeTerm
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

let disable_derived_lc1 = ref false

(* assumption: the resource is owned *)
let derived_lc1 ((resource : Req.t), O output) =
  if !disable_derived_lc1 then
    []
  else (
    let here = Locations.other __LOC__ in
    match resource with
    | P { name = Owned (ct, _); pointer; iargs = _ } ->
      let addr = MT.addr_ pointer here in
      let upper = MT.upper_bound addr ct here in
      let alloc_bounds =
        if !MT.use_vip then
          let module H = Alloc.History in
          let H.{ base; size } = H.(split (lookup_ptr pointer here) here) in
          [ MT.(le_ (base, addr) here); MT.(le_ (upper, add_ (base, size) here) here) ]
        else
          []
      in
      let within_addr_space = 
	if !BaseTypes.cnBV then (MT.(le_ (addr, upper) here))
        else MT.le_ (upper, MT.z_ Memory.max_pointer here) here
      in
      within_addr_space :: MT.hasAllocId_ pointer here :: alloc_bounds
    | P { name; pointer; iargs = [] }
      when !MT.use_vip && Req.(equal_name name Predicate.alloc) ->
      let module H = Alloc.History in
      let lookup = H.lookup_ptr pointer here in
      let H.{ base; size } = H.split lookup here in
      [ MT.(eq_ (lookup, output) here); MT.(le_ (base, add_ (base, size) here) here) ]
    | Q { name = Owned _; pointer; _ } -> [ MT.hasAllocId_ pointer here ]
    | P { name = PName _; pointer = _; iargs = _ } | Q { name = PName _; _ } -> [])


(* assumption: both resources are owned at the same *)
(* todo, depending on how much we need *)

let derived_lc2 =
  let derived ((resource : Req.t), _) ((resource' : Req.t), _) =
    match (resource, resource') with
    | ( P { name = Owned (ct1, _); pointer = p1; iargs = _ },
        P { name = Owned (ct2, _); pointer = p2; iargs = _ } ) ->
      let here = Locations.other __LOC__ in
      let addr1 = MT.addr_ p1 here in
      let addr2 = MT.addr_ p2 here in
      let up1 = MT.upper_bound addr1 ct1 here in
      let up2 = MT.upper_bound addr2 ct2 here in
      [ MT.(or2_ (le_ (up2, addr1) here, le_ (up1, addr2) here) here) ]
    | _ -> []
  in
  let rec aux acc = function
    | [] -> acc
    | r :: rs -> aux (List.concat_map (derived r) rs @ acc) rs
  in
  fun rs -> aux [] rs
