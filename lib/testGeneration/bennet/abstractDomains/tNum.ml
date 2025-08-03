(* module TNumDomain : Domain.T = struct
  type t =
    { value : int64;
      mask : int64
    }
  [@@deriving eq, ord]

  (* let is_top { value; mask } =
    Int64.equal value Int64.zero && Int64.equal mask Int64.max_int *)

  (* let is_bot { value; mask; _ } = not (Int64.equal (Int64.logand value mask) Int64.zero) *)

  let join d1 d2 =
    { value = Int64.logand d1.value d2.value;
      mask =
        (let combined_mask = Int64.logor d1.mask d2.mask in
         let eta = Int64.lognot combined_mask in
         let delta =
           Int64.logxor (Int64.logand d1.value eta) (Int64.logand d2.value eta)
         in
         Int64.logor combined_mask delta)
    }


  let meet d1 d2 =
    { value = Int64.logor d1.value d2.value; mask = Int64.logand d1.mask d2.mask }


  let update_it = failwith ""

  let abstract_it = failwith ""

  let pp { value; mask } = Pp.(!^"tnum" ^^ parens (int64 value ^^ comma ^^^ int64 mask))
end *)
