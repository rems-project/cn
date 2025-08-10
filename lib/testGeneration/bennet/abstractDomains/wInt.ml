(* module BT = BaseTypes

module WrappedInterval = struct
  type t =
    { bt : BT.t;
      is_top : bool;
      is_bot : bool;
      start : Z.t;
      stop : Z.t
    }
  [@@deriving eq, ord]

  let top (bt : BT.t) =
    let start, stop =
      BT.bits_range
        (Option.get
           (BT.is_bits_bt
              (match bt with
               | Loc () -> Memory.uintptr_bt
               | Bits _ -> bt
               | _ -> failwith __LOC__)))
    in
    { bt; is_top = true; is_bot = false; start; stop }


  let bottom (bt : BT.t) =
    { bt; is_top = false; is_bot = true; start = Z.zero; stop = Z.zero }


  let forward_abs_it (it : IndexTerms.t) (rs : t list) : t = failwith ""

  let backward_abs_it (it : IndexTerms.t) (rs : t list) : t = failwith ""
end *)

(* module Inner (* : Domain.T *) = struct
  (* C interface for code generation *)
  module CInt : Domain.C_INTERFACE = struct
    open Pp

    let name = !^"wint"

    let definitions () = empty
  end

  let name = "wrapped_interval"


  (* Domain maps symbols to wrapped intervals *)
  type t = b Sym.Map.t  [@@deriving eq, ord]

  let bottom = None

  let top = Some Sym.Map.empty

  let complement d = 

  let leq = failwith ""
end *)

(* include Inner *)
