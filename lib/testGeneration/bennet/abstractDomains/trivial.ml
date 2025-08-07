let name = "trivial"

module CInt : Domain.C_INTERFACE = struct
  open Pp

  let name = !^name

  let definitions () = empty
end

type t = unit [@@deriving eq, ord]

let bottom = ()

let top = ()

let leq _ _ = true

let join _ _ = ()

let meet _ _ = ()

let rename ~from:_ ~to_:_ _ = ()

let remove _ _ = ()

let retain _ _ = ()

let pp _ = Pp.string "()"

module Interpreter = struct
  let abs_stmt _ _ (d : t) : t = d
end
