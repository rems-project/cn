module S = Cerb_frontend.Symbol

let print_nums = ref false

let executable_spec_enabled = ref false

module Ord = struct
  type t = S.sym

  let compare = S.symbol_compare
end

include Ord

let equal = S.symbolEquality

module Set = Set.Make (Ord)
module Map = Map.Make (Ord)

let description = S.symbol_description

let pp_string sym =
  if !executable_spec_enabled then
    Cerb_frontend.Pp_symbol.to_string_pretty sym
  else (
    let print_nums = !print_nums in
    Cerb_frontend.Pp_symbol.to_string_pretty_cn ~print_nums sym)


let pp_string_no_nums sym =
  Cerb_frontend.Pp_symbol.to_string_pretty_cn ~print_nums:false sym


let pp sym = Pp.string (pp_string sym)

let pp_debug sym = Pp.string (S.show_raw_less sym)

let num = S.symbol_num

let fresh_anon () = S.fresh ()

let fresh = S.fresh_cn

let fresh_same s = S.fresh_description (S.symbol_description s)

let has_id = function S.Symbol (_digest, _nat, SD_Id str) -> Some str | _ -> None

let has_id_with f sym = match has_id sym with None -> false | Some str -> f str

let has_cn_id_with f = function S.Symbol (_, _, SD_CN_Id str) -> f str | _ -> false

let make_uniq =
  let module Map =
    Hashtbl.Make (struct
      (* Sign, String.hash is only available in OCaml 5.0 onwwards. *)
      type t = string

      let equal = String.equal

      let hash = Hashtbl.hash
    end)
  in
  let name_uses = Map.create 20 in
  fun str ->
    let next = match Map.find_opt name_uses str with None -> 0 | Some i -> i + 1 in
    Map.add name_uses str next;
    str ^ string_of_int next


let fresh_make_uniq name = fresh (make_uniq name)

let fresh_make_uniq_kind ~prefix name = fresh (make_uniq prefix ^ "_" ^ name)

let json sym = `String (pp_string sym)

let hash = num
