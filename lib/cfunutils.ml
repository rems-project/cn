module Loc = Locations

open Pp

let skip_and_only = ref (([] : string list), ([] : string list))



(** Filter functions according to [(skip, only)]: first according to [only],
    then according to [skip]. When [strict] is true, error and exit if any
    function in [only] is not found. *)
let select_functions
      ?(strict = false)
      (skip_and_only : string list * string list)
      (fsyms : Sym.Set.t)
  : Sym.Set.t
  =
  let matches_str s fsym = String.equal s (Sym.pp_string fsym) in
  let str_fsyms s = Sym.Set.filter (matches_str s) fsyms in
  let str_fsyms_warn s =
    let ss = str_fsyms s in
    if Sym.Set.is_empty ss then
      Pp.warn_noloc (!^"function" ^^^ !^s ^^^ !^"not found");
    ss
  in
  let only_list = snd skip_and_only in
  (* When strict, error on missing --only functions *)
  if strict then (
    let missing_only = List.filter (fun s -> Sym.Set.is_empty (str_fsyms s)) only_list in
    if not (List.is_empty missing_only) then (
      let missing_str = String.concat ", " missing_only in
      Pp.error
        (Loc.other __LOC__)
        (!^"functions specified in --only not found:" ^^^ !^missing_str)
        [];
      exit 1));
  let strs_fsyms ss =
    ss |> List.map str_fsyms_warn |> List.fold_left Sym.Set.union Sym.Set.empty
  in
  let skip = strs_fsyms (fst skip_and_only) in
  let only = strs_fsyms only_list in
  let only_funs =
    match only_list with
    | [] -> fsyms
    | _ss -> Sym.Set.filter (fun fsym -> Sym.Set.mem fsym only) fsyms
  in
  Sym.Set.filter (fun fsym -> not (Sym.Set.mem fsym skip)) only_funs
