module BT = BaseTypes
module AT = ArgumentTypes
module Loc = Locations
module StringSet = Set.Make (String)
module CI = Coq_ir
module CC = Cn_to_coq

let ret_sym = "ν"

let parse_directions directions = (directions, StringSet.singleton "all")

let header filename =
  let open Pp in
  !^"-- "
  ^^^ !^filename
  ^^ !^": generated lemma specifications from CN *)"
  ^^ hardline
  ^^ hardline
  ^^ !^"import CN_Lib_Iris"
  ^^ hardline
  ^^ !^"import CN_Lib_Iris_Fixpoint"
  ^^ hardline
  ^^ !^"import Iris.ProofMode"
  ^^ hardline
  ^^ hardline
  ^^ hardline
  ^^ !^"namespace Gen_Spec"
  ^^ hardline
  ^^ hardline
  ^^ !^"open Iris CN_Lib_Iris ProofMode"
  ^^ hardline
  ^^ hardline
  ^^ !^"variable {hlc GF} [MyHeap hlc GF]"
  ^^ hardline
  ^^ hardline


let print_section section_name comment section_body =
  let open Pp in
  !^"section "
  ^^^ !^section_name
  ^^ hardline
  ^^ hardline
  ^^^ !^comment
  ^^ hardline
  ^^ hardline
  ^^ flow hardline section_body
  ^^ hardline
  ^^ hardline
  ^^ !^"end "
  ^^^ !^section_name
  ^^ hardline
  ^^ hardline


(* Convenient printing functions *)

let fail msg details =
  let open Pp in
  print stdout (format [ Bold; Red ] msg ^^ colon ^^ space ^^ details);
  failwith msg


let build = function
  | [] -> fail "build" (Pp.string "empty")
  | xs ->
    let docs = List.map (fun x -> x) xs in
    Pp.flow (Pp.break 1) docs


let parensM x = Pp.parens x

let rets s = Pp.string s

let iris_pure x = build [ rets "⌜"; x; rets "⌝" ]

let rec intersperse (sep : string) (last : string) xs =
  let open Pp in
  match xs with
  | [] -> !^""
  | x :: [] -> x ^^ !^last
  | x :: xs -> x ^^ !^sep ^^ intersperse sep last xs


let print_ctype (ctyp : Sctypes.t) =
  match ctyp with
  | Void -> "unsupported ctype void"
  | Integer _ -> "Int"
  | Array _ -> "unsupported ctype array"
  | Pointer _ -> "Ptr"
  | Struct s -> Sym.pp_string s
  | Function _ -> "unsupported ctype function"
  | Byte -> "unsupported ctype function" (* TODO(HK): added for plumbing *)


let enc_z z =
  if Z.leq Z.zero z then
    rets (Z.to_string z)
  else
    parensM (rets (Z.to_string z))


let f_appM nm xs = parensM (build (rets nm :: xs))

let defn nm args opt_ty rhs =
  let open Pp in
  let tyeq = match opt_ty with None -> [] | Some ty -> [ colon; ty ] in
  flow (break 1) ([ !^"  def"; !^nm ] @ args @ tyeq @ [ !^":=" ])
  ^^ hardline
  ^^ !^"    "
  ^^ rhs
  ^^ hardline


let binop s x y =
  let open Pp in
  parens (flow (break 1) [ x; !^s; y ])


let tuple_itp_ty doc fld_tys =
  let open Pp in
  let rec stars = function
    | [] -> fail "tuple_itp_ty: empty" doc
    | [ x ] -> [ x ]
    | x :: xs -> x :: star :: stars xs
  in
  parens (flow (break 1) (stars fld_tys))


let generate (_ : Global.t) directions (_ : (Sym.t * (Loc.t * AT.lemmat)) list) =
  let f =
    let filename, _kinds = parse_directions directions in
    let channel = open_out filename in
    Pp.print channel (header filename);
    (* translate everything to itp AST*)
    Pp.print channel (rets "hi")
  in
  f
