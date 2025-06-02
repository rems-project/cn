module CF = Cerb_frontend
module A = CF.Annot
module Cn = CF.Cn

let allow_split_magic_comments = ref false

(* type msg = Cerb_frontend.Errors.cparser_cause *)
type msg = TypeErrors.message

type err = TypeErrors.t
(* { loc : Locations.t; *)
(*   msg : msg *)
(* } *)

open TypeErrors

module Monad = struct
  type 'a t = ('a, err) Result.t

  let bind = Result.bind

  let fail = Result.error

  let return = Result.ok
end

open Effectful.Make (Monad)

(* NOTE: There are four types of CN parsing constructs, each with
   a different entry point from which a parser can be started:
   - cn_statement: for proof guidance, debugging
   - cn_function_spec: pre and post conditions
   - cn_loop_spec: loop invariants
   - cn_toplevel: for declarations

   1. C program is parsed into a C abstract sytnax tree (Cabs)
   2. Toplevel magic comments are turned into CN toplevel declarations.
   3. Magic statements, function specifications and loop specifications are
   inserted as string annotations (attributes).
   4. Cabs is desugared, then elaborated into Core (including the CN toplevel declarations).
   5. Core is turned into mucore, during which process the remaining magic
   comments are parsed and desugared into mucore as well. *)

(* the character @ is not a separator in C, so supporting @start as a
   legacy syntax requires special hacks *)
let fiddle_at_hack string =
  let ss = String.split_on_char '@' string in
  let rec fix = function
    | [] -> ""
    | [ s ] -> s
    | s1 :: s2 :: ss ->
      if Tools.starts_with "start" s2 then
        fix ((s1 ^ "%" ^ s2) :: ss)
      else
        fix ((s1 ^ "@" ^ s2) :: ss)
  in
  fix ss


let debug_tokens loc string =
  let toks, pos = C_parser_driver.diagnostic_get_tokens ~inside_cn:true loc string in
  let pp_int_pair (x, y) = Pp.(parens (int x ^^ comma ^^^ int y)) in
  let open Pp.Infix in
  Pp.item "failed to parse tokens" (Pp.braces (Pp.list Pp.string toks))
  ^/^ Pp.item "(line, col)" (Pp.braces (Pp.list pp_int_pair pos))


let parse parser_start (loc, string) =
  let string = fiddle_at_hack string in
  let module Exn = Cerb_frontend.Exception in
  match C_parser_driver.parse_loc_string parser_start (loc, string) with
  | Exn.Result spec -> return spec
  | Exn.Exception (loc, Cerb_frontend.Errors.CPARSER err) ->
    Pp.debug 6 (lazy (debug_tokens loc string));
    Monad.(fail { loc; msg = Parser err })
  | Exn.Exception _ -> assert false


let cn_statements annots =
  annots |> A.get_cerb_magic_attr |> ListM.concat_mapM (parse C_parser.cn_statements)


(* Combine magic-comment content str1 and str2 into a single string,
   with whitespace-padding for the gap from loc1 to loc2. *)
let join_snippets (loc1, str1) (loc2, str2) : Locations.t * string =
  let module L = Cerb_location in
  let module P = Cerb_position in
  let adjust_end_column = -2 in
  (* TODO: fix when column information is correct *)
  let _size_of_magic_start = 3 in
  let start_and_end = function
    | L.Loc_point p -> (p, p)
    | L.Loc_region (s, e, _) -> (s, e)
    | L.Loc_unknown -> assert false
    | L.Loc_other _ -> assert false
    | L.Loc_regions _ -> assert false
  in
  let s1, e1 = start_and_end loc1 in
  let s2, e2 = start_and_end loc2 in
  let loc = L.region (s1, e2) NoCursor in
  let str =
    if P.line e1 = P.line s2 then (
      let () = assert (P.column e1 + adjust_end_column < P.column s2) in
      str1 ^ String.make (P.column s2 - (P.column e1 + adjust_end_column) - 1) ' ' ^ str2)
    else (
      let () = assert (P.line e1 < P.line s2) in
      str1
      ^ String.make (P.line s2 - P.line e1) '\n'
      ^ String.make (P.column s2 - 1) ' '
      ^ str2)
  in
  (loc, str)


let join_snippets_list snippets =
  List.fold_left
    (fun acc snip' ->
       match acc with None -> Some snip' | Some snip -> Some (join_snippets snip snip'))
    None
    snippets


let not_too_many_snippets = function
  | (loc1, _) :: (loc2, _) :: _ when not !allow_split_magic_comments ->
    let msg = TypeErrors.Spec_split_across_multiple_magic_comments { loc1; loc2 } in
    Monad.fail { loc = loc2; msg }
  | _ -> return ()


let cn_ghost_args annots =
  annots |> A.get_cerb_magic_attr |> ListM.concat_mapM (parse C_parser.cn_ghost_args)


let function_spec (A.Attrs attributes) =
  let magic_attrs = A.get_cerb_magic_attr [ A.Aattrs (Attrs (List.rev attributes)) ] in
  let@ () = not_too_many_snippets magic_attrs in
  match join_snippets_list magic_attrs with
  | None -> return None
  | Some spec ->
    let@ spec = parse C_parser.fundef_spec spec in
    return (Some spec)


let loop_spec attrs =
  let magic_attrs = A.get_cerb_magic_attr [ A.Aattrs attrs ] in
  let@ () = not_too_many_snippets magic_attrs in
  match join_snippets_list magic_attrs with
  | None -> return []
  | Some spec ->
    let@ (Cn.CN_inv (_loc, conds)) = parse C_parser.loop_spec spec in
    return conds
