module CF = Cerb_frontend
open CF.Cn
open Utils
module ESE = Extract
module A = CF.AilSyntax
module C = CF.Ctype
module BT = BaseTypes
module MT = MakeTerm
module T = Terms
module TN = Terms.Normal
module LRT = LogicalReturnTypes
module RT = ReturnTypes
module LAT = LogicalArgumentTypes
module AT = ArgumentTypes
module OE = Ownership
open Terms

(* Based on `Terms.preds_of` *)
(* let uninterp_funs_of t  =
  let uninterp_fun = function IT (Apply (id, _), _, _) -> 
  let add_fun s = function IT (Apply (id, _), _, _) -> Sym.Set.add id s | _ -> s in
  fold_subterms (fun _ -> add_fun) Sym.Set.empty t

  
let rec it_contains_uninterp_application = function
 *)

let rec accept_uninterp_lat uninterp_val_syms = function
  | LAT.Define ((name, it), info, lat) -> failwith "TODO"
  | Resource ((name, (ret, bt)), (loc, _str_opt), lat) -> failwith "TODO"
  | Constraint (lc, (loc, _str_opt), lat) -> failwith "TODO"
  | I (post, (stats, loops)) -> failwith "TODO"


let rec accept_uninterp_at uninterp_val_syms (uninterp_fn_syms, uninterp_pred_syms)
  = function
  | AT.Computational (sym_and_bt, info, at) ->
    AT.Computational
      ( sym_and_bt,
        info,
        accept_uninterp_at uninterp_val_syms (uninterp_fn_syms, uninterp_pred_syms) at )
  | Ghost (sym_and_bt, info, at) ->
    Ghost
      ( sym_and_bt,
        info,
        accept_uninterp_at uninterp_val_syms (uninterp_fn_syms, uninterp_pred_syms) at )
  | L lat -> L (accept_uninterp_lat uninterp_val_syms lat)


let accept_uninterp_toplevel (uninterp_fn_syms, uninterp_pred_syms) =
  let uninterp_val_syms = Sym.Set.empty in
  function
  | Some internal ->
    Some
      (accept_uninterp_at
         uninterp_val_syms
         (uninterp_fn_syms, uninterp_pred_syms)
         internal)
  | None -> None
