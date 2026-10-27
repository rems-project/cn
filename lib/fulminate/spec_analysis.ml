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


let rec accept_uninterp_lat uninterp_syms = function 
  | LAT.Define ((name, it), info, lat) -> 

    failwith "TODO"
  | Resource ((name, (ret, bt)), (loc, _str_opt), lat) -> failwith "TODO"
  | Constraint (lc, (loc, _str_opt), lat) -> failwith "TODO"
  | I (post, (stats, loops)) -> failwith "TODO"



let rec accept_uninterp_toplevel_at uninterp_syms = function 
  | AT.Computational (sym_and_bt, info, at) -> AT.Computational (sym_and_bt, info, accept_uninterp_toplevel_at uninterp_syms at)
  | Ghost (sym_and_bt, info, at) -> Ghost (sym_and_bt, info, accept_uninterp_toplevel_at uninterp_syms at)
  | L lat -> L (accept_uninterp_lat uninterp_syms lat)


let accept_uninterp_toplevel cn_fns =
  let uninterp_syms = Sym.Set.empty in 
  function
  | Some internal -> Some (accept_uninterp_toplevel_at uninterp_syms internal)
  | None -> None
