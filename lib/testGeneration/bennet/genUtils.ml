module CF = Cerb_frontend
module BT = BaseTypes
module IT = IndexTerms

let ocaml_int_bt = BT.Bits (Signed, Sys.int_size + 1)

let names = ref []

let get_mangled_name (fsym : Sym.t) : Sym.t =
  match List.assoc_opt Sym.equal fsym !names with
  | Some sym -> sym
  | None ->
    let res_sym = Sym.fresh ("cn_gen_" ^ Sym.pp_string fsym) in
    names := (fsym, res_sym) :: !names;
    res_sym
