open Pp
open List
module BT = BaseTypes
module Res = Resource
module LC = LogicalConstraints

type l_info = Locations.t * Pp.document Lazy.t

let pp_l_info doc (l : l_info) =
  typ doc (Lazy.force (snd l) ^^ break 1 ^^ Locations.pp (fst l))


type basetype_or_value =
  | BaseType of BT.t
  | Value of IndexTerms.t

let bt_of = function BaseType bt -> bt | Value v -> IndexTerms.get_bt v

let has_value = function BaseType _ -> false | Value _ -> true

type t =
  { computational : (basetype_or_value * l_info) Sym.Map.t;
    logical : (basetype_or_value * l_info) Sym.Map.t;
    resources : Res.t list;
    constraints : LC.Set.t;
    global : Global.t;
    where : Where.t
  }

let empty =
  let logical =
    let loc_str = __FILE__ ^ ":" ^ string_of_int __LINE__ in
    let l_info = (Locations.other loc_str, lazy (Pp.string loc_str)) in
    Sym.Map.(empty |> add Alloc.History.sym (BaseType Alloc.History.bt, l_info))
  in
  { computational = Sym.Map.empty;
    logical;
    resources = [];
    constraints = LC.Set.empty;
    global = Global.empty;
    where = Where.empty
  }


let get_rs (ctxt : t) = ctxt.resources

let pp_basetype_or_value = function
  | BaseType bt -> BaseTypes.pp bt
  | Value it -> IndexTerms.pp it


let pp_variable_bindings bindings =
  Pp.list
    (fun (sym, (binding, _)) -> typ (Sym.pp sym) (pp_basetype_or_value binding))
    (Sym.Map.bindings bindings)


let pp_constraints constraints =
  Pp.list
    (fun lc ->
       if !print_level >= 11 || Option.is_none (LC.is_sym_lhs_equality lc) then
         LC.pp lc
       else
         parens !^"...")
    (LC.Set.elements constraints)


let pp (ctxt : t) =
  item "computational" (pp_variable_bindings ctxt.computational)
  ^/^ item "logical" (pp_variable_bindings ctxt.logical)
  ^/^ item "resources" (Pp.list Res.pp (get_rs ctxt))
  ^/^ item "constraints" (pp_constraints ctxt.constraints)


let bound_a s ctxt = Sym.Map.exists (fun s' _ -> Sym.equal s s') ctxt.computational

let bound_l s ctxt = Sym.Map.exists (fun s' _ -> Sym.equal s s') ctxt.logical

let bound s ctxt = bound_a s ctxt || bound_l s ctxt

let get_a s ctxt =
  match Sym.Map.find_opt s ctxt.computational with
  | Some (bt_v, _) -> bt_v
  | None -> failwith ("Context.get_a: not found: " ^ Pp.plain (Sym.pp_debug s))


let get_l s ctxt =
  match Sym.Map.find_opt s ctxt.logical with
  | Some (bt_v, _) -> bt_v
  | None -> failwith ("Context.get_l: not found: " ^ Pp.plain (Sym.pp_debug s))


let add_a_binding s binding info ctxt =
  if bound s ctxt then failwith ("already bound: " ^ Sym.pp_string s);
  { ctxt with computational = Sym.Map.add s (binding, info) ctxt.computational }


let add_a s bt info ctxt = add_a_binding s (BaseType bt) info ctxt

let add_a_value s value info ctxt = add_a_binding s (Value value) info ctxt

let add_l_binding s binding info ctxt =
  if bound s ctxt then failwith ("already bound: " ^ Sym.pp_string s);
  { ctxt with logical = Sym.Map.add s (binding, info) ctxt.logical }


let add_l s bt info ctxt = add_l_binding s (BaseType bt) info ctxt

let add_l_value s value info ctxt = add_l_binding s (Value value) info ctxt

(* Move s from computational to logical world so we can keep the constraints that may be
   attached to s: s will still be bound "logically", but out of scope as far as the Core
   program goes. *)
let remove_a s ctxt =
  let binding, info = Sym.Map.find s ctxt.computational in
  add_l_binding
    s
    binding
    info
    { ctxt with computational = Sym.Map.remove s ctxt.computational }


let add_c c (ctxt : t) =
  let s = ctxt.constraints in
  if LC.Set.mem c s then
    ctxt
  else
    { ctxt with constraints = LC.Set.add c s }


let modify_where (f : Where.t -> Where.t) ctxt = { ctxt with where = f ctxt.where }

(* let add_label_to_trace label ctxt = *)
(*   { ctxt with trace = { label; trace = [] } :: ctxt.trace } *)

(* let modify_current_label_trace f ctxt =  *)
(*   let label, labels = match ctxt.trace with *)
(*     | hd::tl -> hd, tl *)
(*     | [] -> assert false *)
(*   in *)
(*   { ctxt with trace = f label :: labels }  *)

(* let add_trace_item_to_trace i ctxt = *)
(*   modify_current_label_trace (fun label -> *)
(*       { label with trace = i :: label.trace} *)
(*     ) ctxt *)

let add_r _loc r (ctxt : t) = { ctxt with resources = r :: ctxt.resources }

(* picks out universally quantified constraints, recursive functions,
   and resource predicates that will not be given to the solver *)
let not_given_to_solver ctxt =
  let global = ctxt.global in
  let constraints =
    filter LogicalConstraints.is_forall (LC.Set.elements ctxt.constraints)
  in
  let funs =
    Sym.Map.bindings
      (Sym.Map.filter
         (fun _ v -> not (Definition.Function.given_to_solver v))
         global.logical_functions)
  in
  let preds =
    Sym.Map.bindings
      (Sym.Map.filter
         (fun _ v -> not (Definition.Predicate.given_to_solver v))
         global.resource_predicates)
  in
  (constraints, funs, preds)
