open Mucore

type statement = Locations.t * Cnstatement.statement Cnprog.t list

let statement_subst subst ((loc, cnprogs) : statement) : statement =
  (loc, List.map ((Cnprog.subst Cnstatement.subst) subst) cnprogs)


type statements = statement list

let statements_subst subst = List.map (statement_subst subst)

type loop = bool * Locations.t * Locations.t * statements ArgumentTypes.t

let loop_subst subst ((contains_user_spec, cond_loc, loop_loc, at) : loop) =
  (contains_user_spec, cond_loc, loop_loc, ArgumentTypes.subst statements_subst subst at)


type loops = loop list

let loops_subst subst = List.map (loop_subst subst)

type fn_body = statements * loops

let fn_body_subst subst ((statements, loops) : fn_body) =
  (statements_subst subst statements, loops_subst subst loops)


type fn_rt_and_body = ReturnTypes.t * fn_body

let fn_rt_and_body_subst subst ((rt, fn_body) : fn_rt_and_body) =
  (ReturnTypes.subst subst rt, fn_body_subst subst fn_body)


type fn_args_and_body = fn_rt_and_body ArgumentTypes.t

let fn_args_and_body_subst subst (at : fn_args_and_body) : fn_args_and_body =
  ArgumentTypes.subst fn_rt_and_body_subst subst at


type fn_largs_and_body = fn_rt_and_body LogicalArgumentTypes.t

let fn_largs_and_body_subst subst (lat : fn_largs_and_body) : fn_largs_and_body =
  LogicalArgumentTypes.subst fn_rt_and_body_subst subst lat


type instrumentation =
  { fn : Sym.t;
    fn_loc : Locations.t;
    internal : fn_args_and_body option;
    trusted : bool;
    is_static : bool
  }

(* replace `s_replace` of basetype `bt` with `s_with` *)
let sym_subst (s_replace, bt, s_with) =
  let module IT = IndexTerms in
  IT.make_subst [ (s_replace, IT.sym_ (s_with, bt, Cerb_location.unknown)) ]


let rec stmts_in_expr (Mucore.Expr (loc, _, _, e_)) =
  match e_ with
  | Epure _ -> []
  | Ememop _ -> []
  | Eaction _ -> []
  | Eskip -> []
  | Eccall _ -> []
  | Elet (_, _, e) -> stmts_in_expr e
  | Eunseq es -> List.concat_map stmts_in_expr es
  | Ewseq (_, e1, e2) -> stmts_in_expr e1 @ stmts_in_expr e2
  | Esseq (_, e1, e2) -> stmts_in_expr e1 @ stmts_in_expr e2
  | Eif (_, e1, e2) -> stmts_in_expr e1 @ stmts_in_expr e2
  | Ebound e -> stmts_in_expr e
  | End es -> List.concat_map stmts_in_expr es
  | Erun _ -> []
  | CN_progs (_stmts_s, stmts_i) -> [ (loc, stmts_i) ]


let from_loop ((_label_sym : Sym.t), (label_def : _ label_def)) : loop option =
  match label_def with
  | Return _ -> None
  | Label
      ( _loc,
        label_args_and_body,
        _annots,
        _,
        `Loop (loop_condition_loc, loop_loc, contains_user_spec) ) ->
    let label_args_and_body = Core_to_mucore.at_of_arguments Fun.id label_args_and_body in
    let label_args_and_statements = ArgumentTypes.map stmts_in_expr label_args_and_body in
    Some (contains_user_spec, loop_condition_loc, loop_loc, label_args_and_statements)


let from_fn cabs_tunit (fn, decl) =
  match decl with
  | ProcDecl (fn_loc, _fn) ->
    { fn; fn_loc; internal = None; trusted = false; is_static = false }
  | Proc { loc = fn_loc; args_and_body; trusted } ->
    let args_and_body = Core_to_mucore.at_of_arguments Fun.id args_and_body in
    let internal =
      ArgumentTypes.map
        (fun (body, labels, rt) ->
           let stmts = stmts_in_expr body in
           let loops = List.filter_map from_loop (Pmap.bindings_list labels) in
           (rt, (stmts, loops)))
        args_and_body
    in
    let trusted_flag = match trusted with Mucore.Trusted _ -> true | _ -> false in
    let is_static =
      let (Cerb_frontend.Cabs.TUnit decls) = cabs_tunit in
      List.exists
        (fun decl ->
           match decl with
           | Cerb_frontend.Cabs.EDecl_func
               (FunDef
                  ( _,
                    _,
                    { storage_classes; _ },
                    Declarator
                      (_, DDecl_function (DDecl_identifier (_, Identifier (_, fn')), _)),
                    _ ))
             when String.equal (Sym.pp_string fn) fn'
                  && List.exists
                       (fun scs ->
                          match scs with
                          | Cerb_frontend.Cabs.SC_static -> true
                          | _ -> false)
                       storage_classes ->
             true
           | _ -> false)
        decls
    in
    { fn; fn_loc; internal = Some internal; trusted = trusted_flag; is_static }


let collect_instrumentation cabs_tunit (file : _ Mucore.file) =
  let instrs = List.map (from_fn cabs_tunit) (Pmap.bindings_list file.funs) in
  (instrs, Compile.exec_spec_hack_syms)
