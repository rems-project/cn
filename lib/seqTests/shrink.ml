module CF = Cerb_frontend
module A = CF.AilSyntax
module C = CF.Ctype
module AT = ArgumentTypes
module LAT = LogicalArgumentTypes
module Config = SeqTestGenConfig
module SymSet = Set.Make (Sym)
module T = Types
module SUtils = Utils

let gen_lst_shrinks (ty : C.ctype) (arg : string) : string list =
  match ty with
  | Ctype (_, ty1) ->
    (match ty1 with
     | Basic (Integer (Unsigned _)) ->
       let gend_int = int_of_string arg in
       List.map string_of_int [ 0; 1; gend_int / 4; gend_int / 2 ]
     | Basic (Integer (Signed _)) ->
       let gend_int = int_of_string arg in
       List.map
         string_of_int
         [ 0; 1; -1; gend_int / 4; -gend_int / 4; gend_int / 2; -gend_int / 2 ]
     | Basic (Floating _) ->
       let gend_float = float_of_string arg in
       List.map
         string_of_float
         [ 0.0;
           1.0;
           -1.0;
           gend_float /. 4.0;
           gend_float /. -4.0;
           gend_float /. 2.0;
           gend_float /. -2.0
         ]
     | _ -> [])


let shrink
      (seq : T.context)
      (output_dir : string)
      (filename : string)
      (fun_decls : Pp.document)
  : int * Pp.document
  =
  let open Pp in
  (* print_endline "entering shrink"; *)
  match seq with
  | [] -> (0, empty)
  | start :: seq as seq' ->
    let rec dfs (graph : T.context) (visited : T.context) ((_, _, _, _, args) as node)
      : T.context
      =
      if not (List.mem SUtils.name_eq node visited) then (
        let names = List.map (fun (_, name) -> name) args in
        let succs =
          List.filter
            (fun (name, _, _, _, _) ->
               match name with
               | None -> false
               | Some name ->
                 List.mem String.equal (Sym.pp_string name) names
                 || List.mem String.equal ("&" ^ Sym.pp_string name) names)
            graph
        in
        List.fold_left (dfs graph) (node :: visited) succs)
      else
        visited
    in
    let rev_dep_graph : T.context =
      List.map
        (fun (name, is_static, ret_ty, f, _) ->
           ( name,
             is_static,
             ret_ty,
             f,
             match name with
             | None -> []
             | Some name ->
               List.filter_map
                 (fun (name, _, ret_ty, _, _) ->
                    match name with
                    | None -> None
                    | Some name -> Some (ret_ty, Sym.pp_string name))
                 (List.filter
                    (fun (_, _, _, _, args) ->
                       List.mem
                         (fun name (_, var) ->
                            String.equal (Sym.pp_string name) var
                            || String.equal ("&" ^ Sym.pp_string name) var)
                         name
                         args)
                    seq') ))
        seq'
    in
    let reqs = dfs seq [] start in
    let rec shrink_1 (tests : T.context) : T.context =
      let generate_lists (lst : T.context) : T.context list =
        let rec aux (left : T.context) (right : T.context) (acc : T.context list)
          : T.context list
          =
          match right with
          | [] -> acc
          | ((name, _, _, _, _) as test) :: rest ->
            if List.mem SUtils.name_eq test reqs then
              aux (test :: left) rest (List.rev_append left right :: acc)
            else (
              let deps =
                match name with
                | None -> []
                | Some name1 ->
                  dfs
                    rev_dep_graph
                    []
                    (List.find
                       (fun (name2, _, _, _, _) ->
                          match name2 with
                          | None -> false
                          | Some name2 ->
                            String.equal (Sym.pp_string name1) (Sym.pp_string name2))
                       rev_dep_graph)
              in
              let removed =
                List.rev_append
                  left
                  (List.filter
                     (fun ((_, _, _, _, args) as test) ->
                        (not (List.mem SUtils.name_eq test deps))
                        && List.for_all
                             (fun (_, arg) ->
                                not
                                  (List.mem
                                     (fun name1 (name2, _, _, _, _) ->
                                        match name2 with
                                        | None -> false
                                        | Some name2 ->
                                          (String.equal name1) (Sym.pp_string name2)
                                          || String.equal name1 ("&" ^ Sym.pp_string name2))
                                     arg
                                     deps))
                             args)
                     rest)
              in
              aux (test :: left) rest (removed :: acc))
        in
        aux [] lst []
      in
      let shrunken_sequences =
        List.sort
          (fun l1 l2 -> Int.compare (List.length l1) (List.length l2))
          (generate_lists tests)
      in
      let filename_base = filename |> Filename.basename |> Filename.chop_extension in
      let script_doc' = BuildScript.generate_intermediate ~output_dir ~filename_base in
      SUtils.save ~perm:0o777 output_dir "run_tests_intermediate.sh" script_doc';
      let elt = Sym.fresh_anon () in
      let results =
        SUtils.analyze_results
          (List.init
             (List.length shrunken_sequences)
             (Fun.const
                (Some (-1, (None, false, C.Ctype ([], Basic (Integer Char)), elt, [])))))
          (List.map
             (fun tests -> SUtils.ctx_to_tests filename tests ^^ hardline)
             shrunken_sequences)
          filename
          output_dir
          fun_decls
      in
      match
        List.find_index
          (fun result ->
             match result with Some (`PostConditionViolation _) -> true | _ -> false)
          results
      with
      | Some i ->
        let shrunken_sequence = List.nth shrunken_sequences i in
        if List.length shrunken_sequence = List.length tests then
          shrunken_sequence
        else
          shrink_1 (List.nth shrunken_sequences i)
      | None ->
        (* tests *)
        (match
           List.find_index
             (fun result -> match result with Some `OtherFailure -> true | _ -> false)
             results
         with
         | Some i ->
           Pp.print stdout (SUtils.ctx_to_tests filename (List.nth shrunken_sequences i));
           print_string (SUtils.ctx_to_string rev_dep_graph);
           failwith "Unexpected failure during shrinking"
         | None -> tests)
    in
    let rec shrink_2 ((prev, next) : T.context * T.context) : T.context =
      let shrink_arg ((ty, arg_name) : C.ctype * string) : string list =
        let gend_val =
          if String.starts_with ~prefix:"x" arg_name then (
            match SUtils.gen_val ty with [] -> None | x :: _ -> Some x)
          else
            None
        in
        match gend_val with
        | Some v ->
          gen_lst_shrinks ty v @ [ v ] (* generate arg for variable, then shrink *)
        | None -> gen_lst_shrinks ty arg_name
        (* if arg is not a variable then just shrink *)
      in
      match next with
      | [] -> List.rev prev
      | (name, is_static, ret_ty, f, args) :: t ->
        let rec shrink_args
                  ((prev_args, next_args) :
                    (C.ctype * string) list * (C.ctype * string) list)
          =
          let try_shrinks (arg_shrinks : string list) (orig_arg : string) : string =
            match arg_shrinks with
            | [] -> orig_arg
            | _ ->
              let try_shrink (new_arg : string) =
                match next_args with
                | [] -> List.rev prev_args
                | (ty, _) :: next_args ->
                  List.rev_append prev_args ((ty, new_arg) :: next_args)
              in
              let new_tests =
                List.map
                  (fun arg -> (-1, (name, is_static, ret_ty, f, try_shrink arg)))
                  arg_shrinks
              in
              let new_sequences =
                List.map
                  (fun (_, (name, is_static, ret_ty, f, args)) ->
                     SUtils.ctx_to_tests
                       filename
                       (List.rev_append prev ((name, is_static, ret_ty, f, args) :: t)))
                  new_tests
              in
              let results =
                SUtils.analyze_results
                  (List.map (fun test -> Some test) new_tests)
                  new_sequences
                  filename
                  output_dir
                  fun_decls
              in
              (match
                 List.find_index
                   (fun result ->
                      match result with
                      | Some (`PostConditionViolation _) -> true
                      | _ -> false)
                   results
               with
               | Some i -> List.nth arg_shrinks i
               | None -> orig_arg)
          in
          match next_args with
          | [] -> (name, is_static, ret_ty, f, List.rev prev_args)
          | (ty, arg) :: next_args ->
            shrink_args
              ((ty, try_shrinks (shrink_arg (ty, arg)) arg) :: prev_args, next_args)
        in
        let shrunken_call = shrink_args ([], args) in
        shrink_2 (shrunken_call :: prev, t)
    in
    let shrunken = shrink_1 (List.rev seq') in
    let shrunken' = List.rev (shrink_2 ([], shrunken)) in
    let shrunken'' = shrink_1 (List.rev shrunken') in
    (List.length shrunken'', SUtils.ctx_to_tests filename shrunken'')
