module CF = Cerb_frontend
module A = CF.AilSyntax
module C = CF.Ctype
module AT = ArgumentTypes
module LAT = LogicalArgumentTypes
module Utils = Fulminate.Utils
module Config = SeqTestGenConfig
module SymSet = Set.Make (Sym)
module FExtract = Fulminate.Extract

type call = SymSet.elt option * C.ctype * SymSet.elt * (C.ctype * string) list

type context = call list

type test_stats =
  { successes : int;
    failures : int;
    skipped : int;
    discarded : int;
    distrib : (string * int) list
  }

type pass_or_violation =
  | Postcond of call
  | Precond
  | Pass of call * int

let no_qs : C.qualifiers = { const = false; restrict = false; volatile = false }

let save ?(perm = 0o666) (output_dir : string) (filename : string) (doc : Pp.document)
  : unit
  =
  let oc =
    Stdlib.open_out_gen
      [ Open_wronly; Open_creat; Open_trunc; Open_text ]
      perm
      (Filename.concat output_dir filename)
  in
  output_string oc (Pp.plain ~width:80 doc);
  close_out oc


let rec pick
          (distribution :
            (int
            * (SymSet.elt * ((C.qualifiers * C.ctype) * (SymSet.elt * C.ctype) list)))
              list)
          (i : int)
  : SymSet.elt * ((C.qualifiers * C.ctype) * (SymSet.elt * C.ctype) list)
  =
  match distribution with
  | [] -> failwith "impossible case"
  | (score, f) :: fs -> if i <= score then f else pick fs (i - score)


let rec ty_eq (ty1 : C.ctype) (ty2 : C.ctype) : bool =
  match (ty1, ty2) with
  | Ctype (_, Pointer (_, ty1)), Ctype (_, Pointer (_, ty2)) -> ty_eq ty1 ty2
  | _, _ -> C.ctypeEqual ty1 ty2


let callable
      (ctx : context)
      ((_, (_, args)) :
        SymSet.elt * ((C.qualifiers * C.ctype) * (SymSet.elt * C.ctype) list))
  : bool
  =
  List.for_all
    (fun (_, (ty : C.ctype)) ->
       (match ty with
        | Ctype (_, ty) ->
          (match ty with
           | Basic (Integer Char)
           | Basic (Integer Bool)
           | Basic (Integer (Signed _))
           | Basic (Integer (Unsigned _))
           | Basic (Floating _)
           | Void ->
             true
           | Pointer (_, ty) -> List.exists (fun (_, ct, _, _) -> ty_eq ty ct) ctx
           | _ -> false))
       || List.exists (fun (_, ct, _, _) -> ty_eq ty ct) ctx)
    args


(* needs way more complexity here *)
let calc_score (ctx : context) (args : (SymSet.elt * C.ctype) list) : int =
  List.fold_left
    (fun acc (_, ty) ->
       if List.exists (fun (_, ct, _, _) -> ty_eq ty ct) ctx then
         acc + 25
       else
         acc)
    1
    args


let ctx_to_string (ctx : context) : string =
  List.fold_left
    (fun acc (name, ty, f, args) ->
       match name with
       | Some name ->
         acc
         ^ "("
         ^ Sym.pp_string name
         ^ ":"
         ^ CF.String_core_ctype.string_of_ctype ty
         ^ ":"
         ^ Sym.pp_string f
         ^ ":"
         ^ List.fold_left (fun acc (_, arg) -> arg ^ "," ^ acc) "" args
         ^ ")"
         ^ "\n"
       | None ->
         acc
         ^ "(void:"
         ^ Sym.pp_string f
         ^ ":"
         ^ List.fold_left (fun acc (_, arg) -> arg ^ "," ^ acc) "" args
         ^ ")"
         ^ "\n")
    "\n"
    ctx


let gen_arg (ctx : context) ((name, ty) : SymSet.elt * C.ctype) : string =
  let generated_base_ty =
    match ty with
    | Ctype (_, ty1) ->
      (match ty1 with
       | Basic (Integer Char) ->
         [ "\'" ^ String.make 1 (char_of_int (Random.int 96 + 32)) ^ "\'" ]
       | Basic (Integer Bool) -> [ (if Random.int 2 = 1 then "true" else "false") ]
       | Basic (Integer (Signed _)) ->
         let rand_int = Random.int 32767 in
         [ string_of_int (if Random.int 2 = 1 then rand_int * -1 else rand_int) ]
       | Basic (Integer (Unsigned _)) -> [ string_of_int (Random.int 65536) ]
       | Basic (Floating _) -> [ string_of_float (Random.float 65536.0) ]
       | _ -> [])
  in
  let prev_call =
    let prev_calls =
      List.filter
        (fun (name, ct, _, _) ->
           Option.is_some name
           && (ty_eq ty ct
               || match ty with Ctype (_, Pointer (_, ty)) -> ty_eq ty ct | _ -> false))
        ctx
    in
    match List.length prev_calls with
    | 0 -> []
    | n ->
      (match List.nth prev_calls (Random.int n) with
       | Some name, ty', _, _ ->
         if not (ty_eq ty' ty) then (* only way they're not directly equal is if pointer*)
           [ "&" ^ Sym.pp_string name ]
         else
           [ Sym.pp_string name ]
       | None, _, _, _ -> failwith "impossible case")
  in
  let options = List.append generated_base_ty prev_call in
  match List.length options with
  | 0 ->
    failwith
      ("unable to generate arg or reuse context for "
       ^ Sym.pp_string name
       ^ " in context "
       ^ ctx_to_string ctx)
  | n -> List.nth options (Random.int n)


let stmt_to_doc (stmt : CF.GenTypes.genTypeCategory A.statement_) : Pp.document =
  CF.Pp_ail.(with_executable_spec (pp_statement ~bs:[]) (Utils.mk_stmt stmt))


let create_test_file
      (sequence : Pp.document)
      (filename_base : string)
      (fun_decls : Pp.document)
  : Pp.document
  =
  let open Pp in
  (if Config.with_static_hack () then
     string "#include "
     ^^ dquotes (string (filename_base ^ ".exec.c"))
     ^^ hardline
     ^^ string "#include "
     ^^ dquotes (string "cn.c")
   else
     string "#include "
     ^^ dquotes (string (filename_base ^ ".cn.h"))
     ^^ twice hardline
     ^^ fun_decls)
  ^^ twice hardline
  ^^ string "int main"
  ^^ parens (string "int argc, char* argv[]")
  ^^ break 1
  ^^ braces
       (nest
          2
          (hardline
           ^^
           let init_ghost = Fulminate.Ownership.get_ownership_global_init_stats () in
           separate_map hardline stmt_to_doc init_ghost ^^ hardline ^^ sequence)
        ^^ hardline)


let test_to_doc
      (name : SymSet.elt option)
      (ret_ty : C.ctype)
      (f : SymSet.elt)
      (args : (C.ctype * string) list)
  : Pp.document
  =
  let open Pp in
  let f_call =
    Sym.pp f
    ^^ parens
         (separate
            (comma ^^ space)
            [ separate (comma ^^ space) (List.map (fun (_, arg) -> string arg) args) ])
    ^^ semi
    ^^ hardline
  in
  match name with
  | Some name ->
    separate space [ CF.Pp_ail.pp_ctype no_qs ret_ty; Sym.pp name; equals ]
    ^^ f_call
    ^^ stmt_to_doc
         (A.AilSexpr
            (Fulminate.Ownership.generate_c_local_ownership_entry_fcall (name, ret_ty)))
    ^^ hardline
  | None -> f_call


let create_intermediate_test_file
      (sequences : Pp.document list)
      (tests_to_try : Pp.document list)
      (filename_base : string)
      (fun_decls : Pp.document)
  : Pp.document
  =
  let open Pp in
  let create_try (i : int) (test : document) (sequence : document) =
    string "int try"
    ^^ int i
    ^^ parens empty
    ^^ break 1
    ^^ braces
         (nest
            2
            (hardline
             ^^
             let init_ghost = Fulminate.Ownership.get_ownership_global_init_stats () in
             separate_map hardline stmt_to_doc init_ghost
             ^^ hardline
             ^^ sequence
             ^^ hardline
             ^^ test
             ^^ hardline
             ^^ string "return 0;")
          ^^ hardline)
  in
  let create_case (i : int) (_ : Pp.document) =
    string "case"
    ^^ space
    ^^ int i
    ^^ colon
    ^^ nest
         2
         (hardline
          ^^ string "try"
          ^^ int i
          ^^ parens empty
          ^^ semi
          ^^ hardline
          ^^ string "break"
          ^^ semi)
  in
  (if Config.with_static_hack () then
     string "#include "
     ^^ dquotes (string (filename_base ^ "-exec.c"))
     ^^ hardline
     ^^ string "#include "
     ^^ dquotes (string "cn.c")
   else
     string "#include "
     ^^ dquotes (string (filename_base ^ ".cn.h"))
     ^^ twice hardline
     ^^ fun_decls)
  ^^ twice hardline
  ^^ separate
       (twice hardline)
       (List.mapi
          (fun i (sequence, call) -> create_try i call sequence)
          (List.combine sequences tests_to_try))
  ^^ twice hardline
  ^^ string "int main"
  ^^ parens (string "int argc, char* argv[]")
  ^^ break 1
  ^^ braces
       (nest
          2
          (hardline
           ^^ (string "switch(atoi(argv[1]))"
               ^^ break 1
               ^^ braces
                    (nest
                       2
                       (hardline
                        ^^ separate hardline (List.mapi create_case tests_to_try)
                        ^^ hardline)))
           ^^ hardline
           ^^ string "return 0;"
           ^^ hardline))


let ctx_to_tests =
  let open Pp in
  separate_map empty (fun (name, ret_ty, f, args) -> test_to_doc name ret_ty f args)


let analyze_results
      (prev_and_tests :
        (int * SymSet.elt option * C.ctype * SymSet.elt * (C.ctype * string) list) option
          list)
      (sequences : PPrint.document list)
      (filename_base : string)
      (output_dir : string)
      (fun_decls : PPrint.document)
  : pass_or_violation option list
  =
  let tests_to_try =
    List.map
      (fun call ->
         match call with
         | None | Some (-1, _, _, _, _) ->
           Pp.string "/* unable to generate call at this point */"
         | Some (_, name, ret_ty, f, args) -> test_to_doc name ret_ty f args)
      prev_and_tests
  in
  let intermediate_file =
    create_intermediate_test_file sequences tests_to_try filename_base fun_decls
  in
  save output_dir (filename_base ^ ".test.c") intermediate_file;
  match Unix.system (output_dir ^ "/run_tests_intermediate.sh > /dev/null") with
  | WEXITED 0 ->
    let num_tests = List.length prev_and_tests in
    let rec run_tests (test_num : int) =
      if test_num < num_tests then (
        match
          Unix.system
            (output_dir ^ "/tests.out " ^ string_of_int test_num ^ " > /dev/null")
        with
        | WEXITED n -> n :: run_tests (test_num + 1)
        | _ -> failwith "Process was stopped or killed by signal :(")
      else
        []
    in
    let tests_and_msgs = List.combine prev_and_tests (run_tests 0) in
    let rec analyze_results_h
              (tests_and_msgs :
                ((int
                 * SymSet.elt option
                 * C.ctype
                 * SymSet.elt
                 * (C.ctype * string) list)
                   option
                * int)
                  list)
      =
      match tests_and_msgs with
      | [] -> []
      | (None, _) :: t -> None :: analyze_results_h t
      | (Some (prev', name, ret_ty, f, args), exit_code) :: t ->
        (match exit_code with
         | 0 -> Some (Pass ((name, ret_ty, f, args), prev')) :: analyze_results_h t
         | 1 -> Some Precond :: analyze_results_h t
         | 2 -> Some (Postcond (name, ret_ty, f, args)) :: analyze_results_h t
         | _ -> failwith "unhandled exit code")
    in
    analyze_results_h tests_and_msgs
  | WEXITED _ -> failwith "compilation failure"
  | _ -> failwith "compilation stopped unexpectedly"


let shrink
      (seq : context)
      (output_dir : string)
      (filename_base : string)
      (fun_decls : Pp.document)
  : int * Pp.document
  =
  (* Printf.printf "--> LOG: [%s]\n%!" "STARTING SHRINKING"; *)
  let open Pp in
  let name_eq (n1, _, _, _) (n2, _, _, _) =
    match (n1, n2) with Some name1, Some name2 -> Sym.equal name1 name2 | _ -> false
  in
  match seq with
  | [] -> (0, empty)
  | start :: seq as seq' ->
    let rec dfs (graph : context) (visited : context) ((_, _, _, args) as node) : context =
      if not (List.mem name_eq node visited) then (
        let names = List.map (fun (_, name) -> name) args in
        let succs =
          List.filter
            (fun (name, _, _, _) ->
               match name with
               | None -> false
               | Some name -> List.mem String.equal (Sym.pp_string name) names)
            graph
        in
        List.fold_left (dfs seq) (node :: visited) succs)
      else
        visited
    in
    let rev_dep_graph =
      List.map
        (fun (name, ret_ty, f, _) ->
           ( name,
             ret_ty,
             f,
             match name with
             | None -> []
             | Some name ->
               List.filter_map
                 (fun (name, ret_ty, _, _) ->
                    match name with
                    | None -> None
                    | Some name -> Some (ret_ty, Sym.pp_string name))
                 (List.filter
                    (fun (_, _, _, args) ->
                       List.mem
                         (fun name (_, var) -> String.equal (Sym.pp_string name) var)
                         name
                         args)
                    seq') ))
        seq'
    in
    let reqs = dfs seq [] start in
    let rec shrink_1 (tests : context) =
      let generate_lists lst =
        (* Printf.printf "--> LOG: [%s]\n%!" (ctx_to_string lst ^ "\n");
        Printf.printf "--> LOG: [%s]\n%!" (ctx_to_string rev_dep_graph ^ "\n");
        Printf.printf "--> LOG: [%s]\n%!" (ctx_to_string reqs ^ "\n"); *)
        let rec aux left right acc =
          match right with
          | [] -> acc
          | ((name, _, _, _) as test) :: rest ->
            if List.mem name_eq test reqs then
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
                       (fun (name2, _, _, _) ->
                          match name2 with
                          | None -> false
                          | Some name2 ->
                            String.equal (Sym.pp_string name1) (Sym.pp_string name2))
                       rev_dep_graph)
              in
              let removed =
                List.rev_append
                  left
                  (List.filter (fun test -> not (List.mem name_eq test deps)) rest)
              in
              (* Printf.printf "--> LOG: [%s]\n%!" ("in AUX");
              Printf.printf "--> LOG: [%s]\n%!" (Sym.pp_string f);
              Printf.printf "--> LOG: [%s]\n%!" (ctx_to_string deps ^ "\n");
              Printf.printf "--> LOG: [%s]\n%!" (ctx_to_string removed ^ "\n"); *)
              aux (test :: left) rest (removed :: acc))
        in
        aux [] lst []
      in
      let shrunken_sequences =
        List.sort
          (fun l1 l2 -> Int.compare (List.length l1) (List.length l2))
          (generate_lists tests)
      in
      let script_doc' = BuildScript.generate_intermediate ~output_dir ~filename_base in
      save ~perm:0o777 output_dir "run_tests_intermediate.sh" script_doc';
      let elt = Sym.fresh_anon () in
      let results =
        analyze_results
          (List.init
             (List.length shrunken_sequences)
             (Fun.const (Some (-1, None, C.Ctype ([], Basic (Integer Char)), elt, []))))
          (List.map
             (fun tests -> ctx_to_tests tests ^^ hardline ^^ string "return 0;")
             shrunken_sequences)
          filename_base
          output_dir
          fun_decls
      in
      match
        List.find_index
          (fun result -> match result with Some (Postcond _) -> true | _ -> false)
          results
      with
      | Some i ->
        let shrunken_sequence = List.nth shrunken_sequences i in
        if List.length shrunken_sequence = List.length tests then
          shrunken_sequence
        else
          shrink_1 (List.nth shrunken_sequences i)
      | None -> tests
    in
    let rec shrink_2 ((prev, next) : context * context) : context =
      let shrink_arg ((ty, arg_name) : C.ctype * string) : string list =
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
        in
        let gend_val =
          if String.starts_with ~prefix:"x" arg_name then (
            match ty with
            | Ctype (_, ty1) ->
              (match ty1 with
               | Basic (Integer Char) ->
                 Some ("\'" ^ String.make 1 (char_of_int (Random.int 96 + 32)) ^ "\'")
               | Basic (Integer Bool) ->
                 Some (if Random.int 2 = 1 then "true" else "false")
               | Basic (Integer (Signed _)) ->
                 let rand_int = Random.int 32767 in
                 Some
                   (string_of_int (if Random.int 2 = 1 then rand_int * -1 else rand_int))
               | Basic (Integer (Unsigned _)) -> Some (string_of_int (Random.int 65536))
               | Basic (Floating _) -> Some (string_of_float (Random.float 65536.0))
               | _ -> None))
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
      | (name, ret_ty, f, args) :: t ->
        let rec shrink_args
                  ((prev_args, next_args) :
                    (C.ctype * string) list * (C.ctype * string) list)
          =
          let try_shrinks (arg_shrinks : string list) (orig_arg : string) =
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
                List.map (fun arg -> (-1, name, ret_ty, f, try_shrink arg)) arg_shrinks
              in
              let new_sequences =
                List.map
                  (fun (_, name, ret_ty, f, args) ->
                     ctx_to_tests (List.rev_append prev ((name, ret_ty, f, args) :: t)))
                  new_tests
              in
              let results =
                analyze_results
                  (List.map (fun test -> Some test) new_tests)
                  new_sequences
                  filename_base
                  output_dir
                  fun_decls
              in
              (match
                 List.find_index
                   (fun result ->
                      match result with Some (Postcond _) -> true | _ -> false)
                   results
               with
               | Some i -> List.nth arg_shrinks i
               | None -> orig_arg)
          in
          match next_args with
          | [] -> (name, ret_ty, f, List.rev prev_args)
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
    (List.length shrunken'', ctx_to_tests shrunken'')


let rec combine_stats (stats_list : test_stats list) (combined_stats : test_stats)
  : test_stats
  =
  match stats_list with
  | [] -> combined_stats
  | stats :: stats_list ->
    let combined_stats =
      { successes = combined_stats.successes + stats.successes;
        failures = combined_stats.failures + stats.failures;
        discarded = combined_stats.discarded + stats.discarded;
        skipped = combined_stats.skipped + stats.skipped;
        distrib =
          List.fold_left
            (fun acc (f, num_calls) ->
               match List.assoc_opt String.equal f acc with
               | None -> (f, num_calls) :: acc
               | Some n -> (f, n + num_calls) :: List.remove_assoc f acc)
            combined_stats.distrib
            stats.distrib
      }
    in
    combine_stats stats_list combined_stats


let rec gen_sequence
          (funcs :
            (SymSet.elt * ((C.qualifiers * C.ctype) * (SymSet.elt * C.ctype) list)) list)
          (fuel : int)
          (test_states : (int * Pp.document * context * test_stats) list)
          (output_dir : string)
          (filename_base : string)
          (fun_decls : Pp.document)
  : (Pp.document * test_stats, Pp.document * test_stats) Either.either
  =
  let open Pp in
  match fuel with
  | 0 ->
    let test_strs =
      List.map
        (fun (_, seq_so_far, ctx, _) ->
           let unmap_stmt =
             List.filter_map
               (fun (name, ret, _, _) ->
                  match name with
                  | Some name ->
                    Some (Fulminate.Ownership.generate_c_local_ownership_exit (name, ret))
                  | None -> None)
               ctx
           in
           let unmap_str = hardline ^^ separate_map hardline stmt_to_doc unmap_stmt in
           seq_so_far
           ^^ unmap_str
           ^^ hardline
           ^^ string "printf(\"S\");"
           ^^ hardline
           ^^ string "return 0;")
        test_states
    in
    Either.Right
      ( create_intermediate_test_file
          test_strs
          (List.init (Config.get_num_tests ()) (fun _ -> empty))
          filename_base
          fun_decls,
        combine_stats
          (List.map (fun (_, _, _, stats) -> stats) test_states)
          { successes = 0; failures = 0; discarded = 0; skipped = 0; distrib = [] } )
  | _ ->
    let callables =
      List.map
        (fun (_, _, ctx, _) ->
           List.map
             (fun ((_, (_, args)) as f) -> (calc_score ctx args, f))
             (List.filter (callable ctx) funcs))
        test_states
    in
    let gen_test ()
      : ( int * Pp.document * context * test_stats,
            int * Pp.document * context * test_stats )
          Either.either
          list
      =
      let gen_test_h (f, ((_, ret_ty), params)) (ctx : context) (prev : int)
        : int * SymSet.elt option * C.ctype * SymSet.elt * (C.ctype * string) list
        =
        let name, prev =
          match ret_ty with
          | C.Ctype (_, Void) ->
            (None, prev) (* attempted to use fresh_cn but did not work for some reason?*)
          | _ -> (Some (Sym.fresh ("x" ^ string_of_int prev)), prev + 1)
        in
        let args = List.map (fun ((_, ty) as param) -> (ty, gen_arg ctx param)) params in
        (prev, name, ret_ty, f, args)
      in
      let prev_and_tests =
        List.map
          (fun (fs, (prev, _, ctx, _)) ->
             if List.length fs = 0 then
               None
             else
               Some
                 (gen_test_h
                    (pick fs (Random.int (List.fold_left ( + ) 1 (List.map fst fs))))
                    ctx
                    prev))
          (List.combine callables test_states)
      in
      let results =
        analyze_results
          prev_and_tests
          (List.map (fun (_, test_str, _, _) -> test_str) test_states)
          filename_base
          output_dir
          fun_decls
      in
      let rec update_tests
                (test_states : (int * Pp.document * context * test_stats) list)
                (results : pass_or_violation option list)
        =
        match (test_states, results) with
        | [], [] -> []
        | (prev, test_so_far, ctx, stats) :: test_states, result :: results ->
          let updated_state =
            match result with
            | None ->
              Either.Right
                (prev, test_so_far, ctx, { stats with skipped = stats.skipped + 1 })
            | Some Precond -> Either.Right (prev, test_so_far, ctx, stats)
            | Some (Pass (((name, ret_ty, f, args) as call), prev')) ->
              let distrib =
                let name = Sym.pp_string f in
                match List.assoc_opt String.equal name stats.distrib with
                | None -> (name, 1) :: stats.distrib
                | Some n -> (name, n + 1) :: List.remove_assoc name stats.distrib
              in
              Either.Right
                ( prev',
                  test_so_far ^^ test_to_doc name ret_ty f args,
                  call :: ctx,
                  { stats with successes = stats.successes + 1; distrib } )
            | Some (Postcond ((_, _, f, _) as call)) ->
              let distrib =
                let name = Sym.pp_string f in
                match List.assoc_opt String.equal name stats.distrib with
                | None -> (name, 1) :: stats.distrib
                | Some n -> (name, n + 1) :: List.remove_assoc name stats.distrib
              in
              Either.Left
                ( prev,
                  empty,
                  call :: ctx,
                  { stats with failures = stats.failures + 1; distrib } )
          in
          updated_state :: update_tests test_states results
        | _, _ -> failwith "impossible"
      in
      update_tests test_states results
    in
    let test_states = gen_test () in
    let test_states' =
      List.map
        (fun result ->
           match result with Either.Right result | Either.Left result -> result)
        test_states
    in
    let postcond_violations =
      List.filter_map
        (fun result -> match result with Either.Left x -> Some x | _ -> None)
        test_states
    in
    if List.length postcond_violations <> 0 then (
      let _, _, ctx, _ =
        List.hd
          (List.sort
             (fun (_, _, ctx1, _) (_, _, ctx2, _) ->
                Int.compare (List.length ctx1) (List.length ctx2))
             postcond_violations)
      in
      let num_left, seq = shrink ctx output_dir filename_base fun_decls in
      let combined_stats =
        combine_stats
          (List.map (fun (_, _, _, stats) -> stats) test_states')
          { successes = 0; failures = 0; discarded = 0; skipped = 0; distrib = [] }
      in
      Either.Left
        ( create_test_file
            (seq ^^ hardline ^^ string "return 123;")
            filename_base
            fun_decls,
          { combined_stats with discarded = combined_stats.successes + 1 - num_left } ))
    else
      gen_sequence funcs (fuel - 1) test_states' output_dir filename_base fun_decls


let compile_sequence
      (sigma : CF.GenTypes.genTypeCategory A.sigma)
      (insts : FExtract.instrumentation list)
      (num_samples : int)
      (output_dir : string)
      (filename_base : string)
      (fun_decls : Pp.document)
  : (Pp.document * test_stats, Pp.document * test_stats) Either.either
  =
  let fuel = num_samples in
  let declarations : A.sigma_declaration list =
    insts
    |> List.map (fun (inst : FExtract.instrumentation) ->
      (inst.fn, List.assoc Sym.equal inst.fn sigma.declarations))
  in
  let args_map
    : (SymSet.elt * ((C.qualifiers * C.ctype) * (SymSet.elt * C.ctype) list)) list
    =
    List.map
      (fun (inst : Fulminate.Extract.instrumentation) ->
         ( inst.fn,
           let _, _, _, xs, _ = List.assoc Sym.equal inst.fn sigma.function_definitions in
           match List.assoc Sym.equal inst.fn declarations with
           | _, _, Decl_function (_, (qual, ret), cts, _, _, _) ->
             ((qual, ret), List.combine xs (List.map (fun (_, ct, _) -> ct) cts))
           | _ ->
             failwith
               (String.concat
                  " "
                  [ "Function declaration not found for";
                    Sym.pp_string inst.fn;
                    "@";
                    __LOC__
                  ]) ))
      insts
  in
  gen_sequence
    args_map
    fuel
    (List.map
       (fun _ ->
          ( 0,
            Pp.empty,
            [],
            { successes = 0; failures = 0; skipped = 0; discarded = 0; distrib = [] } ))
       (List.init (Config.get_num_tests ()) Fun.id))
    output_dir
    filename_base
    fun_decls


let generate
      ~(output_dir : string)
      ~(filename : string)
      (sigma : CF.GenTypes.genTypeCategory A.sigma)
      (insts : FExtract.instrumentation list)
  : int
  =
  if List.is_empty insts then failwith "No testable functions";
  let filename_base = filename |> Filename.basename |> Filename.chop_extension in
  let test_file = filename_base ^ ".test.c" in
  let script_doc' = BuildScript.generate_intermediate ~output_dir ~filename_base in
  save ~perm:0o777 output_dir "run_tests_intermediate.sh" script_doc';
  let fun_to_decl (inst : FExtract.instrumentation) =
    CF.Pp_ail.(
      with_executable_spec
        (fun () ->
           pp_function_prototype
             inst.fn
             (let _, _, decl = List.assoc Sym.equal inst.fn sigma.declarations in
              decl))
        ())
  in
  let open Pp in
  let fun_decls = separate_map hardline fun_to_decl insts in
  let compiled_seq =
    compile_sequence
      sigma
      insts
      (Config.get_num_calls ())
      output_dir
      filename_base
      fun_decls
  in
  let exit_code, seq, output_msg =
    match compiled_seq with
    | Left (seq, stats) ->
      let script_doc = BuildScript.generate ~output_dir ~filename_base 1 in
      save ~perm:0o777 output_dir "run_tests.sh" script_doc;
      ( 123,
        seq,
        Printf.sprintf
          "============================================\n\n\
           Stats for nerds:\n\
           %d tests succeeded\n\
           POST-CONDITION VIOLATION DETECTED.\n\
           %d tests discarded after shrinking. (%.2f%% reduction)\n\n\
           ============================================"
          stats.successes
          stats.discarded
          (float_of_int stats.discarded /. float_of_int (stats.successes + 1) *. 100.0) )
    | Right (seq, stats) ->
      let num_tests = List.fold_left (fun acc (_, num) -> acc + num) 0 stats.distrib in
      let distrib_to_str =
        if List.length stats.distrib = 0 then
          "No calls generated"
        else
          List.fold_left
            (fun acc (f, count) ->
               acc
               ^ Printf.sprintf
                   "%s: %d calls (%.2f%% of calls)\n"
                   f
                   count
                   (100.0 *. (float_of_int count /. float_of_int num_tests)))
            ""
            stats.distrib
      in
      let script_doc =
        BuildScript.generate ~output_dir ~filename_base (Config.get_num_tests ())
      in
      save ~perm:0o777 output_dir "run_tests.sh" script_doc;
      ( 0,
        seq,
        Printf.sprintf
          "============================================\n\n\
           Stats for nerds:\n\
           passed: %d, failed: %d, skipped: %d\n\
           Distribution of calls:\n\
           %s\n\
           ============================================"
          stats.successes
          stats.failures
          stats.skipped
          distrib_to_str )
  in
  print_endline output_msg;
  save output_dir test_file seq;
  exit_code


type seq_config = SeqTestGenConfig.t

let default_seq_cfg : seq_config = SeqTestGenConfig.default

let set_seq_config = SeqTestGenConfig.initialize

(** Workaround for https://github.com/rems-project/cerberus/issues/784 *)
let needs_static_hack
      ~(with_warning : bool)
      (cabs_tunit : CF.Cabs.translation_unit)
      (sigma : CF.GenTypes.genTypeCategory A.sigma)
      (inst : Fulminate.Extract.instrumentation)
  =
  let (TUnit decls) = cabs_tunit in
  let is_static_func () =
    List.exists
      (fun decl ->
         match decl with
         | CF.Cabs.EDecl_func
             (FunDef
                ( loc,
                  _,
                  { storage_classes; _ },
                  Declarator
                    (_, DDecl_function (DDecl_identifier (_, Identifier (_, fn')), _)),
                  _ ))
           when String.equal (Sym.pp_string inst.fn) fn'
                && List.exists
                     (fun scs -> match scs with CF.Cabs.SC_static -> true | _ -> false)
                     storage_classes ->
           if with_warning then
             Cerb_colour.with_colour
               (fun () ->
                  Pp.(
                    warn
                      loc
                      (string "Static function"
                       ^^^ squotes (Sym.pp inst.fn)
                       ^^^ string "could not be tested."
                       ^/^ string "Try again with '--with-static-hack'")))
               ();
           true
         | _ -> false)
      decls
  in
  let _, _, _, args, _ = List.assoc Sym.equal inst.fn sigma.function_definitions in
  let depends_on_static_glob () =
    let global_syms =
      inst.internal
      |> Option.get
      |> AT.get_lat
      |> LAT.free_vars (fun _ -> Sym.Set.empty)
      |> Sym.Set.to_seq
      |> List.of_seq
      |> List.filter (fun x ->
        not
          (List.mem (fun x y -> String.equal (Sym.pp_string x) (Sym.pp_string y)) x args))
    in
    let static_globs =
      List.filter_map
        (fun sym ->
           match List.assoc Sym.equal sym sigma.declarations with
           | loc, _, Decl_object ((Static, _), _, _, _) -> Some (sym, loc)
           | _ -> None)
        global_syms
    in
    if List.is_empty static_globs then
      false
    else (
      if with_warning then
        Cerb_colour.with_colour
          (fun () ->
             List.iter
               (fun (sym, loc) ->
                  Pp.(
                    warn
                      loc
                      (string "Function"
                       ^^^ squotes (Sym.pp inst.fn)
                       ^^^ string "relies on static global"
                       ^^^ squotes (Sym.pp sym)
                       ^^ comma
                       ^^^ string "so could not be tested."
                       ^^^ string "Try again with '--with-static-hack'.")))
               static_globs)
          ();
      true)
  in
  is_static_func () || depends_on_static_glob ()


(** Workaround for https://github.com/rems-project/cerberus/issues/765 *)
let needs_enum_hack
      ~(with_warning : bool)
      (sigma : CF.GenTypes.genTypeCategory A.sigma)
      (inst : Fulminate.Extract.instrumentation)
  =
  match List.assoc Sym.equal inst.fn sigma.declarations with
  | loc, _, Decl_function (_, (_, ret_ct), cts, _, _, _) ->
    if
      List.exists
        (fun (_, ct, _) ->
           match ct with C.Ctype (_, Basic (Integer (Enum _))) -> true | _ -> false)
        cts
    then (
      if with_warning then
        Cerb_colour.with_colour
          (fun () ->
             Pp.(
               warn
                 loc
                 (string "Function"
                  ^^^ squotes (Sym.pp inst.fn)
                  ^^^ string "has enum arguments and so could not be tested."
                  ^/^ string "Try again with '--with-static-hack'")))
          ();
      true)
    else if match ret_ct with C.Ctype (_, Basic (Integer (Enum _))) -> true | _ -> false
    then (
      if with_warning then
        Cerb_colour.with_colour
          (fun () ->
             Pp.(
               warn
                 loc
                 (string "Function"
                  ^^^ squotes (Sym.pp inst.fn)
                  ^^^ string "has an enum return type and so could not be tested."
                  ^/^ string "Try again with '--with-static-hack'")))
          ();
      true)
    else
      false
  | _ -> false


let functions_under_test
      ~(with_warning : bool)
      (cabs_tunit : CF.Cabs.translation_unit)
      (sigma : CF.GenTypes.genTypeCategory A.sigma)
      (prog5 : unit Mucore.file)
  : Fulminate.Extract.instrumentation list
  =
  let insts = prog5 |> Fulminate.Extract.collect_instrumentation |> fst in
  let selected_fsyms =
    Check.select_functions
      (Sym.Set.of_list
         (List.map (fun (inst : Fulminate.Extract.instrumentation) -> inst.fn) insts))
  in
  insts
  |> List.filter (fun (inst : Fulminate.Extract.instrumentation) ->
    Option.is_some inst.internal
    && Sym.Set.mem inst.fn selected_fsyms
    && (Config.with_static_hack ()
        || not
             (needs_static_hack ~with_warning cabs_tunit sigma inst
              || needs_enum_hack ~with_warning sigma inst)))


let run_seq
      ~output_dir
      ~filename
      (cabs_tunit : CF.Cabs.translation_unit)
      (sigma : Cerb_frontend.GenTypes.genTypeCategory Cerb_frontend.AilSyntax.sigma)
      (prog5 : unit Mucore.file)
  : int
  =
  Cerb_debug.begin_csv_timing ();
  let insts = functions_under_test ~with_warning:false cabs_tunit sigma prog5 in
  if Option.is_some prog5.main then
    failwith "Cannot test a file with a `main` function";
  let exit_code = generate ~output_dir ~filename sigma insts in
  Cerb_debug.end_csv_timing "sequence test generation";
  exit_code
