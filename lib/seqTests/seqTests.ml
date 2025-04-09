module CF = Cerb_frontend
module A = CF.AilSyntax
module C = CF.Ctype
module AT = ArgumentTypes
module LAT = LogicalArgumentTypes
module Utils = Fulminate.Executable_spec_utils
module Config = SeqTestGenConfig
module SymSet = Set.Make (Sym)
module FExtract = Fulminate.Executable_spec_extract

type call = SymSet.elt option * C.ctype * SymSet.elt * (C.ctype * string) list

type context = call list

type test_stats =
  { successes : int;
    failures : int;
    skipped : int;
    discarded : int;
    distrib : (string * int) list
  }

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
     ^^ dquotes (string (filename_base ^ "-exec.c"))
     ^^ hardline
     ^^ string "#include "
     ^^ dquotes (string "cn.c")
   else
     string "#include " ^^ dquotes (string "cn.h") ^^ twice hardline ^^ fun_decls)
  ^^ twice hardline
  ^^ string "int main"
  ^^ parens (string "int argc, char* argv[]")
  ^^ break 1
  ^^ braces
       (nest
          2
          (hardline
           ^^
           let init_ghost = Fulminate.Ownership_exec.get_ownership_global_init_stats () in
           separate_map hardline stmt_to_doc init_ghost ^^ hardline ^^ sequence)
        ^^ hardline)


let create_intermediate_test_file
      (sequence : Pp.document)
      (tests_to_try : Pp.document list)
      (filename_base : string)
      (fun_decls : Pp.document)
  : Pp.document
  =
  let open Pp in
  let create_try (i : int) (test : document) =
    string "int try"
    ^^ int i
    ^^ parens empty
    ^^ break 1
    ^^ braces
         (nest
            2
            (hardline
             ^^
             let init_ghost =
               Fulminate.Ownership_exec.get_ownership_global_init_stats ()
             in
             separate_map hardline stmt_to_doc init_ghost
             ^^ hardline
             ^^ sequence
             ^^ hardline
             ^^ test
             ^^ hardline
             ^^ string "return 0;")
          ^^ hardline)
  in
  let create_case (i : int) (_ : document) =
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
     string "#include " ^^ dquotes (string "cn.h") ^^ twice hardline ^^ fun_decls)
  ^^ twice hardline
  ^^ separate (twice hardline) (List.mapi create_try tests_to_try)
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


let rec get_violation_line test_output =
  let violation_regex = Str.regexp {| +\^~+ .+\.c:\([0-9]+\):[0-9]+-[0-9]+|} in
  match test_output with
  | [] -> 0
  | line :: lines ->
    if Str.string_match violation_regex line 0 then
      int_of_string (Str.matched_group 1 line)
    else
      get_violation_line lines


let rec is_precond_violation code =
  let is_post_regex = Str.regexp {|[ \t]*\(/\*@\)?[ \t]*ensures|} in
  let is_pre_regex = Str.regexp {|[ \t]*\(/\*@\)?[ \t]*requires|} in
  match code with
  | [] -> true
  | line :: lines ->
    if Str.string_match is_post_regex line 0 then
      false
    else if Str.string_match is_pre_regex line 0 then
      true
    else
      is_precond_violation lines


let out_to_list (command : string) =
  let chan = Unix.open_process_in command in
  let res = ref ([] : string list) in
  let rec go () =
    let e = input_line chan in
    res := e :: !res;
    go ()
  in
  try go () with
  | End_of_file ->
    let status = Unix.close_process_in chan in
    (!res, status)


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
            (Fulminate.Ownership_exec.generate_c_local_ownership_entry_fcall
               (name, ret_ty)))
    ^^ hardline
  | None -> f_call


let drop n l =
  (* ripped from OCaml 5.3 *)
  let rec aux i = function _x :: l when i < n -> aux (i + 1) l | rest -> rest in
  if n < 0 then invalid_arg "List.drop";
  aux 0 l


let ctx_to_tests =
  let open Pp in
  separate_map empty (fun (name, ret_ty, f, args) -> test_to_doc name ret_ty f args)


let shrink
      (seq : context)
      (output_dir : string)
      (filename_base : string)
      (src_code : string list)
      (fun_decls : Pp.document)
  : int * Pp.document
  =
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
    (* print_string ((ctx_to_string rev_dep_graph) ^ "\n"); *)
    let reqs = dfs seq [] start in
    let rec shrink_1 ((prev, ((name, _, _, _) as curr), next) : context * call * context) =
      if List.mem name_eq curr reqs then (
        match next with [] -> curr :: prev | h :: t -> shrink_1 (curr :: prev, h, t))
      else (
        let prev' =
          match name with
          | Some name ->
            let removed =
              List.filter_map
                (fun (name, _, _, _) ->
                   match name with None -> None | Some name -> Some (Sym.pp_string name))
                (dfs
                   rev_dep_graph
                   []
                   (List.find (name_eq (Some name, 0, 0, 0)) rev_dep_graph))
            in
            (* print_string ((Sym.pp_string name) ^ ":" ^ (List.fold_left (fun acc x -> acc ^ "," ^ x) "" removed) ^ "\n"); *)
            List.filter
              (fun ((name, _, _, args) : call) ->
                 List.for_all
                   (fun (_, arg_name) -> not (List.mem String.equal arg_name removed))
                   args
                 &&
                 match name with
                 | None -> true
                 | Some name ->
                   not
                     (List.mem
                        (fun name var -> String.equal (Sym.pp_string name) var)
                        name
                        removed))
              prev
          | None -> prev
        in
        let test_shrink = List.rev next @ prev' in
        (* print_string ((ctx_to_string test_shrink) ^ "\n"); *)
        save
          output_dir
          (filename_base ^ "_test.c")
          (create_test_file
             (ctx_to_tests test_shrink ^^ hardline ^^ string "return 123;")
             filename_base
             fun_decls);
        let output, status = out_to_list (output_dir ^ "/run_tests.sh") in
        match status with
        | WEXITED 0 | WEXITED 1 ->
          (match next with [] -> curr :: prev | h :: t -> shrink_1 (curr :: prev, h, t))
          (* indicating no more bug (which means that call was part of the cause, or compiler error)
             6 is a CN error
          *)
        | _ ->
          let violation_line_num = get_violation_line output in
          if
            is_precond_violation
              (drop (List.length src_code - violation_line_num) src_code)
          then (
            match next with [] -> curr :: prev | h :: t -> shrink_1 (curr :: prev, h, t))
          else (
            match next with [] -> prev' | h :: t -> shrink_1 (prev', h, t)))
    in
    let rec shrink_2 ((prev, next) : context * context) : context =
      let shrink_arg ((ty, arg_name) : C.ctype * string) : string list =
        let gen_lst_shrinks (ty : C.ctype) (arg : string) : string list =
          match ty with
          | Ctype (_, ty1) ->
            (match ty1 with
             | Basic (Integer (Signed _)) | Basic (Integer (Unsigned _)) ->
               let gend_int = int_of_string arg in
               List.map
                 string_of_int
                 (List.sort_uniq
                    (fun i1 i2 -> Int.compare (abs i1) (abs i2))
                    [ 0; 1; -1; -gend_int / 4; gend_int / 4; -gend_int / 2; gend_int / 2 ])
             | Basic (Floating _) ->
               let gend_float = float_of_string arg in
               List.map
                 string_of_float
                 (List.sort_uniq
                    (fun f1 f2 -> Float.compare (abs_float f1) (abs_float f2))
                    [ 0.0;
                      1.0;
                      -1.0;
                      gend_float /. -4.0;
                      (* fix shrinking bug here (sort_uniq issues) *)
                      gend_float /. 4.0;
                      gend_float /. -2.0;
                      gend_float /. 2.0
                    ])
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
          gen_lst_shrinks ty v @ [ v; arg_name ]
          (* generate arg for variable, then shrink *)
        | None -> gen_lst_shrinks ty arg_name @ [ arg_name ]
        (* if arg is not a variable then just shrink *)
      in
      match next with
      | [] -> List.rev prev
      | ((name, ret_ty, f, args) as curr) :: t ->
        let shrinks = List.map shrink_arg args in
        (match shrinks with
         | [] -> shrink_2 (curr :: prev, t)
         | poss_args ->
           let rec shrink_args
                     (poss_args : string list list)
                     ((prev_args, next_args) :
                       (C.ctype * string) list * (C.ctype * string) list)
             =
             let rec try_shrinks (arg_shrinks : string list) =
               let try_shrink
                     (shrinks : string list)
                     ((prev_args, next_args) :
                       (C.ctype * string) list * (C.ctype * string) list)
                 =
                 match (shrinks, next_args) with
                 | [], [] -> List.rev prev_args
                 | shrink, (ty, arg) :: next_args ->
                   (match shrink with
                    | [] -> List.rev prev_args @ ((ty, arg) :: next_args)
                    | new_arg :: _ -> List.rev prev_args @ ((ty, new_arg) :: next_args))
                 | _, _ -> failwith "impossible"
               in
               match arg_shrinks with
               | [] ->
                 failwith
                   "there will always be at least one possible arg (the original one)"
               | orig_arg :: [] -> orig_arg
               | new_arg :: rest ->
                 let new_args = try_shrink arg_shrinks (prev_args, next_args) in
                 let new_test = (name, ret_ty, f, new_args) in
                 let test_shrink = List.rev prev @ (new_test :: t) in
                 save
                   output_dir
                   (filename_base ^ "_test.c")
                   (create_test_file
                      (ctx_to_tests test_shrink ^^ hardline ^^ string "return 123;")
                      filename_base
                      fun_decls);
                 let output, status = out_to_list (output_dir ^ "/run_tests.sh") in
                 (match status with
                  | WEXITED 0 | WEXITED 1 ->
                    try_shrinks rest
                    (* indicating no more bug (which means that call was part of the cause, or compiler error)
                       6 is a CN error
                    *)
                  | _ ->
                    let violation_line_num = get_violation_line output in
                    if
                      is_precond_violation
                        (drop (List.length src_code - violation_line_num) src_code)
                    then
                      try_shrinks rest
                    else
                      new_arg)
             in
             match (poss_args, next_args) with
             | [], [] -> (name, ret_ty, f, List.rev prev_args)
             | arg_shrinks :: rest, (ty, _) :: next_args ->
               shrink_args rest ((ty, try_shrinks arg_shrinks) :: prev_args, next_args)
             | _, _ -> failwith "poss_args and next must have same length"
           in
           let shrunken_call = shrink_args poss_args ([], args) in
           shrink_2 (shrunken_call :: prev, t))
    in
    let shrunken = shrink_1 ([], start, seq) in
    let shrunken' = List.rev (shrink_2 ([], shrunken)) in
    let shrunken'' = shrink_1 ([], List.hd shrunken', List.tl shrunken') in
    (* print_string (ctx_to_string shrunken''); *)
    print_string (Printf.sprintf "%d %d\n" (List.length seq') (List.length shrunken''));
    (List.length shrunken'', ctx_to_tests shrunken'')


let rec gen_sequence
          (funcs :
            (SymSet.elt * ((C.qualifiers * C.ctype) * (SymSet.elt * C.ctype) list)) list)
          (fuel : int)
          (stats : test_stats)
          (ctx : context)
          (prev : int)
          (seq_so_far : Pp.document)
          (output_dir : string)
          (filename_base : string)
          (src_code : string list)
          (fun_decls : Pp.document)
  : (Pp.document * test_stats, Pp.document * test_stats) Either.either
  =
  let max_retries = Config.get_max_backtracks () in
  let num_resets = Config.get_max_resets () in
  let num_samples = Config.get_num_samples () in
  let instr_per_test = num_samples / (num_resets + 1) in
  let instr_per_test =
    if num_samples mod (num_resets + 1) = 0 then instr_per_test else instr_per_test + 1
  in
  let open Pp in
  match fuel with
  | 0 ->
    let unmap_stmts =
      List.filter_map
        (fun (name, ret, _, _) ->
           match name with
           | Some name ->
             Some (Fulminate.Ownership_exec.generate_c_local_ownership_exit (name, ret))
           | None -> None)
        ctx
    in
    let unmap_str = hardline ^^ separate_map hardline stmt_to_doc unmap_stmts in
    Right (seq_so_far ^^ unmap_str ^^ hardline ^^ string "return 0;", stats)
  | n ->
    let fs =
      List.map
        (fun ((_, (_, args)) as f) -> (calc_score ctx args, f))
        (List.filter (callable ctx) funcs)
    in
    let rec gen_test
              ((f, ((_, ret_ty), params)) :
                SymSet.elt * ((C.qualifiers * C.ctype) * (SymSet.elt * C.ctype) list))
              (retries_left : int)
      : context * int * (string, string * document) Either.either
      =
      if retries_left = 0 then
        (ctx, prev, Either.Left "")
      else (
        let name, prev =
          match ret_ty with
          | Ctype (_, Void) ->
            (None, prev) (* attempted to use fresh_cn but did not work for some reason?*)
          | _ -> (Some (Sym.fresh ("x" ^ string_of_int prev)), prev + 1)
        in
        let args = List.map (fun ((_, ty) as param) -> (ty, gen_arg ctx param)) params in
        let curr_test = test_to_doc name ret_ty f args in
        save
          output_dir
          (filename_base ^ "_test.c")
          (create_test_file (seq_so_far ^^ curr_test) filename_base fun_decls);
        let output, status = out_to_list (output_dir ^ "/run_tests.sh") in
        let ctx' = (name, ret_ty, f, args) :: ctx in
        match status with
        | WEXITED 0 -> (ctx', prev, Either.Right (Sym.pp_string f, curr_test))
        | _ ->
          let violation_line_num = get_violation_line output in
          if
            is_precond_violation
              (drop (List.length src_code - violation_line_num) src_code)
          then
            gen_test
              (pick fs (Random.int (List.fold_left ( + ) 1 (List.map fst fs))))
              (retries_left - 1)
          else
            ( ctx',
              prev,
              Either.Left
                (Printf.sprintf
                   "//^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n\
                    //violation of post-condition at line number %d in %s detected on \
                    this call"
                   violation_line_num
                   (filename_base ^ ".c")) ))
    in
    (match List.length fs with
     | 0 ->
       Right
         ( seq_so_far ^^ string "/* unable to generate call at this point */",
           { stats with failures = stats.failures + 1 } )
     | _ ->
       (match
          gen_test
            (pick fs (Random.int (List.fold_left ( + ) 1 (List.map fst fs))))
            max_retries
        with
        | ctx', prev, Either.Right (name, test) ->
          let distrib =
            match List.assoc_opt String.equal name stats.distrib with
            | None -> (name, 1) :: stats.distrib
            | Some n -> (name, n + 1) :: List.remove_assoc name stats.distrib
          in
          let ctx', rest =
            if
              (n - (num_samples mod instr_per_test) - 1) mod instr_per_test = 0
              && n >= instr_per_test
            then (
              let unmap_stmts =
                List.filter_map
                  (fun (name, ret, _, _) ->
                     Option.bind name (fun name ->
                       Some
                         (Fulminate.Ownership_exec.generate_c_local_ownership_exit
                            (name, ret))))
                  ctx'
              in
              ( [],
                hardline
                ^^ separate_map hardline stmt_to_doc unmap_stmts
                ^^ twice hardline ))
            else
              (ctx', empty)
          in
          gen_sequence
            funcs
            (n - 1)
            { stats with successes = stats.successes + 1; distrib }
            ctx'
            prev
            (seq_so_far ^^ test ^^ rest)
            output_dir
            filename_base
            src_code
            fun_decls
        | ctx', prev, Either.Left err ->
          if String.compare err "" = 0 then
            gen_sequence
              funcs
              (n - 1)
              { stats with skipped = stats.skipped + 1 }
              ctx
              prev
              seq_so_far
              output_dir
              filename_base
              src_code
              fun_decls
          else (
            let num_left, seq_so_far =
              shrink ctx' output_dir filename_base src_code fun_decls
            in
            (* Either.Left
              ( string "/*" ^^ ctx_to_tests (List.rev ctx') ^^ string "*/" ^^ hardline ^^ seq_so_far ^^ hardline ^^ string "return 123;",
                { stats with discarded; failures = stats.failures + 1 } )))) *)
            Either.Left
              ( seq_so_far ^^ string err ^^ hardline ^^ string "return 123;",
                { stats with
                  discarded = stats.successes + 1 - num_left;
                  failures = stats.failures + 1
                } ))))


let compile_sequence
      (sigma : CF.GenTypes.genTypeCategory A.sigma)
      (insts : FExtract.instrumentation list)
      (num_samples : int)
      (output_dir : string)
      (filename_base : string)
      (src_code : string list)
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
      (fun (inst : FExtract.instrumentation) ->
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
    { successes = 0; failures = 0; skipped = 0; discarded = 0; distrib = [] }
    []
    0
    Pp.empty
    output_dir
    filename_base
    src_code
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
  let test_file = filename_base ^ "_test.c" in
  let script_doc = BuildScript.generate ~output_dir ~filename_base in
  let script_doc' = BuildScript.generate_intermediate ~output_dir ~filename_base in
  let intermediate_test_file =
    create_intermediate_test_file
      Pp.empty
      [ Pp.empty; Pp.empty; Pp.empty; Pp.empty ]
      filename_base
      Pp.empty
  in
  let src_code, _ = out_to_list ("cat " ^ filename) in
  save ~perm:0o777 output_dir "run_tests.sh" script_doc;
  save ~perm:0o777 output_dir "run_tests_intermediate.sh" script_doc';
  save ~perm:0o777 output_dir "intermediate.c" intermediate_test_file;
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
      (Config.get_num_samples ())
      output_dir
      filename_base
      src_code
      fun_decls
  in
  let exit_code, seq, output_msg =
    match compiled_seq with
    | Left (seq, stats) ->
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
  let tests_doc = create_test_file seq filename_base fun_decls in
  print_endline output_msg;
  save output_dir test_file tests_doc;
  exit_code


type seq_config = SeqTestGenConfig.t

let default_seq_cfg : seq_config = SeqTestGenConfig.default

let set_seq_config = SeqTestGenConfig.initialize

(** Workaround for https://github.com/rems-project/cerberus/issues/784 *)
let needs_static_hack
      ~(with_warning : bool)
      (cabs_tunit : CF.Cabs.translation_unit)
      (sigma : CF.GenTypes.genTypeCategory A.sigma)
      (inst : FExtract.instrumentation)
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
      (inst : FExtract.instrumentation)
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
  : FExtract.instrumentation list
  =
  let insts = prog5 |> FExtract.collect_instrumentation |> fst in
  let selected_fsyms =
    Check.select_functions
      (Sym.Set.of_list
         (List.map (fun (inst : FExtract.instrumentation) -> inst.fn) insts))
  in
  insts
  |> List.filter (fun (inst : FExtract.instrumentation) ->
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
