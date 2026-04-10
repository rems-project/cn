module SymSet = Set.Make (Sym)
module T = Types
module CF = Cerb_frontend
module C = CF.Ctype
module A = CF.AilSyntax
module FUtils = Fulminate.Utils
module Config = SeqTestGenConfig

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
            * (bool
              * SymSet.elt
              * ((C.qualifiers * C.ctype) * (SymSet.elt * C.ctype) list)))
              list)
          (i : int)
  : bool * SymSet.elt * ((C.qualifiers * C.ctype) * (SymSet.elt * C.ctype) list)
  =
  match distribution with
  | [] -> failwith "impossible case"
  | (score, f) :: fs -> if i <= score then f else pick fs (i - score)


let rec ty_eq (ty1 : C.ctype) (ty2 : C.ctype) : bool =
  match (ty1, ty2) with
  | Ctype (_, Pointer (_, ty1)), Ctype (_, Pointer (_, ty2)) -> ty_eq ty1 ty2
  | Ctype (_, Void), Ctype (_, Void) -> false
  | _, _ -> C.ctypeEqual ty1 ty2


let print_scores distribution : unit =
  List.iter
    (fun (score, (is_static, f, ((_, ret_ty), args))) ->
       Printf.printf
         "Score: %d, Function: %s, Return Type: %s, Is Static: %b, Args: [ %s ]\n"
         score
         (Sym.pp_string f)
         (CF.String_core_ctype.string_of_ctype ret_ty)
         is_static
         (String.concat
            "; "
            (List.map
               (fun (arg_name, arg_ty) ->
                  Printf.sprintf
                    "%s: %s"
                    (Sym.pp_string arg_name)
                    (CF.String_core_ctype.string_of_ctype arg_ty))
               args)))
    distribution


let extract_actual_ctx (ctx : T.context) =
  List.filter
    (fun (name, _, _, _, _) ->
       match name with
       | Some name when String.starts_with ~prefix:"x" (Sym.pp_string name) -> true
       | None -> true
       | _ -> false)
    ctx


let ctx_length (ctx : T.context) : int = List.length (extract_actual_ctx ctx)

let ctx_to_string (ctx : T.context) : string =
  List.fold_left
    (fun acc (name, _, ty, f, args) ->
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
    ""
    ctx


let stmt_to_doc (stmt : CF.GenTypes.genTypeCategory A.statement_) : Pp.document =
  CF.Pp_ail.(with_executable_spec (pp_statement ~bs:[]) (FUtils.mk_stmt stmt))


let test_to_doc
      (filename : string)
      (name : SymSet.elt option)
      (is_static : bool)
      (ret_ty : C.ctype)
      (f : SymSet.elt)
      (args : (C.ctype * string) list)
  : Pp.document
  =
  let open Pp in
  let f_call =
    (if is_static then
       string (Fulminate.Utils.static_prefix filename) ^^ underscore ^^ Sym.pp f
     else
       Sym.pp f)
    ^^ parens
         (separate
            (comma ^^ space)
            [ separate (comma ^^ space) (List.map (fun (_, arg) -> string arg) args) ])
    ^^ semi
    ^^ hardline
  in
  match name with
  | Some name ->
    separate space [ CF.Pp_ail.pp_ctype T.no_qs ret_ty; Sym.pp name; equals ]
    ^^ f_call
    ^^ stmt_to_doc
         (A.AilSexpr
            (Fulminate.Ownership.generate_c_local_ownership_entry_fcall (name, ret_ty)))
    ^^ hardline
  | None -> f_call


let ctx_to_tests (filename : string) (ctx : T.context) =
  let open Pp in
  separate_map
    empty
    (fun (name, is_static, ret_ty, f, args) ->
       match name with
       | Some name when not (String.starts_with ~prefix:"x" (Sym.pp_string name)) -> empty
       | _ -> test_to_doc filename name is_static ret_ty f args)
    ctx


let rec combine_stats (stats_list : T.test_stats list) (combined_stats : T.test_stats)
  : T.test_stats
  =
  match stats_list with
  | [] -> combined_stats
  | stats :: stats_list ->
    let combined_stats =
      { T.successes = combined_stats.successes + stats.successes;
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


let create_intermediate_test_file
      (sequences : Pp.document list)
      (tests_to_try : Pp.document list)
      (fun_decls : Pp.document)
  : Pp.document
  =
  let open Pp in
  let create_try (i : int) (test : document) (sequence : document) =
    string "int test"
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
          ^^ string "test"
          ^^ int i
          ^^ parens empty
          ^^ semi
          ^^ hardline
          ^^ string "break"
          ^^ semi)
  in
  (separate
     hardline
     (List.map
        string
        [ "#ifndef NULL\n";
          "#include <stdlib.h>\n";
          "#endif\n";
          "#include <stdint.h>\n";
          "#include <stdio.h>";
          "#include <stddef.h>";
          "#include <cn-executable/utils.h>\n";
          "#include <cn-executable/cerb_types.h>\n"
        ])
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


let analyze_results
      (prev_and_tests : (int * T.call) option list)
      (sequences : PPrint.document list)
      (filename : string)
      (output_dir : string)
      (fun_decls : PPrint.document)
  : [ `OtherFailure
    | `PreConditionViolation
    | `PostConditionViolation of T.call
    | `Success of T.call * int
    ]
      option
      list
  =
  let tests_to_try =
    List.map
      (fun call ->
         match call with
         | None | Some (-1, (_, _, _, _, _)) ->
           Pp.string "/* unable to generate call at this point */"
         | Some (_, (name, is_static, ret_ty, f, args)) ->
           test_to_doc filename name is_static ret_ty f args)
      prev_and_tests
  in
  let intermediate_file =
    create_intermediate_test_file sequences tests_to_try fun_decls
  in
  save
    output_dir
    ((filename |> Filename.basename |> Filename.chop_extension) ^ ".test.c")
    intermediate_file;
  match Unix.system (output_dir ^ "/run_tests_intermediate.sh > /dev/null") with
  | WEXITED 0 ->
    let num_tests = List.length prev_and_tests in
    let rec run_tests (test_num : int) =
      if test_num < num_tests then (
        let process_status =
          Unix.system
            (output_dir ^ "/tests.out " ^ string_of_int test_num ^ " > /dev/null")
        in
        process_status :: run_tests (test_num + 1))
      else
        []
    in
    let results = run_tests 0 in
    let tests_and_msgs = List.combine prev_and_tests results in
    let rec analyze_results_h
              (tests_and_msgs : ((int * T.call) option * Unix.process_status) list)
      : [ `OtherFailure
        | `PreConditionViolation
        | `PostConditionViolation of T.call
        | `Success of T.call * int
        ]
          option
          list
      =
      match tests_and_msgs with
      | [] -> []
      | (None, _) :: t -> None :: analyze_results_h t
      | (Some (prev', (name, is_static, ret_ty, f, args)), exit_code) :: t ->
        (match exit_code with
         | WEXITED 0 ->
           Some (`Success ((name, is_static, ret_ty, f, args), prev'))
           :: analyze_results_h t
         | WEXITED 1 -> Some `PreConditionViolation :: analyze_results_h t
         | WEXITED 2 | WEXITED 3 | WEXITED 4 | WEXITED 5 ->
           Some (`PostConditionViolation (name, is_static, ret_ty, f, args))
           :: analyze_results_h t
         | _ -> [ Some `OtherFailure ])
    in
    analyze_results_h tests_and_msgs
  | _ -> [ Some `OtherFailure ]


let name_eq (n1, _, _, _, _) (n2, _, _, _, _) =
  match (n1, n2) with Some name1, Some name2 -> Sym.equal name1 name2 | _ -> false


let gen_val (ty : C.ctype) =
  match ty with
  | Ctype (_, ty1) ->
    (match ty1 with
     | Basic (Integer Char) ->
       [ "\'" ^ String.make 1 (char_of_int (Random.int 96 + 32)) ^ "\'" ]
     | Basic (Integer Bool) -> [ (if Random.int 2 = 1 then "true" else "false") ]
     | Basic (Integer (Signed _)) ->
       let rand_int = Random.int 128 in
       [ string_of_int (if Random.int 2 = 1 then rand_int * -1 else rand_int) ]
     | Basic (Integer (Unsigned _)) -> [ string_of_int (Random.int 256) ]
     | Basic (Floating _) -> [ string_of_float (Random.float 256.0) ]
     | _ -> [])
