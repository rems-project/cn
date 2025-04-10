module CF = Cerb_frontend
module A = CF.AilSyntax
module C = CF.Ctype
module AT = ArgumentTypes
module LAT = LogicalArgumentTypes
module Utils = Fulminate.Utils
module Config = SeqTestGenConfig
module SymSet = Set.Make (Sym)
module FExtract = Fulminate.Extract

type test_stats =
  { successes : int;
    failures : int;
    skipped : int;
    distrib : (string * int) list
  }

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
            (int * (Sym.t * ((C.qualifiers * C.ctype) * (Sym.t * C.ctype) list))) list)
          (i : int)
  : Sym.t * ((C.qualifiers * C.ctype) * (Sym.t * C.ctype) list)
  =
  match distribution with
  | [] -> failwith "impossible case"
  | (score, f) :: fs -> if i <= score then f else pick fs (i - score)


let rec ty_eq (ty1 : C.ctype) (ty2 : C.ctype) : bool =
  match (ty1, ty2) with
  | Ctype (_, Pointer (_, ty1)), Ctype (_, Pointer (_, ty2)) -> ty_eq ty1 ty2
  | _, _ -> C.ctypeEqual ty1 ty2


let callable
      (ctx : (Sym.t * C.ctype) list)
      ((_, (_, args)) : Sym.t * ((C.qualifiers * C.ctype) * (Sym.t * C.ctype) list))
  : bool
  =
  List.for_all
    (fun x -> x)
    (List.map
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
              | Pointer (_, ty) -> List.exists (fun (_, ct) -> ty_eq ty ct) ctx
              | _ -> false))
          || List.exists (fun (_, ct) -> ty_eq ty ct) ctx)
       args)


let calc_score (ctx : (Sym.t * C.ctype) list) (args : (Sym.t * C.ctype) list) : int =
  List.fold_left
    (fun acc (_, ty) ->
       if List.exists (fun (_, ct) -> ty_eq ty ct) ctx then
         acc + 10
       else
         acc)
    1
    args


let ctx_to_string (ctx : (Sym.t * C.ctype) list) : string =
  List.fold_left
    ( ^ )
    ""
    (List.map
       (fun (name, ty) ->
          "(" ^ Sym.pp_string name ^ ":" ^ CF.String_core_ctype.string_of_ctype ty ^ ")")
       ctx)


let gen_arg (ctx : (Sym.t * C.ctype) list) ((name, ty) : Sym.t * C.ctype) : Pp.document =
  let open Pp in
  let generated_base_ty =
    match ty with
    | Ctype (_, ty1) ->
      (match ty1 with
       | Basic (Integer Char) ->
         [ string "\'" ^^ char (char_of_int (Random.int 96 + 32)) ^^ string "\'" ]
       | Basic (Integer Bool) ->
         [ (if Random.int 2 = 1 then string "true" else string "false") ]
       | Basic (Integer (Signed _)) ->
         let rand_int = Random.int 32767 in
         [ int (if Random.int 2 = 1 then rand_int * -1 else rand_int) ]
       | Basic (Integer (Unsigned _)) -> [ int (Random.int 65536) ]
       | Basic (Floating _) -> [ string (string_of_float (Random.float 65536.0)) ]
       | Void -> [ empty ]
       | _ -> [])
  in
  let prev_call =
    let prev_calls =
      List.filter
        (fun (_, ct) ->
           ty_eq ty ct
           || match ty with Ctype (_, Pointer (_, ty)) -> ty_eq ty ct | _ -> false)
        ctx
    in
    match List.length prev_calls with
    | 0 -> []
    | n ->
      let name, ty' = List.nth prev_calls (Random.int n) in
      if not (ty_eq ty' ty) then (* only way they're not directly equal is if pointer*)
        [ string "&" ^^ Sym.pp name ]
      else
        [ Sym.pp name ]
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
           let init_ghost = Fulminate.Ownership.get_ownership_global_init_stats () in
           separate_map hardline stmt_to_doc init_ghost ^^ hardline ^^ sequence)
        ^^ hardline)


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


let rec gen_sequence
          (funcs : (Sym.t * ((C.qualifiers * C.ctype) * (Sym.t * C.ctype) list)) list)
          (fuel : int)
          (stats : test_stats)
          (ctx : (Sym.t * C.ctype) list)
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
    let unmap_stmts = List.map Fulminate.Ownership.generate_c_local_ownership_exit ctx in
    let unmap_str = hardline ^^ separate_map hardline stmt_to_doc unmap_stmts in
    Right (seq_so_far ^^ unmap_str ^^ hardline ^^ string "return 0;", stats)
  | n ->
    let fs =
      List.map
        (fun ((_, (_, args)) as f) -> (calc_score ctx args, f))
        (List.filter (callable ctx) funcs)
    in
    let rec gen_test
              (args_map : Sym.t * ((C.qualifiers * C.ctype) * (Sym.t * C.ctype) list))
              (retries_left : int)
      : (SymSet.elt * C.ctype) list * int * (document, string * document) Either.either
      =
      if retries_left = 0 then
        (ctx, prev, Either.Left empty)
      else (
        match args_map with
        | f, ((qualifiers, ret_ty), args) ->
          let ctx', name, assign, prev =
            match ret_ty with
            | Ctype (_, Void) ->
              (ctx, None, empty, prev)
              (* attempted to use fresh_cn but did not work for some reason?*)
            | _ ->
              let name = Sym.fresh ("x" ^ string_of_int prev) in
              ( (name, ret_ty) :: ctx,
                Some name,
                separate
                  space
                  [ CF.Pp_ail.pp_ctype qualifiers ret_ty; Sym.pp name; equals ],
                prev + 1 )
          in
          let curr_test =
            assign
            ^^ Sym.pp f
            ^^ parens
                 (separate
                    (comma ^^ space)
                    [ separate_map (comma ^^ space) (gen_arg ctx) args ])
            ^^ semi
            ^^ hardline
            ^^
            match name with
            | None -> empty
            | Some name ->
              stmt_to_doc
                (A.AilSexpr
                   (Fulminate.Ownership.generate_c_local_ownership_entry_fcall
                      (name, ret_ty)))
              ^^ hardline
          in
          let _ =
            save
              output_dir
              (filename_base ^ "_test.c")
              (create_test_file (seq_so_far ^^ curr_test) filename_base fun_decls)
          in
          let output, status = out_to_list (output_dir ^ "/run_tests.sh") in
          (match status with
           | WEXITED 0 -> (ctx', prev, Either.Right (Sym.pp_string f, curr_test))
           | _ ->
             let violation_regex = Str.regexp {| +\^~+ .+\.c:\([0-9]+\):[0-9]+-[0-9]+|} in
             let is_post_regex = Str.regexp {|[ \t]*\(/\*@\)?[ \t]*ensures|} in
             let is_pre_regex = Str.regexp {|[ \t]*\(/\*@\)?[ \t]*requires|} in
             let rec get_violation_line test_output =
               match test_output with
               | [] -> 0
               | line :: lines ->
                 if Str.string_match violation_regex line 0 then
                   int_of_string (Str.matched_group 1 line)
                 else
                   get_violation_line lines
             in
             let rec is_precond_violation code =
               match code with
               | [] -> true
               | line :: lines ->
                 if Str.string_match is_post_regex line 0 then
                   false
                 else if Str.string_match is_pre_regex line 0 then
                   true
                 else
                   is_precond_violation lines
             in
             let drop n l =
               (* ripped from OCaml 5.3 *)
               let rec aux i = function
                 | _x :: l when i < n -> aux (i + 1) l
                 | rest -> rest
               in
               if n < 0 then invalid_arg "List.drop";
               aux 0 l
             in
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
                   (string
                      (Printf.sprintf
                         "/* violation of post-condition at line number %d in %s \
                          detected on this call: */"
                         violation_line_num
                         (filename_base ^ ".c"))
                    ^^ hardline
                    ^^ curr_test
                    ^^ hardline
                    ^^ string "return 123;") )))
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
                List.map Fulminate.Ownership.generate_c_local_ownership_exit ctx'
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
        | _, prev, Either.Left err ->
          if is_empty err then
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
          else
            Either.Left (seq_so_far ^^ err, { stats with failures = 1 })))


let compile_sequence
      (sigma : CF.GenTypes.genTypeCategory A.sigma)
      (insts : Fulminate.Extract.instrumentation list)
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
    |> List.map (fun (inst : Fulminate.Extract.instrumentation) ->
      (inst.fn, List.assoc Sym.equal inst.fn sigma.declarations))
  in
  let args_map : (Sym.t * ((C.qualifiers * C.ctype) * (Sym.t * C.ctype) list)) list =
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
    { successes = 0; failures = 0; skipped = 0; distrib = [] }
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
      (insts : Fulminate.Extract.instrumentation list)
  : int
  =
  if List.is_empty insts then failwith "No testable functions";
  let filename_base = filename |> Filename.basename |> Filename.chop_extension in
  let test_file = filename_base ^ "_test.c" in
  let script_doc = BuildScript.generate ~output_dir ~filename_base in
  let src_code, _ = out_to_list ("cat " ^ filename) in
  save ~perm:0o777 output_dir "run_tests.sh" script_doc;
  let fun_to_decl (inst : Fulminate.Extract.instrumentation) =
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
          "Stats for nerds:\n\
           %d tests succeeded\n\
           POST-CONDITION VIOLATION DETECTED.\n\
           See %s/%s for details"
          stats.successes
          output_dir
          test_file )
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
          "Stats for nerds:\n\
           passed: %d, failed: %d, skipped: %d\n\
           Distribution of calls:\n\
           %s"
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
