(* The external Austen backend: locating its executable, building its argument
   vector, and running it. Arguments are passed as an array, never through a
   shell, so paths and macro values reach Austen unchanged. *)

open Cn

let executable = "austen"

let install_hint = "cargo install austen --version 0.0.1 --locked --features bennet,darcy"

(* The first executable regular file named [executable] on [PATH]. An empty
   [PATH] entry names the current directory, as for the shell. *)
let find_executable () : string option =
  let is_executable path =
    try
      match (Unix.stat path).st_kind with
      | S_REG ->
        Unix.access path [ X_OK ];
        true
      | _ -> false
    with
    | Unix.Unix_error _ -> false
  in
  Option.value ~default:"" (Sys.getenv_opt "PATH")
  |> String.split_on_char ':'
  |> List.find_map (fun dir ->
    let path = Filename.concat (if String.equal dir "" then "." else dir) executable in
    if is_executable path then Some path else None)


(* The compiler arguments that make Austen's build of [FILE] see the same
   source as CN's frontend: the testing macros, then the user's macros,
   include directories, and forced includes. Each option and its operand are
   separate arguments, as Austen requires. *)
let cflags (frontend : Frontend.args) : string list =
  List.concat_map
    (fun macro -> [ "-D"; Setup.macro_def macro ])
    (Frontend.test_macros @ frontend.macros)
  @ List.concat_map (fun dir -> [ "-I"; dir ]) frontend.incl_dirs
  @ List.concat_map (fun file -> [ "-include"; file ]) frontend.incl_files


(* `austen test --build-only` for one function. [--] ends Austen's options, so
   the specification and source are positional whatever their names. *)
let build_only_argv ~exe ~engine ~fn ~work ~cflags ~spec ~source : string list =
  [ exe;
    "test";
    "--build-only";
    "--engine=" ^ engine;
    "--function=" ^ fn;
    "--work=" ^ work
  ]
  @ List.map (fun flag -> "--cflag=" ^ flag) cflags
  @ [ "--"; spec; source ]


(* Run [argv] with the inherited standard streams and wait for it. *)
let run (argv : string list) : (Unix.process_status, string) result =
  flush_all ();
  match
    Unix.create_process
      (List.hd argv)
      (Array.of_list argv)
      Unix.stdin
      Unix.stdout
      Unix.stderr
  with
  | exception Unix.Unix_error (err, _, _) -> Error (Unix.error_message err)
  | pid ->
    let rec wait () =
      match Unix.waitpid [] pid with
      | _, status -> status
      | exception Unix.Unix_error (EINTR, _, _) -> wait ()
    in
    Ok (wait ())


let string_of_status = function
  | Unix.WEXITED code -> Printf.sprintf "exited with status %d" code
  | Unix.WSIGNALED signal -> Printf.sprintf "was killed by signal %d" signal
  | Unix.WSTOPPED signal -> Printf.sprintf "was stopped by signal %d" signal
