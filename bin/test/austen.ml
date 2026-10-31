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


(* `austen test` for one function with the given mode options. [--] ends
   Austen's options, so the specification and source are positional whatever
   their names. *)
let argv ~exe ~engine ~fn ~work ~check_heap_ownership ~cflags ~spec ~source mode_options
  : string list
  =
  [ exe; "test"; "--engine=" ^ engine; "--function=" ^ fn; "--work=" ^ work ]
  @ mode_options
  @ (if check_heap_ownership then [ "--check-heap-ownership" ] else [])
  @ List.map (fun flag -> "--cflag=" ^ flag) cflags
  @ [ "--"; spec; source ]


let build_only_argv ~exe ~engine ~fn ~work ~check_heap_ownership ~cflags ~spec ~source =
  argv
    ~exe
    ~engine
    ~fn
    ~work
    ~check_heap_ownership
    ~cflags
    ~spec
    ~source
    [ "--build-only" ]


(* Native, single-function campaigns are always bounded. Keep findings beside
   the work directory instead of Austen's default [./crashes]. *)
let campaign_argv
      ~exe
      ~engine
      ~fn
      ~work
      ~duration
      ~report
      ~crashes
      ~check_heap_ownership
      ~seed
      ~cflags
      ~spec
      ~source
  =
  argv
    ~exe
    ~engine
    ~fn
    ~work
    ~check_heap_ownership
    ~cflags
    ~spec
    ~source
    ([ "--duration=" ^ string_of_int duration;
       "--strict-duration";
       "--fail-on-crash";
       "--report=" ^ report;
       "--crashes=" ^ crashes
     ]
     @ match seed with None -> [] | Some seed -> [ "--seed=" ^ seed ])


(* Run Austen in its own session. CN forwards each interrupt to that process
   group, including build tools and harness children, and reports any
   interrupted run as exit status 130. The close-on-exec pipe
   distinguishes a failed exec from an Austen process that exits unsuccessfully. *)
let run (argv : string list) : (Unix.process_status, string) result =
  flush_all ();
  let child = ref None in
  let interrupted = ref false in
  let forward_interrupt pid =
    try Unix.kill (-pid) Sys.sigint with Unix.Unix_error (ESRCH, _, _) -> ()
  in
  let previous =
    Sys.signal
      Sys.sigint
      (Signal_handle
         (fun _ ->
           interrupted := true;
           Option.iter forward_interrupt !child))
  in
  let rec wait pid =
    match Unix.waitpid [] pid with
    | _, status ->
      child := None;
      status
    | exception Unix.Unix_error (EINTR, _, _) -> wait pid
  in
  Fun.protect
    ~finally:(fun () -> Sys.set_signal Sys.sigint previous)
    (fun () ->
       try
         let reader, writer = Unix.pipe ~cloexec:true () in
         match Unix.fork () with
         | exception exn ->
           Unix.close reader;
           Unix.close writer;
           raise exn
         | 0 ->
           Unix.close reader;
           (try
              ignore (Unix.setsid ());
              Sys.set_signal Sys.sigint Signal_default;
              Unix.execv (List.hd argv) (Array.of_list argv)
            with
            | Unix.Unix_error (err, _, _) ->
              let message = Unix.error_message err in
              (try
                 ignore (Unix.write_substring writer message 0 (String.length message))
               with
               | Unix.Unix_error _ -> ());
              Unix._exit 127)
         | pid ->
           Unix.close writer;
           let startup_error =
             try
               Fun.protect
                 ~finally:(fun () -> Unix.close reader)
                 (fun () -> In_channel.input_all (Unix.in_channel_of_descr reader))
             with
             | exn ->
               (try Unix.kill pid Sys.sigkill with Unix.Unix_error (ESRCH, _, _) -> ());
               ignore (wait pid);
               raise exn
           in
           (* The pipe closes on exec, so the new process group exists before
              forwarding. Block interrupts while transferring a pending one to
              the child to avoid sending the same interrupt twice. *)
           let mask = Unix.sigprocmask SIG_BLOCK [ Sys.sigint ] in
           child := Some pid;
           if !interrupted then forward_interrupt pid;
           ignore (Unix.sigprocmask SIG_SETMASK mask);
           let status = wait pid in
           if not (String.equal startup_error "") then
             Error startup_error
           else
             Ok
               (match status with
                | _ when !interrupted -> Unix.WEXITED 130
                | WSIGNALED signal when signal = Sys.sigint -> WEXITED 130
                | status -> status)
       with
       | Unix.Unix_error (err, _, _) -> Error (Unix.error_message err))


let string_of_status = function
  | Unix.WEXITED code -> Printf.sprintf "exited with status %d" code
  | Unix.WSIGNALED signal -> Printf.sprintf "was killed by signal %d" signal
  | Unix.WSTOPPED signal -> Printf.sprintf "was stopped by signal %d" signal
