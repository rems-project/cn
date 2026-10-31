(* Frontend preparation shared by every `cn test` subcommand: global flags, the
   testing macros, and the well-formedness-checked frontend pipeline. Export and
   testing go through the same preparation, so exported specifications come
   from the same source that testing sees. *)

open Cn
open Cmdliner

(* Preprocessor macros defined for every `cn test` subcommand, and forwarded to
   Austen's build of the same source. Unlike `cn instrument`, testing does not
   define [__CN_INSTRUMENT]. *)
let test_macros = [ ("__CN_TEST", None); ("__AUSTEN_TEST", None) ]

(* The frontend flags, as given on the command line. *)
type args =
  { files : string list;
    cc : string;
    macros : (string * string option) list;
    permissive : bool;
    incl_dirs : string list;
    incl_files : string list;
    debug_level : int;
    print_level : int;
    csv_times : string option;
    stdout_times : bool;
    astprints : Cerb_backend.Pipeline.language list;
    no_inherit_loc : bool;
    magic_comment_char_dollar : bool;
    allow_split_magic_comments : bool
  }

let term : args Term.t =
  let make
        files
        cc
        macros
        permissive
        incl_dirs
        incl_files
        debug_level
        print_level
        csv_times
        stdout_times
        astprints
        no_inherit_loc
        magic_comment_char_dollar
        allow_split_magic_comments
    =
    { files;
      cc;
      macros;
      permissive;
      incl_dirs;
      incl_files;
      debug_level;
      print_level;
      csv_times;
      stdout_times;
      astprints;
      no_inherit_loc;
      magic_comment_char_dollar;
      allow_split_magic_comments
    }
  in
  Term.(
    const make
    $ Common.Flags.file
    $ Common.Flags.cc
    $ Common.Flags.macros
    $ Common.Flags.permissive
    $ Common.Flags.incl_dirs
    $ Common.Flags.incl_files
    $ Common.Flags.debug_level
    $ Common.Flags.print_level
    $ Common.Flags.csv_times
    $ Common.Flags.stdout_times
    $ Common.Flags.astprints
    $ Common.Flags.no_inherit_loc
    $ Common.Flags.magic_comment_char_dollar
    $ Common.Flags.allow_split_magic_comments)


(* Set the global flags from [a], run the CN frontend on its single source
   file, and call [f] with that file's name and the checked program.
   Linemarkers are kept, so diagnostics cite original source lines. *)
let with_checked_program (a : args) ~f =
  Cerb_debug.debug_level := a.debug_level;
  Pp.print_level := a.print_level;
  Sym.executable_spec_enabled := true;
  let filename = Common.there_can_only_be_one a.files in
  Common.with_well_formedness_check (* CLI arguments *)
    ~filename
    ~cc:a.cc
    ~macros:(test_macros @ a.macros)
    ~permissive:a.permissive
    ~incl_dirs:a.incl_dirs
    ~incl_files:a.incl_files
    ~csv_times:a.csv_times
    ~stdout_times:a.stdout_times
    ~coq_export_file:None
    ~coq_mucore:false
    ~coq_proof_log:false
    ~coq_check_proof_log:false
    ~astprints:a.astprints
    ~no_inherit_loc:a.no_inherit_loc
    ~magic_comment_char_dollar:a.magic_comment_char_dollar
    ~allow_split_magic_comments:a.allow_split_magic_comments (* Callbacks *)
    ~save_cpp:None
    ~disable_linemarkers:false
    ~handle_error:Common.print_type_error_and_exit
    ~f:(f ~filename)


(* Write the specification module of the checked program to [path], reporting
   an unexpected failure as a CN-Export issue. *)
let export_specs ~path ~filename sigma prog5 paused =
  try TestGeneration.export_specs ~path ~filename sigma prog5 paused with
  | e -> Common.handle_error_with_user_guidance ~label:"CN-Export" e
