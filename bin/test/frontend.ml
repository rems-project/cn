(* Frontend preparation shared by every `cn test` subcommand: global flags, the
   testing macros, and the well-formedness-checked frontend pipeline. Export and
   testing go through the same preparation, so exported specifications come
   from the same source that testing sees. *)

open Cn

(* Preprocessor macros defined for every `cn test` subcommand. *)
let test_macros = [ ("__CN_TEST", None); ("__CN_INSTRUMENT", None) ]

let set_flags ~debug_level ~print_level =
  Cerb_debug.debug_level := debug_level;
  Pp.print_level := print_level;
  Sym.executable_spec_enabled := true


(* Run the CN frontend on [filename] and call [f] with the checked program.
   [save_cpp] and [disable_linemarkers] are the only settings that differ
   between export (diagnostics cite original lines) and the legacy testing path
   (Fulminate consumes the saved preprocessed file). *)
let with_checked_program
      ~filename
      ~cc
      ~macros
      ~permissive
      ~incl_dirs
      ~incl_files
      ~csv_times
      ~stdout_times
      ~astprints
      ~no_inherit_loc
      ~magic_comment_char_dollar
      ~allow_split_magic_comments
      ~save_cpp
      ~disable_linemarkers
      ~f
  =
  Common.with_well_formedness_check (* CLI arguments *)
    ~filename
    ~cc
    ~macros:(test_macros @ macros)
    ~permissive
    ~incl_dirs
    ~incl_files
    ~csv_times
    ~stdout_times
    ~coq_export_file:None
    ~coq_mucore:false
    ~coq_proof_log:false
    ~coq_check_proof_log:false
    ~astprints
    ~no_inherit_loc
    ~magic_comment_char_dollar
    ~allow_split_magic_comments (* Callbacks *)
    ~save_cpp
    ~disable_linemarkers
    ~handle_error:Common.print_type_error_and_exit
    ~f
