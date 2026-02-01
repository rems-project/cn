module CF = Cerb_frontend
module A = CF.AilSyntax
module C = CF.Ctype
module Records = Fulminate.Records

module Make (AD : Domain.T) = struct
  module Stage5 = Stage5.Make (AD)
  module PathSelector = PathSelector.Make (AD)
  module Gather = Gather.Make (AD)
  module Concretize = Concretize.Make (AD)
  module Harness = Harness.Make (AD)
  module Setup = Setup_.Make (AD)
  module Ctx = Stage5.Ctx
  module Def = Stage5.Def

  (** Convert Stage 1 context with multiple definitions to a C source file *)
  let transform
        (sigma : CF.GenTypes.genTypeCategory A.sigma)
        (prog5 : unit Mucore.file)
        (ctx : Ctx.t)
    : Pp.document
    =
    let open Pp in
    let struct_defs =
      ctx
      |> List.filter_map (fun ((_, def) : Sym.t * Def.t) ->
        (* Generate struct definitions for spec functions only *)
        if not def.spec then
          None
        else (
          let struct_name = "cn_test_generator_" ^ Sym.pp_string def.name ^ "_record" in
          (* Get parameter C types from def *)
          let c_types = Option.get def.c_types in
          let param_name_strings =
            c_types |> List.map (fun (param_name, _) -> Sym.pp_string param_name)
          in
          let param_field_docs =
            c_types
            |> List.map (fun (param_name, ctype) ->
              let field_ty_doc =
                CF.Pp_ail.(
                  with_executable_spec (pp_ctype ~is_human:false C.no_qualifiers) ctype)
              in
              field_ty_doc ^^^ Sym.pp param_name ^^ semi)
          in
          let global_field_docs =
            def.iargs
            |> List.filter_map (fun (sym, _bt) ->
              let sym_str = Sym.pp_string sym in
              (* Skip if this is a parameter *)
              if List.exists (String.equal sym_str) param_name_strings then
                None
              else (
                (* Look up in globals using string comparison *)
                  match
                    prog5.globs
                    |> List.find_opt (fun (global_sym, _) ->
                      String.equal (Sym.pp_string global_sym) sym_str)
                  with
                  | Some (_, Mucore.GlobalDecl sct) | Some (_, Mucore.GlobalDef (sct, _))
                    ->
                    (* Globals are stored as pointers in the struct *)
                    let ctype =
                      C.mk_ctype_pointer C.no_qualifiers (Sctypes.to_ctype sct)
                    in
                    let field_ty_doc =
                      CF.Pp_ail.(
                        with_executable_spec
                          (pp_ctype ~is_human:false C.no_qualifiers)
                          ctype)
                    in
                    Some (field_ty_doc ^^^ Sym.pp sym ^^ semi)
                  | None ->
                    failwith
                      (Printf.sprintf
                         "Could not find C type for global %s in function %s"
                         sym_str
                         (Sym.pp_string def.name))))
          in
          let field_docs = param_field_docs @ global_field_docs in
          (* Skip if we couldn't find the function declaration *)
          if List.length field_docs = 0 then
            None
          else
            Some
              (!^"struct"
               ^^^ !^struct_name
               ^^^ braces (nest 2 (hardline ^^ separate hardline field_docs) ^^ hardline)
               ^^ semi
               ^^ hardline
               ^^ !^"typedef struct"
               ^^^ !^struct_name
               ^^^ !^struct_name
               ^^ semi)))
    in
    let record_defs = Records.generate_all_record_strs () in
    (* Generate forward declarations for all functions *)
    let forward_decls =
      ctx
      |> List.map snd
      |> List.concat_map (fun (def : Def.t) ->
        [ PathSelector.path_selector_forward_decl def;
          Gather.gather_forward_decl def;
          Concretize.concretize_forward_decl def
        ])
    in
    let functions =
      ctx
      |> List.map snd
      |> List.concat_map (fun (def : Def.t) ->
        if def.spec then
          (* Generate gathering and concretization functions as well as a [bennet_*] harness *)
          [ PathSelector.path_selector_def ctx def;
            Gather.gather_def sigma def;
            Concretize.concretize_def sigma def;
            Harness.transform_def sigma prog5 def
          ]
        else
          (* Generate gathering, concretization, and path selector functions for non-spec definitions *)
          [ PathSelector.path_selector_def ctx def;
            Gather.gather_def sigma def;
            Concretize.concretize_def sigma def
          ])
    in
    (* Structure output like stage6/convert.ml for compatibility *)
    hardline
    ^^ hardline
    ^^ !^"#include <cn-smt/prelude.h>"
    ^^ hardline
    ^^ !^"#include <cn-smt/branch_history.h>"
    ^^ hardline
    ^^ hardline
    ^^ !^"/* TAG DEFINITIONS */"
    ^^ hardline
    ^^ !^record_defs
    ^^ hardline
    ^^ !^"/* STRUCT DEFINITIONS */"
    ^^ hardline
    ^^ separate hardline struct_defs
    ^^ hardline
    ^^ !^"/* FORWARD DECLARATIONS */"
    ^^ hardline
    ^^ separate hardline forward_decls
    ^^ hardline
    ^^ !^"/* FUNCTION DEFINITIONS */"
    ^^ twice hardline
    ^^ Setup.generate_smt_setup sigma prog5 ctx
    ^^ separate (twice hardline) functions
end
