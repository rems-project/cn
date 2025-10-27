module BT = BaseTypes
module CtA = Fulminate.Cn_to_ail
module Records = Fulminate.Records

module Make (AD : Domain.T) = struct
  module Stage4 = Stage4.Make (AD)
  module PathSelector = PathSelector.Make (AD)
  module Gather = Gather.Make (AD)
  module Concretize = Concretize.Make (AD)
  module Harness = Harness.Make (AD)
  module Setup = Setup_.Make (AD)
  module Ctx = Stage4.Ctx
  module Def = Stage4.Def

  (** Convert Stage 1 context with multiple definitions to a C source file *)
  let transform (prog5 : unit Mucore.file) (ctx : Ctx.t) : Pp.document =
    let open Pp in
    let typedef_docs =
      let defs =
        List.map
          (fun ((_, gr) : _ * Def.t) ->
             (Sym.fresh ("cn_test_generator_" ^ Sym.pp_string gr.name), gr))
          ctx
      in
      defs
      |> List.map (fun ((name, def) : Sym.t * Def.t) ->
        let loc = Locations.other __LOC__ in
        let inputs_outputs =
          match def.oarg with BT.Unit -> def.iargs | _ -> def.iargs
        in
        let bt =
          BT.Record
            (List.map (fun (x, bt) -> (Id.make loc (Sym.pp_string x), bt)) inputs_outputs)
        in
        let new_tag = Option.get (CtA.generate_record_tag name bt) in
        let typedef_doc tag =
          !^"typedef struct" ^^^ Sym.pp tag ^^^ Sym.pp new_tag ^^ semi
        in
        typedef_doc (CtA.lookup_records_map_with_default bt))
    in
    let record_defs = Records.generate_all_record_strs () in
    let functions =
      ctx
      |> List.map snd
      |> List.concat_map (fun (def : Def.t) ->
        if def.spec then
          (* Generate gathering and concretization functions as well as a [bennet_*] harness *)
          [ PathSelector.path_selector_def ctx def;
            Gather.gather_def def;
            Concretize.concretize_def def;
            Harness.transform_def prog5 def
          ]
        else
          (* Generate gathering, concretization, and path selector functions for non-spec definitions *)
          [ PathSelector.path_selector_def ctx def;
            Gather.gather_def def;
            Concretize.concretize_def def
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
    ^^ !^"/* TYPEDEFS */"
    ^^ hardline
    ^^ separate hardline typedef_docs
    ^^ !^"/* FUNCTION DECLARATIONS */"
    ^^ twice hardline
    ^^ Setup.generate_smt_setup prog5 ctx
    ^^ separate (twice hardline) functions
end
