module BT = BaseTypes
module CtA = Fulminate.Cn_to_ail
module Records = Fulminate.Records

module Make (AD : Domain.T) = struct
  module Stage2 = Stage2.Make (AD)
  module Gather = Gather.Make (AD)
  module Concretize = Concretize.Make (AD)
  module Harness = Harness.Make (AD)
  module Ctx = Stage2.Ctx
  module Def = Stage2.Def

  (** Convert Stage 1 context with multiple definitions to a C source file *)
  let transform (ctx : Ctx.t) : Pp.document =
    let open Pp in
    let typedef_docs =
      let defs =
        List.map
          (fun ((_, gr) : _ * Def.t) -> (GenUtils.get_mangled_name gr.name, gr))
          ctx
      in
      defs
      |> List.map (fun ((name, def) : Sym.t * Def.t) ->
        let loc = Locations.other __LOC__ in
        let bt =
          BT.Record
            (List.map
               (fun (x, bt) -> (Id.make loc (Sym.pp_string x), bt))
               (def.iargs @ def.oargs))
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
          [ Gather.gather_def def;
            Concretize.concretize_def def;
            Harness.transform_def def
          ]
        else
          (* Generate gathering and concretization functions for non-spec definitions *)
          [ Gather.gather_def def; Concretize.concretize_def def ])
    in
    (* Structure output like stage6/convert.ml for compatibility *)
    hardline
    ^^ hardline
    ^^ !^"#include <cn-smt/prelude.h>"
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
    ^^ hardline
    ^^ separate (hardline ^^ hardline) functions
end
