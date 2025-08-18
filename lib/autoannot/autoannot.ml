module CF = Cerb_frontend
module A = CF.AilSyntax

let log_filename = ref "cn_auto_annot.log"

let run_autoannot
      ~_output_dir
      ~_filename
      (_cabs_tunit : CF.Cabs.translation_unit)
      (_sigma : Cerb_frontend.GenTypes.genTypeCategory Cerb_frontend.AilSyntax.sigma)
      (_prog5 : unit Mucore.file)
  : int
  =
  0
