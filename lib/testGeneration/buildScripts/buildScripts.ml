let generate_and_save ~output_dir ~filename (tool : TestGenConfig.build_tool) =
  match tool with
  | Bash -> Bash.generate_and_save ~output_dir ~filename
  | Make -> Make.generate_and_save ~output_dir ~filename
