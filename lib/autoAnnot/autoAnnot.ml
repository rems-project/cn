module CF = Cerb_frontend
module A = CF.AilSyntax

let log_filename = ref "cn_auto_annot.log"

let get_log_filename filename =
  Filename.(remove_extension (basename filename)) ^ ".autoannot.log"


let trim (s : string) : string =
  let is_space = function ' ' | '\t' | '\r' | '\n' -> true | _ -> false in
  let len = String.length s in
  let i = ref 0 in
  let j = ref (len - 1) in
  while !i < len && is_space s.[!i] do
    incr i
  done;
  while !j >= !i && is_space s.[!j] do
    decr j
  done;
  if !i > !j then "" else String.sub s !i (!j - !i + 1)


type assignment =
  { accessor : string;
    value : int
  }

type focus =
  { filename : string;
    line : int;
    assignments : assignment list
  }

type annot = Focus of focus

let parse (log_file : string) : annot list =
  (* Open the file *)
  let ic = open_in log_file in
  let prefix = "[auto annot (focus)]" in
  let split_and_trim ch s = String.split_on_char ch s |> List.map trim in
  let parse_line (line : string) : annot option =
    if not (String.starts_with ~prefix line) then
      None
    else (
      let rest =
        String.sub line (String.length prefix) (String.length line - String.length prefix)
        |> trim
      in
      match split_and_trim ',' rest |> List.filter (fun s -> not (String.equal s "")) with
      | [] -> None
      | loc :: assigns_parts ->
        (match split_and_trim ':' loc with
         | filename :: line_str :: _ ->
           (match int_of_string_opt line_str with
            | None -> None
            | Some line ->
              let assignments =
                assigns_parts
                |> List.filter_map (fun p ->
                  match String.split_on_char '=' p |> List.map trim with
                  | [ key; vstr ] when not (String.equal key "") ->
                    (match int_of_string_opt vstr with
                     | Some v -> Some { accessor = key; value = v }
                     | None -> None)
                  | _ -> None)
              in
              Some (Focus { filename; line; assignments }))
         | _ -> None))
  in
  let rec loop res =
    match input_line ic with
    | line ->
      let res = match parse_line line with None -> res | Some f -> f :: res in
      loop res
    | exception End_of_file -> res
  in
  let res =
    try loop [] with
    | e ->
      close_in_noerr ic;
      raise e
  in
  close_in ic;
  res


let run_autoannot (log_file : string) : unit =
  let _data = parse log_file in
  ()
