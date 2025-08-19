module CF = Cerb_frontend
module A = CF.AilSyntax

let log_filename = ref "cn_auto_annot.log"

let get_log_filename filename =
  Filename.(remove_extension (basename filename)) ^ ".autoannot.log"


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

let generate_focus_annot_aux filename line assignments_list : unit =
  Pp.(
    debug
      10
      (lazy
        (item
           "Generating annotations"
           (string filename ^^ string ":" ^^ (line |> string_of_int |> string)))));
  let first = List.hd assignments_list in
  let variables =
    first
    |> List.map (fun (a : assignment) -> a.accessor)
    |> List.sort_uniq String.compare
  in
  (* Sanity check: all occurrences have the same variable set *)
  let all_unique =
    List.for_all (fun x -> not (List.exists (String.equal x) variables)) variables
  in
  if not all_unique then
    failwith "AutoAnnot: inconsistent environments";
  (* Build values per variable across occurrences *)
  let tbl : (string, int list) Hashtbl.t = Hashtbl.create (List.length variables) in
  List.iter (fun v -> Hashtbl.replace tbl v []) variables;
  let add_value v n = Hashtbl.replace tbl v (n :: Hashtbl.find tbl v) in
  let lookup_value v env = (List.find (fun a -> String.equal a.accessor v) env).value in
  List.iter
    (fun env -> List.iter (fun v -> lookup_value v env |> add_value v) variables)
    assignments_list


let generate_focus_annot (annots : focus list) : unit =
  let tbl : (string * int, assignment list list) Hashtbl.t = Hashtbl.create 16 in
  let add filename line assigns =
    let key = (filename, line) in
    let prev = match Hashtbl.find_opt tbl key with Some xs -> xs | None -> [] in
    Hashtbl.replace tbl key (prev @ [ assigns ])
  in
  List.iter
    (function { filename; line; assignments } -> add filename line assignments)
    annots;
  Hashtbl.iter
    (fun (filename, line) assigns -> generate_focus_annot_aux filename line assigns)
    tbl


let parse (log_file : string) : annot list =
  (* Open the file *)
  let ic = open_in log_file in
  let prefix = "[auto annot (focus)]" in
  let split_and_trim ch s = String.split_on_char ch s |> List.map String.trim in
  let parse_line (line : string) : annot option =
    if not (String.starts_with ~prefix line) then
      None
    else (
      let rest =
        String.sub line (String.length prefix) (String.length line - String.length prefix)
        |> String.trim
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
                  match String.split_on_char '=' p |> List.map String.trim with
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
  Pp.(debug 10 (lazy (item "Running auto-annotation" (string log_file))));
  let data = parse log_file in
  let focus_annots = data |> List.filter_map (function Focus f -> Some f) in
  generate_focus_annot focus_annots
