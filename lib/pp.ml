module CF = Cerb_frontend
include PPrint

module Infix = struct
  let ( ^^ ) = ( ^^ )

  let ( !^ ) = ( !^ )

  let ( ^/^ ) = ( ^/^ )

  let ( ^//^ ) = ( ^//^ )

  let ( ^^^ ) = Cerb_pp_prelude.( ^^^ )
end

include Infix

(* copying from backend.ml *)
let term_col =
  match Cerb_util.terminal_size () with Some (_, col) -> col - 1 | _ -> 80 - 1


type loc_pp =
  | Hex
  | Dec

let loc_pp = ref Dec

let int i = string (string_of_int i)

let int64 i = string (Int64.to_string i)

let z i = string (Z.to_string i)

let bool b = if b then string "true" else string "false"

let html_escapes = ref false

let unicode = ref true

let print_level = ref 0

let print_timestamps = ref true

let html_langle = !^"&lt;"

let html_rangle = !^"&gt;"

let langle () = if !html_escapes then html_langle else PPrint.langle

let rangle () = if !html_escapes then html_rangle else PPrint.rangle

let angles doc = langle () ^^ doc ^^ rangle ()

(* from run_pp *)
let print channel doc =
  PPrint.ToChannel.pretty 1.0 term_col channel (doc ^^ hardline);
  flush channel


(* adapting from pipeline.ml *)
let print_file filename doc =
  let oc = open_out filename in
  print oc doc;
  close_out oc


let plain = CF.Pp_utils.to_plain_pretty_string

let ( ^^^ ) = Cerb_pp_prelude.( ^^^ )

let format_string format str = Cerb_colour.ansi_format format str

let format format string =
  let n = String.length string in
  fancystring (format_string format string) n


let uformat format string n = fancystring (format_string format string) n

type alignment =
  | L
  | R

let pad_ alignment should_width has_width pp =
  let diff = should_width - has_width in
  if diff < 0 then
    pp
  else (
    match alignment with L -> pp ^^ repeat diff space | R -> repeat diff space ^^ pp)


let pad alignment width pp = pad_ alignment width (requirement pp) pp

let pad_string_ alignment should_width has_width pp =
  let diff = should_width - has_width in
  if diff < 0 then
    pp
  else (
    match alignment with L -> pp ^ String.make diff ' ' | R -> String.make diff ' ' ^ pp)


let pad_string alignment width pp = pad_string_ alignment width (String.length pp) pp

let list f l = match l with [] -> !^"(empty)" | l -> flow_map (comma ^^ break 1) f l

(* let commas l = list (fun pp -> pp) l *)

let list_filtered f l =
  match List.filter_map f l with [] -> !^"(empty)" | l -> flow (comma ^^ break 1) l


let option f none_msg opt = match opt with None -> !^none_msg | Some v -> f v

(* let nats n = let rec aux n = if n < 0 then [] else n :: aux (n - 1) in List.rev (aux
   n) *)

(* module IntMap = Map.Make(Int) *)

let typ n typ = n ^^^ colon ^^^ typ

let infix_arrow x y = x ^^^ !^"->" ^^^ y

let item item content = format [ Bold ] item ^^ colon ^^ hardline ^^ content

let c_comment pp = !^"/*" ^^ pp ^^ !^"*/"

let c_app f args =
  group @@ f ^^ group @@ parens @@ nest 2 @@ flow (break 0 ^^ comma ^^ space) args


let ineq x y = x ^^^ !^"!=" ^^^ y

let headline a =
  (if !print_level >= 2 then hardline else empty) ^^ format [ Bold; Magenta ] ("# " ^ a)


let bold a = format [ Bold ] a

let action a = format [ Cyan ] ("## " ^ a ^ " ")

let debug l pp =
  if !print_level >= l then (
    let pp1 = Lazy.force pp in
    let pp2 =
      if !print_timestamps then (
        let time = Sys.time () in
        format [ Green ] ("[" ^ Printf.sprintf "%.6f" time ^ "] ") ^^ pp1)
      else
        pp1
    in
    print stderr pp2)


let warn_noloc pp = print stderr (format [ Bold; Yellow ] "Warning:" ^^^ pp)

let times = ref (None : (out_channel * string) option)

let time_start () = Unix.gettimeofday ()

let time_end descr ?(info1 = "") ?(info2 = lazy PPrint.empty) start_time =
  let end_time = Unix.gettimeofday () in
  let diff = end_time -. start_time in
  match !times with
  | Some (channel, "csv") ->
    Printf.fprintf
      channel
      "%s; %s; \"%s\"; %.6f\n"
      descr
      info1
      (plain (Lazy.force info2))
      diff
  | _ -> ()


let maybe_open_times_channel = function
  | None -> ()
  | Some (filename, style) ->
    let channel = open_out filename in
    times := Some (channel, style);
    if String.equal style "csv" then (
      Printf.fprintf channel "sep=;\n";
      Printf.fprintf channel "description; information; extra information; time\n")
    else
      warn_noloc !^"unknown time-logging style -- not logging"


let maybe_close_times_channel () =
  match !times with
  | None -> ()
  | Some (channel, _) ->
    flush channel;
    close_out channel


(* stealing some logic from pp_errors *)
let error (loc : Locations.t) (msg : document) extras =
  let head, pos = Locations.head_pos_of_location loc in
  print
    stderr
    (format [ Bold ] head
     ^^^ format [ Bold; Red ] "error:"
     ^^^ format [ Bold ]
     @@ plain msg);
  if Locations.is_unknown_location loc then () else print stderr !^pos;
  List.iter (fun pp -> print stderr pp) extras


(* stealing some logic from pp_errors *)
let warn (loc : Locations.t) msg =
  let head, pos = Locations.head_pos_of_location loc in
  print stderr (format [ Bold ] head ^^^ format [ Bold; Yellow ] "warning:" ^^^ msg);
  if Locations.is_unknown_location loc then () else print stderr !^pos


let loc_headline (loc : Locations.t) msg =
  let head, _pos = Locations.head_pos_of_location loc in
  format [ Bold ] head ^^^ msg


(* stealing from Cerb_debug *)
let json_output_channel = ref None

let maybe_open_json_output mfile =
  match mfile with
  | None -> json_output_channel := None
  | Some file ->
    let oc = open_out file in
    json_output_channel := Some oc;
    output_string oc "[\n"


let maybe_close_json_output () =
  match !json_output_channel with
  | None -> ()
  | Some oc ->
    output_string oc "\n]";
    json_output_channel := None;
    close_out oc


let print_json =
  let first = ref true in
  fun json ->
    match !json_output_channel with
    | Some oc ->
      if !first then first := false else output_string oc ",\n";
      Yojson.Safe.pretty_to_channel ~std:true oc (Lazy.force json);
      output_char oc '\n'
    | _ -> ()


let progress_simple title name =
  let msg = format [ Blue ] title ^^ colon ^^^ !^name in
  print stdout msg


let of_total cur total = Printf.sprintf "[%d/%d]" cur total

let document_to_yojson (doc : document) : Yojson.Safe.t =
  let buf_size =
    1024
    (* chosen pretty arbitrarily *)
  in
  let buf = Stdlib.Buffer.create buf_size in
  PPrint.ToBuffer.compact buf doc;
  let str = Stdlib.Buffer.contents buf in
  `String str


let document_of_yojson (json : Yojson.Safe.t) : (document, string) Result.t =
  match json with
  | `String str -> Ok (PPrint.arbitrary_string str)
  | _ ->
    Error ("document_of_yojson: expected `String, found " ^ Yojson.Safe.to_string json)


let string str =
  let amp = Str.regexp "&" in
  if !html_escapes then (
    let str = Str.global_replace amp "&amp;" str in
    string str)
  else
    string str
