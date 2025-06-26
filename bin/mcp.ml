module YB = Yojson.Basic

(** Basic MCP message types *)
type mcp_request =
  { id : int;
    method_ : string;
    params : YB.t option
  }

let parse_request json =
  let open YB.Util in
  { id = json |> member "id" |> to_int;
    method_ = json |> member "method" |> to_string;
    params = json |> member "params" |> to_option (fun x -> x)
  }


type mcp_notif = { method_ : string }

let parse_notif json =
  let open YB.Util in
  { method_ = json |> member "method" |> to_string }


let json_of_result id result =
  `Assoc [ ("jsonrpc", `String "2.0"); ("id", `Int id); ("result", result) ]


let json_of_error id code ?(data : string option) message =
  `Assoc
    [ ("jsonrpc", `String "2.0");
      ("id", `Int id);
      ( "error",
        `Assoc
          ([ ("code", `Int code); ("message", `String message) ]
           @ match data with Some d -> [ ("data", `String d) ] | None -> []) )
    ]


(** Tools available via MCP *)
let available_tools =
  `List
    [ `Assoc
        [ ("name", `String "wellFormedness");
          ( "description",
            `String
              "Given a C file (and optionally a list of function names), runs CN's \
               well-formedness check, which finds errors such as, ill-typing CN \
               definitions (predicates, specifications, lemmas) and ill-formed recursion \
               in datatypes. If given just a filename, all functions will be checked. \
               Otherwise, only the functions listed will be checked. NOTE: this tool \
               does not prove correctness, you need `verifier` for that." );
          ( "inputSchema",
            `Assoc
              [ ("type", `String "object");
                ( "properties",
                  `Assoc
                    [ ("filename", `Assoc [ ("type", `String "string") ]);
                      ( "functions",
                        `Assoc
                          [ ("type", `String "array");
                            ("items", `Assoc [ ("type", `String "string") ])
                          ] )
                    ] );
                ("required", `List [ `String "filename" ])
              ] );
          ( "annotations",
            `Assoc
              [ ("title", `String "CN well-formedness check");
                ("destructiveHint", `Bool false);
                ("idempotentHint", `Bool true)
              ] )
        ];
      `Assoc
        [ ("name", `String "verifier");
          ( "description",
            `String
              "Given a C file (and optionally a list of function names), use the CN \
               verifier to try and prove correctness. If given just a filename, it will \
               try to prove correctness of all functions not marked with `trusted`. \
               Otherwise, only the functions listed will be checked." );
          ( "inputSchema",
            `Assoc
              [ ("type", `String "object");
                ( "properties",
                  `Assoc
                    [ ("filename", `Assoc [ ("type", `String "string") ]);
                      ( "functions",
                        `Assoc
                          [ ("type", `String "array");
                            ("items", `Assoc [ ("type", `String "string") ])
                          ] )
                    ] );
                ("required", `List [ `String "filename" ])
              ] );
          ( "annotations",
            `Assoc
              [ ("title", `String "CN verification");
                ("destructiveHint", `Bool false);
                ("idempotentHint", `Bool true)
              ] )
        ];
      `Assoc
        [ ("name", `String "instrumentAndRun");
          ( "description",
            `String
              "Given a C file (and optionally a list of function names), instrument the \
               file to check CN specifications and run it. If given just a filename, all \
               functions with specs will be have their specs checked at runtime. \
               Otherwise, only the functions listed will have their specs checked at \
               runtime. The file must have a `main` function for this to do anything." );
          ( "inputSchema",
            `Assoc
              [ ("type", `String "object");
                ( "properties",
                  `Assoc
                    [ ("filename", `Assoc [ ("type", `String "string") ]);
                      ( "functions",
                        `Assoc
                          [ ("type", `String "array");
                            ("items", `Assoc [ ("type", `String "string") ])
                          ] )
                    ] );
                ("required", `List [ `String "filename" ])
              ] );
          ( "annotations",
            `Assoc
              [ ("title", `String "Run with CN instrumentation");
                ("destructiveHint", `Bool false);
                ("idempotentHint", `Bool false)
              ] )
        ];
      `Assoc
        [ ("name", `String "randomTesting");
          ( "description",
            `String
              "Given a C file (and optionally a list of function names), use \
               specification-based random testing. If given just a filename, as many \
               functions as possible will be tested. Otherwise, only the functions \
               listed will be tested." );
          ( "inputSchema",
            `Assoc
              [ ("type", `String "object");
                ( "properties",
                  `Assoc
                    [ ("filename", `Assoc [ ("type", `String "string") ]);
                      ( "functions",
                        `Assoc
                          [ ("type", `String "array");
                            ("items", `Assoc [ ("type", `String "string") ])
                          ] )
                    ] );
                ("required", `List [ `String "filename" ])
              ] );
          ( "annotations",
            `Assoc
              [ ("title", `String "CN random testing");
                ("destructiveHint", `Bool false);
                ("idempotentHint", `Bool false)
              ] )
        ]
    ]


(** Handle different MCP methods *)
let handle_request (req : mcp_request) =
  let open YB.Util in
  match req.method_ with
  | "ping" -> Lwt.return (json_of_result req.id (`Assoc []))
  | "initialize" ->
    let result =
      `Assoc
        [ ("protocolVersion", `String "2025-03-26");
          ("capabilities", `Assoc [ ("tools", `Assoc [ ("listChanged", `Bool false) ]) ]);
          ( "serverInfo",
            `Assoc
              [ ("name", `String "CN MCP Server");
                ("version", `String Common.version_str)
              ] )
        ]
    in
    Lwt.return (json_of_result req.id result)
  | "tools/list" ->
    let result = `Assoc [ ("tools", available_tools) ] in
    Lwt.return (json_of_result req.id result)
  | "tools/call" ->
    let toolName = req.params |> Option.get |> member "name" |> to_string in
    let args = req.params |> Option.get |> member "arguments" in
    (match toolName with
     | "wellFormedness" ->
       let cmd =
         "cn wf "
         ^ (args |> member "filename" |> to_string)
         ^ (match args |> member "functions" with
            | `List functions when not (List.is_empty functions) ->
              "--only=" ^ String.concat "," (List.map to_string functions)
            | _ -> "")
         ^ " 2>&1"
       in
       let inp = Unix.open_process_in cmd in
       let result = In_channel.input_all inp in
       In_channel.close inp;
       let resp =
         `Assoc
           [ ( "content",
               `List [ `Assoc [ ("type", `String "text"); ("text", `String result) ] ] );
             ("isError", `Bool false)
           ]
       in
       Lwt.return (json_of_result req.id resp)
     | "verifier" ->
       let cmd =
         "cn verify "
         ^ (args |> member "filename" |> to_string)
         ^ (match args |> member "functions" with
            | `List functions when not (List.is_empty functions) ->
              "--only=" ^ String.concat "," (List.map to_string functions)
            | _ -> "")
         ^ " 2>&1"
       in
       let inp = Unix.open_process_in cmd in
       let result = In_channel.input_all inp in
       In_channel.close inp;
       let resp =
         `Assoc
           [ ( "content",
               `List [ `Assoc [ ("type", `String "text"); ("text", `String result) ] ] );
             ("isError", `Bool false)
           ]
       in
       Lwt.return (json_of_result req.id resp)
     | "instrumentAndRun" ->
       let cmd =
         "cn instrument --run --tmp "
         ^ (args |> member "filename" |> to_string)
         ^ (match args |> member "functions" with
            | `List functions when not (List.is_empty functions) ->
              "--only=" ^ String.concat "," (List.map to_string functions)
            | _ -> "")
         ^ " 2>&1"
       in
       let inp = Unix.open_process_in cmd in
       let result = In_channel.input_all inp in
       In_channel.close inp;
       let resp =
         `Assoc
           [ ( "content",
               `List [ `Assoc [ ("type", `String "text"); ("text", `String result) ] ] );
             ("isError", `Bool false)
           ]
       in
       Lwt.return (json_of_result req.id resp)
     | "randomTesting" ->
       let cmd =
         "cn test --progress-level=function "
         ^ (args |> member "filename" |> to_string)
         ^ (match args |> member "functions" with
            | `List functions when not (List.is_empty functions) ->
              "--only=" ^ String.concat "," (List.map to_string functions)
            | _ -> "")
         ^ " 2>&1"
       in
       let inp = Unix.open_process_in cmd in
       let result = In_channel.input_all inp in
       In_channel.close inp;
       let resp =
         `Assoc
           [ ( "content",
               `List [ `Assoc [ ("type", `String "text"); ("text", `String result) ] ] );
             ("isError", `Bool false)
           ]
       in
       Lwt.return (json_of_result req.id resp)
     | toolName ->
       output_string
         stderr
         ("Invalid request: " ^ YB.pretty_to_string (Option.get req.params));
       flush stderr;
       let err = json_of_error req.id (-32602) ("Unknown tool: " ^ toolName) in
       Lwt.return err)
  | _ ->
    let err = json_of_error req.id (-32601) "Method not found" in
    Lwt.return err


(** Handle different MCP notifications *)
let handle_notif (notif : mcp_notif) = match notif.method_ with _ -> ()

(** Start MCP server *)
let mcp () =
  let rec main_loop () =
    let open Lwt.Infix in
    Lwt_io.read_line_opt Lwt_io.stdin
    >>= function
    | Some line ->
      (try
         let json = YB.from_string line in
         if YB.equal (YB.Util.member "id" json) `Null then (
           (* Is a notification *)
           handle_notif (parse_notif json);
           main_loop ())
         else (
           let req = parse_request json in
           handle_request req
           >>= fun resp_json ->
           let resp_str = YB.to_string resp_json in
           Lwt_io.write_line Lwt_io.stdout resp_str
           >>= fun () -> Lwt_io.flush Lwt_io.stdout >>= fun () -> main_loop ())
       with
       | Yojson.Json_error exn ->
         let err = json_of_error (-1) (-32700) ~data:exn "Parse error" in
         let err_str = YB.to_string err in
         Lwt_io.write_line Lwt_io.stdout err_str
         >>= fun () -> Lwt_io.flush Lwt_io.stdout >>= fun () -> main_loop ())
    | None -> Lwt.return_unit
  in
  Lwt_main.run (main_loop ())


open Cmdliner

module Flags = struct end

let cmd =
  let open Term in
  let mcp_t = const mcp $ const () in
  let doc =
    "Starts a model context protocol (MCP) server exposing CN functionality. Offers \
     tools for testing and verifying C functions with CN specifications."
  in
  let info = Cmd.info "mcp" ~doc in
  Cmd.v info mcp_t
