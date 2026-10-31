(* Validate the outcome-bearing part of Austen 0.0.1's schema-v1 report.
   A process exit alone cannot establish that a campaign exercised its target. *)

exception Invalid_report of string

let invalid format =
  Printf.ksprintf (fun message -> raise (Invalid_report message)) format


let object_at path = function
  | `Assoc fields ->
    let names = List.map fst fields in
    if List.length names <> List.length (List.sort_uniq String.compare names) then
      invalid "%s contains duplicate fields" path;
    fields
  | _ -> invalid "%s must be an object" path


let field fields name =
  match List.assoc_opt name fields with
  | Some value -> value
  | None -> invalid "missing field %s" name


let string_at path = function
  | `String value -> value
  | _ -> invalid "%s must be a string" path


let bool_at path = function
  | `Bool value -> value
  | _ -> invalid "%s must be a boolean" path


let uint64_max = Z.pred (Z.shift_left Z.one 64)

let counter_at path value =
  let count =
    match value with
    | `Int value -> Z.of_int value
    | `Intlit value ->
      (try Z.of_string value with
       | Invalid_argument _ -> invalid "%s must be a nonnegative integer" path)
    | _ -> invalid "%s must be a nonnegative integer" path
  in
  if Z.sign count < 0 || Z.gt count uint64_max then
    invalid "%s must be a nonnegative 64-bit integer" path;
  count


let expect_string fields name expected =
  let actual = string_at name (field fields name) in
  if not (String.equal actual expected) then
    invalid "%s is %S, expected %S" name actual expected


let expect_bool fields name expected =
  let actual = bool_at name (field fields name) in
  if not (Bool.equal actual expected) then
    invalid "%s is %b, expected %b" name actual expected


let counter ?(prefix = "") fields name = counter_at (prefix ^ name) (field fields name)

let at_most name left right =
  if Z.gt left right then invalid "%s counters are inconsistent" name


let validate_completed
      ~engine
      ~fn
      ~duration
      ~seed
      ~check_heap_ownership
      ~process_exit
      json
  =
  let report = object_at "report" json in
  if not (Z.equal (counter report "schema_version") Z.one) then
    invalid "unsupported schema_version (expected 1)";
  expect_string report "status" "ok";
  expect_string report "stage" "complete";
  (match field report "error" with
   | `Null -> ()
   | _ -> invalid "a completed report must have no error");
  expect_string report "function" fn;
  expect_string report "engine" engine;
  expect_string report "budget" "seconds";
  if not (Z.equal (counter report "duration") (Z.of_int duration)) then
    invalid "duration does not match the requested campaign budget";
  Option.iter
    (fun requested ->
       if not (Z.equal (counter report "seed") (Z.of_string requested)) then
         invalid "seed does not match the requested campaign seed")
    seed;
  expect_bool report "check_heap_ownership" check_heap_ownership;
  expect_bool report "effective_check_heap_ownership" check_heap_ownership;
  let seen = counter report "seen" in
  let selected = counter report "selected" in
  let dispatched = counter report "dispatched" in
  if Z.equal dispatched Z.zero then invalid "the campaign dispatched zero inputs";
  at_most "dispatched/selected" dispatched selected;
  at_most "selected/seen" selected seen;
  let oracle = object_at "oracle" (field report "oracle") in
  let oracle_counter = counter ~prefix:"oracle." oracle in
  let parse_rejected = oracle_counter "parse_rejected" in
  let pre_passed = oracle_counter "pre_passed" in
  let pre_failed = oracle_counter "pre_failed" in
  let pre_unsupported = oracle_counter "pre_unsupported" in
  let pre_internal_error = oracle_counter "pre_internal_error" in
  let post_unsupported = oracle_counter "post_unsupported" in
  if not (Z.equal pre_unsupported Z.zero && Z.equal post_unsupported Z.zero) then
    invalid "the oracle reported unsupported results";
  if not (Z.equal pre_internal_error Z.zero) then
    invalid "the oracle reported internal errors";
  at_most "dispatched/passed precondition" dispatched pre_passed;
  let observed = Z.add parse_rejected (Z.add pre_passed pre_failed) in
  at_most "oracle/selected" observed selected;
  let findings = object_at "findings" (field report "findings") in
  expect_string findings "unit" "crashing-executions";
  let confirmed = counter ~prefix:"findings." findings "confirmed" in
  let post_failed = counter ~prefix:"findings." findings "post_failed" in
  (match field findings "capped" with
   | `Bool _ | `Null -> ()
   | _ -> invalid "findings.capped must be a boolean or null");
  if not (Z.equal (counter report "crashes") confirmed) then
    invalid "crashes and findings.confirmed disagree";
  at_most "postcondition failures/confirmed findings" post_failed confirmed;
  at_most "confirmed findings/dispatched" confirmed dispatched;
  let expected_exit = if Z.equal confirmed Z.zero then 0 else 1 in
  if not (Z.equal (counter report "exit") (Z.of_int process_exit)) then
    invalid "report exit does not match the Austen process status";
  if process_exit <> expected_exit then
    invalid "Austen process status does not match the confirmed findings";
  expected_exit


let validate ~path ~engine ~fn ~duration ~seed ~check_heap_ownership status =
  match status with
  | Unix.WEXITED 130 -> Ok 130
  | Unix.WSIGNALED signal ->
    Error (Printf.sprintf "Austen was killed by signal %d" signal)
  | Unix.WSTOPPED signal ->
    Error (Printf.sprintf "Austen was stopped by signal %d" signal)
  | Unix.WEXITED ((0 | 1) as process_exit) ->
    (try
       let json = Yojson.Safe.from_file path in
       Ok
         (validate_completed
            ~engine
            ~fn
            ~duration
            ~seed
            ~check_heap_ownership
            ~process_exit
            json)
     with
     | Invalid_report message -> Error ("invalid Austen report: " ^ message)
     | Yojson.Json_error message -> Error ("malformed Austen report: " ^ message)
     | Sys_error message -> Error ("cannot read Austen report: " ^ message)
     | Unix.Unix_error (err, _, _) ->
       Error ("cannot read Austen report: " ^ Unix.error_message err))
  | Unix.WEXITED code -> Error (Printf.sprintf "Austen exited with status %d" code)
