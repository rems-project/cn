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
    (* TODO: should be int64 *)
    target : int;
    assignments : assignment list
  }

type annot = Focus of focus

let print_focus_suggestion filename line w0 coeffs =
  let pos = filename ^ ":" ^ string_of_int line in
  let body =
    List.filter_map
      (fun (v, c) ->
         match c with 0 -> None | 1 -> Some v | c -> Some (string_of_int c ^ " * " ^ v))
      coeffs
  in
  let elems = if w0 = 0 then body else string_of_int w0 :: body in
  let sum = String.concat " + " elems in
  Pp.(
    debug
      10
      (lazy
        (item "AutoAnnot: suggested annotation" (string pos ^^ string ": " ^^ string sum))))


let generate_focus_annot_aux
      filename
      line
      (assignments_list : (int * assignment list) list)
  : unit
  =
  Pp.(
    debug
      10
      (lazy
        (item
           "Generating annotations"
           (string filename ^^ string ":" ^^ (line |> string_of_int |> string)))));
  let _, first = List.hd assignments_list in
  let variables =
    first |> List.map (fun a -> a.accessor) |> List.sort_uniq String.compare
  in
  (* Sanity: all occurrences have the same variable set *)
  let all_unique =
    List.for_all
      (fun x ->
         List.fold_left
           (fun acc y -> if String.equal x y then acc + 1 else acc)
           0
           variables
         = 1)
      variables
  in
  if not all_unique then
    failwith "AutoAnnot: inconsistent environment";
  (* Create a template *)
  (* like ax + by + c = 0, and try to find a, b, and c *)
  (* For [[x = 1; y = 2]; [ x = 2; y = 4]] *)
  (* generate a * 1 + b * 2 + c = 0 /\ a * 2 + b * 4 + c = 0 *)
  (* and find a, b, and c that satisfy the above using Z3 Optimize *)
  let open Z3 in
  let ctx = Z3.mk_context [ ("model", "true") ] in
  let opt = Optimize.mk_opt ctx in
  let i_k n = Arithmetic.Integer.mk_numeral_i ctx n in
  let mk_int name = Arithmetic.Integer.mk_const_s ctx name in
  let add = Arithmetic.mk_add ctx in
  let mul x y = Arithmetic.mk_mul ctx [ x; y ] in
  let neg x = Arithmetic.mk_unary_minus ctx x in
  let ge = Arithmetic.mk_ge ctx in
  let eq = Boolean.mk_eq ctx in
  (* constant *)
  let w0 = mk_int "w0" in
  (* coefficients *)
  let coeffs = List.map (fun v -> (v, mk_int ("w_" ^ v))) variables in
  (* For each sample env, assert: w0 + sum(w_v * val_v) = 0 *)
  let value_of v (env : assignment list) =
    (List.find (fun a -> String.equal a.accessor v) env).value
  in
  let sum_terms env =
    let terms = List.map (fun (v, wv) -> mul wv (i_k (value_of v env))) coeffs in
    add (w0 :: terms)
  in
  List.iter
    (fun (target, env) -> Optimize.add opt [ eq (sum_terms env) (i_k target) ])
    assignments_list;
  (* L1 norm: minimize t_v where t_v >= w_v and t_v >= -w_v*)
  let ts =
    List.map
      (fun (v, wv) ->
         let tv = mk_int ("t_" ^ v) in
         Optimize.add opt [ ge tv wv; ge tv (neg wv) ];
         tv)
      coeffs
  in
  let objective = add ts in
  ignore (Optimize.minimize opt objective : Optimize.handle);
  (* Solve and read model *)
  match Optimize.check opt with
  | Solver.SATISFIABLE ->
    let m = Optimize.get_model opt in
    let get_int e =
      let m = match m with Some m -> m | None -> failwith "No model found" in
      match Model.eval m e true with
      | None -> 0
      | Some v -> int_of_string (Expr.to_string v)
    in
    let w0_v = get_int w0 in
    let coeff_vals = List.map (fun (v, wv) -> (v, get_int wv)) coeffs in
    print_focus_suggestion filename line w0_v coeff_vals
  | Solver.UNSATISFIABLE | Solver.UNKNOWN ->
    Pp.(
      debug
        5
        (lazy
          (item
             "AutoAnnot: no relation found"
             (string (filename ^ ":" ^ string_of_int line)))))


let generate_focus_annot (annots : focus list) : unit =
  let tbl : (string * int, (int * assignment list) list) Hashtbl.t = Hashtbl.create 16 in
  let add filename line assigns =
    let key = (filename, line) in
    let prev = match Hashtbl.find_opt tbl key with Some xs -> xs | None -> [] in
    Hashtbl.replace tbl key (prev @ [ assigns ])
  in
  List.iter
    (function
      | { filename; line; assignments; target } -> add filename line (target, assignments))
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
                |> List.map (fun p ->
                  match String.split_on_char '=' p |> List.map String.trim with
                  | [ key; vstr ] ->
                    let v = int_of_string vstr in
                    { accessor = key; value = v }
                  | _ -> failwith "ill-formed")
              in
              (* assignments must be non-empty; as the first element is the target*)
              let fst = List.hd assignments in
              assert (String.equal fst.accessor "!index");
              let assignments = List.tl assignments in
              Some (Focus { filename; line; assignments; target = fst.value }))
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
