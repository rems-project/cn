module CF = Cerb_frontend
module A = CF.AilSyntax
module C = CF.Ctype

let use_preproc_loc = ref true

module Pos : sig
  type t = private
    { line : int; (* 1 based *)
      col : int (* 1 based *)
    }

  val compare : t -> t -> int

  val v : int -> int -> t

  val initial : t

  val newline : t -> t

  val offset_col : off:int -> t -> (t, string) result

  val increment_line : t -> int -> t

  val of_location : Cerb_location.t -> (t * t, string) result

  val to_string : t -> string [@@warning "-unused-value-declaration"]
end = struct
  type t =
    { line : int;
      col : int
    }

  let compare pos1 pos2 = Stdlib.compare (pos1.line, pos1.col) (pos2.line, pos2.col)

  let to_string pos = Printf.sprintf "{line: %d, col: %d}" pos.line pos.col

  let v line col = { line; col }

  let initial = v 1 1

  (* Go to the beginning of the next line *)
  let newline pos = { line = pos.line + 1; col = 1 }

  let offset_col ~off pos =
    if pos.col + off < 0 then
      Error (__LOC__ ^ ": pos.col < off")
    else
      Ok { pos with col = pos.col + off }


  (* Go down to the next line (does not affect column) *)
  let increment_line pos n = { pos with line = pos.line + n }

  let of_location loc =
    (* if not (Cerb_location.from_main_file loc) then Printf.fprintf stderr
       "\x1b[31mHEADER LOC: %s\x1b[0m\n" (Option.value ~default:"<unknown>"
       (Cerb_location.get_filename loc)) ; *)
    let to_cart =
      if !use_preproc_loc then
        Cerb_location.to_cartesian_raw
      else
        Cerb_location.to_cartesian_user
    in
    match to_cart loc with
    | None -> Error (__LOC__ ^ ": failed to get line/col positions")
    | Some ((start_line, start_col), (end_line, end_col)) ->
      Ok (v (start_line + 1) (start_col + 1), v (end_line + 1) (end_col + 1))
end

type state =
  { input : Stdlib.in_channel;
    output : Stdlib.out_channel;
    current_pos : Pos.t;
    last_indent : int
  }

type precedence =
  | Bot
  (* Cartesian (x, y): ordered in reverse lexicographic order
   (larger x/y ==> smaller in ordering, i.e. higher precedence) *)
  | Cartesian of (int * int)

let compare_precedence p1 p2 =
  match (p1, p2) with
  | Bot, Bot -> 0
  | Bot, Cartesian _ -> -1
  | Cartesian _, Bot -> 1
  | Cartesian (x1, y1), Cartesian (x2, y2) ->
    let cmp = Stdlib.compare x1 x2 in
    if cmp = 0 then -Stdlib.compare y1 y2 else -cmp


let ident_of_line str =
  let rec aux i =
    match String.get str i with
    | ' ' | '\t' -> aux (i + 1)
    | (exception Invalid_argument _) | _ -> i
  in
  aux 0


let with_debug = false

let decorate_indent str =
  if with_debug then
    "\x1b[30;41m" ^ str ^ "\x1b[0m"
  else
    str


let decorate_injection str =
  if with_debug then
    "\x1b[33m" ^ str ^ "\x1b[0m"
  else
    str


(* TODO: why isn't no_ident used? *)
(* let [@warning "-27"] move_to_line ?(print=true) ?(no_ident=false) st line = assert
   (line > 0); assert (line >= st.current_pos.line); let rec aux st n = if n = 0 then st
   else match Stdlib.input_line st.input with | str -> if print then begin
   Stdlib.output_string st.output (str ^ "\n"); end; aux { st with current_pos=
   Pos.newline st.current_pos } (n-1) | exception End_of_file -> begin Printf.fprintf
   stderr "st.line= %d --> line= %d [n: %d]\n" st.current_pos.line line n; failwith "end
   of file" end in aux st (line - st.current_pos.line) *)

let move_to ?(print = true) ?(no_ident = false) st pos =
  let open Pos in
  (* Printf.fprintf stderr "MOVE_TO[%s] [off: %d] current_pos: %s --> %s\n" (if print then
     "print" else "\x1b[33m skip\x1b[0m") (Stdlib.pos_in st.input) (Pos.to_string
     st.current_pos) (Pos.to_string pos); *)
  assert (pos.line > 0);
  assert (pos.line >= st.current_pos.line);
  (* if not (pos.line >= st.current_pos.line) then begin Printf.fprintf stderr "pos.line:
     %d -- current_pos.line: %d\n" pos.line st.current_pos.line; failwith "BOOM" end ; *)
  let ident_of_line st str =
    if st.current_pos.col > 1 then
      st.last_indent
    else
      ident_of_line str
  in
  let rec aux st =
    if st.current_pos.line = pos.line then (
      let len = pos.col - st.current_pos.col in
      let str =
        if len = 0 && not no_ident then
          ""
        else
          Stdlib.really_input_string st.input len
      in
      if print then
        Stdlib.output_string st.output (decorate_indent str);
      let last_indent = ident_of_line st str in
      ({ st with current_pos = pos; last_indent }, str))
    else (
      match Stdlib.input_line st.input with
      | str ->
        if print then
          Stdlib.output_string st.output (str ^ "\n");
        aux
          { st with
            current_pos =
              Pos.newline st.current_pos (*{ line= st.current_pos.line + 1; col= 1 }*)
          }
      | exception End_of_file ->
        (* TODO this should be a Pp.error but I don't see how to get that module here *)
        Printf.fprintf stderr "st.line= %d\npos.line= %d\n" st.current_pos.line pos.line;
        failwith "end of file")
  in
  aux st


(* Various kinds of edits we can perform *)
type injection_kind =
  (* Inject a statement in a function *)
  | InStmt of precedence * int * string
  (* | Return of (Pos.t * Pos.t) option *)
  (* | Return of Pos.t option *)

  (* Inject a pre-condition for a function *)
  | Pre of
      string list * C.ctype * bool (* flag for whether injection is for main function *)
  (* Inject a post-condition for a function *)
  | Post of string list * C.ctype
  (* Delete `main` function *)
  | DeleteMain of bool (* flag for whether start (true) or end (false) *)
  (* Insert a non-static wrapper of a static function *)
  | WrapStatic of string * CF.AilSyntax.sigma_declaration

(* Describes how much space we need for an edit. *)
type injection_footprint =
  { start_pos : Pos.t;
    end_pos : Pos.t
  }

(* A low-level edit we'd like to perform.  See function [inject]. *)
type injection =
  { footprint : injection_footprint;
    kind : injection_kind
  }

(* let string_of_footprint {start_pos; end_pos} = Printf.sprintf "%s - %s" (Pos.to_string
   start_pos) (Pos.to_string end_pos) *)

(* start (1, 1) and end (1, 1) for include headers *)
let inject st inj =
  let do_output st str =
    Stdlib.output_string st.output (decorate_injection str);
    st
  in
  let st, _ = move_to st inj.footprint.start_pos in
  let st =
    match inj.kind with
    | InStmt (_, _, str) ->
      let st, _ = move_to ~no_ident:true ~print:false st inj.footprint.end_pos in
      do_output st str
    | Pre (strs, ret_ty, is_main) ->
      let indent = String.make (st.last_indent + 2) ' ' in
      let indented_strs = List.map (fun str -> str ^ indent) strs in
      let str = List.fold_left ( ^ ) "" indented_strs in
      do_output
        st
        ("\n"
         ^ indent
         ^ "/* EXECUTABLE CN PRECONDITION */"
         ^ "\n"
         ^ indent
         ^ (if CF.AilTypesAux.is_void ret_ty then
              ""
            else (
              let cn_ret_sym = Sym.fresh "__cn_ret" in
              let ret_type_doc =
                CF.Pp_ail.(
                  with_executable_spec
                    (pp_ctype_declaration
                       (CF.Pp_ail.pp_id_obj cn_ret_sym)
                       C.no_qualifiers)
                    ret_ty)
              in
              let initialisation_str = if is_main then " = 0" else "" in
              CF.Pp_utils.to_plain_pretty_string ret_type_doc
              ^ initialisation_str
              ^ ";\n"
              ^ indent))
         ^ str)
    | Post (strs, ret_ty) ->
      let indent = String.make st.last_indent ' ' in
      let epilogue_indent = String.make (st.last_indent + 2) ' ' in
      let indented_strs =
        List.map
          (fun str ->
             let indent = if String.contains str '{' then indent else epilogue_indent in
             "\n" ^ indent ^ str)
          strs
      in
      let str = List.fold_left ( ^ ) "" indented_strs in
      do_output
        st
        ("\n"
         ^ indent
         ^ "/* EXECUTABLE CN POSTCONDITION */"
         ^ "\n__cn_epilogue:\n"
         ^ (if List.is_empty strs then indent ^ ";\n" else "")
         ^ str
         ^
         if CF.AilTypesAux.is_void ret_ty then
           ""
         else
           indent ^ "\nreturn __cn_ret;\n\n")
    | DeleteMain pre ->
      if pre then do_output st "\n#if 0\n" else do_output st "\n#endif\n"
    | WrapStatic (prefix, decl) ->
      let fsym = Sym.fresh (prefix ^ "_" ^ Sym.pp_string (fst decl)) in
      let declarations =
        [ ( fsym,
            match snd decl with
            | _, _, A.Decl_function (_, ret_ct, args_ct, _, _, _) ->
              ( Locations.other __LOC__,
                CF.Annot.Attrs [],
                A.Decl_function (false, ret_ct, args_ct, false, false, false) )
            | _ -> failwith __LOC__ )
        ]
      in
      let ret_ty, arg_tys =
        match decl with
        | _, (_, _, A.Decl_function (_, (_, ret_ty), arg_tys, _, _, _)) ->
          (ret_ty, arg_tys)
        | _ -> failwith __LOC__
      in
      let args =
        let rec aux n tys =
          match tys with
          | _ :: tys' -> Sym.fresh ("arg_" ^ string_of_int n) :: aux (n + 1) tys'
          | [] -> []
        in
        aux 0 arg_tys
      in
      let e_call =
        Utils.mk_expr
          (AilEcall
             ( Utils.mk_expr (AilEident (fst decl)),
               List.map (fun x -> Utils.mk_expr (AilEident x)) args ))
      in
      let function_definitions =
        [ ( fsym,
            ( Locations.other __LOC__,
              0,
              CF.Annot.Attrs [],
              args,
              Utils.mk_stmt
                A.(
                  AilSblock
                    ( [],
                      [ Utils.mk_stmt
                          (if C.ctypeEqual C.void ret_ty then
                             AilSexpr e_call
                           else
                             AilSreturn e_call)
                      ] )) ) )
        ]
      in
      do_output
        st
        Pp.(
          plain
            (hardline
             ^^ CF.Pp_ail.pp_program
                  ~show_include:true
                  (None, { A.empty_sigma with declarations; function_definitions })
             ^^ hardline))
  in
  fst (move_to ~print:false st inj.footprint.end_pos)


let sort_injects xs =
  let cmp inj1 inj2 =
    let c_start = Pos.compare inj1.footprint.start_pos inj2.footprint.start_pos in
    let c_end = Pos.compare inj1.footprint.end_pos inj2.footprint.end_pos in
    let c_precedence =
      match (inj1.kind, inj2.kind) with
      | InStmt (n1, _, _), InStmt (n2, _, _) -> compare_precedence n1 n2
      | _ -> 0
    in
    if c_start = 0 then
      if c_end = 0 then c_precedence else c_end
    else
      c_start
  in
  let xs = List.sort cmp xs in
  (* List.iteri (fun i inj -> Printf.fprintf stderr "\x1b[35m[%d] -> %s @ %s\x1b[0\n" i
     begin match inj.kind with | InStmt (n, str) -> "InStmt["^ string_of_int n ^ "] ==> '"
     ^ String.escaped str ^ "'" | Pre (strs, _, _) -> "Pre ==> [" ^ String.concat ","
     (List.map (fun s -> "\"" ^ String.escaped s ^ "\"" ) strs) ^ "]" | Post _ -> "Post"
     end (string_of_footprint inj.footprint) ) xs; *)
  xs


let inject_all oc filename xs =
  let st =
    { input = Stdlib.open_in filename;
      output = oc;
      current_pos = Pos.initial;
      last_indent = 0
    }
  in
  let st = List.fold_left (fun st m -> inject st m) st (sort_injects xs) in
  let rec aux () =
    match Stdlib.input_line st.input with
    (* | Some str -> *)
    | str ->
      Stdlib.output_string st.output (str ^ "\n");
      aux () (* | None -> *)
    | exception End_of_file -> st
  in
  aux ()


let ( let* ) = Result.bind

let rec mapM f xs =
  match xs with
  | [] -> Ok []
  | x :: xs ->
    let* y = f x in
    let* ys = mapM f xs in
    Ok (y :: ys)


let _posOf_stmt stmt =
  let loc = A.instance_Loc_Located_AilSyntax_statement_dict.locOf_method stmt in
  Pos.of_location loc


let in_stmt_injs xs num_headers =
  mapM
    (fun (precedence, (loc, strs)) ->
       let* start_pos, end_pos = Pos.of_location loc in
       let num_headers = if num_headers != 0 then num_headers + 1 else num_headers in
       (* Printf.fprintf stderr "IN_STMT_INJS[%s], start: %s -- end: %s ---> [%s]\n"
         (Cerb_location.location_to_string loc) (Pos.to_string start_pos) (Pos.to_string
         end_pos) (String.concat "; " (List.map (fun str -> "'" ^ String.escaped str ^
         "'") strs)); *)
       Ok
         { footprint =
             { start_pos = Pos.increment_line start_pos num_headers;
               end_pos = Pos.v (end_pos.line + num_headers) end_pos.col
             };
           kind = InStmt (precedence, List.length strs, String.concat "\n" strs)
         })
    xs


(* (List.filter (fun (loc, _) -> Cerb_location.from_main_file loc) xs) *)

(* build the injections for the pre/post conditions of a C function *)
let pre_post_injs pre_post is_void is_main A.{ loc; _ } =
  let* pre_pos, post_pos =
    let* pre_pos, post_pos = Pos.of_location loc in
    let* pre_pos = Pos.offset_col ~off:1 pre_pos in
    let* pos_pos = Pos.offset_col ~off:(-1) post_pos in
    Ok (pre_pos, pos_pos)
  in
  (* match stmt_ with | AilSblock (_bindings, []) -> Pos.of_location loc | AilSblock
     (_bindings, ss) -> let first = List.hd ss in let last = Lem_list_extra.last ss in
     let* (pre_pos, _) = posOf_stmt first in let* (_, post_pos) = posOf_stmt last in Ok
     (pre_pos, post_pos) | _ -> Error (__LOC__ ^ ": must be called on a function body
     statement") in *)
  (* Printf.fprintf stderr "\x1b[35mPRE[%s], pos: %s\x1b[0m\n"
     (Cerb_location.location_to_string loc) (Pos.to_string pre_pos); Printf.fprintf stderr
     "\x1b[35mPOST[%s], pos: %s\x1b[0m\n" (Cerb_location.location_to_string loc)
     (Pos.to_string post_pos); *)
  Ok
    ( { footprint = { start_pos = pre_pos; end_pos = pre_pos };
        kind = Pre (fst pre_post, is_void, is_main)
      },
      { footprint = { start_pos = post_pos; end_pos = post_pos };
        kind = Post (snd pre_post, is_void)
      } )


(* build the injections decorating for all return statements *)
let return_injs xs =
  let rec aux acc = function
    | [] -> acc
    | (loc, (e_opt, inj_strs)) :: xs ->
      (match acc with
       | Error _ -> acc
       | Ok acc ->
         let* start_pos, end_pos = Pos.of_location loc in
         let* acc' =
           match e_opt with
           | None ->
             Ok
               ({ footprint = { start_pos; end_pos };
                  kind =
                    InStmt
                      ( Bot,
                        1,
                        String.concat "" ("{ " :: inj_strs) ^ "goto __cn_epilogue; }\n" )
                }
                :: acc)
           | Some e ->
             let loc = A.instance_Loc_Located_AilSyntax_expression_dict.locOf_method e in
             let* e_start_pos, e_end_pos = Pos.of_location loc in
             Ok
               ({ footprint = { start_pos; end_pos = e_start_pos };
                  kind = InStmt (Bot, 1, "{ __cn_ret = ")
                }
                :: { footprint = { start_pos = e_end_pos; end_pos };
                     kind =
                       InStmt
                         ( Bot,
                           1,
                           "; " ^ String.concat "" inj_strs ^ "goto __cn_epilogue; }" )
                   }
                :: acc)
         in
         aux (Ok acc') xs)
  in
  aux (Ok []) xs


let main_inj with_testing pre_post is_void loc stmt =
  if not with_testing then
    pre_post_injs pre_post is_void true stmt
  else
    let* pre_pos, post_pos =
      let* pre_pos, post_pos = Pos.of_location loc in
      let* pre_pos = Pos.offset_col ~off:0 pre_pos in
      let* pos_pos = Pos.offset_col ~off:0 post_pos in
      Ok (pre_pos, pos_pos)
    in
    Ok
      ( { footprint = { start_pos = pre_pos; end_pos = pre_pos }; kind = DeleteMain true },
        { footprint = { start_pos = post_pos; end_pos = post_pos };
          kind = DeleteMain false
        } )


let static_inj filename loc decl =
  let* _, pos = Pos.of_location loc in
  Ok
    { footprint = { start_pos = pos; end_pos = pos };
      kind = WrapStatic (Utils.static_prefix filename, decl)
    }


(* EXTERNAL *)
type 'a cn_injection =
  { orig_filename : string;
    filename : string;
    program : 'a A.ail_program;
    static_funcs : Sym.Set.t;
    pre_post : (Sym.t * (string list * string list)) list;
    in_stmt : (precedence * (Cerb_location.t * string list)) list;
    returns : (Cerb_location.t * ('a A.expression option * string list)) list;
    inject_in_preproc : bool;
    with_testing : bool
  }

let output_injections oc cn_inj =
  use_preproc_loc := cn_inj.inject_in_preproc;
  Cerb_colour.without_colour
    (fun () ->
       let* injs =
         List.fold_left
           (fun acc_ (fun_sym, (loc, _, _, _, stmt)) ->
              if not (Cerb_location.from_main_file loc) then
                (* let () = Printf.fprintf stderr "\x1b[31mSKIPPING ==> %s\x1b[0m\n"
                 (Cerb_location.simple_location loc) in *)
                acc_
              else (
                match List.assoc_opt Sym.equal fun_sym cn_inj.pre_post with
                | Some pre_post_strs ->
                  (match
                     ( acc_,
                       List.assoc Sym.equal fun_sym (snd cn_inj.program).A.declarations )
                   with
                   | ( Ok acc,
                       ((_, _, A.Decl_function (_, (_, ret_ty), _, _, _, _)) as decl') )
                     ->
                     let is_main =
                       match fst cn_inj.program with
                       | Some main_sym when Sym.equal main_sym fun_sym -> true
                       | _ -> false
                     in
                     let* pre, post =
                       if is_main then
                         main_inj cn_inj.with_testing pre_post_strs ret_ty loc stmt
                       else
                         pre_post_injs pre_post_strs ret_ty false stmt
                     in
                     let acc = pre :: post :: acc in
                     if cn_inj.with_testing && Sym.Set.mem fun_sym cn_inj.static_funcs
                     then
                       let* wrapper =
                         static_inj cn_inj.orig_filename loc (fun_sym, decl')
                       in
                       Ok (wrapper :: acc)
                     else
                       Ok acc
                   | _ -> assert false)
                | None -> acc_))
           (Ok [])
           (snd cn_inj.program).A.function_definitions
       in
       let* in_stmt = in_stmt_injs cn_inj.in_stmt 0 in
       let* rets = return_injs cn_inj.returns in
       let injs = in_stmt @ rets @ injs in
       ignore (inject_all oc cn_inj.filename injs);
       Ok ())
    ()

(* This appears to be unused:

   open Cerb_frontend

   let get_magics_of_statement stmt =
   let open AilSyntax in
   let rec aux acc (AnnotatedStatement (_loc, Annot.Attrs xs, stmt_)) =
   let acc =
   List.fold_left
   (fun acc attr ->
   let open Annot in
   match (attr.attr_ns, attr.attr_id, attr.attr_args) with
   | Some (Symbol.Identifier (_, "cerb")), Symbol.Identifier (_, "magic"), xs ->
   List.map (fun (loc, str, _) -> (loc, str)) xs :: acc
   | _ -> acc)
   acc
   xs
   in
   match stmt_ with
   | AilSskip | AilSexpr _ | AilSbreak | AilScontinue | AilSreturnVoid | AilSreturn _
   | AilSgoto _ | AilSdeclaration _ | AilSreg_store _ ->
   acc
   | AilSblock (_, ss) | AilSpar ss -> List.fold_left aux acc ss
   | AilSif (_, s1, s2) -> aux (aux acc s1) s2
   | AilSwhile (_, s, _)
   | AilSdo (s, _, _)
   | AilSswitch (_, s)
   | AilScase (_, s)
   | AilScase_rangeGNU (_, _, s)
   | AilSdefault s
   | AilSlabel (_, s, _)
   | AilSmarker (_, s) ->
   aux acc s
   in
   aux [] stmt
*)
