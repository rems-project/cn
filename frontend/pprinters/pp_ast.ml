open Pp_prelude
open Pp_utils
open Colour

module P = PPrint

type doc_tree =
  | Dleaf of P.document
  | Dnode of P.document * doc_tree list

let pp_doc_tree dtree =
  let to_space = function
    | '|'
      -> "|"
    | _
      -> " " in
  let pp_prefix pref =
    !^ (ansi_format [Blue] pref) in
  let rec aux (pref, (current : char)) = function
    | Dleaf doc ->
        pp_prefix (pref ^ String.make 1 current ^ "-") ^^ doc
    | Dnode (doc, []) ->
        pp_prefix (pref ^ String.make 1 current ^ "-") ^^ doc ^^^ !^ "EMPTY"
        (* TODO: do a failwith ? *)
    | Dnode (doc, dtrees) ->
        P.separate P.hardline begin
          (pp_prefix (pref ^ String.make 1 current ^ "-") ^^ doc) ::
          map_with_last
            (aux (pref ^ to_space current ^ " ", '|'))
            (aux (pref ^ to_space current ^ " ", '`'))
            dtrees
        end
  in
  begin match dtree with
    | Dleaf doc ->
        doc
    | Dnode (doc, dtrees) ->
        doc ^^ P.hardline ^^
        P.separate P.hardline begin
          map_with_last
            (aux ("", '|'))
            (aux ("", '`'))
            dtrees
        end
  end

let pp_keyword w      = !^ (ansi_format [Bold; Cyan] w)
let pp_const c        = !^ (ansi_format [Magenta] c)

let pp_ctor k =
  !^ (ansi_format [Bold; Cyan] k)

let pp_type_keyword w = !^ (ansi_format [Green] w)

let pp_stmt_ctor k =
  !^ (ansi_format [Bold; Magenta] k)

let pp_decl_ctor w = !^ (ansi_format [Bold; Green] w)
let pp_id_type id = !^ (ansi_format [Green] (Pp_symbol.to_string_pretty id))

let pp_cond switch doc =
  if switch then
    (^^^) doc
  else
    (^^) P.empty

let filter_opt_list xs =
  List.fold_left (fun acc opt -> match opt with None -> acc | Some x -> x::acc) [] (List.rev xs)
let opt_list f = function
  | [] -> None
  | xs -> Some (f xs)

let leaf_opt_list ctor pp =
  opt_list (fun xs -> (Dleaf (pp_ctor ctor ^^^ P.brackets (comma_list pp xs))))

let node_opt_list ctor pp =
  opt_list (fun xs -> (Dnode (pp_ctor ctor, List.map pp xs)))

let guarded_opt b x =
  if b then Some x else None

let option z f = function
  | Some x -> Some (f x)
  | None   -> z


let add_dtree_of_attributes (Annot.Attrs xs) dtree =
  let open Annot in
  match xs with
    | [] ->
        dtree
    | _ ->
        let string_of_ident (Symbol.Identifier (_, str)) = str in
        Dnode ( pp_ctor "Attributes"
              , List.map (fun attr ->
                  let str =
                    begin match attr.attr_ns with
                  | Some z ->
                          string_of_ident z ^ "::"
                      | None ->
                          ""
                    end ^ string_of_ident attr.attr_id ^
                    "(" ^ String.concat ", " (List.map (fun z -> "\"" ^ String.escaped z ^ "\"") attr.attr_args) ^  ")" in
                  Dleaf (!^ str)
                ) xs )
        :: dtree
