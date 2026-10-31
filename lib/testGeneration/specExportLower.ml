(* Austen draws C storage values, while CN specifications use mathematical
   integers. This pass runs after CN typechecking, solely on exported IR. *)
module BT = BaseTypes
module T = Terms.Normal
module LAT = LogicalArgumentTypes
module LRT = LogicalReturnTypes
module AT = ArgumentTypes
module RT = ReturnTypes
module Req = Request
module Logical = SpecExportLogical

exception Unrepresentable = Logical.Unrepresentable

type binding =
  | Define of Sym.t * T.t
  | Resource of Sym.t * Req.t * BT.t

type t =
  { prog : unit Mucore.file;
    mutable generated : Sym.t list;
    mutable fresh_count : int;
    mutable helpers : (Sym.t * Definition.Predicate.t) list;
    mutable converters : (Sctypes.t * Sym.t) list;
    mutable owners : (Sctypes.t * Sym.t) list
  }

let create prog =
  { prog; generated = []; fresh_count = 0; helpers = []; converters = []; owners = [] }


let fresh c label =
  let s = Sym.fresh ("__cn_austen_" ^ label ^ "_" ^ string_of_int c.fresh_count) in
  c.fresh_count <- c.fresh_count + 1;
  c.generated <- s :: c.generated;
  s


let loc = Locations.other __LOC__

let info = (loc, None)

let term bt node = Terms.IT (node, bt, loc)

let sym s bt = MakeTerm.sym_ (s, bt, loc)

let key_bt = BT.Bits (BT.Unsigned, 64)

let logical_bt c = Logical.bt c.prog

let logical_it c = Logical.it ~fresh:(fresh c) c.prog

let logical_lc c = Logical.lc ~fresh:(fresh c) c.prog

let logical_req c = Logical.req ~fresh:(fresh c) c.prog

let value_bt c ct = logical_bt c (Memory.bt_of_sct ct)

let rec storage_bt = function
  | Sctypes.Void -> BT.Unit
  | Integer ity ->
    BT.Bits
      ( (if Memory.is_signed_integer_type ity then BT.Signed else BT.Unsigned),
        8 * Memory.size_of_integer_type ity )
  | Pointer _ -> BT.Loc ()
  | Struct tag -> BT.Struct tag
  | Array (elem, Some _) -> BT.Map (key_bt, storage_bt elem)
  | Array (_, None) -> raise (Unrepresentable "an unsized array value")
  | Byte -> raise (Unrepresentable "an owned byte value")
  | Function _ -> raise (Unrepresentable "a function value")


let storage_member raw id ct here = Terms.IT (StructMember (raw, id), storage_bt ct, here)

(* Generate each per-C-type helper once, recording its name before building it
   so recursive types refer back to it. *)
let memo c (get, set) label ct build =
  match List.assoc_opt Sctypes.equal ct (get c) with
  | Some name -> name
  | None ->
    let name = fresh c label in
    set c ((ct, name) :: get c);
    build name;
    name


let converters = ((fun c -> c.converters), fun c v -> c.converters <- v)

let owners = ((fun c -> c.owners), fun c v -> c.owners <- v)

let prepend bindings rest =
  List.fold_right
    (fun b rest ->
       match b with
       | Define (s, it) -> LAT.Define ((s, it), info, rest)
       | Resource (s, req, bt) -> LAT.Resource ((s, (req, bt)), info, rest))
    bindings
    rest


let add_helper c name pointer iargs output bindings value =
  let clause =
    Definition.Clause.
      { loc;
        guard = MakeTerm.bool_ true loc;
        packing_ft = prepend bindings (LAT.I value)
      }
  in
  let pred =
    Definition.Predicate.
      { loc;
        pointer;
        iargs;
        oarg = (loc, output);
        clauses = Some [ clause ];
        recursive = false;
        attrs = []
      }
  in
  c.helpers <- (name, pred) :: c.helpers


(* A converter's quantified calls are pure: they only inspect an already
   generated value. Keeping the original whole-object Owned preserves padding
   and allocation claims. Finite maps have no invented out-of-domain default. *)
let rec convert c ct raw =
  match ct with
  | Sctypes.Integer _ -> ([], MakeTerm.cast_ (value_bt c ct) raw loc)
  | Pointer _ | Void -> ([], raw)
  | Struct tag ->
    let bindings, fields =
      List.split
        (List.map
           (fun (id, ct) ->
              let more, value = convert c ct (storage_member raw id ct loc) in
              (more, (id, value)))
           (Logical.struct_members c.prog tag))
    in
    (List.concat bindings, term (value_bt c ct) (Terms.Record fields))
  | Array (elem, Some length) ->
    let helper = converter c elem in
    let index = fresh c "index" in
    let i = sym index BT.Integer in
    let element =
      term (storage_bt elem) (Terms.MapGet (raw, MakeTerm.cast_ key_bt i loc))
    in
    let permission =
      MakeTerm.and2_
        ( MakeTerm.le_ (MakeTerm.int_ 0 loc, i) loc,
          MakeTerm.lt_ (i, MakeTerm.int_ length loc) loc )
        loc
    in
    let req =
      Req.Q
        { name = Req.PName helper;
          pointer = MakeTerm.null_ loc;
          q = (index, BT.Integer);
          q_loc = loc;
          step = Sctypes.Integer (Sctypes.IntegerTypes.Unsigned Ichar);
          permission;
          iargs = [ element ]
        }
    in
    let output = fresh c "array" in
    let bt = value_bt c ct in
    ([ Resource (output, req, bt) ], sym output bt)
  | Array (_, None) | Byte | Function _ ->
    ignore (storage_bt ct);
    assert false


and converter c ct =
  memo c converters "convert" ct (fun name ->
    let pointer = fresh c "unused_pointer" in
    let input = fresh c "storage" in
    let bt = storage_bt ct in
    let bindings, value = convert c ct (sym input bt) in
    add_helper c name pointer [ (input, bt) ] (value_bt c ct) bindings value)


let owner c ct =
  memo c owners "owned" ct (fun name ->
    let pointer = fresh c "pointer" in
    let raw = fresh c "storage" in
    let bt = storage_bt ct in
    let req =
      Req.P
        { name = Req.Owned (ct, Req.Init); pointer = sym pointer (BT.Loc ()); iargs = [] }
    in
    let bindings, value = convert c ct (sym raw bt) in
    add_helper
      c
      name
      pointer
      []
      (value_bt c ct)
      (Resource (raw, req, bt) :: bindings)
      value)


(* A value used only by CN's automatic representability check need not acquire
   a logical aggregate view. Check its original storage value instead, retaining
   the finite array keys and whole-object ownership. Boolean fields still need
   the C value range 0..1 rather than Austen's storage-byte range. *)
let storage_representable c ct raw here =
  Logical.value_check
    ~fresh:(fresh c)
    c.prog
    ~member:storage_member
    ~good:false
    ct
    raw
    here


let rec lower_lat : 'i 'j. t -> ('i -> 'j) -> ('i -> Sym.Set.t) -> 'i LAT.t -> 'j LAT.t =
  fun c terminal free lat ->
  match lat with
  | LAT.I value -> LAT.I (terminal value)
  | Define ((s, it), info, rest) ->
    LAT.Define ((s, logical_it c it), info, lower_lat c terminal free rest)
  | Constraint (lc, info, rest) ->
    LAT.Constraint (logical_lc c lc, info, lower_lat c terminal free rest)
  | Resource ((s, (req, bt)), info, rest) ->
    let used = lazy (Sym.Set.mem s (LAT.free_vars free rest)) in
    let req = logical_req c req in
    let lower = lower_lat c terminal free in
    (match Req.get_name req with
     | Req.PName _ -> LAT.Resource ((s, (req, logical_bt c bt)), info, lower rest)
     | Owned (ct, Uninit) ->
       if Lazy.force used then
         raise (Unrepresentable "a specification reading an uninitialized Owned value");
       let bt =
         match req with Req.P _ -> storage_bt ct | Q q -> BT.Map (snd q.q, storage_bt ct)
       in
       LAT.Resource ((s, (req, bt)), info, lower rest)
     | Owned (ct, Init) ->
       (match req with
        | Req.Q q ->
          let req = Req.Q { q with name = Req.PName (owner c ct) } in
          LAT.Resource ((s, (req, logical_bt c bt)), info, lower rest)
        | Req.P _ ->
          let raw_bt = storage_bt ct in
          let rec storage_checks = function
            | LAT.Constraint
                ( LogicalConstraints.T
                    (Terms.IT
                       ( Terms.Representable (check_ct, Terms.IT (Terms.Sym checked, _, _)),
                         _,
                         here )),
                  check_info,
                  tail )
              when Sctypes.equal ct check_ct
                   && Sym.equal s checked
                   && not (Sym.Set.mem s (LAT.free_vars free tail)) ->
              Some
                (LAT.Constraint
                   ( LogicalConstraints.T (storage_representable c ct (sym s raw_bt) here),
                     check_info,
                     lower tail ))
            | LAT.Constraint (lc, info, tail)
              when not (Sym.Set.mem s (LogicalConstraints.free_vars lc)) ->
              (* CN inserts independent pointer checks before checking the
                   value. Preserve them without crossing another binder. *)
              Option.map
                (fun rest -> LAT.Constraint (logical_lc c lc, info, rest))
                (storage_checks tail)
            | _ -> None
          in
          let storage_only =
            if not (Lazy.force used) then Some (lower rest) else storage_checks rest
          in
          (match storage_only with
           | Some rest -> LAT.Resource ((s, (req, raw_bt)), info, rest)
           | None ->
             let raw = fresh c "owned_value" in
             let bindings, value = convert c ct (sym raw raw_bt) in
             LAT.Resource
               ( (raw, (req, raw_bt)),
                 info,
                 prepend (bindings @ [ Define (s, value) ]) (lower rest) ))))


let lat_of_lrt lrt = LAT.of_lrt lrt (LAT.I ())

let rec lrt_of_lat = function
  | LAT.I () -> LRT.I
  | Define (bound, info, rest) -> LRT.Define (bound, info, lrt_of_lat rest)
  | Resource (bound, info, rest) -> LRT.Resource (bound, info, lrt_of_lat rest)
  | Constraint (lc, info, rest) -> LRT.Constraint (lc, info, lrt_of_lat rest)


(* A scalar cast is injective when its bitvector signedness and width are fixed.
   Retain integer equalities, but expose the equivalent storage equality as an
   additional constraint. This lets Austen solve finite draws directly rather
   than rediscovering these equalities through mixed integer/bitvector casts.
   Arithmetic, different storage types, and non-representable literals are left
   alone. *)
type scalar_views = T.t Sym.Map.t

let forget_view symbol views =
  Sym.Map.filter
    (fun logical raw ->
       (not (Sym.equal logical symbol)) && not (Sym.Set.mem symbol (T.free_vars raw)))
    views


let scalar_view views = function
  | Terms.IT (Terms.Sym symbol, BT.Integer, _) -> Sym.Map.find_opt symbol views
  | Terms.IT (Terms.Cast (BT.Integer, raw), BT.Integer, _)
    when Option.is_some (BT.is_bits_bt (T.get_bt raw)) ->
    Some raw
  | _ -> None


let bind_view views symbol value =
  let raw = scalar_view views value in
  let views = forget_view symbol views in
  match raw with
  | Some raw when not (Sym.Set.mem symbol (T.free_vars raw)) ->
    Sym.Map.add symbol raw views
  | _ -> views


let equality_projection views = function
  | LogicalConstraints.T (Terms.IT (Terms.Binop (Terms.EQ, left, right), BT.Bool, here))
    when BT.equal (T.get_bt left) BT.Integer && BT.equal (T.get_bt right) BT.Integer ->
    let literal_at raw = function
      | Terms.IT (Terms.Const (Terms.Z value), BT.Integer, loc) ->
        (match BT.is_bits_bt (T.get_bt raw) with
         | Some bits when BT.fits_range bits value ->
           Some (Terms.IT (Terms.Const (Terms.Bits (bits, value)), T.get_bt raw, loc))
         | _ -> None)
      | _ -> None
    in
    let pair =
      match (scalar_view views left, scalar_view views right) with
      | Some left, Some right when BT.equal (T.get_bt left) (T.get_bt right) ->
        Some (left, right)
      | Some raw, None -> Option.map (fun value -> (raw, value)) (literal_at raw right)
      | None, Some raw -> Option.map (fun value -> (value, raw)) (literal_at raw left)
      | _ -> None
    in
    Option.map
      (fun (left, right) ->
         LogicalConstraints.T
           (Terms.IT (Terms.Binop (Terms.EQ, left, right), BT.Bool, here)))
      pair
  | _ -> None


let rec project_lat terminal views = function
  | LAT.I value -> LAT.I (terminal views value)
  | LAT.Define ((symbol, value), info, rest) ->
    LAT.Define
      ((symbol, value), info, project_lat terminal (bind_view views symbol value) rest)
  | LAT.Resource ((symbol, resource), info, rest) ->
    LAT.Resource
      ((symbol, resource), info, project_lat terminal (forget_view symbol views) rest)
  | LAT.Constraint (constraint_, info, rest) ->
    let rest = LAT.Constraint (constraint_, info, project_lat terminal views rest) in
    (match equality_projection views constraint_ with
     | None -> rest
     | Some projected -> LAT.Constraint (projected, info, rest))


let project_return views (RT.Computational ((symbol, bt), info, body)) =
  let body = project_lat (fun _ () -> ()) (forget_view symbol views) (lat_of_lrt body) in
  RT.Computational ((symbol, bt), info, lrt_of_lat body)


let rec project_arguments views = function
  | AT.Computational ((symbol, bt), info, rest) ->
    AT.Computational
      ((symbol, bt), info, project_arguments (forget_view symbol views) rest)
  | AT.Ghost ((symbol, bt), info, rest) ->
    AT.Ghost ((symbol, bt), info, project_arguments (forget_view symbol views) rest)
  | AT.L lat ->
    AT.L (project_lat (fun views (rt, body) -> (project_return views rt, body)) views lat)


let lower_return c ct (RT.Computational ((s, bt), info, body)) =
  let body = lower_lat c Fun.id (fun () -> Sym.Set.empty) (lat_of_lrt body) in
  match ct with
  | Sctypes.Struct _ ->
    let raw = fresh c "return" in
    let raw_bt = storage_bt ct in
    let bindings, value = convert c ct (sym raw raw_bt) in
    RT.Computational
      ((raw, raw_bt), info, lrt_of_lat (prepend (bindings @ [ Define (s, value) ]) body))
  | _ -> RT.Computational ((s, logical_bt c bt), info, lrt_of_lat body)


let lower_test c ~params ~ret (test : Test.t) =
  let terminal (rt, body) = (lower_return c ret rt, body) in
  let free (rt, _) = RT.free_vars rt in
  let rec args params aliases = function
    | AT.Computational ((s, bt), info, rest) ->
      (match params with
       | [] -> raise (Unrepresentable "a C signature with too few argument types")
       | ct :: params ->
         let ct =
           match ct with Sctypes.Array (elem, _) -> Sctypes.Pointer elem | _ -> ct
         in
         let raw_bt = storage_bt ct in
         if BT.equal raw_bt (logical_bt c bt) then
           AT.Computational ((s, raw_bt), info, args params aliases rest)
         else (
           let raw = fresh c "argument" in
           let bindings, value = convert c ct (sym raw raw_bt) in
           AT.Computational
             ( (raw, raw_bt),
               info,
               args params (aliases @ bindings @ [ Define (s, value) ]) rest )))
    | AT.Ghost ((s, bt), info, rest) ->
      AT.Ghost ((s, logical_bt c bt), info, args params aliases rest)
    | AT.L lat ->
      if not (List.is_empty params) then
        raise (Unrepresentable "a C signature with too many argument types");
      AT.L (prepend aliases (lower_lat c terminal free lat))
  in
  { test with internal = project_arguments Sym.Map.empty (args params [] test.internal) }


let lower_definitions c =
  let resource_predicates =
    List.map
      (fun (s, (p : Definition.Predicate.t)) ->
         let clauses =
           Option.map
             (List.map (fun (cl : Definition.Clause.t) ->
                { cl with
                  guard = logical_it c cl.guard;
                  packing_ft =
                    project_lat
                      (fun _ value -> value)
                      Sym.Map.empty
                      (lower_lat c (logical_it c) T.free_vars cl.packing_ft)
                }))
             p.clauses
         in
         ( s,
           { p with
             iargs = List.map (fun (s, bt) -> (s, logical_bt c bt)) p.iargs;
             oarg = (fst p.oarg, logical_bt c (snd p.oarg));
             clauses
           } ))
      c.prog.resource_predicates
  in
  let logical_predicates =
    List.map
      (fun (s, (f : Definition.Function.t)) ->
         let body =
           match f.body with
           | Uninterp -> Definition.Function.Uninterp
           | Def it -> Def (logical_it c it)
           | Rec_Def it -> Rec_Def (logical_it c it)
         in
         ( s,
           { f with
             args = List.map (fun (s, bt) -> (s, logical_bt c bt)) f.args;
             return_bt = logical_bt c f.return_bt;
             body
           } ))
      c.prog.logical_predicates
  in
  let datatypes =
    List.map
      (fun (s, (dt : Mucore.datatype)) ->
         ( s,
           { dt with
             cases =
               List.map
                 (fun (s, fields) ->
                    (s, List.map (fun (id, bt) -> (id, logical_bt c bt)) fields))
                 dt.cases
           } ))
      c.prog.datatypes
  in
  { c.prog with resource_predicates; logical_predicates; datatypes }


let with_helpers c (prog : unit Mucore.file) =
  { prog with resource_predicates = prog.resource_predicates @ List.rev c.helpers }


(* Global Sym.fresh values depend on preceding exports. Normalize only the
   new symbols, preserving every original symbol and selector identity, and
   return the module's fields with the first symbol number above all of them. *)
let renumber c ~max_sym_num (fields : (string * Yojson.Safe.t) list) =
  let ids = Hashtbl.create (List.length c.generated) in
  List.iter (fun s -> Hashtbl.replace ids (Sym.num s) (-1)) c.generated;
  let first = max_sym_num ~skip:(Hashtbl.mem ids) (`Assoc fields) + 1 in
  List.iteri
    (fun i s -> Hashtbl.replace ids (Sym.num s) (first + i))
    (List.rev c.generated);
  let rec rewrite fields =
    List.map
      (fun (k, j) ->
         ( k,
           match (k, j) with
           | "num", `Int n -> `Int (Option.value ~default:n (Hashtbl.find_opt ids n))
           | _ -> rewrite_json j ))
      fields
  and rewrite_json = function
    | `Assoc fields -> `Assoc (rewrite fields)
    | `List xs -> `List (List.map rewrite_json xs)
    | j -> j
  in
  (rewrite fields, first + List.length c.generated)
