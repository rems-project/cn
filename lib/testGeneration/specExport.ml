(* Dump CN's specification IR as JSON in the shape AustenTest consumes.

   AustenTest is a Rust reimplementation of Bennet and Darcy. Its
   `austen-spec` crate ports CN's `ArgumentTypes` / `LogicalArgumentTypes` /
   `Request` / `Definition` / `Terms` / `BaseTypes` / `Sctypes` type for type
   and reads them as serde JSON. Until now those files were hand-written,
   which capped its corpus at four toy fixtures; this exports the real thing,
   so `tests/cn-test-gen/src/*.c` can be driven through that pipeline.

   Why hand-written rather than [@@deriving yojson]: serde's encoding and the
   ppx's do not agree. serde renders an externally-tagged enum as an object
   ({"Loc": null}) where the ppx renders a positional array (["Loc", null]);
   serde renders a record variant's payload as an object where the ppx
   renders an array; `Sym.t` is an opaque Cerberus symbol that has to become
   {"name", "num"}; and a number of constructors are simply spelled
   differently on the two sides (BW_CLZ_NoSMT vs BwClz). So the encoding
   is spelled out here, and the far side's
   `crates/austen-spec/tests/fixtures_round_trip.rs` re-parses everything
   this produces.

   Dropped throughout, all deliberately: `Locations.t` and the LAT/AT `info`
   strings (AustenTest carries no source spans), `Definition.Predicate`'s
   `recursive` (it recomputes recursion from the call graph) and `attrs`,
   `Definition.Function.emit_coq`, and the `Extract.fn_body` half of the
   argument type's terminal — the `cn_statement`s and loop invariants inside
   the body, which are neither pre- nor postcondition. Operators are
   normalized rather than transcribed: see `json_of_unop` below. *)

module BT = BaseTypes
module T = Terms.Normal
module LC = LogicalConstraints
module LAT = LogicalArgumentTypes
module LRT = LogicalReturnTypes
module AT = ArgumentTypes
module RT = ReturnTypes
module Req = Request

type json = Yojson.Safe.t

(* ------------------------------------------------------------ primitives *)

let obj fields : json = `Assoc fields

(* serde's externally-tagged encodings, named so the intent is visible at
   every use site rather than inferred from the shape. *)
let unit_variant (name : string) : json = `String name

let newtype_variant (name : string) (payload : json) : json = obj [ (name, payload) ]

let tuple_variant (name : string) (payloads : json list) : json =
  obj [ (name, `List payloads) ]


let record_variant (name : string) (fields : (string * json) list) : json =
  obj [ (name, obj fields) ]


let pair (a : json) (b : json) : json = `List [ a; b ]

(* A CN construct AustenTest's specification language cannot carry. Raised
   anywhere inside a function's export and handled at the top level, where it
   becomes a warning that skips exporting that one function. *)
exception Unrepresentable of string

(* `austen_gen::Sym`. `name` is an `Option<String>`: an anonymous Cerberus
   symbol has no description to render, and inventing one would make two
   distinct symbols look alike in a diff. `num` is the load-bearing half —
   AustenTest's `Sym` compares on it alone, and `Sym.json` here in CN throws
   it away, which is why that function is no use for this.

   `pp_string_no_nums`, not `pp_string`: `bin/test/shared.ml` sets
   `Sym.executable_spec_enabled`, which changes what `pp_string` renders. *)
let json_of_sym (s : Sym.t) : json =
  let name =
    match Sym.description s with
    | Cerb_frontend.Symbol.SD_None -> `Null
    | _ -> `String (Sym.pp_string_no_nums s)
  in
  obj [ ("name", name); ("num", `Int (Sym.num s)) ]


(* `austen_gen::Id`, a newtype over String. *)
let json_of_id (i : Id.t) : json = `String (Id.get_string i)

(* `num_bigint::BigInt`'s serde encoding: [sign, [u32 limbs, little-endian]],
   sign being -1/0/1 and the limbs those of the absolute value. Zero is
   [0, []] — an empty limb list, not [0]. This is the one shape here that
   cannot be read off the Rust type definition; it comes from num-bigint
   0.4's own `bigint/serde.rs`, which documents the format as frozen. *)
let json_of_z (z : Z.t) : json =
  let base = Z.shift_left Z.one 32 in
  let rec limbs a acc =
    if Z.equal a Z.zero then
      List.rev acc
    else
      limbs (Z.div a base) (`Int (Z.to_int (Z.rem a base)) :: acc)
  in
  `List [ `Int (Z.sign z); `List (limbs (Z.abs z) []) ]


(* `num_rational::Ratio`: [numer, denom]. *)
let json_of_q (q : Q.t) : json = pair (json_of_z (Q.num q)) (json_of_z (Q.den q))

let json_of_sign : BT.sign -> json = function
  | BT.Signed -> unit_variant "Signed"
  | BT.Unsigned -> unit_variant "Unsigned"


(* ---------------------------------------------------------------- Sctype *)

let json_of_ibt : Sctypes.IntegerBaseTypes.t -> json =
  let open Sctypes.IntegerBaseTypes in
  function
  | Ichar -> unit_variant "Ichar"
  | Short -> unit_variant "Short"
  | Int_ -> unit_variant "Int"
  | Long -> unit_variant "Long"
  | LongLong -> unit_variant "LongLong"
  | IntN_t n -> newtype_variant "IntN" (`Int n)
  | Int_leastN_t n -> newtype_variant "IntLeastN" (`Int n)
  | Int_fastN_t n -> newtype_variant "IntFastN" (`Int n)
  | Intmax_t -> unit_variant "Intmax"
  | Intptr_t -> unit_variant "Intptr"


let json_of_ity : Sctypes.IntegerTypes.t -> json =
  let open Sctypes.IntegerTypes in
  function
  | Char -> unit_variant "Char"
  | Bool -> unit_variant "Bool"
  | Signed ibt -> newtype_variant "Signed" (json_of_ibt ibt)
  | Unsigned ibt -> newtype_variant "Unsigned" (json_of_ibt ibt)
  | Enum s -> newtype_variant "Enum" (json_of_sym s)
  | Wchar_t -> unit_variant "WcharT"
  | Wint_t -> unit_variant "WintT"
  | Size_t -> unit_variant "SizeT"
  | Ptrdiff_t -> unit_variant "PtrdiffT"
  | Ptraddr_t -> unit_variant "PtraddrT"


(* The Rust field is `const_`: `const` is a keyword there, and the escape is
   part of the serialized name because nothing renames it. *)
let json_of_quals (q : Sctypes.Qualifiers.t) : json =
  obj
    [ ("const_", `Bool q.const);
      ("restrict", `Bool q.restrict);
      ("volatile", `Bool q.volatile)
    ]


let rec json_of_sct (ct : Sctypes.t) : json =
  match ct with
  | Sctypes.Void -> unit_variant "Void"
  | Sctypes.Integer ity -> newtype_variant "Integer" (json_of_ity ity)
  | Sctypes.Array (ct', n) ->
    tuple_variant
      "Array"
      [ json_of_sct ct'; (match n with None -> `Null | Some n -> `Int n) ]
  | Sctypes.Pointer ct' -> newtype_variant "Pointer" (json_of_sct ct')
  | Sctypes.Struct tag -> newtype_variant "Struct" (json_of_sym tag)
  | Sctypes.Function ((quals, ret), args, variadic) ->
    record_variant
      "Function"
      [ ("return_type", pair (json_of_quals quals) (json_of_sct ret));
        ( "args",
          `List (List.map (fun (a, is_reg) -> pair (json_of_sct a) (`Bool is_reg)) args)
        );
        ("variadic", `Bool variadic)
      ]
  | Sctypes.Byte -> unit_variant "Byte"


(* -------------------------------------------------------------- BaseType *)

(* CN's `ArgumentTypes` are over `BaseTypes.t = unit t_gen`, so every `Loc`
   here carries `()` and serializes as {"Loc": null}. AustenTest's `Loc`
   takes an optional pointee, which its own `annotate_pointees` pass fills
   in from the definitions, so exporting them all bare loses nothing. *)
let rec json_of_bt (bt : BT.t) : json =
  match bt with
  | BT.Unit -> unit_variant "Unit"
  | BT.Bool -> unit_variant "Bool"
  | BT.Integer -> unit_variant "Integer"
  | BT.MemByte -> unit_variant "MemByte"
  | BT.Bits (sign, n) -> tuple_variant "Bits" [ json_of_sign sign; `Int n ]
  | BT.Real -> unit_variant "Real"
  | BT.Alloc_id -> unit_variant "AllocId"
  | BT.Loc () -> newtype_variant "Loc" `Null
  | BT.CType -> unit_variant "CType"
  | BT.Struct tag -> newtype_variant "Struct" (json_of_sym tag)
  | BT.Datatype tag -> newtype_variant "Datatype" (json_of_sym tag)
  | BT.Record members -> newtype_variant "Record" (json_of_members members)
  | BT.Map (k, v) -> tuple_variant "Map" [ json_of_bt k; json_of_bt v ]
  | BT.List bt' -> newtype_variant "List" (json_of_bt bt')
  | BT.Tuple bts -> newtype_variant "Tuple" (`List (List.map json_of_bt bts))
  | BT.Set bt' -> newtype_variant "Set" (json_of_bt bt')
  | BT.Option bt' -> newtype_variant "Option" (json_of_bt bt')


and json_of_members (ms : unit BT.member_types_gen) : json =
  `List (List.map (fun (id, bt) -> pair (json_of_id id) (json_of_bt bt)) ms)


(* ------------------------------------------------------------- IndexTerm *)

let json_of_const : Terms.const -> json =
  let open Terms in
  function
  | Z z -> newtype_variant "Z" (json_of_z z)
  | Bits ((sign, n), z) ->
    tuple_variant "Bits" [ pair (json_of_sign sign) (`Int n); json_of_z z ]
  | Q q -> newtype_variant "Q" (json_of_q q)
  | MemByte { alloc_id; value } ->
    record_variant
      "MemByte"
      [ ("alloc_id", match alloc_id with None -> `Null | Some z -> json_of_z z);
        ("value", json_of_z value)
      ]
  | Pointer { alloc_id; addr } ->
    record_variant
      "Pointer"
      [ ("alloc_id", json_of_z alloc_id); ("addr", json_of_z addr) ]
  | Alloc_id z -> newtype_variant "AllocId" (json_of_z z)
  | Bool b -> newtype_variant "Bool" (`Bool b)
  | Unit -> unit_variant "Unit"
  | Null -> unit_variant "Null"
  | CType_const ct -> newtype_variant "CTypeConst" (json_of_sct ct)
  | Default bt -> newtype_variant "Default" (json_of_bt bt)


(* CN spells these in SCREAMING style, AustenTest in CamelCase. Both matches
   are exhaustive, so a new operator on either side is a compile error rather
   than a silently mistranslated term.

   Two normalizations happen here, both collapsing CN distinctions that only
   exist for CN's own solver and that AustenTest has no use for. Neither is
   invertible: the export is a lowering, not a round trip.

   The `_NoSMT` suffix marks the operator CN emits when it wants an
   uninterpreted function rather than an SMT-LIB primitive, because the
   primitive is nonlinear (`MulNoSMT`, `ExpNoSMT`) or partial (`DivNoSMT`,
   `RemNoSMT`, `ModNoSMT`) or has no SMT-LIB spelling at all (the four
   bitwise counting operators). The *semantics* are the interpreted ones in
   every case, so the suffix says how to hand the term to Z3, not what the
   term means, and is dropped. The bitwise four have no un-suffixed CN
   spelling to collapse into and are simply renamed. *)
let json_of_unop : Terms.unop -> json =
  let open Terms in
  function
  | Not -> unit_variant "Not"
  | Negate -> unit_variant "Negate"
  | BW_CLZ -> unit_variant "BwClz"
  | BW_CTZ -> unit_variant "BwCtz"
  | BW_FFS -> unit_variant "BwFfs"
  | BW_FLS -> unit_variant "BwFls"
  | BW_Compl -> unit_variant "BwCompl"
  | Abs -> failwith "todo"

let json_of_binop : Terms.binop -> json =
  let open Terms in
  function
  | And -> unit_variant "And"
  | Or -> unit_variant "Or"
  | Implies -> unit_variant "Implies"
  | Add -> unit_variant "Add"
  | Sub -> unit_variant "Sub"
  | Mul -> unit_variant "Mul"
  | Div -> unit_variant "Div"
  | Exp -> unit_variant "Exp"
  | Rem -> unit_variant "Rem"
  | Mod -> unit_variant "Mod"
  | BW_Xor -> unit_variant "BwXor"
  | BW_And -> unit_variant "BwAnd"
  | BW_Or -> unit_variant "BwOr"
  | ShiftLeft -> unit_variant "ShiftLeft"
  | ShiftRight -> unit_variant "ShiftRight"
  | LT -> unit_variant "Lt"
  | LE -> unit_variant "Le"
  | Min -> unit_variant "Min"
  | Max -> unit_variant "Max"
  | EQ -> unit_variant "Eq"
  | LTPointer -> unit_variant "LtPointer"
  | LEPointer -> unit_variant "LePointer"
  | SetUnion -> unit_variant "SetUnion"
  | SetIntersection -> unit_variant "SetIntersection"
  | SetDifference -> unit_variant "SetDifference"
  | SetMember -> unit_variant "SetMember"
  | Subset -> unit_variant "Subset"


let rec json_of_pattern (p : BT.t Terms.pattern) : json =
  let (Terms.Pat (p_, bt, _loc)) = p in
  let pattern =
    match p_ with
    | Terms.PSym s -> newtype_variant "Sym" (json_of_sym s)
    | Terms.PWild -> unit_variant "Wild"
    | Terms.PConstructor (s, args) ->
      tuple_variant
        "Constructor"
        [ json_of_sym s;
          `List
            (List.map (fun (id, p') -> pair (json_of_id id) (json_of_pattern p')) args)
        ]
  in
  obj [ ("pattern", pattern); ("bt", json_of_bt bt) ]


(* `IT (term, bt, loc)` becomes {"term": …, "bt": …}; the location is dropped. *)
let rec json_of_it (it : T.t) : json =
  let (Terms.IT (t, bt, _loc)) = it in
  obj [ ("term", json_of_term t); ("bt", json_of_bt bt) ]


and json_of_named_terms (xs : (Id.t * T.t) list) : json =
  `List (List.map (fun (id, it) -> pair (json_of_id id) (json_of_it it)) xs)


and json_of_term (t : BT.t Terms.term) : json =
  let open Terms in
  match t with
  | Const c -> newtype_variant "Const" (json_of_const c)
  | Sym s -> newtype_variant "Sym" (json_of_sym s)
  | Unop (op, a) -> tuple_variant "Unop" [ json_of_unop op; json_of_it a ]
  | Binop (op, a, b) ->
    tuple_variant "Binop" [ json_of_binop op; json_of_it a; json_of_it b ]
  | ITE (c, a, b) -> tuple_variant "Ite" [ json_of_it c; json_of_it a; json_of_it b ]
  | EachI ((start, (s, bt), stop), body) ->
    record_variant
      "EachI"
      [ ("start", `Int start);
        ("var", pair (json_of_sym s) (json_of_bt bt));
        ("end", `Int stop);
        ("body", json_of_it body)
      ]
  | Tuple xs -> newtype_variant "Tuple" (`List (List.map json_of_it xs))
  | NthTuple (n, x) -> tuple_variant "NthTuple" [ `Int n; json_of_it x ]
  | Struct (tag, members) ->
    tuple_variant "Struct" [ json_of_sym tag; json_of_named_terms members ]
  | StructMember (x, id) -> tuple_variant "StructMember" [ json_of_it x; json_of_id id ]
  | StructUpdate ((target, member), value) ->
    record_variant
      "StructUpdate"
      [ ("target", json_of_it target);
        ("member", json_of_id member);
        ("value", json_of_it value)
      ]
  | Record members -> newtype_variant "Record" (json_of_named_terms members)
  | RecordMember (x, id) -> tuple_variant "RecordMember" [ json_of_it x; json_of_id id ]
  | RecordUpdate ((target, member), value) ->
    record_variant
      "RecordUpdate"
      [ ("target", json_of_it target);
        ("member", json_of_id member);
        ("value", json_of_it value)
      ]
  | Constructor (s, args) ->
    tuple_variant "Constructor" [ json_of_sym s; json_of_named_terms args ]
  | MemberShift (x, tag, id) ->
    tuple_variant "MemberShift" [ json_of_it x; json_of_sym tag; json_of_id id ]
  | ArrayShift { base; ct; index } ->
    record_variant
      "ArrayShift"
      [ ("base", json_of_it base); ("ct", json_of_sct ct); ("index", json_of_it index) ]
  | CopyAllocId { addr; loc } ->
    record_variant "CopyAllocId" [ ("addr", json_of_it addr); ("loc", json_of_it loc) ]
  | HasAllocId x -> newtype_variant "HasAllocId" (json_of_it x)
  | SizeOf ct -> newtype_variant "SizeOf" (json_of_sct ct)
  | OffsetOf (tag, id) -> tuple_variant "OffsetOf" [ json_of_sym tag; json_of_id id ]
  | Nil bt -> newtype_variant "Nil" (json_of_bt bt)
  | Cons (h, t') -> tuple_variant "Cons" [ json_of_it h; json_of_it t' ]
  | Head x -> newtype_variant "Head" (json_of_it x)
  | Tail x -> newtype_variant "Tail" (json_of_it x)
  | Representable (ct, x) ->
    tuple_variant "Representable" [ json_of_sct ct; json_of_it x ]
  | Good (ct, x) -> tuple_variant "Good" [ json_of_sct ct; json_of_it x ]
  | Aligned { t = x; align } ->
    record_variant "Aligned" [ ("t", json_of_it x); ("align", json_of_it align) ]
  | WrapI (ity, x) -> tuple_variant "WrapI" [ json_of_ity ity; json_of_it x ]
  | MapConst (bt, x) -> tuple_variant "MapConst" [ json_of_bt bt; json_of_it x ]
  | MapSet (m, k, v) ->
    tuple_variant "MapSet" [ json_of_it m; json_of_it k; json_of_it v ]
  | MapGet (m, k) -> tuple_variant "MapGet" [ json_of_it m; json_of_it k ]
  | MapDef ((s, bt), x) ->
    tuple_variant "MapDef" [ pair (json_of_sym s) (json_of_bt bt); json_of_it x ]
  | Apply (f, args) ->
    tuple_variant "Apply" [ json_of_sym f; `List (List.map json_of_it args) ]
  | Let ((s, bound), body) ->
    tuple_variant "Let" [ pair (json_of_sym s) (json_of_it bound); json_of_it body ]
  | Match (scrutinee, arms) ->
    tuple_variant
      "Match"
      [ json_of_it scrutinee;
        `List (List.map (fun (p, x) -> pair (json_of_pattern p) (json_of_it x)) arms)
      ]
  | Cast (bt, x) -> tuple_variant "Cast" [ json_of_bt bt; json_of_it x ]
  | CN_None bt -> newtype_variant "CnNone" (json_of_bt bt)
  | CN_Some x -> newtype_variant "CnSome" (json_of_it x)
  | IsSome x -> newtype_variant "IsSome" (json_of_it x)
  | GetOpt x -> newtype_variant "GetOpt" (json_of_it x)


(* ------------------------------------------------- constraints, requests *)

(* A `Forall` is exported with its permission split out of the body: the
   frontend always builds the body as `impl_ (permission, body)`
   (`compile.ml:1208,1243` are the only construction sites that reach an
   exported specification), and AustenTest's `LogicalConstraint::Forall`
   carries the two halves as fields — the `_NoSMT` collapse's rule, one
   construct over: normalize CN's internal spelling into what the term
   means. A forall that is not an implication has no AustenTest rendering;
   Fulminate refuses the same shape (`cn_to_ail.ml:3455`). *)
let json_of_lc : LC.t -> json = function
  | LC.T it -> newtype_variant "T" (json_of_it it)
  | LC.Forall ((s, bt), Terms.IT (Terms.Binop (Terms.Implies, permission, body), _, _)) ->
    record_variant
      "Forall"
      [ ("var", pair (json_of_sym s) (json_of_bt bt));
        ("permission", json_of_it permission);
        ("body", json_of_it body)
      ]
  | LC.Forall _ ->
    raise (Unrepresentable "a forall constraint that is not an implication")


let json_of_init : Req.init -> json = function
  | Req.Init -> unit_variant "Init"
  | Req.Uninit -> unit_variant "Uninit"


let json_of_req_name : Req.name -> json = function
  | Req.Owned (ct, init) -> tuple_variant "Owned" [ json_of_sct ct; json_of_init init ]
  | Req.PName s -> newtype_variant "PName" (json_of_sym s)


(* `q_loc` goes with every other location. *)
let json_of_req : Req.t -> json = function
  | Req.P p ->
    record_variant
      "P"
      [ ("name", json_of_req_name p.name);
        ("pointer", json_of_it p.pointer);
        ("iargs", `List (List.map json_of_it p.iargs))
      ]
  | Req.Q q ->
    record_variant
      "Q"
      [ ("name", json_of_req_name q.name);
        ("pointer", json_of_it q.pointer);
        ("q", pair (json_of_sym (fst q.q)) (json_of_bt (snd q.q)));
        ("step", json_of_sct q.step);
        ("permission", json_of_it q.permission);
        ("iargs", `List (List.map json_of_it q.iargs))
      ]


(* ------------------------------------------------------ LAT, AT, LRT, RT *)

(* `info` is dropped from every binder. *)
let rec json_of_lat : 'i. ('i -> json) -> 'i LAT.t -> json =
  fun json_of_i lat ->
  match lat with
  | LAT.Define ((s, it), _info, rest) ->
    tuple_variant
      "Define"
      [ pair (json_of_sym s) (json_of_it it); json_of_lat json_of_i rest ]
  | LAT.Resource ((s, (req, bt)), _info, rest) ->
    tuple_variant
      "Resource"
      [ pair (json_of_sym s) (pair (json_of_req req) (json_of_bt bt));
        json_of_lat json_of_i rest
      ]
  | LAT.Constraint (lc, _info, rest) ->
    tuple_variant "Constraint" [ json_of_lc lc; json_of_lat json_of_i rest ]
  | LAT.I i -> newtype_variant "I" (json_of_i i)


let rec json_of_at : 'i. ('i -> json) -> 'i AT.t -> json =
  fun json_of_i at ->
  match at with
  | AT.Computational ((s, bt), _info, rest) ->
    tuple_variant
      "Computational"
      [ pair (json_of_sym s) (json_of_bt bt); json_of_at json_of_i rest ]
  | AT.Ghost ((s, bt), _info, rest) ->
    tuple_variant
      "Ghost"
      [ pair (json_of_sym s) (json_of_bt bt); json_of_at json_of_i rest ]
  | AT.L lat -> newtype_variant "L" (json_of_lat json_of_i lat)


(* `LogicalReturnTypes.t` is `LogicalArgumentTypes.t` with nothing at the end
   of it. `I` is emitted as {"I": null} rather than the bare "I" a literal
   transcription would give, so a postcondition chain is byte-identical to a
   `LAT.t` whose terminal is unit and the far side needs no second chain type
   to read it. *)
let rec json_of_lrt (lrt : LRT.t) : json =
  match lrt with
  | LRT.Define ((s, it), _info, rest) ->
    tuple_variant "Define" [ pair (json_of_sym s) (json_of_it it); json_of_lrt rest ]
  | LRT.Resource ((s, (req, bt)), _info, rest) ->
    tuple_variant
      "Resource"
      [ pair (json_of_sym s) (pair (json_of_req req) (json_of_bt bt)); json_of_lrt rest ]
  | LRT.Constraint (lc, _info, rest) ->
    tuple_variant "Constraint" [ json_of_lc lc; json_of_lrt rest ]
  | LRT.I -> newtype_variant "I" `Null


(* The postcondition: the binder standing for the returned value, which the
   `ensures` terms refer to, and the chain those clauses desugar into. Its
   `Resource`s are kept rather than filtered out — a postcondition's `take` is
   the ownership the function hands back, and dropping it would make a
   specification look like it returns none. *)
let json_of_rt (rt : RT.t) : json =
  let (RT.Computational ((s, bt), _info, lrt)) = rt in
  obj [ ("var", pair (json_of_sym s) (json_of_bt bt)); ("body", json_of_lrt lrt) ]


(* ---------------------------------------------------------- declarations *)

let json_of_clause (c : Definition.Clause.t) : json =
  obj
    [ ("guard", json_of_it c.guard); ("packing_ft", json_of_lat json_of_it c.packing_ft) ]


let json_of_predicate (name : Sym.t) (p : Definition.Predicate.t) : json =
  (* AustenTest has no equivalent of `nounfold`, so it will unfold a
     predicate CN would not. Worth saying out loud rather than exporting a
     definition that behaves differently on the far side. *)
  if Definition.Predicate.is_nounfold p then
    Pp.warn_noloc
      (Pp.string
         ("predicate "
          ^ Sym.pp_string_no_nums name
          ^ " is [nounfold]; AustenTest has no equivalent and will unfold it"));
  obj
    [ ("name", json_of_sym name);
      ("pointer", json_of_sym p.pointer);
      ( "iargs",
        `List (List.map (fun (s, bt) -> pair (json_of_sym s) (json_of_bt bt)) p.iargs) );
      ("oarg", json_of_bt (snd p.oarg));
      ( "clauses",
        match p.clauses with
        | None -> `Null
        | Some cs -> `List (List.map json_of_clause cs) )
    ]


let json_of_logical_function (name : Sym.t) (f : Definition.Function.t) : json =
  obj
    [ ("name", json_of_sym name);
      ( "args",
        `List (List.map (fun (s, bt) -> pair (json_of_sym s) (json_of_bt bt)) f.args) );
      ("return_bt", json_of_bt f.return_bt);
      ( "body",
        match f.body with
        | Definition.Function.Uninterp -> `Null
        | Definition.Function.Def it -> newtype_variant "Def" (json_of_it it)
        | Definition.Function.Rec_Def it -> newtype_variant "RecDef" (json_of_it it) )
    ]


let json_of_datatype (name : Sym.t) (dt : Mucore.datatype) : json =
  obj
    [ ("name", json_of_sym name);
      ( "constructors",
        `List
          (List.map
             (fun (cname, params) ->
                obj
                  [ ("name", json_of_sym cname);
                    ( "params",
                      `List
                        (List.map
                           (fun (id, bt) -> pair (json_of_id id) (json_of_bt bt))
                           params) )
                  ])
             dt.cases) )
    ]


(* ----------------------------------------------------------------- CType *)

(* AustenTest's `CType` is its own type, distinct from `Sctype`: the C
   signature as the shim needs it, and internally tagged
   ({"kind": "int", "bits": 32, "signed": true}). *)
let rec json_of_ctype (ct : Sctypes.t) : json =
  match ct with
  | Sctypes.Void -> obj [ ("kind", `String "void") ]
  | Sctypes.Integer Sctypes.IntegerTypes.Bool -> obj [ ("kind", `String "bool") ]
  | Sctypes.Integer ity ->
    (* An enum resolves to its width and signedness rather than being
       exported as such: AustenTest fails closed on `Enum` in every sizing
       position, so a resolved approximation beats an unusable exact type. *)
    obj
      [ ("kind", `String "int");
        ("bits", `Int (8 * Memory.size_of_integer_type ity));
        ("signed", `Bool (Memory.is_signed_integer_type ity))
      ]
  | Sctypes.Pointer ct' ->
    obj [ ("kind", `String "pointer"); ("pointee", json_of_ctype ct') ]
  | Sctypes.Array (ct', _) ->
    (* Decay, which is what the C ABI does to an array parameter anyway. A
       struct *field* instead goes through [json_of_field_ctype] below, so its
       fixed bound remains part of the layout AustenTest sizes heap writes
       with. *)
    obj [ ("kind", `String "pointer"); ("pointee", json_of_ctype ct') ]
  | Sctypes.Struct tag ->
    obj [ ("kind", `String "named"); ("name", `String (Sym.pp_string_no_nums tag)) ]
  | Sctypes.Function _ -> raise (Unrepresentable "a function type")
  | Sctypes.Byte -> raise (Unrepresentable "the `byte` type")


let rec json_of_field_ctype (ct : Sctypes.t) : json =
  match ct with
  | Sctypes.Array (ct', Some length) ->
    obj
      [ ("kind", `String "array");
        ("element", json_of_field_ctype ct');
        ("length", `Int length)
      ]
  | Sctypes.Array (_, None) -> raise (Unrepresentable "an unsized array field")
  | _ -> json_of_ctype ct


(* ------------------------------------------------------------ collectors *)

(* `types`: the struct layouts, from which AustenTest computes member offsets
   and hence the extent of a whole-node `asgn`. `Memory.member_types` drops
   the padding pieces. *)
let json_of_structs (prog5 : unit Mucore.file) : json list =
  Pmap.fold
    (fun tag (def : Mucore.tag_definition) acc ->
       match def with
       | Mucore.UnionDef ->
         Pp.warn_noloc
           (Pp.string
              ("union "
               ^ Sym.pp_string_no_nums tag
               ^ " has no AustenTest counterpart and is not exported"));
         acc
       | Mucore.StructDef layout ->
         let fields =
           List.map
             (fun (id, ct) ->
                let ty = json_of_field_ctype ct in
                obj [ ("name", `String (Id.get_string id)); ("type", ty) ])
             (Memory.member_types layout)
         in
         obj
           [ ("name", `String (Sym.pp_string_no_nums tag));
             ("tag", json_of_sym tag);
             ("fields", `List fields)
           ]
         :: acc)
    prog5.tagDefs
    []


(* The C signature of one exported function, recovered from the Ail sigma
   exactly where `bennet/stage1/convert.ml` recovers its own `c_types`.

   A declared-but-undefined function has no parameter names; positional ones
   are used, since AustenTest matches `params` against the argument type's
   computational binders by position, not by name. *)
(* A C parameter's symbol is a Cerberus *object address* (`SD_ObjectAddress`),
   which pretty-prints as `&x`. AustenTest splices this name straight into the
   generated `extern "C"` declaration, so it has to be a plain identifier. *)
let param_name (s : Sym.t) : string =
  match Sym.description s with
  | Cerb_frontend.Symbol.SD_ObjectAddress name -> name
  | _ -> Sym.pp_string_no_nums s


let c_signature sigma (fn : Sym.t) : (string * Sctypes.t) list * Sctypes.t =
  let open Cerb_frontend in
  let decl = List.assoc_opt Sym.equal fn sigma.AilSyntax.declarations in
  match decl with
  | Some (_, _, AilSyntax.Decl_function (_, (_, ret_ct), arg_cts, _, _, _)) ->
    let names =
      match List.assoc_opt Sym.equal fn sigma.AilSyntax.function_definitions with
      | Some (_, _, _, param_names, _) -> List.map param_name param_names
      | None -> List.mapi (fun i _ -> "arg" ^ string_of_int i) arg_cts
    in
    let sct_of ct =
      match Sctypes.of_ctype ct with
      | Some sct -> sct
      | None ->
        raise
          (Unrepresentable
             "a C type Sctypes cannot express (a float, an atomic, or a union)")
    in
    let args = List.map (fun (_quals, ct, _reg) -> sct_of ct) arg_cts in
    let names =
      if List.length names = List.length args then
        names
      else
        List.mapi (fun i _ -> "arg" ^ string_of_int i) args
    in
    (List.combine names args, sct_of ret_ct)
  | _ -> raise (Unrepresentable "no C declaration")


let test_name filename (test : Test.t) : string =
  if test.is_static then
    Fulminate.Utils.static_prefix filename ^ "_" ^ Sym.pp_string test.fn
  else
    Sym.pp_string test.fn


(* A global object has two identities which must not be conflated. [address]
   is the logical symbol occurring in CN terms, while [c_name] is the source
   identifier the target linker exposes (or which AustenTest promotes for an
   internal-linkage object). *)
let global_c_name (sym : Sym.t) : string =
  match Sym.description sym with
  | Cerb_frontend.Symbol.SD_ObjectAddress name -> name
  | _ ->
    raise
      (Unrepresentable
         ("global " ^ Sym.pp_string_no_nums sym ^ " has no C object-address identifier"))


let has_external_linkage sigma (sym : Sym.t) : bool =
  Pmap.fold
    (fun _ (candidate, _) found -> found || Sym.equal sym candidate)
    sigma.Cerb_frontend.AilSyntax.extern_idmap
    false


let rec validate_global_type_with find_tag (ct : Sctypes.t) : unit =
  match ct with
  | Sctypes.Integer _ | Sctypes.Pointer _ -> ()
  | Sctypes.Array (_, None) -> raise (Unrepresentable "an incomplete global array")
  | Sctypes.Array (_, Some n) when n <= 0 ->
    raise (Unrepresentable "a zero-sized global array")
  | Sctypes.Array (elem, Some _) -> validate_global_type_with find_tag elem
  | Sctypes.Struct tag ->
    (match find_tag tag with
     | Some (Mucore.StructDef _) -> ()
     | Some Mucore.UnionDef ->
       raise (Unrepresentable ("global " ^ Sym.pp_string_no_nums tag ^ " has union type"))
     | None ->
       raise
         (Unrepresentable
            ("global " ^ Sym.pp_string_no_nums tag ^ " has incomplete struct type")))
  | Sctypes.Void -> raise (Unrepresentable "a void global")
  | Sctypes.Function _ -> raise (Unrepresentable "a function global")
  | Sctypes.Byte -> raise (Unrepresentable "a byte-typed global")


let validate_global_type (prog5 : unit Mucore.file) =
  validate_global_type_with (fun tag -> Pmap.lookup tag prog5.tagDefs)


let json_of_global_linkage ~is_external ~owner ~c_name =
  if is_external then
    obj [ ("kind", `String "external") ]
  else (
    match owner with
    | Some owner -> obj [ ("kind", `String "internal"); ("owner", `String owner) ]
    | None ->
      raise
        (Unrepresentable
           ("internal global " ^ c_name ^ " has no exported non-static owner function")))


let first_non_static_owner candidates =
  List.find_map
    (fun (is_static, name) -> if is_static then None else Some name)
    candidates


let global_declaration sigma prog5 (sym : Sym.t) (ct : Sctypes.t) =
  let open Cerb_frontend in
  match List.assoc_opt Sym.equal sym sigma.AilSyntax.declarations with
  | Some (_, _, AilSyntax.Decl_object ((AilSyntax.Thread, _), _, _, _)) ->
    raise (Unrepresentable ("thread-local global " ^ global_c_name sym))
  | Some (_, _, AilSyntax.Decl_object ((AilSyntax.Automatic, _), _, _, _)) ->
    raise
      (Unrepresentable ("automatic object " ^ global_c_name sym ^ " used as a global"))
  | Some (_, _, AilSyntax.Decl_object ((AilSyntax.Static, true), _, _, _)) ->
    raise (Unrepresentable ("register object " ^ global_c_name sym ^ " used as a global"))
  | Some (_, _, AilSyntax.Decl_object ((AilSyntax.Static, false), _, quals, ail_ct)) ->
    if quals.const then
      raise (Unrepresentable ("const global " ^ global_c_name sym));
    if quals.volatile then
      raise (Unrepresentable ("volatile global " ^ global_c_name sym));
    (match Sctypes.of_ctype ail_ct with
     | None ->
       raise
         (Unrepresentable
            ("global " ^ global_c_name sym ^ " has an atomic, floating, or union type"))
     | Some _ -> ());
    validate_global_type prog5 ct;
    quals
  | Some (_, _, AilSyntax.Decl_function _) ->
    raise (Unrepresentable ("function " ^ global_c_name sym ^ " used as a global object"))
  | None ->
    raise (Unrepresentable ("global " ^ global_c_name sym ^ " has no C declaration"))


(* Symbol numbers are the identity in both CN and AustenTest. Looking for the
   emitted serde symbol record means this collector automatically covers every
   exported term position, including postconditions and logical functions,
   without maintaining a second free-variable walk beside the serializer. *)
let rec json_mentions_sym (sym : Sym.t) (j : json) : bool =
  match j with
  | `Assoc fields ->
    let here =
      match List.assoc_opt String.equal "num" fields with
      | Some (`Int n) -> Int.equal n (Sym.num sym)
      | _ -> false
    in
    here || List.exists (fun (_, value) -> json_mentions_sym sym value) fields
  | `List values -> List.exists (json_mentions_sym sym) values
  | _ -> false


let json_of_global sigma prog5 owner (surface : json) (sym, glob) : json option =
  if not (json_mentions_sym sym surface) then
    None
  else (
    let ct = match glob with Mucore.GlobalDef (ct, _) | Mucore.GlobalDecl ct -> ct in
    let quals = global_declaration sigma prog5 sym ct in
    let linkage =
      json_of_global_linkage
        ~is_external:(has_external_linkage sigma sym)
        ~owner
        ~c_name:(global_c_name sym)
    in
    Some
      (obj
         [ ("address", json_of_sym sym);
           ("c_name", `String (global_c_name sym));
           ("type", json_of_sct ct);
           ("linkage", linkage);
           ("qualifiers", json_of_quals quals)
         ]))


let json_of_function_linkage (test : Test.t) : json =
  if test.is_static then
    `String "internal"
  else
    `String "external"


let json_of_test _filename sigma (test : Test.t) : json =
  let name = Sym.pp_string test.fn in
  let params, ret = c_signature sigma test.fn in
  let post, _fn_body = AT.get_return test.internal in
  obj
    [ ("name", `String name);
      ("linkage", json_of_function_linkage test);
      ( "params",
        `List
          (List.map
             (fun (pname, ct) ->
                obj [ ("name", `String pname); ("type", json_of_ctype ct) ])
             params) );
      ("return", json_of_ctype ret);
      (* AustenTest's ArgType terminal is `()`: it generates inputs, and the
         terminal is where CN keeps the things that are not inputs. *)
      ("internal", json_of_at (fun _ -> `Null) test.internal);
      (* The postcondition, beside `internal` rather than at its terminal
         where CN keeps it. Leaving the terminal `null` is what makes this
         additive: an AustenTest that has no `ensures` field yet still reads
         every specification exported here, and one that grows one does not
         have to re-type `ArgType`. The cost is that the chain of `Define`s
         and `Resource`s binding these terms' free variables lives in the
         sibling field, so a reader has to associate the two by symbol
         number rather than by nesting. *)
      ("ensures", json_of_rt post)
    ]


(* The first symbol number not already used. AustenTest allocates fresh
   symbols from here when it destructures draws and inserts scopes; left out,
   it scans the module and starts above the largest number it finds.

   Computed from the emitted JSON rather than taken from `Sym.fresh_int ()`
   so the file is reproducible: the global counter depends on how much of the
   pipeline has run, so exporting it would churn every file on every run. *)
let rec max_sym_num (j : json) : int =
  match j with
  | `Assoc fields ->
    let here =
      match List.assoc_opt String.equal "num" fields with Some (`Int n) -> n | _ -> -1
    in
    List.fold_left (fun acc (_, v) -> max acc (max_sym_num v)) here fields
  | `List xs -> List.fold_left (fun acc v -> max acc (max_sym_num v)) (-1) xs
  | _ -> -1


(* ----------------------------------------------------------------- entry *)

(* One `SpecModule`, covering one translation unit and every representable
   function definition. The order of `functions` is the selector ABI:
   AustenTest prefixes each input with the exact zero-based position of its
   target in this array. MuCore's function map supplies CN's deterministic
   definition-discovery order, and the filtering in [save] retains the
   relative order of every survivor. *)
let json_of_module
      ~filename
      sigma
      (prog5 : unit Mucore.file)
      (functions : (Test.t * string * json) list)
  : json
  =
  let surface_fields =
    [ ("filename", `String (Filename.basename filename));
      ("types", `List (json_of_structs prog5));
      ( "predicates",
        `List
          (List.map (fun (name, p) -> json_of_predicate name p) prog5.resource_predicates)
      );
      ( "datatypes",
        `List (List.map (fun (name, dt) -> json_of_datatype name dt) prog5.datatypes) );
      ( "logical_functions",
        `List
          (List.map
             (fun (name, f) -> json_of_logical_function name f)
             prog5.logical_predicates) );
      ("functions", `List (List.map (fun (_, _, json) -> json) functions))
    ]
  in
  let surface = obj surface_fields in
  let owner =
    first_non_static_owner
      (List.map (fun (test, name, _) -> (test.Test.is_static, name)) functions)
  in
  let globals = List.filter_map (json_of_global sigma prog5 owner surface) prog5.globs in
  let fields = ("globals", `List globals) :: surface_fields in
  obj (("next_sym", `Int (max_sym_num (obj fields) + 1)) :: fields)


(* Write one ordered specification module for the translation unit.

   A function whose signature AustenTest cannot represent is skipped with a
   warning rather than failing the run: the point of a corpus export is to
   get as much across as possible and say plainly what did not make it. Its
   removal necessarily closes the selector array around it; the exact IDs are
   the positions in the module that is actually written, never positions in
   CN's pre-filter list.

   In contrast, exceeding the one-byte selector capacity or producing
   duplicate exported names refuses the entire module. Truncating would hide
   otherwise supported roots, while duplicate names would make
   AustenTest's --function filter ambiguous. *)
let save ~path ~filename sigma prog5 (tests : Test.t list) : unit =
  let functions =
    List.filter_map
      (fun (test : Test.t) ->
         match json_of_test filename sigma test with
         | function_json -> Some (test, test_name filename test, function_json)
         | exception Unrepresentable what ->
           Pp.warn_noloc
             (Pp.string
                ("not exporting "
                 ^ Sym.pp_string_no_nums test.fn
                 ^ " from "
                 ^ Filename.basename filename
                 ^ ": AustenTest cannot represent "
                 ^ what));
           None)
      tests
  in
  let rec duplicate_name seen = function
    | [] -> None
    | (_, name, _) :: rest ->
      if List.exists (String.equal name) seen then
        Some name
      else
        duplicate_name (name :: seen) rest
  in
  match functions with
  | [] -> ()
  | _ when List.length functions > 256 ->
    Pp.warn_noloc
      (Pp.string
         ("not exporting "
          ^ Filename.basename filename
          ^ ": AustenTest's one-byte function selector supports at most 256 functions, \
             but "
          ^ string_of_int (List.length functions)
          ^ " are representable"))
  | _ ->
    (match duplicate_name [] functions with
     | Some name ->
       Pp.warn_noloc
         (Pp.string
            ("not exporting "
             ^ Filename.basename filename
             ^ ": more than one function has the exported name "
             ^ name))
     | None ->
       (match json_of_module ~filename sigma prog5 functions with
        | j ->
          let oc = open_out path in
          output_string oc (Yojson.Safe.pretty_to_string j);
          output_char oc '\n';
          close_out oc
        | exception Unrepresentable what ->
          Pp.warn_noloc
            (Pp.string
               ("not exporting "
                ^ Filename.basename filename
                ^ ": AustenTest cannot represent "
                ^ what))))
