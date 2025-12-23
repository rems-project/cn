(** Tests for DestructProducts Stage1 transformation *)

open OUnit2
module BT = Cn.BaseTypes
module IT = Cn.IndexTerms
module LC = Cn.LogicalConstraints
module Sym = Cn.Sym
module Memory = Cn.Memory
module Mucore = Cn.Mucore
module Id = Cn.Id
module Sctypes = Cn.Sctypes
module Pp = Cn.Pp

(* Use Ownership domain for testing *)
module Domain = Cn.TestGeneration.Private.Bennet.Private.AbstractDomains.Ownership

module DestructProducts =
  Cn.TestGeneration.Private.Bennet.Private.Stage1.Private.DestructProducts.Make (Domain)

module Term = Cn.TestGeneration.Private.Bennet.Private.Stage1.Private.Term.Make (Domain)

module Def = Cn.TestGeneration.Private.Bennet.Private.Stage1.Private.Def.Make (Domain)

module Ctx = Cn.TestGeneration.Private.Bennet.Private.Stage1.Private.Ctx.Make (Domain)

(** Helper: Create test location *)
let test_loc = Cerb_location.unknown

(** Helper: Pretty-print a base type *)
let pp_basetype bt = Pp.plain (BT.pp bt)

(** Helper: Create a struct member *)
let make_member name stype size offset =
  { Memory.member_or_padding = Some (Id.make test_loc name, stype); size; offset }


(** Helper: Assert that an argument has the expected type *)
let assert_arg_type ~msg expected idx iargs =
  let actual = snd (List.nth iargs idx) in
  assert_equal ~msg ~cmp:BT.equal ~printer:pp_basetype expected actual


(** Helper: Assert that an argument has the expected name *)
let assert_arg_name ~msg expected idx iargs =
  let actual = fst (List.nth iargs idx) |> Sym.pp_string in
  assert_bool (Printf.sprintf "%s, got '%s'" msg actual) (String.equal expected actual)


(** Helper: Find a struct tag in the program that matches a predicate *)
let find_struct_tag prog predicate =
  Pmap.bindings_list prog.Mucore.tagDefs |> List.find predicate |> fst


(** Helper: Check if a struct has a member with the given name *)
let has_member member_name = function
  | Mucore.StructDef pieces ->
    List.exists
      (fun p ->
         match p.Memory.member_or_padding with
         | Some (member, _) -> Id.equal member (Id.make test_loc member_name)
         | None -> false)
      pieces
  | _ -> false


(** Helper: Get the number of pieces in a struct *)
let num_pieces = function Mucore.StructDef pieces -> List.length pieces | _ -> 0

(** Helper: Create a test Mucore file with struct definitions *)
let make_test_prog_with_struct () : unit Mucore.file =
  (* Create struct abc with fields x: u16 and y: struct def *)
  (* struct def has field y: u8 *)
  let def_tag = Sym.fresh "def" in
  let abc_tag = Sym.fresh "abc" in
  let def_struct =
    Mucore.StructDef
      [ make_member
          "y"
          (Sctypes.Integer
             (Sctypes.IntegerTypes.Unsigned (Sctypes.IntegerBaseTypes.IntN_t 8)))
          1
          0
      ]
  in
  let abc_struct =
    Mucore.StructDef
      [ make_member
          "x"
          (Sctypes.Integer
             (Sctypes.IntegerTypes.Unsigned (Sctypes.IntegerBaseTypes.IntN_t 16)))
          2
          0;
        make_member "y" (Sctypes.Struct def_tag) 1 2
      ]
  in
  let tag_defs =
    Pmap.empty Sym.compare |> Pmap.add def_tag def_struct |> Pmap.add abc_tag abc_struct
  in
  { Mucore.main = None;
    tagDefs = tag_defs;
    globs = [];
    funs = Pmap.empty Sym.compare;
    extern = Pmap.empty Id.compare;
    stdlib_syms = Sym.Set.empty;
    mk_functions = [];
    resource_predicates = [];
    logical_predicates = [];
    datatypes = [];
    lemmata = [];
    call_funinfo = Pmap.empty Sym.compare
  }


(** Helper: Create a generator definition with assertions *)
let make_test_generator
      (name : Sym.t)
      (iargs : (Sym.t * BT.t) list)
      (assertions : (IT.t * IT.t) list)
  : Def.t
  =
  let rec make_body assertions =
    match assertions with
    | [] -> Term.return_ (IT.unit_ test_loc) () test_loc
    | (lhs, rhs) :: rest ->
      let constraint_it = IT.eq_ (lhs, rhs) test_loc in
      let constraint_lc = LC.T constraint_it in
      let rest_body = make_body rest in
      Term.assert_ (constraint_lc, rest_body) () test_loc
  in
  { Def.filename = "test.c";
    recursive = false;
    spec = false;
    name;
    iargs;
    oarg = BT.Unit;
    c_types = None;
    body = make_body assertions
  }


(** Test: Nested struct argument is flattened *)
let test_nested_struct_destruction _ =
  let prog = make_test_prog_with_struct () in
  let abc_tag =
    find_struct_tag prog (fun (_, def) -> num_pieces def = 2 && has_member "x" def)
  in
  (* Create generator: void ABC(pointer unused, struct abc ghi) *)
  let gen_name = Sym.fresh "ABC" in
  let unused_arg = (Sym.fresh "unused", BT.Loc ()) in
  let ghi_arg = (Sym.fresh "ghi", BT.Struct abc_tag) in
  (* Create assertions: ghi.x == 0u16 and ghi.y.y == 10u8 *)
  let ghi_sym = snd ghi_arg |> fun _bt -> fst ghi_arg in
  let ghi_it = IT.sym_ (ghi_sym, BT.Struct abc_tag, test_loc) in
  let ghi_x =
    IT.member_ ~member_bt:(BT.Bits (Unsigned, 16)) (ghi_it, Id.make test_loc "x") test_loc
  in
  let zero_u16 = IT.num_lit_ (Z.of_int 0) (BT.Bits (Unsigned, 16)) test_loc in
  let def_tag = find_struct_tag prog (fun (_, def) -> num_pieces def = 1) in
  let ghi_y =
    IT.member_ ~member_bt:(BT.Struct def_tag) (ghi_it, Id.make test_loc "y") test_loc
  in
  let ghi_y_y_bt = BT.Bits (Unsigned, 8) in
  let ghi_y_y = IT.member_ ~member_bt:ghi_y_y_bt (ghi_y, Id.make test_loc "y") test_loc in
  let ten_u8 = IT.num_lit_ (Z.of_int 10) ghi_y_y_bt test_loc in
  let gen_def =
    make_test_generator
      gen_name
      [ unused_arg; ghi_arg ]
      [ (ghi_x, zero_u16); (ghi_y_y, ten_u8) ]
  in
  let ctx : Ctx.t = [ (gen_name, gen_def) ] in
  let transformed_ctx = DestructProducts.transform prog ctx in
  let transformed_def = List.assoc gen_name transformed_ctx in
  assert_equal
    ~msg:"Should have 3 flattened arguments"
    3
    (List.length transformed_def.iargs);
  assert_arg_type ~msg:"First arg should be pointer" (BT.Loc ()) 0 transformed_def.iargs;
  assert_arg_type
    ~msg:"Second arg should be u16"
    (BT.Bits (Unsigned, 16))
    1
    transformed_def.iargs;
  assert_arg_type
    ~msg:"Third arg should be u8"
    (BT.Bits (Unsigned, 8))
    2
    transformed_def.iargs;
  assert_arg_name ~msg:"First arg should be 'unused'" "unused" 0 transformed_def.iargs;
  assert_arg_name ~msg:"Second arg should be 'ghi_x0'" "ghi_x0" 1 transformed_def.iargs;
  assert_arg_name
    ~msg:"Third arg should be 'ghi_y0_y0'"
    "ghi_y0_y0"
    2
    transformed_def.iargs


(** Test: Simple struct (non-nested) argument is flattened *)
let test_simple_struct_destruction _ =
  (* Create a simple struct with just one field *)
  let simple_tag = Sym.fresh "simple" in
  let simple_struct =
    Mucore.StructDef
      [ make_member
          "field"
          (Sctypes.Integer
             (Sctypes.IntegerTypes.Signed (Sctypes.IntegerBaseTypes.IntN_t 32)))
          4
          0
      ]
  in
  let prog =
    { Mucore.main = None;
      tagDefs = Pmap.singleton Sym.compare simple_tag simple_struct;
      globs = [];
      funs = Pmap.empty Sym.compare;
      extern = Pmap.empty Id.compare;
      stdlib_syms = Sym.Set.empty;
      mk_functions = [];
      resource_predicates = [];
      logical_predicates = [];
      datatypes = [];
      lemmata = [];
      call_funinfo = Pmap.empty Sym.compare
    }
  in
  let gen_name = Sym.fresh "test_gen" in
  let arg_sym = Sym.fresh "s" in
  let arg = (arg_sym, BT.Struct simple_tag) in
  let s_it = IT.sym_ (arg_sym, BT.Struct simple_tag, test_loc) in
  let s_field =
    IT.member_ ~member_bt:(BT.Bits (Signed, 32)) (s_it, Id.make test_loc "field") test_loc
  in
  let five = IT.num_lit_ (Z.of_int 5) (BT.Bits (Signed, 32)) test_loc in
  let gen_def = make_test_generator gen_name [ arg ] [ (s_field, five) ] in
  let ctx : Ctx.t = [ (gen_name, gen_def) ] in
  let transformed_ctx = DestructProducts.transform prog ctx in
  let transformed_def = List.assoc gen_name transformed_ctx in
  assert_equal
    ~msg:"Should have 1 flattened argument"
    1
    (List.length transformed_def.iargs);
  assert_arg_type
    ~msg:"Argument should be i32"
    (BT.Bits (Signed, 32))
    0
    transformed_def.iargs


(** Test suite *)
let suite =
  "DestructProducts Tests"
  >::: [ "nested struct destruction" >:: test_nested_struct_destruction;
         "simple struct destruction" >:: test_simple_struct_destruction
       ]
