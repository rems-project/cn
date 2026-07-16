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


(** Helper: Assert the transformed body references no symbol outside its own iargs. A
    dangling symbol means some destructured aggregate (at any nesting depth) was flattened
    away without being rebound from its leaves. *)
let assert_no_dangling (transformed_def : Def.t) =
  let iarg_syms = transformed_def.iargs |> List.map fst |> Sym.Set.of_list in
  let dangling = Sym.Set.diff (Term.free_vars transformed_def.body) iarg_syms in
  assert_bool
    (Printf.sprintf
       "Body must not reference symbols outside its iargs; dangling: %s"
       (dangling |> Sym.Set.elements |> List.map Sym.pp_string |> String.concat ", "))
    (Sym.Set.is_empty dangling)


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
      (assertions : (Cn.Terms.Normal.t * Cn.Terms.Normal.t) list)
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
  assert_arg_name ~msg:"Second arg should be 'ghi_x'" "ghi_x" 1 transformed_def.iargs;
  assert_arg_name ~msg:"Third arg should be 'ghi_y_y'" "ghi_y_y" 2 transformed_def.iargs


(** Test: A whole-value reference to a *nested* struct member (e.g. [ghi.y], not a leaf
    access [ghi.y.y]) must not leave the intermediate symbol [ghi_y] dangling: it has to be
    rebound to a value reconstructed from the leaves, just like the top-level parent. *)
let test_nested_whole_struct_reference _ =
  let prog = make_test_prog_with_struct () in
  let abc_tag =
    find_struct_tag prog (fun (_, def) -> num_pieces def = 2 && has_member "x" def)
  in
  let def_tag = find_struct_tag prog (fun (_, def) -> num_pieces def = 1) in
  let gen_name = Sym.fresh "ABC" in
  let ghi_arg = (Sym.fresh "ghi", BT.Struct abc_tag) in
  let ghi_it = IT.sym_ (fst ghi_arg, BT.Struct abc_tag, test_loc) in
  (* Reference the whole nested struct [ghi.y] (bt = struct def), not a leaf field. After
     member rewriting this becomes a bare [Sym ghi_y]. *)
  let ghi_y =
    IT.member_ ~member_bt:(BT.Struct def_tag) (ghi_it, Id.make test_loc "y") test_loc
  in
  let gen_def = make_test_generator gen_name [ ghi_arg ] [ (ghi_y, ghi_y) ] in
  let ctx : Ctx.t = [ (gen_name, gen_def) ] in
  let transformed_def = List.assoc gen_name (DestructProducts.transform prog ctx) in
  assert_no_dangling transformed_def


(** Test: A tuple argument is flattened into one leaf argument per item. *)
let test_tuple_destruction _ =
  let prog = make_test_prog_with_struct () in
  let gen_name = Sym.fresh "TUP" in
  let i32 = BT.Bits (Signed, 32) in
  let t_arg = (Sym.fresh "t", BT.Tuple [ i32; i32 ]) in
  let t_it = IT.sym_ (fst t_arg, snd t_arg, test_loc) in
  let t0 = IT.nthTuple_ ~item_bt:i32 (0, t_it) test_loc in
  let t1 = IT.nthTuple_ ~item_bt:i32 (1, t_it) test_loc in
  let five = IT.num_lit_ (Z.of_int 5) i32 test_loc in
  let gen_def = make_test_generator gen_name [ t_arg ] [ (t0, five); (t1, five) ] in
  let ctx : Ctx.t = [ (gen_name, gen_def) ] in
  let transformed_def = List.assoc gen_name (DestructProducts.transform prog ctx) in
  assert_equal
    ~msg:"Should have 2 flattened arguments"
    2
    (List.length transformed_def.iargs);
  assert_arg_type ~msg:"First item should be i32" i32 0 transformed_def.iargs;
  assert_arg_type ~msg:"Second item should be i32" i32 1 transformed_def.iargs;
  assert_arg_name ~msg:"First arg should be 't_0'" "t_0" 0 transformed_def.iargs;
  assert_arg_name ~msg:"Second arg should be 't_1'" "t_1" 1 transformed_def.iargs


(** Test: A nested tuple is flattened to leaves, and a whole-value reference to the inner
    tuple element [t.0] must not leave the intermediate tuple symbol [t_0] dangling. *)
let test_nested_tuple_whole_reference _ =
  let prog = make_test_prog_with_struct () in
  let gen_name = Sym.fresh "TUP" in
  let i32 = BT.Bits (Signed, 32) in
  let inner_tup = BT.Tuple [ i32; i32 ] in
  let t_arg = (Sym.fresh "t", BT.Tuple [ inner_tup; i32 ]) in
  let t_it = IT.sym_ (fst t_arg, snd t_arg, test_loc) in
  (* Whole inner tuple element [t.0] (bt = (i32, i32)), not a leaf item. *)
  let t0 = IT.nthTuple_ ~item_bt:inner_tup (0, t_it) test_loc in
  let gen_def = make_test_generator gen_name [ t_arg ] [ (t0, t0) ] in
  let ctx : Ctx.t = [ (gen_name, gen_def) ] in
  let transformed_def = List.assoc gen_name (DestructProducts.transform prog ctx) in
  assert_equal
    ~msg:"Should have 3 flattened leaf arguments"
    3
    (List.length transformed_def.iargs);
  assert_arg_name ~msg:"First arg should be 't_0_0'" "t_0_0" 0 transformed_def.iargs;
  assert_arg_name ~msg:"Second arg should be 't_0_1'" "t_0_1" 1 transformed_def.iargs;
  assert_arg_name ~msg:"Third arg should be 't_1'" "t_1" 2 transformed_def.iargs;
  assert_no_dangling transformed_def


(** Test: A struct nested inside a tuple. A whole-value reference to the struct element
    [t.0] must not leave the intermediate tuple-item symbol [t_0] dangling (recursion runs
    through the tuple arm into the struct arm). *)
let test_tuple_of_struct_whole_reference _ =
  let prog = make_test_prog_with_struct () in
  let def_tag = find_struct_tag prog (fun (_, def) -> num_pieces def = 1) in
  let gen_name = Sym.fresh "TUP" in
  let i32 = BT.Bits (Signed, 32) in
  let t_arg = (Sym.fresh "t", BT.Tuple [ BT.Struct def_tag; i32 ]) in
  let t_it = IT.sym_ (fst t_arg, snd t_arg, test_loc) in
  (* Whole nested struct element [t.0] (bt = struct def), not a leaf field. *)
  let t0 = IT.nthTuple_ ~item_bt:(BT.Struct def_tag) (0, t_it) test_loc in
  let gen_def = make_test_generator gen_name [ t_arg ] [ (t0, t0) ] in
  let ctx : Ctx.t = [ (gen_name, gen_def) ] in
  let transformed_def = List.assoc gen_name (DestructProducts.transform prog ctx) in
  assert_equal
    ~msg:"Should have 2 flattened leaf arguments"
    2
    (List.length transformed_def.iargs);
  assert_arg_name ~msg:"First arg should be 't_0_y'" "t_0_y" 0 transformed_def.iargs;
  assert_arg_name ~msg:"Second arg should be 't_1'" "t_1" 1 transformed_def.iargs;
  assert_no_dangling transformed_def


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
         "nested whole-struct reference" >:: test_nested_whole_struct_reference;
         "tuple destruction" >:: test_tuple_destruction;
         "nested tuple whole reference" >:: test_nested_tuple_whole_reference;
         "tuple of struct whole reference" >:: test_tuple_of_struct_whole_reference;
         "simple struct destruction" >:: test_simple_struct_destruction
       ]
