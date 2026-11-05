(* The logical half of Austen export keeps mathematical integers. Austen's
   nominal struct types instead describe C storage, including bitvector fields,
   so logical struct values use records. This pass runs before the boundary
   lowering introduces any raw C values. *)

module BT = BaseTypes
module T = Terms.Normal
module LC = LogicalConstraints
module Req = Request

exception Unrepresentable of string

let struct_members (prog : unit Mucore.file) tag =
  match Pmap.lookup tag prog.tagDefs with
  | Some (Mucore.StructDef layout) ->
    let members, flexible = Memory.member_types layout in
    (match flexible with
     | None -> members
     | Some _ -> raise (Unrepresentable "a logical struct with a flexible array member"))
  | Some Mucore.UnionDef -> raise (Unrepresentable "a logical union value")
  | None -> raise (Unrepresentable "a logical value of incomplete struct type")


let rec bt prog : BT.t -> BT.t = function
  | BT.Struct tag ->
    BT.Record
      (List.map
         (fun (id, ct) -> (id, bt prog (Memory.bt_of_sct ct)))
         (struct_members prog tag))
  | BT.Record members ->
    BT.Record (List.map (fun (id, member) -> (id, bt prog member)) members)
  | BT.Map (key, value) -> BT.Map (bt prog key, bt prog value)
  | BT.List elem -> BT.List (bt prog elem)
  | BT.Tuple elems -> BT.Tuple (List.map (bt prog) elems)
  | BT.Set elem -> BT.Set (bt prog elem)
  | BT.Option elem -> BT.Option (bt prog elem)
  | ( BT.Unit | BT.Bool | BT.Integer | BT.MemByte | BT.Bits _ | BT.Real | BT.Alloc_id
    | BT.Loc ()
    | BT.CType | BT.Datatype _ ) as unchanged ->
    unchanged


let rec pattern prog (Terms.Pat (node, typ, loc)) =
  let node =
    match node with
    | Terms.PSym _ | Terms.PWild -> node
    | Terms.PConstructor (name, fields) ->
      Terms.PConstructor
        (name, List.map (fun (id, value) -> (id, pattern prog value)) fields)
  in
  Terms.Pat (node, bt prog typ, loc)


(* Austen expands a struct value check through its C field types. Expand the
   aggregate checks here instead, so their projections refer to the logical
   records and their integer-valued fields. Scalar checks retain their ordinary
   exported representation. [member] projects a struct field; the lowering pass
   supplies one that reads C storage rather than a logical record. *)
let logical_member prog about id member_ct loc =
  Terms.IT (RecordMember (about, id), bt prog (Memory.bt_of_sct member_ct), loc)


let value_check ~fresh prog ?(member = logical_member prog) ~good ct about loc =
  let rec check ct about =
    match ct with
    | Sctypes.Integer Sctypes.IntegerTypes.Bool ->
      (* Austen's C-type range helper uses the storage byte's range for _Bool.
         CN's logical value must still be exactly zero or one. *)
      let typ = Terms.get_bt about in
      let zero = MakeTerm.num_lit_ Z.zero typ loc in
      let one = MakeTerm.num_lit_ Z.one typ loc in
      MakeTerm.and_ [ MakeTerm.le_ (zero, about) loc; MakeTerm.le_ (about, one) loc ] loc
    | Sctypes.Struct tag ->
      MakeTerm.and_
        (List.map
           (fun (id, member_ct) -> check member_ct (member about id member_ct loc))
           (struct_members prog tag))
        loc
    | Sctypes.Array (_, Some 0) -> MakeTerm.bool_ true loc
    | Sctypes.Array (element_ct, Some count) when count > 0 ->
      let key_bt, element_bt =
        match Terms.get_bt about with
        | BT.Map (key, element) -> (key, element)
        | _ -> raise (Unrepresentable "an array value check on a non-map value")
      in
      let index = fresh "check_index" in
      let index_it = Terms.IT (Sym index, key_bt, loc) in
      let element = Terms.IT (MapGet (about, index_it), element_bt, loc) in
      MakeTerm.eachI_ (0, (index, key_bt), count - 1) (check element_ct element) loc
    | Sctypes.Array _ ->
      raise (Unrepresentable "a value check on an array without a finite size")
    | _ ->
      Terms.IT ((if good then Good (ct, about) else Representable (ct, about)), Bool, loc)
  in
  check ct about


let it ~fresh prog (term : T.t) : T.t =
  Terms.map_term_post
    (fun (Terms.IT (node, typ, loc)) ->
       let rebuild node = Terms.IT (node, bt prog typ, loc) in
       match node with
       | Terms.Good (ct, about) -> value_check ~fresh prog ~good:true ct about loc
       | Terms.Representable (ct, about) ->
         value_check ~fresh prog ~good:false ct about loc
       | Terms.Struct (_, fields) -> rebuild (Terms.Record fields)
       | Terms.StructMember (value, id) -> rebuild (Terms.RecordMember (value, id))
       | Terms.StructUpdate ((value, id), replacement) ->
         rebuild (Terms.RecordUpdate ((value, id), replacement))
       | Terms.Const (Default typ) -> rebuild (Terms.Const (Default (bt prog typ)))
       | Terms.EachI ((start, (sym, typ), stop), body) ->
         rebuild (Terms.EachI ((start, (sym, bt prog typ), stop), body))
       | Terms.Nil typ -> rebuild (Terms.Nil (bt prog typ))
       | Terms.MapConst (typ, value) -> rebuild (Terms.MapConst (bt prog typ, value))
       | Terms.MapDef ((sym, typ), body) ->
         rebuild (Terms.MapDef ((sym, bt prog typ), body))
       | Terms.Match (value, arms) ->
         rebuild
           (Terms.Match
              (value, List.map (fun (pat, body) -> (pattern prog pat, body)) arms))
       | Terms.Cast (typ, value) -> rebuild (Terms.Cast (bt prog typ, value))
       | Terms.CN_None typ -> rebuild (Terms.CN_None (bt prog typ))
       | _ -> rebuild node)
    term


let lc ~fresh prog : LC.t -> LC.t = function
  | LC.T term -> LC.T (it ~fresh prog term)
  | LC.Forall ((sym, typ), body) -> LC.Forall ((sym, bt prog typ), it ~fresh prog body)


let req ~fresh prog : Req.t -> Req.t = function
  | Req.P pred ->
    Req.P
      { pred with
        pointer = it ~fresh prog pred.pointer;
        iargs = List.map (it ~fresh prog) pred.iargs
      }
  | Req.Q pred ->
    Req.Q
      { pred with
        pointer = it ~fresh prog pred.pointer;
        q = (fst pred.q, bt prog (snd pred.q));
        permission = it ~fresh prog pred.permission;
        iargs = List.map (it ~fresh prog) pred.iargs
      }
