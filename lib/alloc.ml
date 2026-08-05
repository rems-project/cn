let history_str = "allocs"
let history_sym = Sym.fresh history_str

let predicate_str = "Alloc"
let predicate_sym = Sym.fresh predicate_str


module F (R: Bt_of_sct.Repr) = struct

module History = struct

  module MakeTerm = MakeTerm.F(R)

  let str = history_str

  let sym = history_sym

  let here = Locations.other __LOC__

  let base_id = Id.make here "base"

  let base_bt = R.uintptr_bt

  let size_id = Id.make here "size"

  let size_bt = R.uintptr_bt

  let value_bt = BaseTypes.Record [ (base_id, base_bt); (size_id, size_bt) ]

  let make_value ~base ~size loc =
    MakeTerm.(
      record_ [ (base_id, base); (size_id, num_lit_ (Z.of_int size) size_bt loc) ] loc)


  let bt = BaseTypes.Map (Alloc_id, value_bt)

  let it loc = MakeTerm.sym_ (sym, bt, loc)

  let lookup_ptr ptr loc =
    assert (BaseTypes.(equal (Terms.get_bt ptr) (Loc ())));
    MakeTerm.(map_get_ (it loc) (allocId_ ptr loc) loc)


  type value =
    { base : Terms.Normal.t;
      size : Terms.Normal.t
    }

  let split value loc =
    MakeTerm.
      { base = recordMember_ ~member_bt:base_bt (value, base_id) loc;
        size = recordMember_ ~member_bt:size_bt (value, size_id) loc
      }


  let sbt = BaseTypes.Surface.inj bt
end

module Predicate = struct

  let str = predicate_str

  let loc = Locations.other __MODULE__

  let sym = predicate_sym

  let name = Request.PName sym

  let def =
    Definition.Predicate.
    { loc = Locations.other __LOC__;
      pointer = Sym.fresh "ptr";
      iargs = [];
      oarg = (Locations.other __LOC__, History.value_bt);
      clauses = None;
      recursive = false;
      attrs = []
    }

  let make_request pointer = Request.Predicate.{name; pointer; iargs = []}

end

end
