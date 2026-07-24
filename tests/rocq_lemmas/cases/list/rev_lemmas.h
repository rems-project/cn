/*@
lemma Append_Nil_RList (datatype List L1)
  requires true;
  ensures Append(L1, Nil{}) == L1;

lemma Append_Cons_RList (datatype List L1, i32 X, datatype List L2)
  requires true;
  ensures Append(L1, Cons {Head: X, Tail: L2})
          == Append(Snoc(L1, X), L2);
@*/
