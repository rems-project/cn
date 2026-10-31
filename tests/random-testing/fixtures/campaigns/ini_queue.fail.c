/*@
// copying from list_cn_types.h
datatype seq {
  Seq_Nil {},
  Seq_Cons {integer head, datatype seq tail}
}
@*/

/*@
function [rec] (datatype seq) snoc(datatype seq xs, integer y) {
  match xs {
    Seq_Nil {} => {
      Seq_Cons {head: y, tail: Seq_Nil{}}
    }
    Seq_Cons {head: x, tail: zs}  => {
      Seq_Cons{head: x, tail: snoc (zs, y)}
    }
  }
}
@*/

/*@
function (integer) queue_size (integer inp, integer outp, integer bufsize)
{
  rem((inp - outp) + bufsize, bufsize)
}


function [rec] (datatype seq) seq_of_buf (map<integer,integer> buf, integer inp, integer outp, integer bufsize) {
  if (queue_size (inp, outp, bufsize) > 0) {
    Seq_Cons {
      head: buf[outp],
      tail: seq_of_buf(buf, inp, rem(outp + 1, bufsize), bufsize)
    }
  }
  else {
    Seq_Nil {}
  }
}

@*/


struct queue
{
  int inp;
  int outp;
  int size;
  int* buf;
};

/*@
function (boolean) queue_wf (integer inp, integer outp, integer bufsize)
{
  bufsize > 0
  && bufsize + bufsize <= 2147483647
  && (0 <= inp && inp < bufsize)
  && (0 <= outp && outp < bufsize)
}


type_synonym state = {
  datatype seq content,
  integer size  // max size
}

predicate state QueueAbs(pointer p)
{
  take q = Owned<struct queue>(p);
  take buf = each (integer i; 0 <= i && i < q.size) { Owned<int>(q.buf + i) };
  assert (queue_wf (q.inp, q.outp, q.size));
  let content = seq_of_buf(buf, q.inp, q.outp, q.size);
  return {content: content, size: q.size - 1};
}

@*/

// This is invalid because the precondition does not require free space in
// the queue (nothing bounds the length of `queue.content` by `queue.size`),
// so `put` may overrun a full queue.
void put(struct queue* q, int n)
/*@ requires take queue = QueueAbs(q);
             let expected_content = snoc(queue.content, n);  // Why not inline below?
    ensures take queue_out = QueueAbs(q);
            queue_out.content == expected_content;
            queue_out.size == queue.size;
@*/
{
  /*@ extract Owned<int>, q->inp; @*/
  q->buf[q->inp] = n;
  q->inp = (q->inp + 1) % q->size;
}
