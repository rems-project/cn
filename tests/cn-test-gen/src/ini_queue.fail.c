/*@
// copying from list_cn_types.h
datatype seq {
  Seq_Nil {},
  Seq_Cons {i32 head, datatype seq tail}
}
@*/

/*@
function [rec] (datatype seq) snoc(datatype seq xs, i32 y) {
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
function (i32) queue_size (i32 inp, i32 outp, i32 bufsize)
{
  ((inp - outp) + bufsize) % bufsize
}


function [rec] (datatype seq) seq_of_buf (map<i32,i32> buf, i32 inp, i32 outp, i32 bufsize) {
  if (queue_size (inp, outp, bufsize) > 0i32) {
    Seq_Cons {
      head: buf[outp],
      tail: seq_of_buf(buf, inp, (outp + 1i32) % bufsize, bufsize)
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
function (boolean) queue_wf (i32 inp, i32 outp, i32 bufsize)
{
  bufsize > 0i32
  && (i64) bufsize + (i64) bufsize <= 2147483647i64
  && (0i32 <= inp && inp < bufsize)
  && (0i32 <= outp && outp < bufsize)
}


type_synonym state = {
  datatype seq content,
  i32 size  // max size
}

predicate state QueueAbs(pointer p)
{
  take q = Owned<struct queue>(p);
  take buf = each (i32 i; 0i32 <= i && i < q.size) { Owned<int>(q.buf + i) };
  assert (queue_wf (q.inp, q.outp, q.size));
  let content = seq_of_buf(buf, q.inp, q.outp, q.size);
  return {content: content, size: q.size - 1i32};
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
