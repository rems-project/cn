#include <stdlib.h>

/*@
// copying from list_cn_types.h
datatype seq {
  Seq_Nil {},
  Seq_Cons {integer head, datatype seq tail}
}
@*/

/*@
function (integer) hd (datatype seq xs) {
  match xs {
    Seq_Nil {} => {
      0
    }
    Seq_Cons {head : h, tail : _} => {
      h
    }
  }
}

function (datatype seq) tl (datatype seq xs) {
  match xs {
    Seq_Nil {} => {
      Seq_Nil {}
    }
    Seq_Cons {head : _, tail : tail} => {
      tail
    }
  }
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
// copying from list_length.c
function [rec] (integer) length(datatype seq xs) {
  match xs {
    Seq_Nil {} => {
      0
    }
    Seq_Cons {head : h, tail : zs}  => {
      1 + length(zs)
    }
  }
}

function (integer) queue_size (integer inp, integer outp, integer bufsize)
{
  ((inp - outp) + bufsize) % bufsize
}


function [rec] (datatype seq) seq_of_buf (map<integer,integer> buf, integer inp, integer outp, integer bufsize) {
  if (queue_size (inp, outp, bufsize) > 0) {
    Seq_Cons {
      head: buf[outp],
      tail: seq_of_buf(buf, inp, (outp + 1) % bufsize, bufsize)
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
  && bufsize + bufsize <= 2147483647i64
  && (0 <= inp && inp < bufsize)
  && (0 <= outp && outp < bufsize)
}


type_synonym state = {
  datatype seq content,
  integer size  // max size
}

predicate state QueueAbs(pointer p)
{
  take q = RW<struct queue>(p);
  take buf = each (integer i; 0 <= i && i < q.size) { RW<int>(q.buf + i) };
  assert (queue_wf (q.inp, q.outp, q.size));
  let content = seq_of_buf(buf, q.inp, q.outp, q.size);
  return {content: content, size: q.size - 1};
}

@*/

void* cn_malloc(unsigned long size);

struct queue* new(int n)
  /*@ requires 0 < n;
               (integer) n + (integer) n + 2 < 8192;
      ensures take queue_out = QueueAbs(return);
              queue_out.size == n;
              queue_out.content == Seq_Nil {};
  @*/
{
  int bufsize = n + 1;
  int* buff = cn_malloc(bufsize * sizeof(int));
  struct queue q = {0, 0, bufsize, buff};
  struct queue* qptr = cn_malloc(sizeof(struct queue));
  *qptr = q;
  return qptr;
}

void put(struct queue* q, int n)
/*@ requires take queue = QueueAbs(q);
             let expected_content = snoc(queue.content, n);  // Why not inline below?
    ensures take queue_out = QueueAbs(q);
            queue_out.content == expected_content;
            queue_out.size == queue.size;
@*/
{
  /*@ extract RW<int>, q->inp; @*/
  q->buf[q->inp] = n;
  q->inp = (q->inp + 1) % q->size;
}

int get(struct queue* q)
/*@ requires take queue = QueueAbs(q);
             length(queue.content) > 1;
    ensures take queue_out = QueueAbs(q);
            return == hd(queue.content);
            queue_out.content == tl(queue.content);
            queue_out.size == queue.size;
@*/
{
  /*@ extract RW<int>, q->outp; @*/
  int ans = q->buf[q->outp];
  q->outp = q->outp % q->size;
  return ans;
}

int queueSize(struct queue* q)
/*@ requires take queue = QueueAbs(q);
    ensures take queue_out = QueueAbs(q);
            queue == queue_out;
            return == length(queue.content);
@*/
{
  return (q->inp - q->outp + q->size) % q->size;
}

