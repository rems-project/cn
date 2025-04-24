// Client state machine.  Each connection has a separate `struct client`
// object, which tracks progress through the key request protocol for that
// connection.

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include <sys/socket.h>
#include <unistd.h>

// TODO: Have to hardcode the values as CN test doesn't support cn_function :(
/*@ function (u64) KEY_ID_SIZE ()   { 1u64} @*/
/*@ function (u64) NONCE_SIZE ()    {16u64} @*/
/*@ function (u64) MEASURE_SIZE ()  {32u64} @*/
/*@ function (u64) HMAC_SIZE ()     {32u64} @*/
/*@ function (u64) HMAC_KEY_SIZE () {32u64} @*/
/*@ function (u64) KEY_SIZE ()      {32u64} @*/
// A single entry in the MKM policy.  Each entry consists of a set of criteria
// and a key.  For each request, the MKM server looks for a policy entry where
// all criteria match the request, and if it finds one, it sends the key
// associated with that entry.
struct policy_entry {
  // Expected measure.  The measure included in the attestation must match
  // this value for the policy entry to apply.
  _Alignas(_Alignof(void *)) uint8_t measure[32];
  // Secret key to send if the policy entry matches.
  _Alignas(_Alignof(void *)) uint8_t key[32];
  // Key ID.  This must match the client's requested key ID for the policy
  // entry to apply.
  _Alignas(_Alignof(void *)) uint8_t key_id[1];
};
// Add an entry to the policy table.  Returns 0 on success and -1 on failure.
int policy_add(const uint8_t key_id[1], const uint8_t measure[32],
               const uint8_t key[32]);
// TODO: can't write a spec here thanks to #371
// Look for a policy entry matching the given key ID, nonce/challenge, measure,
// and attestation HMAC.  If a matching entry is found, this returns a pointer
// to the secret key from that entry; otherwise, it returns NULL.
const uint8_t *policy_match(uint8_t key_id[1], uint8_t nonce[16],
                            uint8_t measure[32], uint8_t hmac[32]);
enum client_state {
  // Waiting to receive a request for a specific key ID.
  CS_RECV_KEY_ID,
  // In the process of sending a challenge for attestation.
  CS_SEND_CHALLENGE,
  // Waiting to receive the attestation response.
  CS_RECV_RESPONSE,
  // In the process of sending the key.
  CS_SEND_KEY,
  // Protocol finished - no more input or output is expected.
  CS_DONE,
};
enum client_op {
  OP_NONE,
  OP_READ,
  OP_WRITE,
};
struct client {
  int fd;
  // Buffers for async read/write operations.
  uint8_t challenge[16];
  uint8_t response[32 + 32];
  const uint8_t *key;
  uint8_t key_id[1];
  // Read/write position within the current buffer.  Which buffer this refers
  // to depends on the current state.  For the chosen buffer, `buf[i]` is
  // initialized only within the range `0 <= i < pos`.
  uint8_t pos;
  unsigned int state;
};
enum client_event_result {
  // An error occurred while processing the event.  This could be an I/O
  // error or a protocol-level error.
  RES_ERROR = -2,
  // End of file was reached unexpectedly while processing the event.
  RES_EOF = -1,
  // The event was processed successfully, but some async operation is
  // pending, meaning the protocol is not yet finished.
  RES_PENDING = 0,
  // The event was processed successfully, and the protocol is now complete.
  RES_DONE = 1,
};
struct client *client_new(int fd);
// Deallocate client data.  The client should be removed from epoll before
// calling this.
void client_delete(struct client *c);
// Update the epoll event mask for `c`'s file descriptor to match the pending
// async operation for the current state.  The epoll FD `epfd` and operation
// `op` are passed through directly to `epoll_ctl`.
int client_epoll_ctl(struct client *c, int epfd, int op);
// Process an epoll wakeup with the given event flags.
//
// If this returns `RES_PENDING`, then we may have finished one async operation
// and started a new one, so the caller should next call `client_epoll_ctl`
// next to update the epoll event mask.
enum client_event_result client_event(struct client *c, uint32_t events);
// Either the key is in memory and owned, or the pointer is null
/*@
predicate (map<u64, u8>) KeyPred (pointer p)
{
    if (! is_null(p)) {
        take K = each(u64 i; i < KEY_SIZE())
{Owned<uint8_t>(array_shift<uint8_t>(p,i))}; return K; } else { return default<
map<u64, u8> >;
    }
}
@*/
// Pure predicate representing valid states of `enum client_state`.
// CN could easily generate this automatically (see #796)
/*@
function (boolean) ValidState (u32 state) {
   ((state == (u32) CS_RECV_KEY_ID) ||
    (state == (u32) CS_SEND_CHALLENGE) ||
    (state == (u32) CS_RECV_RESPONSE) ||
    (state == (u32) CS_SEND_KEY) ||
    (state == (u32) CS_DONE) )
}
@*/
// NOTE Wrapper predicate for the allocation record. We distinguish between
// cases because `cn test` doesn't handle Alloc() yet. See
// https://github.com/rems-project/cerberus/issues/776
/*@
predicate (boolean) ClientAlloc (pointer p)
{
    return true;
}
@*/
// Predicate representing a valid client object
/*@
predicate (struct client) ClientObject (pointer p)
{
    take C = Owned<struct client>(p);
    take Log = ClientAlloc(p);
    assert ( ValidState(C.state) ) ;
    take K = KeyPred(C.key); // Discard the key
    return C;
}
@*/
// Pure predicate representing the MKM state machine transitions
/*
    start:
   ┌────────────────┐
┌─►│ CS_RECV_KEY_ID ├──────┐
│  └┬─────┬─────────┘      │
└───┘     │                │
   ┌──────▼────────────┐   │
┌─►│ CS_SEND_CHALLENGE ├──►│
│  └┬─────┬────────────┘   │
└───┘     │                │
   ┌──────▼───────────┐    │
┌─►│ CS_RECV_RESPONSE ├───►│
│  └┬─────┬───────────┘    │
└───┘     │                │
   ┌──────▼──────┐         │
┌─►│ CS_SEND_KEY ├────────►│
│  └┬─────┬──────┘         │
└───┘     │                │
   ┌──────▼──┐             │
┌─►│ CS_DONE │◄────────────┘
│  └┬────────┘
└───┘
*/
/*@
function (boolean) ValidTransition (u32 state1, u32 state2) {
       ( state1 == state2 )
    || ( (state1 == (u32) CS_RECV_KEY_ID)    && (state2 == (u32)
CS_SEND_CHALLENGE) )
    || ( (state1 == (u32) CS_SEND_CHALLENGE) && (state2 == (u32)
CS_RECV_RESPONSE)  )
    || ( (state1 == (u32) CS_RECV_RESPONSE)  && (state2 == (u32) CS_SEND_KEY) )
    || ( ValidState(state1)                  && (state2 == (u32) CS_DONE) )
}
@*/

// Provides substitutes for some declarations that CN has trouble with.
// Cerberus puts some POSIX headers under the `posix/` directory.
// from `cn_memory.h`
// For testing, use the CN-specific instrumented malloc() / free()
void *cn_malloc(unsigned long size);
void cn_free_sized(void *ptr, unsigned long size);
// From `sys/epoll.h`
// From `sys/socket.h`
// From `policy.h`
// Non-deterministically return a pointer to a key, or NULL
const uint8_t *_policy_match(uint8_t key_id[1], uint8_t nonce[16],
                             uint8_t measure[32], uint8_t hmac[32]);
/*@ spec _policy_match(pointer key_id, pointer nonce, pointer measure, pointer
hmac); requires true; ensures take Key_out = KeyPred(return);
@*/
// Mock policy_match by allocating a new key
// // Add an entry to the policy table.  Returns 0 on success and -1 on failure.
// int _policy_add(
//         const uint8_t key_id[KEY_ID_SIZE],
//         const uint8_t measure[MEASURE_SIZE],
//         const uint8_t key[KEY_SIZE]);
// /*@ spec _policy_add(pointer key_id, pointer measure, pointer key);
// requires
//     true;
// ensures
//     return == 0i32 || return == -1i32;
// @*/
// #define policy_add(k,m,h) _policy_add(k,m,h)
// Ghost function which releases the memory representing a key. Implicitly, this
// is returning ownership of the memory to whatever internal state manages the
// key list.
void _key_release(const uint8_t *key);
/*@ spec _key_release(pointer key);
requires
    take Key_in = KeyPred(key);
ensures
    true;
@*/
// Mock key_release by disposing the key
// From `stdio.h`
// From `unistd.h`
int _close(int fildes);
/*@
spec _close(i32 fildes);
requires true;
ensures true;
@*/
ssize_t _read_uint8_t(int fd, void *buf, size_t count);
/*@
spec _read_uint8_t(i32 fd, pointer buf, u64 count);
requires
    take buf_in = each (u64 i; i < count) {
Owned<uint8_t>(array_shift<uint8_t>(buf, i))}; ensures take buf_out = each (u64
i; i < count) { Owned<uint8_t>(array_shift<uint8_t>(buf, i))}; buf_out ==
buf_in; return >= -1i64 && return <= (i64)count;
@*/
ssize_t _read_mock(void *buf, size_t count) { return 0; }
ssize_t _write_uint8_t(int fd, const void *buf, size_t count);
/*@
spec _write_uint8_t(i32 fd, pointer buf, u64 count);
requires
    take buf_in = each(u64 i; i < count)
{Owned<uint8_t>(array_shift<uint8_t>(buf,i))}; ensures take buf_out = each(u64
i; i < count) {Owned<uint8_t>(array_shift<uint8_t>(buf,i))}; buf_in == buf_out;
    return >= -1i64 && return < (i64)count;
@*/
ssize_t _write_mock(const void *buf, size_t count) { return 0; }
int _shutdown(int fildes, int how);
/*@ spec _shutdown(i32 fildes, i32 how);
    requires true;
    ensures true;
@*/
/*
These predicates are a mix of:
* syntactic sugar
* workarounds for the current requirement that CN specs never conditionally
  create a resource without a predicate.
* workarounds for the fact that CN does not equate x[2] and (&x[1])[1]
*/
/*@
//An uninitialized uint8_t array starting at p on indices [0, e)
predicate (map<u64,u8>) ArrayBlock_u8 (pointer p, u64 e)
{
  take pv = each(u64 i; i >= 0u64 && i < e)
{Block<uint8_t>(array_shift<uint8_t>(p,i))}; return pv;
}

//An initialized uint8_t array starting at p on indices [0, e)
predicate (map<u64,u8>) ArrayOwned_u8 (pointer p, u64 e)
{
  take pv = each(u64 i; i >= 0u64 && i < e)
{Owned<uint8_t>(array_shift<uint8_t>(p,i))}; return pv;
}

//An uninitialized slice of some uint8_t array starting at p on indices [s, e)
predicate (map<u64,u8>) ArraySliceBlock_u8 (pointer p, u64 s, u64 e)
{
  take pv = each(u64 i; i >= s && i < e)
{Block<uint8_t>(array_shift<uint8_t>(p,i))}; return pv;
}

//An uninitialized slice of some uint8_t array starting at p on indices [s, e)
predicate (map<u64,u8>) ArraySliceOwned_u8 (pointer p, u64 s, u64 e)
{
  take pv = each(u64 i; i >= s && i < e)
{Owned<uint8_t>(array_shift<uint8_t>(p,i))}; return pv;
}

//When condition c is true, an uninitialized slice of some uint8_t array
//starting at p on indices [s, e). Otherwise nothing.
predicate (map<u64,u8>) CondArraySliceBlock_u8 (pointer p, boolean c, u64 s, u64
e)
{
  if (c) {
    take pv = ArraySliceBlock_u8(p, s, e);
    return pv;
  } else {
    return default<map<u64,u8> >;
  }
}

//When condition c is true, an initialized slice of some uint8_t array
//starting at p on indices [s, e). Otherwise nothing.
predicate (map<u64,u8>) CondArraySliceOwned_u8 (pointer p, boolean c, u64 s, u64
e)
{
  if (c) {
    take pv = ArraySliceOwned_u8(p, s, e);
    return pv;
  } else {
    return default<map<u64,u8> >;
  }
}

//If p is not null, an initialized uint8_t array
//starting at p on indices [0, l). Otherwise nothing.
predicate (map<u64,u8>) ArrayOrNull_u8 (pointer p, u64 l)
{
  if (!is_null(p)) {
    take pv = ArrayOwned_u8(p, l);
    return pv;
  } else {
    return default<map<u64,u8> >;
  }
}

//If p is not null, an uninitialized uint8_t array
//starting at p on indices [0, l). Otherwise nothing.
predicate (map<u64,u8>) ArrayOrNull_Block_u8 (pointer p, u64 l)
{
  if (!is_null(p)) {
    take pv = ArrayBlock_u8(p, l);
    return pv;
  } else {
    return default<map<u64,u8> >;
  }
}

//Starting with an uninitialized uint8_t array at p with length len, cut out the
//slice [at, at+slen) from it, also creating the slices [0, at) and [at+slen,
//len).
lemma SplitAt_Block_u8(pointer tmp, u64 len, u64 at, u64 slen)
  requires
    take a1 = ArrayBlock_u8(tmp, len);
    at >= 0u64;
    len >= 0u64;
    slen >= 0u64;
    at < len;
    slen <= len;
    at + slen <= len;
  ensures
    take a2 = ArraySliceBlock_u8(tmp, 0u64, at);
    take a3 = ArraySliceBlock_u8(tmp, at, at+slen);
    take a4 = ArraySliceBlock_u8(tmp, at+slen, len);

//Starting with an initialized uint8_t array at p with length len, cut out the
//slice [at, at+slen) from it, also creating the slices [0, at) and [at+slen,
//len).
lemma SplitAt_Owned_u8(pointer tmp, u64 len, u64 at, u64 slen)
  requires
    take a1 = ArrayOwned_u8(tmp, len);
    at >= 0u64;
    len >= 0u64;
    slen >= 0u64;
    at < len;
    slen <= len;
    at + slen <= len;
  ensures
    take a2 = ArraySliceOwned_u8(tmp, 0u64, at);
    take a3 = ArraySliceOwned_u8(tmp, at, at+slen);
    take a4 = ArraySliceOwned_u8(tmp, at+slen, len);

// Call this lemma with the same arguments as SplitAt_Block_u8 to undo it.
lemma UnSplitAt_Block_u8(pointer tmp, u64 len, u64 at, u64 slen)
  requires
    take a2 = ArraySliceBlock_u8(tmp, 0u64, at);
    take a3 = ArraySliceBlock_u8(tmp, at, at+slen);
    take a4 = ArraySliceBlock_u8(tmp, at+slen, len);
    at >= 0u64;
    len >= 0u64;
    slen >= 0u64;
    at < len;
    slen <= len;
    at + slen <= len;
  ensures
    take a1 = ArrayBlock_u8(tmp, len);

// construct a slice at tmp [m,n) from slices [m,cut) and [cut,n)
lemma JoinSlice_Block_u8(pointer tmp, u64 m, u64 n, u64 cut)
  requires
    m <= cut;
    cut <= n;
    take a2 = ArraySliceBlock_u8(tmp, m, cut);
    take a3 = ArraySliceBlock_u8(tmp, cut, n);
  ensures
    take a1 = ArraySliceBlock_u8(tmp, m, n);

// Call this lemma with the same arguments as SplitAt_Owned_u8 to undo it.
lemma UnSplitAt_Owned_u8(pointer tmp, u64 len, u64 at, u64 slen)
  requires
    take a2 = ArraySliceOwned_u8(tmp, 0u64, at);
    take a3 = ArraySliceOwned_u8(tmp, at, at+slen);
    take a4 = ArraySliceOwned_u8(tmp, at+slen, len);
    at >= 0u64;
    len >= 0u64;
    slen >= 0u64;
    at < len;
    slen <= len;
    at + slen <= len;
  ensures
    take a1 = ArrayOwned_u8(tmp, len);

// construct a slice at tmp [m,n) from slices [m,cut) and [cut,n)
lemma JoinSlice_Owned_u8(pointer tmp, u64 m, u64 n, u64 cut)
  requires
    m <= cut;
    cut <= n;
    take a2 = ArraySliceOwned_u8(tmp, m, cut);
    take a3 = ArraySliceOwned_u8(tmp, cut, n);
  ensures
    take a1 = ArraySliceOwned_u8(tmp, m, n);

// CN does not automatically consider that a[1] and (&a[1])[0] are equal, so
// this lemma takes a uninitialized slice that has an initial offset and
// produces a new slice with a 0 initial offset.
lemma ViewShift_Block_u8(pointer a, pointer b, u64 at, u64 len)
  requires
    take a1 = ArraySliceBlock_u8(a, at, at+len);
    ptr_eq(array_shift<uint8_t>(b,0u64), array_shift<uint8_t>(a,at));
  ensures
    take a2 = ArrayBlock_u8(b, len);

// CN does not automatically consider that a[1] and (&a[1])[0] are equal, so
// this lemma takes a initialized slice that has an initial offset and
// produces a new slice with a 0 initial offset.
lemma ViewShift_Owned_u8(pointer a, pointer b, u64 at, u64 len)
  requires
    take a1 = ArraySliceOwned_u8(a, at, at+len);
    ptr_eq(array_shift<uint8_t>(b,0u64), array_shift<uint8_t>(a,at));
  ensures
    take a2 = ArrayOwned_u8(b, len);

// Call this with the same arguments as ViewShift_Block_u8 to undo it. Note that
// this could be implemented as ViewShift_Block(b, a, -at, len) if lemmas could
// make calls.
lemma UnViewShift_Block_u8(pointer a, pointer b, u64 at, u64 len)
  requires
    take a2 = ArrayBlock_u8(b, len);
    ptr_eq(array_shift<uint8_t>(b,0u64), array_shift<uint8_t>(a,at));
  ensures
    take a1 = ArraySliceBlock_u8(a, at, at+len);

// TODO this lemma takes an absolute start and end rather than an absolute start
// and a length. This should show in the name.
//
// This allows you to shift the origin of a slice with potentially both ends
// having a nonzero offset.
lemma UnViewShift_Block_At_u8(pointer a, pointer b, u64 oset, u64 s, u64 e)
  requires
    take a2 = ArraySliceBlock_u8(b, s, e);
    ptr_eq(array_shift<uint8_t>(b,0u64), array_shift<uint8_t>(a,oset));
  ensures
    take a1 = ArraySliceBlock_u8(a, oset+s, oset+e);

// Call this with the same arguments as ViewShift_Owned_u8 to undo it. Note that
// this could be implemented as ViewShift_Owned(b, a, -at, len) if lemmas could
// make calls.
lemma UnViewShift_Owned_u8(pointer a, pointer b, u64 at, u64 len)
  requires
    take a2 = ArrayOwned_u8(b, len);
    ptr_eq(array_shift<uint8_t>(b,0u64), array_shift<uint8_t>(a,at));
  ensures
    take a1 = ArraySliceOwned_u8(a, at, at+len);

// Turn an uninitialized uint16_t array resource into an uninitialized uint8_t
// resource. You can now use the to_bytes statement in CN for this.
lemma TransmuteArray_Block_u16_u8(pointer a, u64 l)
  requires
    take ai = each(u64 i; i >= 0u64 && i < l)
{Block<uint16_t>(array_shift<uint16_t>(a,i))}; ensures take ao = each(u64 i; i
>= 0u64 && i < (2u64*l)) {Block<uint8_t>(array_shift<uint8_t>(a,i))};

// Turn an uninitialized uint8_t array resource into an uninitialized uint16_t
// resource. You can now use the from_bytes statement in CN for this.
lemma UnTransmuteArray_Block_u16_u8(pointer a, u64 l)
  requires
    take ai = each(u64 i; i >= 0u64 && i < (2u64*l))
{Block<uint8_t>(array_shift<uint8_t>(a,i))}; ensures take ao = each(u64 i; i >=
0u64 && i < l) {Block<uint16_t>(array_shift<uint16_t>(a,i))};

// Turn an initialized uint8_t array into an initialized uint16_t array into a
// uint16_t array, forgetting information about the values.
lemma UnTransmuteArray_Owned_u16_u8(pointer a, u64 l)
  requires
    take ai = each(u64 i; i >= 0u64 && i < (2u64*l))
{Owned<uint8_t>(array_shift<uint8_t>(a,i))}; ensures take ao = each(u64 i; i >=
0u64 && i < l) {Owned<uint16_t>(array_shift<uint16_t>(a,i))};

// Take two resources representing an array partially initialized in a segment
// starting at index 0 and return one resource representing an uninitialized
// array.
lemma ForgetPartialInit_u8(pointer a, u64 l, u64 ol)
  requires
    ol <= l;
    take aio = ArrayOwned_u8(a, ol);
    take aib = ArraySliceBlock_u8(a, ol, l);
  ensures
    take aob = ArrayBlock_u8(a, l);
@*/
// NOTE `Alloc` construct not supported by `cn test`.
// See https://github.com/rems-project/cerberus/issues/776
uint32_t client_state_epoll_events(enum client_state state)
/*@
requires ValidState( state );
@*/
{
  switch (state) {
  case CS_RECV_KEY_ID:
    return 1;
  case CS_SEND_CHALLENGE:
    return 4;
  case CS_RECV_RESPONSE:
    return 1;
  case CS_SEND_KEY:
    return 4;
  case CS_DONE:
    return 0;
  default: {
    /*@ assert(false); @*/ // <-- Prove this state is unreachable
    return 0;
  }
  }
}
/*@
datatype OptionClientObject {
    SomeClientObject {struct client obj},
    NoneClientObject {}
}
predicate (OptionClientObject) OptionClientNew(pointer p, i32 fd, i32 state)
{
    if (is_null(p)) {
        return NoneClientObject {};
    } else {
        take Client_out = ClientObject(p);
        assert(Client_out.fd == fd);
        assert(((i32) Client_out.state) == state);
        return SomeClientObject { obj: Client_out } ;
    }
}
@*/
struct client *client_new(int fd)
/*@
ensures take Client_out = OptionClientNew(return, fd, CS_RECV_KEY_ID);
@*/
{
  struct client *c = cn_malloc(sizeof(struct client));
  if (c == ((void *)0)) {
    return ((void *)0);
  }
  /*@ from_bytes Block<struct client>(c); @*/
  c->fd = fd;
  c->pos = 0;
  c->state = CS_RECV_KEY_ID;
  c->key = ((void *)0);
  memset(c->challenge, 0, 16);
  memset(c->response, 0, 32 + 32);
  memset(c->key_id, 0, 1);
  return c;
}
void client_delete(struct client *c)
/*@
requires take Client_in = ClientObject(c);
@*/
{
  int ret = 0;
  if (ret != 0) {
    // Keep going.  Even if TCP shutdown fails, we still need to close the
    // file descriptor.
  }
  ret = 0;
  if (ret != 0) {
    // Keep going.  On Linux, `close` always closes the file descriptor,
    // but may report I/O errors afterward.
  }
  // Ghost code: return ownership of the key
  if (c->key != ((void *)0)) {
    cn_free_sized((void *)c->key, 32 * sizeof(const uint8_t));
  };
  /*@ to_bytes Block<struct client>(c); @*/
  cn_free_sized(c, sizeof(struct client));
}
uint8_t *client_read_buffer(struct client *c)
/*@
requires take Client_in = ClientObject(c);
ensures take Client_out = ClientObject(c);
        Client_out == Client_in;
        // Note: ugly notation - CN should support conditional chains
        // See https://github.com/rems-project/cerberus/issues/811
        if (((i32) Client_in.state) == CS_RECV_KEY_ID) {
            ptr_eq( return, member_shift(c, key_id) )
        } else { if (((i32) Client_in.state) == CS_RECV_RESPONSE) {
            ptr_eq( return, member_shift(c, response) )
        } else {
            is_null(return)
        }};
@*/
{
  switch (c->state) {
  case CS_RECV_KEY_ID:
    return c->key_id;
  case CS_RECV_RESPONSE:
    return c->response;
  default:
    return ((void *)0);
  }
}
const uint8_t *client_write_buffer(struct client *c)
/*@
requires take Client_in = ClientObject(c);
ensures take Client_out = ClientObject(c);
        Client_in == Client_out;
        if (((i32) Client_in.state) == CS_SEND_CHALLENGE) {
            ptr_eq( return, member_shift(c, challenge) )
        } else { if (((i32) Client_in.state) == CS_SEND_KEY) {
            // NOTE There's a confusing distinction in CN from the previous
            // case! This is caused by the fact `challenge` is an array field,
            // and `key` is a value field
            // See https://github.com/rems-project/cerberus/issues/810
            ptr_eq( return, Client_in.key )
        } else {
            is_null(return)
        }};
@*/
{
  switch (c->state) {
  case CS_SEND_CHALLENGE:
    return c->challenge;
  case CS_SEND_KEY:
    return c->key;
  default:
    return ((void *)0);
  }
}
size_t client_buffer_size(struct client *c)
/*@
requires take Client_in = ClientObject(c);
ensures take Client_out = ClientObject(c);
        Client_out == Client_in;
        if (((i32) Client_in.state) == CS_RECV_KEY_ID) {
            return == KEY_ID_SIZE()
        } else { if ( ((i32) Client_in.state) == CS_SEND_CHALLENGE ) {
            return == NONCE_SIZE()
        } else { if ( ((i32) Client_in.state) == CS_RECV_RESPONSE ) {
            return == MEASURE_SIZE() + HMAC_SIZE()
        } else { if ( ((i32) Client_in.state) == CS_SEND_KEY ) {
            return == KEY_SIZE()
        } else {
            return == 0u64  // CS_DONE and default cases
        }}}};
@*/
{
  switch (c->state) {
  case CS_RECV_KEY_ID:
    return sizeof(c->key_id);
  case CS_SEND_CHALLENGE:
    return sizeof(c->challenge);
  case CS_RECV_RESPONSE:
    return sizeof(c->response);
  case CS_SEND_KEY:
    return 32;
  case CS_DONE:
    return 0;
  default: {
    /*@ assert(false); @*/ // <-- Prove this state is unreachable
    return 0;
  }
  }
}
int client_epoll_ctl(struct client *c, int epfd, int op)
/*@
// NOTE specification omitted because this function is just a wrapper to the
// epoll library
trusted;
requires
    true;
ensures
    true;
@*/
{
  return 0;
}
enum client_event_result client_read(struct client *c)
/*@
requires
    take Client_in = ClientObject(c);
    let pos = (u64) Client_in.pos;
ensures
    take Client_out = ClientObject(c);
    Client_out.fd == Client_in.fd;
    Client_out.challenge == Client_in.challenge;
    ptr_eq(Client_out.key, Client_in.key);
    Client_out.state == Client_in.state;
@*/
// NOTE true, but not provable with CN verify
/*@
ensures
    Client_out.key_id == Client_in.key_id;
@*/
{
  uint8_t *buf = client_read_buffer(c);
  if (buf == ((void *)0)) {
    // There's no read operation to perform.
    return RES_DONE;
  }
  size_t buf_size = client_buffer_size(c);
  if (c->pos >= buf_size) {
    // Read operation has already finished.
    return RES_DONE;
  }
  // NOTE case split needed so that CN can figure out the size of
  // the buffer down each control-flow path
  /*@ split_case(Client_in.state == (u32) CS_RECV_KEY_ID); @*/
  /*@ apply SplitAt_Owned_u8(buf, buf_size, pos, buf_size - pos ); @*/
  /*@ apply ViewShift_Owned_u8(buf, buf + pos, pos, buf_size - pos ); @*/
  int ret = _read_mock(buf + c->pos, buf_size - (uint64_t)c->pos);
  /*@ apply UnViewShift_Owned_u8(buf, buf + pos, pos, buf_size - pos ); @*/
  /*@ apply UnSplitAt_Owned_u8(buf, buf_size, pos, buf_size - pos ); @*/
  if (ret < 0) {
    return RES_ERROR;
  } else if (ret == 0) {
    return RES_EOF;
  }
  c->pos += ret;
  return c->pos == buf_size ? RES_DONE : RES_PENDING;
}
enum client_event_result client_write(struct client *c)
/*@
requires
    take Client_in = ClientObject(c);
    let pos = (u64) Client_in.pos;
ensures
    take Client_out = ClientObject(c);
    Client_out.state == Client_in.state;
@*/
{
  const uint8_t *buf = client_write_buffer(c);
  if (buf == ((void *)0)) {
    // There's no write operation to perform.
    return RES_DONE;
  }
  size_t buf_size = client_buffer_size(c);
  if (c->pos >= buf_size) {
    // Write operation has already finished.
    return RES_DONE;
  }
  // NOTE case split needed so that CN can figure out the size of
  // the buffer down each control-flow path
  /*@ split_case(Client_in.state == (u32) CS_SEND_CHALLENGE); @*/
  // NOTE extract needed but eventually we'd like CN to figure it out
  /*@ extract Owned<uint8_t>, pos; @*/
  /*@ apply SplitAt_Owned_u8(buf, buf_size, pos, buf_size - pos ); @*/
  /*@ apply ViewShift_Owned_u8(buf, buf + pos, pos, buf_size - pos ); @*/
  int ret = _write_mock(buf + c->pos, buf_size - (uint64_t)c->pos);
  /*@ apply UnViewShift_Owned_u8(buf, buf + pos, pos, buf_size - pos ); @*/
  /*@ apply UnSplitAt_Owned_u8(buf, buf_size, pos, buf_size - pos ); @*/
  if (ret < 0) {
    return RES_ERROR;
  } else if (ret == 0) {
    return RES_EOF;
  }
  c->pos += ret;
  return c->pos == buf_size ? RES_DONE : RES_PENDING;
}
void client_change_state(struct client *c, enum client_state new_state)
/*@
requires
    take Client_in = ClientObject(c);
    ValidState(new_state);
    ValidTransition(Client_in.state, new_state);
ensures
    take Client_out = ClientObject(c);
    Client_out == {state: new_state, pos: 0u8, ..Client_in};
@*/
{
  c->state = new_state;
  c->pos = 0;
}
enum client_event_result client_event(struct client *c, uint32_t events)
/*@
requires
    take Client_in = ClientObject(c);
 ensures
    take Client_out = ClientObject(c);
    ValidTransition(Client_in.state, Client_out.state);
@*/
{
  if (events & 1) {
    enum client_event_result result = client_read(c);
    if (result != RES_DONE) {
      return result;
    }
  }
  if (events & 4) {
    enum client_event_result result = client_write(c);
    if (result != RES_DONE) {
      return result;
    }
  }
  if (c->pos < client_buffer_size(c)) {
    // Async read/write operation isn't yet finished.
    //
    // This should be unreachable.  `client_event` should only be called
    // when progress can be made on a current async read or write
    // operation, and the call to `client_read`/`client_write` above will
    // return `RES_PENDING` (causing `client_event` to return early) if the
    // operation isn't finished.
    //
    // We return `RES_ERROR` here so that the epoll loop will close the
    // connection.  Since we didn't properly process the events indicated
    // by `events`, presumably some of those events are still ready and
    // will show up again the next time around the epoll loop, at which
    // point we'll fail to handle them again.  Closing the connection
    // prevents this from looping infinitely and wasting CPU.
    return RES_ERROR;
  }
  // The async operation for the current state is finished.  We can now
  // transition to the next state.
  switch (c->state) {
  case CS_RECV_KEY_ID: { // NOTE additional block needed for declaration
    // NOTE We can't call memcpy with a string argument because it would
    // be passed as read-only memory, which CN does not support. Instead,
    // declare the string as a temporary variable:
    uint8_t tmp[16] = "random challenge";
    memcpy(c->challenge, tmp, 16);
    client_change_state(c, CS_SEND_CHALLENGE);
    break;
  }
  case CS_SEND_CHALLENGE:
    client_change_state(c, CS_RECV_RESPONSE);
    break;
  case CS_RECV_RESPONSE:
    // Ghost code: return ownership of the key
    if (c->key != ((void *)0)) {
      cn_free_sized((void *)c->key, 32 * sizeof(const uint8_t));
    };
    c->key = cn_malloc(32 * sizeof(const uint8_t));
    if (c->key == ((void *)0)) {
      // No matching key was found for this request.
      /* nothing */;
      return RES_ERROR;
    }
    client_change_state(c, CS_SEND_KEY);
    /* nothing */;
    break;
  case CS_SEND_KEY:
    client_change_state(c, CS_DONE);
    break;
  case CS_DONE:
    // No-op.  This connection is finished, and the main loop should
    // disconnect.
    break;
  }
  return RES_DONE;
}
