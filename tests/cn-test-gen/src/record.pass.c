#include <stdint.h>

struct point {
  int x;
  int y;
};

/*@
datatype Coord {
  Pt {i32 x_coord, i32 y_coord}
}

function (Coord) makeCoord(i32 x, i32 y) {
  Pt {x_coord: x, y_coord: y}
}
@*/

// Test function that uses constructor returning functions
void test_constructor_record(int x, int y)
/*@
requires
    x >= 0i32 && x <= 100i32;
    y >= 0i32 && y <= 100i32;
ensures
    let coord = makeCoord(x, y);
    true;
@*/
{
  /*@ assert(x >= 0i32 && y >= 0i32); @*/
}

// Simple test that should work without complex operations
void test_basic_constructor(int a, int b)
/*@
requires
    a >= 0i32 && a <= 50i32;
    b >= 0i32 && b <= 50i32;
ensures
    true;
@*/
{
  /*@ assert(a >= 0i32); @*/
  /*@ assert(b >= 0i32); @*/
}

// Test simple record operations
void test_simple_record(int a, int b)
/*@
requires
    a >= 0i32 && a <= 100i32;
    b >= 0i32 && b <= 100i32;
ensures
    let my_record = {x: a, y: b};
    my_record.x == a && my_record.y == b;
@*/
{
  /*@ assert(a >= 0i32 && b >= 0i32); @*/
}

/*@
    function [rec] ({u8 res}) count(u8 n) {
        let res = (n == 0u8) ? 0u8 : 1u8 + count(n - 1u8).res;
        { res: res }
    }
@*/

uint8_t f(uint8_t x)
/*@
requires
    x == count(x).res;
ensures
    return == count(x).res;
@*/
{
  return x;
}
