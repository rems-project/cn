#include <stdint.h>

struct point {
  int x;
  int y;
};

/*@
datatype Coord {
  Pt {integer x_coord, integer y_coord}
}

function (Coord) makeCoord(integer x, integer y) {
  Pt {x_coord: x, y_coord: y}
}
@*/

// Test function that uses constructor returning functions
void test_constructor_record(int x, int y)
/*@
requires
    x >= 0 && x <= 100;
    y >= 0 && y <= 100;
ensures
    let coord = makeCoord(x, y);
    true;
@*/
{
  /*@ assert(x >= 0 && y >= 0); @*/
}

// Simple test that should work without complex operations
void test_basic_constructor(int a, int b)
/*@
requires
    a >= 0 && a <= 50;
    b >= 0 && b <= 50;
ensures
    true;
@*/
{
  /*@ assert(a >= 0); @*/
  /*@ assert(b >= 0); @*/
}

// Test simple record operations
void test_simple_record(int a, int b)
/*@
requires
    a >= 0 && a <= 100;
    b >= 0 && b <= 100;
ensures
    let my_record = {x: a, y: b};
    my_record.x == a && my_record.y == b;
@*/
{
  /*@ assert(a >= 0 && b >= 0); @*/
}

/*@
    function [rec] ({integer res}) count(integer n) {
        let res = (n == 0) ? 0 : 1 + count(n - 1).res;
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
