struct coord {
    int x;
    int y;
};

/*@

function (struct coord) foo(struct coord bar) {
    { x : 0i32 , y: 0 , ..bar }
}

@*/

