# Implementation profiles

C leaves some properties of integer types to the implementation, notably
whether plain `char` is signed, and the result of converting an
out-of-range value to a signed integer type. CN checks a program against
one *implementation profile*, chosen with `--impl` (accepted by `verify`,
`instrument`, `test`, `seq-test` and `wf`):

| `--impl` | Platform | Plain `char` | Out-of-range conversion to a signed type |
|---|---|---|---|
| `hafnium` (default) | AArch64 | unsigned, 0..255 | rejected (`integer value not representable`) |
| `x86_64` | x86-64, GCC/Clang | signed, -128..127 | reduced modulo 2^N, as GCC documents |

The sizes of the integer types are the same in both (LP64).

For example, this function verifies with `--impl=x86_64` and fails with
the default profile:

```c
int negative_char(void)
/*@ ensures return == 1; @*/
{
    char c = -1;
    return c < 0;
}
```

A conversion from a pointer to an integer type whose range cannot hold
it is undefined (C11 6.3.2.3p6) and is rejected under both profiles.

## Writing specifications for both profiles

`MINchar ()` and `MAXchar ()` are the range of plain `char` under the
selected profile, like the fixed-width `MINi8 ()` ... `MAXu64 ()`. With them
a specification can be written once for both profiles. For example, the
value of a `char` cell after storing a byte value `v` (0..255):

```c
/*@ function (integer) char_of_byte (integer v) { v <= MAXchar() ? v : v - 256 } @*/
```

## Fulminate and testing

`cn instrument --run`, `cn test` and `cn seq-test` compile the C they
generate with `-funsigned-char` (default profile) or `-fsigned-char`
(`x86_64`), so that the executable specifications run against the same
`char` as the one CN verified against. A flag in `CFLAGS` comes later on
the command line and overrides it.
