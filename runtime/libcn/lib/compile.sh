#!/bin/bash

FLAGS=(
  -O2
  -std=gnu11
)

FLAGS+=(
  -Wall
  -Wcast-function-type
  -Wdeclaration-after-statement
  -Werror=date-time
  -Werror=implicit-function-declaration
  -Werror=implicit-int
  -Werror=incompatible-pointer-types
  -Werror=return-type
  -Werror=strict-prototypes
  -Wframe-larger-than=2048
  -Wimplicit-fallthrough
  -Wundef
  -Wvla
)

FLAGS+=(
  -Wno-declaration-after-statement
)

${CC:-cc} ${FLAGS[@]} -I ../../include/ -c -g "$@"
