#!/bin/bash

FLAGS=(
  -Werror=shadow
  -std=gnu11
)
if [[ -n "${GITHUB_ACTIONS+isset}" ]]; then
    FLAGS+=(-Werror)
else
    FLAGS+=(-Wimplicit-fallthrough)
fi

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
  -Wundef
  -Wvla
)

FLAGS+=(
  -Wno-declaration-after-statement
)

${CC:-cc} ${FLAGS[@]} -I ../../include/ -c -g "$@"
