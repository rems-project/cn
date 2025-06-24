#!/bin/bash

FLAGS="-O2 "
if [[ -n "${GITHUB_ACTIONS+isset}" ]]; then
    FLAGS+="-Werror -Wall"
fi

cc ${FLAGS} -I ../../include/ -c -g "$@"
