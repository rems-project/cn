#!/bin/bash

TUTORIAL_PATH=$1

if [ -n "$TUTORIAL_PATH" ] 
then
    echo "using tutorial path $TUTORIAL_PATH"
else
    echo "missing argument for CN tutorial path"
    exit 1
fi

CN=$OPAM_SWITCH_PREFIX/bin/cn

HERE=$(pwd)

cd "$TUTORIAL_PATH" || exit 1

FAILURE=0

make check CN_PATH="$CN verify --solver-type=cvc5"
((FAILURE+=$?))

make check CN_PATH="$CN verify --solver-type=z3"
((FAILURE+=$?))

cd "$HERE" || exit 1

if [ $FAILURE == 0 ]
then
    echo "Ok"
    exit 0
else 
    echo "There were errors"
    exit 1
fi
