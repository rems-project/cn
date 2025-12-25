#!/bin/bash

CN=$OPAM_SWITCH_PREFIX/bin/cn

DIRNAME=$(dirname "$0")
TEST=$1

# Clean directory
cd "$DIRNAME" || exit
if [[ $TEST == *.pass.c ]]; then
  DIR=passing-$(basename $TEST .pass.c)
else
  DIR=failing-$(basename $TEST .fail.c)
fi
rm -rf $DIR

# For stricter CI
export CPPFLAGS="${CPPFLAGS} -Werror"

# For UBSan
export UBSAN_OPTIONS=halt_on_error=1

# Track failures
NUM_FAILED=0
FAILED=''

function separator() {
  OUTPUT="${OUTPUT}"$'\n===========================================================\n\n'
}

CONFIGS=("--max-num-calls=30 --num-tests=10")

OUTPUT=""

# For each configuration
for CONFIG in "${CONFIGS[@]}"; do
  separator
  OUTPUT="${OUTPUT}Running CI with CLI config \"$CONFIG\""$'\n'
  separator

  if [[ $TEST == *.pass.c ]]; then
    CLEANUP="rm -rf ${DIR} run_tests.sh;separator"
    OUTPUT="${OUTPUT}$($CN seq-test "$TEST" --output-dir="$DIR" $CONFIG 2>&1)"
    RET=$?
    if [[ "$RET" != 0 ]]; then
      OUTPUT="${OUTPUT}"$'\n'"$TEST -- Tests failed unexpectedly"$'\n'
      NUM_FAILED=$(($NUM_FAILED + 1))
      FAILED="$FAILED ($ALT_CONFIG)"
    else
      OUTPUT="${OUTPUT}"$'\n'"$TEST -- Tests passed successfully"$'\n'
    fi
  elif [[ $TEST == *.fail.c ]]; then
    CLEANUP="rm -rf ${DIR} run_tests.sh;separator"
    THIS_OUTPUT=$($CN seq-test "$TEST" --output-dir="$DIR" $CONFIG 2>&1)
    RET=$?
    if [[ "$RET" == 0 ]]; then
      OUTPUT="${OUTPUT}\n$TEST -- Tests passed unexpectedly\n"
      NUM_FAILED=$(($NUM_FAILED + 1))
      FAILED="$FAILED ($ALT_CONFIG)"
    elif [[ "$RET" != 2 ]]; then
      OUTPUT="${OUTPUT}${THIS_OUTPUT}\n$TEST -- Tests failed unnaturally\n"
      NUM_FAILED=$(($NUM_FAILED + 1))
      FAILED="$FAILED ($ALT_CONFIG)"
    else
      OUTPUT="${OUTPUT}"$'\n'"$TEST -- Tests failed successfully"$'\n'
    fi
  fi

  eval "$CLEANUP"
done

if [ -z "$FAILED" ]; then
  echo "$TEST - all configs passed."
  exit 0
else
  OUTPUT="${OUTPUT}$TEST - $NUM_FAILED configs failed:\n  $FAILED"
  printf "%b\n" "${OUTPUT}"
  exit 1
fi
