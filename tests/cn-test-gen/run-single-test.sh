#!/bin/bash

CN=$OPAM_SWITCH_PREFIX/bin/cn

DIRNAME=$(dirname "$0")
TEST=$1

# Clean directory
cd "$DIRNAME" || exit

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

BASE_CONFIG="-I${OPAM_SWITCH_PREFIX}/lib/cerberus-lib/runtime/libc/include/posix \
  --input-timeout=1000 \
  --progress-level=function \
  --sanitize=undefined \
  --print-seed"
if [[ $(basename $TEST) == "mkm.pass.c" ]]; then
  BASE_CONFIG="$BASE_CONFIG --max-array-length=1024"
fi

ALT_CONFIGS=("--sized-null --sizing-strategy=uniform"
  "--coverage --sizing-strategy=quartile"
  "--coverage --sizing-strategy=quickcheck"
  "--random-size-splits --no-replicas"
  "--random-size-splits --allowed-size-split-backtracks=10")

BUILD_TOOLS=("bash" "make")

OUTPUT=""

# For each configuration
for ALT_CONFIG in "${ALT_CONFIGS[@]}"; do
  for BUILD_TOOL in "${BUILD_TOOLS[@]}"; do
    separator
    OUTPUT="${OUTPUT}Running CI with CLI config \"$ALT_CONFIG\""$'\n'
    separator

    FULL_CONFIG="$BASE_CONFIG $ALT_CONFIG --build-tool=$BUILD_TOOL"

    if [[ $TEST == *.pass.c ]]; then
      OUTPUT="${OUTPUT}$($CN test "$TEST" $FULL_CONFIG 2>&1)"
      RET=$?
      if [[ "$RET" != 0 ]]; then
        OUTPUT="${OUTPUT}"$'\n'"$TEST -- Tests failed unexpectedly"$'\n'
        NUM_FAILED=$(($NUM_FAILED + 1))
        FAILED="$FAILED ($ALT_CONFIG)"
      else
        OUTPUT="${OUTPUT}"$'\n'"$TEST -- Tests passed successfully"$'\n'
      fi
    elif [[ $TEST == *.fail.c ]]; then
      THIS_OUTPUT=$($CN test "$TEST" $FULL_CONFIG 2>&1)
      RET=$?
      if [[ "$RET" == 0 ]]; then
        OUTPUT="${OUTPUT}\n$TEST -- Tests passed unexpectedly\n"
        NUM_FAILED=$(($NUM_FAILED + 1))
        FAILED="$FAILED ($ALT_CONFIG)"
      elif [[ "$BUILD_TOOL" == "bash" && "$RET" != 1 ]]; then
        OUTPUT="${OUTPUT}${THIS_OUTPUT}\n$TEST -- Tests failed unnaturally\n"
        NUM_FAILED=$(($NUM_FAILED + 1))
        FAILED="$FAILED ($ALT_CONFIG)"
      else
        OUTPUT="${OUTPUT}"$'\n'"$TEST -- Tests failed successfully"$'\n'
      fi
    elif [[ $TEST == *.flaky.c ]]; then
      THIS_OUTPUT=$($CN test "$TEST" $FULL_CONFIG 2>&1)
      RET=$?

      # Run twice, since flaky
      if [[ "$RET" == 0 ]]; then
        THIS_OUTPUT=$($CN test "$TEST" $FULL_CONFIG 2>&1)
        RET=$?
      fi

      if [[ "$RET" == 0 ]]; then
        OUTPUT="${OUTPUT}\n$TEST -- Tests passed unexpectedly\n"
        NUM_FAILED=$(($NUM_FAILED + 1))
        FAILED="$FAILED ($ALT_CONFIG)"
      elif [[ "$BUILD_TOOL" == "bash" && "$RET" != 1 ]]; then
        OUTPUT="${OUTPUT}${THIS_OUTPUT}\n$TEST -- Tests failed unnaturally\n"
        NUM_FAILED=$(($NUM_FAILED + 1))
        FAILED="$FAILED ($ALT_CONFIG)"
      else
        OUTPUT="${OUTPUT}\n$TEST -- Tests failed successfully"
      fi
    fi

    separator
  done
done

if [ -z "$FAILED" ]; then
  echo "$TEST - all configs passed."
  exit 0
else
  OUTPUT="${OUTPUT}$TEST - $NUM_FAILED configs failed:\n  $FAILED"
  printf "%b\n" "${OUTPUT}"
  exit 1
fi
