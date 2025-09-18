#!/bin/bash

CN=$OPAM_SWITCH_PREFIX/bin/cn

DIRNAME=$(dirname "$0")

# Parse options
SYMBOLIC=false
while getopts "s" opt; do
  case $opt in
    s)
      SYMBOLIC=true
      ;;
    \?)
      echo "Invalid option: -$OPTARG" >&2
      exit 1
      ;;
  esac
done

shift $((OPTIND-1))
TEST=$1

# Clean directory
cd "$DIRNAME" || exit

# For stricter CI
export CPPFLAGS="${CPPFLAGS} -Werror"

# For sanitizers
export UBSAN_OPTIONS=halt_on_error=1
export ASAN_OPTIONS="allocator_may_return_null=1:detect_leaks=0"

# Track failures
NUM_FAILED=0
FAILED=''

function separator() {
  OUTPUT="${OUTPUT}"$'\n===========================================================\n\n'
}

BASE_CONFIG="-I${OPAM_SWITCH_PREFIX}/lib/cerberus-lib/runtime/libc/include/posix \
  --input-timeout=1000 \
  --progress-level=function \
  --sanitize=address,undefined \
  --allow-split-magic-comments \
  --print-seed"

# Set ALT_CONFIGS based on symbolic option
if [ "$SYMBOLIC" = true ]; then
  ALT_CONFIGS=("--symbolic --coverage")
else
  ALT_CONFIGS=(
    "--coverage --sizing-strategy=quickcheck"
    "--coverage --experimental-learning --print-backtrack-info --print-size-info --static-absint=wrapped_interval --smt-pruning-after-absint=slow"
    "--sizing-strategy=uniform --random-size-splits --experimental-product-arg-destruction --static-absint=interval --smt-pruning-before-absint=fast"
    "--random-size-splits --experimental-learning --print-satisfaction-info --output-tyche=results.jsonl"
    )
fi

BUILD_TOOLS=("bash" "make")

OUTPUT=""

# For each configuration
for ALT_CONFIG in "${ALT_CONFIGS[@]}"; do
  for BUILD_TOOL in "${BUILD_TOOLS[@]}"; do
    FULL_CONFIG="$BASE_CONFIG $ALT_CONFIG --build-tool=$BUILD_TOOL"

    separator
    OUTPUT="${OUTPUT}Running CI with CLI config \"$FULL_CONFIG\""$'\n'
    separator

    if [[ $TEST == *.pass.c ]]; then
      OUTPUT="${OUTPUT}$($CN test "$TEST" $FULL_CONFIG 2>&1)"
      RET=$?
      if [[ "$RET" != 0 ]]; then
        OUTPUT="${OUTPUT}"$'\n'"$TEST -- Tests failed unexpectedly"$'\n'
        NUM_FAILED=$(($NUM_FAILED + 1))
        FAILED="$FAILED ($ALT_CONFIG --build-tool=$BUILD_TOOL)"
      else
        OUTPUT="${OUTPUT}"$'\n'"$TEST -- Tests passed successfully"$'\n'
      fi
    elif [[ $TEST == *.fail.c || $TEST == *.buggy.c ]]; then
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
    elif [[ $TEST == *.only.exp.c ]]; then
      if [[ $FULL_CONFIG == *--experimental-learning* ]]; then
        OUTPUT="${OUTPUT}$($CN test "$TEST" $FULL_CONFIG 2>&1)"
        RET=$?
        if [[ "$RET" != 0 ]]; then
          OUTPUT="${OUTPUT}"$'\n'"$TEST -- Tests failed unexpectedly"$'\n'
          NUM_FAILED=$(($NUM_FAILED + 1))
          FAILED="$FAILED ($ALT_CONFIG --build-tool=$BUILD_TOOL)"
        else
          OUTPUT="${OUTPUT}"$'\n'"$TEST -- Tests passed successfully"$'\n'
        fi
      else
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
      fi
    elif [[ $TEST == *.exp.c ]]; then
      if [[ $FULL_CONFIG == *--experimental-learning* ]]; then
        OUTPUT="${OUTPUT}$($CN test "$TEST" $FULL_CONFIG 2>&1)"
        RET=$?
        if [[ "$RET" != 0 ]]; then
          OUTPUT="${OUTPUT}"$'\n'"$TEST -- Tests failed unexpectedly"$'\n'
          NUM_FAILED=$(($NUM_FAILED + 1))
          FAILED="$FAILED ($ALT_CONFIG --build-tool=$BUILD_TOOL)"
        else
          OUTPUT="${OUTPUT}"$'\n'"$TEST -- Tests passed successfully"$'\n'
        fi
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
