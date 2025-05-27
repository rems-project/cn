#!/bin/bash

cd cn-seq-test-gen || exit 1

DIRNAME=$(dirname "$0")

# Clean directory
cd "$DIRNAME"/cn-seq-test-gen || exit
rm -rf build decorated test
mkdir build decorated test

# For UBSan
export UBSAN_OPTIONS=halt_on_error=1

# Get `*.c` files
FILES=$(find "$DIRNAME"/src -name '*.c')

# Track failures
NUM_FAILED=0
FAILED=''

function separator() {
  printf '\n===========================================================\n\n'
}

CONFIGS=("--max-num-calls=30 --num-tests=10")

# For each configuration
for CONFIG in "${CONFIGS[@]}"; do
  separator
  echo "Running CI with CLI config \"$CONFIG\""
  separator

  # Test each `*.c` file
  for TEST in $FILES; do
    CLEANUP="rm -rf test/* run_tests.sh;separator"

    # Run passing tests
    if [[ $TEST == *.pass.c ]]; then
      $CN seq-test "$TEST" --output-dir="test" $CONFIG
      RET=$?
      if [[ "$RET" != 0 ]]; then
        echo
        echo "$TEST -- Tests failed unexpectedly"
        NUM_FAILED=$(($NUM_FAILED + 1))
        FAILED="$FAILED $TEST($CONFIG)"
        eval "$CLEANUP"
        continue
      else
        echo
        echo "$TEST -- Tests passed successfully"
      fi
    fi

    # Run failing tests
    if [[ $TEST == *.fail.c ]]; then
      $CN seq-test "$TEST" --output-dir="test" $CONFIG
      RET=$?
      if [[ "$RET" = 0 ]]; then
        echo
        echo "$TEST -- Tests passed unexpectedly"
        NUM_FAILED=$(($NUM_FAILED + 1))
        FAILED="$FAILED $TEST($CONFIG)"
        eval "$CLEANUP"
        continue
      else
        echo
        echo "$TEST -- Tests failed successfully"
      fi
    fi

    eval "$CLEANUP"
  done
done

echo 'Done running tests.'
echo

if [ -z "$FAILED" ]; then
  echo "All tests passed."
  exit 0
else
  echo "$NUM_FAILED tests failed:"
  echo "  $FAILED"
  exit 1
fi
