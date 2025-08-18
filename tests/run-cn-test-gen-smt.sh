#!/bin/bash

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

cd "$SCRIPT_DIR/cn-test-gen" || exit 1

# List of test files to run with the SMT solver
TO_TEST=(
  "abs_mem.pass.c"
  "abs.pass.c"
  "cast_equality.pass.c"
  "counter.pass.c"
  "delete_main.pass.c"
  "enum1.pass.c"
  "enum2.pass.c"
  "member_shift.pass.c"
  "neg100.pass.c"
  "neq.pass.c"
  "test_macro.fail.c"
)

# Run each test with the SMT solver
for test_file in "${TO_TEST[@]}"; do
  echo "Running symbolic test: $test_file"
  ./run-single-test.sh -s "src/$test_file"
done
