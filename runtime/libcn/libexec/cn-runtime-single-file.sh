#!/bin/bash
set -euo pipefail -o noclobber

USAGE="USAGE: $0 -h\n       $0 [-nqu] FILE.c"

function echo_and_err() {
    printf "$1\n"
    exit 1
}

QUIET=""
NO_CHECK_OWNERSHIP=""

while getopts "hnqu" flag; do
 case "$flag" in
   h)
   printf "${USAGE}"
   exit 0
   ;;
   n)
   NO_CHECK_OWNERSHIP="--without-ownership-checking"
   ;;
   q)
   QUIET=1
   ;;
   u)
   export ASAN_OPTIONS="allocator_may_return_null=1:detect_leaks=0"
   export UBSAN_OPTIONS=halt_on_error=1
   export CFLAGS="-fsanitize=address,undefined ${CFLAGS:-}"
   ;;
   \?)
   echo_and_err "${USAGE}"
   ;;
 esac
done

shift "$((OPTIND -1))"
[ $# -eq 1 ] || echo_and_err "${USAGE}"

INPUT_FN=$1
[ -f "${INPUT_FN}" ] || echo_and_err "Couldn't find ${INPUT_FN}"
INPUT_BASENAME=$(basename "$INPUT_FN" .c)
INPUT_DIR=$(dirname "$INPUT_FN")

RUNTIME_PREFIX="$OPAM_SWITCH_PREFIX/lib/cn/runtime"
[ -d "${RUNTIME_PREFIX}" ] || echo_and_err "Could not find CN's runtime directory (looked at: '${RUNTIME_PREFIX}')"

# Instrument code with CN
if cn instrument "${INPUT_FN}" \
    --run --no-debug-info --tmp --print-steps \
    --output="${INPUT_BASENAME}.exec.c" \
    ${NO_CHECK_OWNERSHIP}; then
  [ "${QUIET}" ] || echo "Success!"
else
  echo_and_err "Test $1 failed."
fi
