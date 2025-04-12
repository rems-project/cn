#!/bin/bash
set -euo pipefail -o noclobber

USAGE="USAGE: $0 -h\n       $0 [-nqu] FILE.c"

function echo_and_err() {
    printf "$1\n"
    exit 1
}

QUIET=""
UBSAN=""
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
   export UBSAN_OPTIONS=halt_on_error=1
   UBSAN="-fsanitize=undefined"
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
 
# the XXXX is ignored by Darwin's mktemp but needed
# by the GNU version
# EXEC_DIR="buddy-exec-2"
# rm -rf $EXEC_DIR
# mkdir $EXEC_DIR
EXEC_DIR=$(mktemp -d -t 'cn-exec.XXXX')
[ -d "${EXEC_DIR}" ] || echo_and_err "Failed to create temporary directory."

# Pre-processing sometimes helps but sometimes doesn't for cn-tutorial/cn-runtime-testing src/examples/*.c
# So disabling for now...
# cpp -P -CC "${INPUT_FN}" > "${EXEC_DIR}/${INPUT_BASENAME}.pp.c"  # macros present - need to preprocess
# INPUT_FN="${EXEC_DIR}/${INPUT_BASENAME}.pp.c"

# Instrument code with CN
if cn instrument "${INPUT_FN}" \
    --run --print-steps \
    --output="${INPUT_BASENAME}-exec.c" \
    --output-dir="${EXEC_DIR}" \
    ${NO_CHECK_OWNERSHIP}; then
  [ "${QUIET}" ] || echo "Success!"
else
  echo_and_err "Test $1 failed in ${EXEC_DIR}."
fi

echo "${EXEC_DIR}"
