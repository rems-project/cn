#!/usr/bin/env bash
set -euo pipefail -o noclobber

DIRNAME=$(dirname "$0")
LEMMA_DIR="${DIRNAME}/rocq_lemmas"
WORK_DIR=$(mktemp -d /tmp/cn-lean-lemmas.XXXXXX)
trap 'rm -rf "${WORK_DIR}"' EXIT HUP INT TERM

run_proof_case() {
  local case_name=$1
  local source_dir=$2
  local input_file=$3
  local project_dir="${WORK_DIR}/Lean_Tests"
  local proof_dir="${LEMMA_DIR}/lean_proofs/${case_name}"

  mkdir -p "${project_dir}"
  cp -r "${proof_dir}"/* "${project_dir}/"

  cn verify "${LEMMA_DIR}/cases/${source_dir}/${input_file}" \
          --lemmata_lean "${project_dir}/Gen_Spec/Gen_Spec.lean"

  (cd "${project_dir}" &&
    lake update &&
    lake build)
}

run_and_report() {
  local case_name=$1
  shift

  printf '[%s]...\n' "${case_name}"
  if "$@"; then
    printf '\033[32mPASS\033[0m\n'
    return 0
  else
    local result=$?
    printf '\033[31mFAIL\033[0m (Unexpected return code: %d)\n' "${result}"
    return 1
  fi
}

FAILED=()

while IFS='|' read -r case_name source_dir input_file; do
  if ! run_and_report \
      "${case_name}" run_proof_case "${case_name}" "${source_dir}" "${input_file}"; then
    FAILED+=("${case_name}")
  fi
done <<'EOF'
list_rev|list|rev.c
list_segment|list_segment|list_seg.c
pop_queue|queue|pop.c
EOF

if [ "${#FAILED[@]}" -eq 0 ]; then
  exit 0
else
  printf '\033[31mFAILED: %s\033[0m\n' "${FAILED[*]}"
  exit 1
fi
