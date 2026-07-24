#!/usr/bin/env bash
set -euo pipefail -o noclobber

DIRNAME=$(dirname "$0")
LEMMA_DIR="${DIRNAME}/rocq_lemmas"
WORK_DIR=$(mktemp -d /tmp/cn-rocq-lemmas.XXXXXX)
trap 'rm -rf "${WORK_DIR}"' EXIT HUP INT TERM

run_proof_case() {
  local case_name=$1
  local source_dir=$2
  local input_file=$3
  local case_dir="${WORK_DIR}/${case_name}"
  local theory_dir="${case_dir}/theories"
  local proof_dir="${LEMMA_DIR}/proofs/${case_name}"

  mkdir -p "${theory_dir}"
  cp "${DIRNAME}/../coq/CN_Lemmas/CN_Lib.v" "${theory_dir}/"
  cp "${DIRNAME}/../coq/CN_Lemmas/CN_Lib_Iris.v" "${theory_dir}/"
  cp "${proof_dir}"/*.v "${theory_dir}/"
  cp "${proof_dir}/_CoqProject" "${case_dir}/"

  if ! (cd "${LEMMA_DIR}/cases/${source_dir}" &&
        timeout 60 cn verify "${input_file}" \
          --lemmata "${theory_dir}/Gen_Spec.v") >"${case_dir}/cn.log" 2>&1; then
    cat "${case_dir}/cn.log"
    return 1
  fi

  (cd "${case_dir}" &&
    coq_makefile -f _CoqProject -o Makefile.coq &&
    timeout 60 make -f Makefile.coq)
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
array_combine|arrays/combine|array_lemma.c
arrays_inductive|arrays/inductive|array_lemma.c
arrays_testing|arrays/testing|array_lemma.c
pop_queue|queue|pop.c
struct_test|struct_test|test.c
EOF

if [ "${#FAILED[@]}" -eq 0 ]; then
  exit 0
else
  printf '\033[31mFAILED: %s\033[0m\n' "${FAILED[*]}"
  exit 1
fi
