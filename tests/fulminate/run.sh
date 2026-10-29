#!/usr/bin/env bash
# Fulminate regression tests: one directory per fix under tests/fulminate/, one C file per
# case, the expectations in `// expect KEY: VALUE` lines at the top of the file (after the
# header comment). Each case is instrumented with `cn instrument`, checked against its
# expectations, compiled with the CN runtime (linked with the directory's driver.c when there
# is one) and run.
#
#   KEY           VALUE
#   flags         extra `cn instrument` flags for the case (e.g. --remove-uninterp).
#   without-flags `refused`: without those flags cn instrument must fail; `same`: it must
#                 succeed and give the same output (the flags change nothing for this case).
#   instrument    `no (reason)`: cn instrument is expected to fail today. Default: it succeeds.
#   addr_cn       `none`, or the comma-separated functions whose bodies declare `_addr_cn` CN
#                 values (the parameters and locals Fulminate keeps for the specifications).
#   braced        a fragment of the instrumented output, with CN_LOAD(...) wrappers stripped:
#                 some line holds it followed by `{` (the single-statement body was braced).
#   unbraced      likewise, followed by anything but `{`.
#   line          likewise, present as is.
#   code-has      a fragment present in the output with its comments removed (and CN_LOAD(...)
#                 stripped): the generated code, not the echoed specification.
#   code-lacks    a fragment absent from that code.
#   compile       `no (reason)`: the instrumented file does not compile today. Default: it does.
#   run           `0`: the binary exits 0; `flat`: exits 0 and prints "flat over 10 calls"
#                 (driver.c's verdict on the bump allocator); `fail`: exits non-zero (a kept
#                 check must fire). Absent: not run.
# A `no (reason)` expectation records a known gap: the case is XFAIL while the gap is there,
# and FAIL once it is gone, so that the expectation gets removed and the later checks apply.
#
#   run.sh [DIR ...]      default: every directory here; from any cwd
# Environment: CN (default: cn on PATH), CC (default: cc), OPAM_SWITCH_PREFIX for the runtime
# (RUNTIME_PREFIX overrides it), KEEP=DIR to keep the instrumented files and binaries there.
set -uo pipefail
HERE=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
CN=${CN:-cn}; CC=${CC:-cc}
RT=${RUNTIME_PREFIX:-${OPAM_SWITCH_PREFIX:-}/lib/cn/runtime}
[ -d "$RT/include" ] || { echo "run.sh: CN runtime not found at $RT (set OPAM_SWITCH_PREFIX or RUNTIME_PREFIX)" >&2; exit 2; }
if [ -n "${KEEP:-}" ]; then mkdir -p "$KEEP"; OUT=$(cd "$KEEP" && pwd); else OUT=$(mktemp -d "${TMPDIR:-/tmp}/cn-fulminate-tests-XXXXXX"); trap 'rm -rf "$OUT"' EXIT; fi
pass=0; fail=0; xfail=0

# strip CN_LOAD(...) (two levels of nesting) so that expectations read as the source does
normalise() { sed -E -e ':a' -e 's/CN_LOAD\(([^()]*)\)/\1/g; ta' -e 's/CN_LOAD\((([^()]|\([^()]*\))*)\)/\1/g; ta' "$1"; }
# the same without the C comments (the echoed specification), one statement per line kept
strip_comments() { perl -0pe 's{/\*.*?\*/}{}gs; s{//[^\n]*}{}g' "$1"; }
# the functions whose bodies declare _addr_cn values, comma-separated
addr_cn_fns() {
    awk '/^(static )?[a-z_ ]+ \**[a-z_0-9]+\(.*\)[^;]*$/ { fn = $0; sub(/\(.*/, "", fn); sub(/.* \**/, "", fn) }
         /_addr_cn =/ { if (!(fn in seen)) { seen[fn] = 1; out = out (out ? "," : "") fn } }
         END { print (out ? out : "none") }' "$1"
}
# yes if some line of FILE holds TEXT followed by `{` (spaces allowed), no if some line holds it
# followed by anything else, missing if no line holds it
after_is_brace() {
    local text=$1 file=$2 l rest verdict=missing
    while IFS= read -r l; do
        rest=${l#*"$text"}
        if [[ $rest =~ ^[[:space:]]*\{ ]]; then verdict=yes; else [ $verdict = yes ] || verdict=no; fi
    done < <(grep -F -- "$text" "$file")
    echo $verdict
}
reason() { local r=${1#no }; r=${r#(}; r=${r%)}; echo "$r"; }
report() { # verdict case detail
    case $1 in
        PASS)  pass=$((pass+1));   printf '[%s]... \033[32mPASS\033[0m\n' "$2" ;;
        XFAIL) xfail=$((xfail+1)); printf '[%s]... \033[33mXFAIL\033[0m (%s)\n' "$2" "$3" ;;
        FAIL)  fail=$((fail+1));   printf '[%s]... \033[31mFAIL\033[0m (%s)\n' "$2" "$3" ;;
    esac
}

run_case() {
    local dir=$1 src=$2 name; name=$(basename "$dir")/$(basename "$src" .c)
    local exec="$OUT/$(basename "$src" .c).exec.c" bin="$OUT/$(basename "$src" .c).bin" log="$OUT/log"
    local x_instr="" x_compile="" x_run="" x_addr="" x_flags="" x_noflags="" k v
    local -a checks=()
    while IFS= read -r line; do
        k=${line#// expect }; v=${k#*: }; k=${k%%:*}
        case $k in
            instrument) x_instr=$v ;; compile) x_compile=$v ;; run) x_run=$v ;; addr_cn) x_addr=$v ;;
            flags) x_flags=$v ;; without-flags) x_noflags=$v ;;
            braced|unbraced|line|code-has|code-lacks) checks+=("$k=$v") ;;
            *) report FAIL "$name" "unknown expectation '$k'"; return ;;
        esac
    done < <(grep '^// expect ' "$src")
    # 0. without the case's flags, when asked: refused, or the same output
    if [ -n "$x_noflags" ]; then
        local nodir="$OUT/noflags"; mkdir -p "$nodir"
        if "$CN" instrument "$src" --output-dir "$nodir" >"$log" 2>&1; then
            [ "$x_noflags" = refused ] && { report FAIL "$name" "instruments without $x_flags too"; return; }
        else
            [ "$x_noflags" = refused ] || { report FAIL "$name" "fails without $x_flags: $(grep -v '^$' "$log" | head -2 | tr '\n' ' ')"; return; }
        fi
    fi
    # 1. instrument
    # shellcheck disable=SC2086
    if ! "$CN" instrument $x_flags "$src" --output-dir "$OUT" >"$log" 2>&1; then
        if [ -n "$x_instr" ]; then report XFAIL "$name" "does not instrument: $(reason "$x_instr")"
        else report FAIL "$name" "cn instrument failed: $(grep -v '^$' "$log" | head -3 | tr '\n' ' ')"; fi
        return
    fi
    [ -n "$x_instr" ] && { report FAIL "$name" "instruments now; drop the 'expect instrument' line"; return; }
    # 2. the instrumented output
    if [ -n "$x_addr" ]; then
        local got; got=$(addr_cn_fns "$exec")
        [ "$got" = "$x_addr" ] || { report FAIL "$name" "_addr_cn in: $got, expected: $x_addr"; return; }
    fi
    local norm="$OUT/norm.c"; normalise "$exec" >"$norm"
    local code="$OUT/code.c"; strip_comments "$norm" >"$code"
    if [ "$x_noflags" = same ] && ! cmp -s "$exec" "$OUT/noflags/$(basename "$exec")"; then
        report FAIL "$name" "output differs with and without $x_flags"; return
    fi
    local c
    for c in "${checks[@]+"${checks[@]}"}"; do
        k=${c%%=*}; v=${c#*=}
        case $k in
            braced)   [ "$(after_is_brace "$v" "$norm")" = yes ] || { report FAIL "$name" "not braced: '$v'"; return; } ;;
            unbraced) [ "$(after_is_brace "$v" "$norm")" = no ]  || { report FAIL "$name" "braced or missing: '$v'"; return; } ;;
            line)     grep -qF -- "$v" "$norm" || { report FAIL "$name" "line missing: '$v'"; return; } ;;
            code-has)   grep -qF -- "$v" "$code" || { report FAIL "$name" "not in the code: '$v'"; return; } ;;
            code-lacks) ! grep -qF -- "$v" "$code" || { report FAIL "$name" "in the code: '$v'"; return; } ;;
        esac
    done
    # 3. compile, with the directory's driver when there is one
    local -a srcs=("$exec"); [ -f "$dir/driver.c" ] && srcs+=("$dir/driver.c")
    if ! "$CC" -w -I"$RT/include" "${srcs[@]}" "$RT/libcn_exec.a" -o "$bin" >"$log" 2>&1; then
        if [ -n "$x_compile" ]; then report XFAIL "$name" "does not compile: $(reason "$x_compile")"
        else report FAIL "$name" "does not compile: $(grep -m1 error "$log")"; fi
        return
    fi
    [ -n "$x_compile" ] && { report FAIL "$name" "compiles now; drop the 'expect compile' line"; return; }
    # 4. run
    if [ -n "$x_run" ]; then
        local rc; timeout 40 "$bin" >"$log" 2>&1; rc=$?
        if [ "$x_run" = fail ]; then
            [ $rc -ne 0 ] || { report FAIL "$name" "exit 0, but a kept check should have fired"; return; }
        else
            [ $rc -eq 0 ] || { report FAIL "$name" "exit $rc: $(grep -v '^$' "$log" | head -2 | tr '\n' ' ')"; return; }
            [ "$x_run" != flat ] || grep -q 'flat over 10 calls' "$log" || { report FAIL "$name" "$(grep -v '^$' "$log" | head -2 | tr '\n' ' ')"; return; }
        fi
    fi
    report PASS "$name"
}

dirs=("$@"); [ ${#dirs[@]} -gt 0 ] || dirs=("$HERE"/*/)
for d in "${dirs[@]}"; do
    d=${d%/}
    for src in "$d"/[0-9]*.c; do [ -f "$src" ] && run_case "$d" "$src"; done
done
echo "passed $pass, expected failures $xfail, failed $fail"
[ $fail -eq 0 ]
