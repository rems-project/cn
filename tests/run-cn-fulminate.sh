#!/usr/bin/env bash
# The Fulminate regression tests under tests/fulminate/ (one directory per fix, the
# expectations in each case file); see tests/fulminate/run.sh and tests/fulminate/README.md.
set -euo pipefail
cd "$(dirname "$0")"
exec ./fulminate/run.sh "$@"
