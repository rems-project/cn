# Random-testing regressions

From an active OPAM environment with CN's test dependencies installed, run:

```sh
python3 tests/random-testing/run.py
```

The runner uses only the Python standard library. It locates the repository from
its own path, so it can also be invoked by absolute path from another directory.
It builds and tests the current checkout through Dune; CN need not be installed.

The default run checks:

- Generic IndexTerms regressions in `../ounit/`.
- Specification export with Austen removed from `PATH`.
- The Bennet/Darcy adapter using a controlled fake Austen executable.
  This covers failure and interruption statuses, signals, process startup errors,
  frontend failures, empty selections, and early rejection of campaign options
  combined with `--no-run`. One-function campaign checks cover bounded engine
  arguments, heap ownership, confirmed findings, report schema and oracle
  validation, and stale reports.
- Real Bennet and Darcy harness builds using `--no-run`, with macro/include
  forwarding and source/output paths containing spaces. These check the exported
  specification, prepared harness engine and schema version, executable, compiled
  coverage instrumentation, ready evaluation harness, and a
  result reporting zero inputs and crashes. They execute no testing campaigns.
- Real Bennet and Darcy campaigns for migrated fixtures, compared with the
  reviewed expectations in `campaigns.json`. Each selected function gets one
  second with seed 0, ownership checking enabled, and a fresh report.

To run just the OUnit suites without real Austen or its build dependencies:

```sh
python3 tests/random-testing/run.py --unit-only
```

`--builds-only` runs just the real harness builds. `--campaigns-only` runs the
fixture outcome baseline. `--austen-only` runs both, as the Austen CI job does.

The runner's own failure-handling tests require only Python, using real child
processes for timeout and interruption checks:

```sh
python3 -m unittest discover -s tests/random-testing -p 'test_*.py' -v
```

## Dependencies

Both modes require Python 3, Dune, CN's OPAM dependencies installed with
`--with-test`, and a working C preprocessor. Activate the intended OPAM switch
before invoking the runner, for example with `eval $(opam env)`.

The full run additionally requires Austen with both `bennet,darcy` features, its
installed runtime, Clang available as `clang`, native build tools, and Z3's native
library available to Austen. The runner also prints versions of Rust, Clang, Z3,
and Austen, so `rustc` and `z3` must be on `PATH`.

The Random Testing workflow runs Austen-independent unit checks and separate
real harness builds and campaigns on OCaml 4.14.1. It installs and
caches Austen 0.0.1, Rust 1.99.1, Clang 22, and OPAM Z3 5.1.0 for the real builds.
Unit checks require none of the Rust/Austen installation steps. Austen is installed
with:

```sh
cargo install austen --version 0.0.1 --locked --features bennet,darcy
```

When installing Austen against OPAM's native Z3, CI supplies
`Z3_NO_PKG_CONFIG=1`, `Z3_LIBRARY_PATH_OVERRIDE=$(opam var lib)/stublibs`, and
`Z3_SYS_Z3_VERSION=5.1.0`, and adds that library directory to `LD_LIBRARY_PATH`.
The runner uses the supplied environment and installs nothing.

## Layout and diagnostics

`ounit/` contains the export and fake-Austen suites; `fixtures/` holds their shared
fixtures, the real-Austen scalar build fixture, and migrated campaign fixtures.
`fixtures/pending/` holds migrated specifications that still cannot produce a
complete export and are excluded from native campaigns.
Generic OUnit tests stay in
`../ounit/` and are included by the centralized runner.

Command output streams to the terminal and is saved in a fresh temporary
directory. Each external command has a 600-second timeout, configurable with
`--timeout SECONDS`. Timed-out or interrupted commands have their child processes
terminated as well, including Austen's separate process group. CN receives an
interrupt first so it can forward it and reap Austen before forced cleanup.

Successful runs remove their temporary directory by default. Use `--output-dir`
to keep artifacts from every run, including successful ones:

```sh
python3 tests/random-testing/run.py --output-dir /tmp/cn-test-results
```

Each invocation creates a fresh `run-*` subdirectory beneath that path, so old
artifacts cannot satisfy a new run's checks. On failure or interruption,
the runner prints logs, generated build scripts, and prepared-harness metadata,
then reports the retained output directory. Failed commands, missing tools, and
failed assertions all make the runner exit unsuccessfully. Keyboard interruption
returns exit status 130.

CI runs the runner's own regressions before dependency installation, bounds job
execution, and uploads logs, OUnit results, and harness artifacts for seven days.
It also displays the campaign outcome table in the job summary.
The workflow also supports manual dispatch; a failed real-Austen setup does not
prevent the independent unit jobs from finishing.

## Campaign outcome baseline

`campaigns.json` lists each fixture/function pair with an intended result and
explicit current expectations for Bennet and Darcy. After the initial 11 scalar
fixtures, a further 42 legacy fixtures have been migrated: 41 join the active
campaign fixtures, while MKM remains pending export support. The active corpus
now includes heap ownership, arrays, globals, records, recursive lists and trees,
queues, and the runway state machine. Migration does not imply that either
backend can execute every specification.

The reviewed manifest covers 114 functions in 56 C fixtures: 228 engine/function
expectations comprising 172 passes, 15 confirmed findings, 19 blocked cases,
21 inconclusive cases, and one unsupported precondition oracle.

Specifications use mathematical integers while preserving finite C domains,
overflow exclusions, and the original algorithms and C types. Where old casts
wrapped, explicit modulo arithmetic retains their behavior: signed lengths cast
to `u64` use modulo `2^64`, and narrowed signed indices reconstruct their signed
value after modulo `2^32`. Recursive arithmetic retains wrapping where reachable;
widening casts are removed only when they preserve the value. Queue specifications
use `rem` for the old signed remainder operation. Their original bounds remain
in place; negative lengths are not silently excluded to make generation easier.

Calls to the retired `cn_malloc`/`cn_free_sized` helpers use standard
`malloc`/`free`. Normal allocation and algorithm behavior is retained, but the
old runtime's ownership bookkeeping and allocation-failure retry handling are
not reproduced. The spec-only static global in `non_access_global.pass.c` has a
Clang-only `used` attribute so the native harness can locate it without adding a
C access to that global.

The exporter gives Austen finite-width C arguments and owned values, then casts
them to mathematical integers for specification expressions. Logical structs
use records, with their C storage layouts retained for native calls and memory.
The width regressions exercise signed boundaries, `UINT64_MAX` with mathematical
addition, canonical Boolean values, scalar ownership, nested structs passed and
returned by value, ownership-only structs with arrays, uninitialized ownership,
and a confirmed postcondition failure.
The original scalar-width blockers were resolved. Equivalent
raw-bitvector equality constraints supplement the original integer equalities
so finite draws can be solved without expensive mixed integer/bitvector search.

`no_args.pass.c:pred_no_args` executes with both engines.
`test_macro.fail.c:always_fail` also executes successfully: its legacy
finding depended on per-access ownership instrumentation, while Austen checks
entry/exit oracles. The manifest retains its intended finding and records this
current difference with a reason; filenames do not determine observed outcomes.
The same distinction matters for `runway.fail.c:tick`, whose original regression
targets an internal callee's precondition. Entry/exit testing does not establish
that every internal call or annotated statement satisfies its specification.

The array functions in `export_widths.pass.c` (`aggregate_echo`,
`owned_aggregate`, `fixed_array`, `quantified_array`, and `empty_array`) have
explicit campaign expectations alongside their structural export coverage.
Real runs exposed Austen 0.0.1 limitations: Bennet rejects
integer map indices, standalone owned arrays lack C-shape-aware generation, and
Darcy's dynamic quantified-array case can dispatch no inputs. Such failures and
empty campaigns remain blockers or inconclusive outcomes; successful campaigns
on supported engine/function combinations are recorded separately. Reproductions and upstream
work are recorded in [the Austen handoff](../../docs/austen-integer-export-followups.md).

Only a completed report with dispatched inputs and supported oracle results can
count as a pass or finding. The baseline distinguishes the other supported
outcomes explicitly:

- `blocked`: a report-backed specification conversion or generation error with
  an exact stage, error kind, blame, and message. Conversion reports must identify
  the requested configuration. A generator may fail after dispatching some
  inputs; its counters and supported oracle results must remain consistent, and
  it must have no confirmed findings. Partial execution does not turn it into a
  pass.
- `inconclusive`: Austen reports a completed campaign with exit 0, but CN exits
  2 because no inputs were seen, selected, or dispatched. Execution and finding
  counters must be zero; rejection counters are retained for diagnosis.
- `unsupported`: a completed Austen report exits 0 while CN exits 2, every
  selected input has an unsupported precondition result, and no inputs are
  dispatched. All other oracle and finding counters must be zero. This narrow
  case records an unsupported oracle without treating it as successful testing.

These expectations track known limitations without counting them as passes or
findings. Missing or malformed reports, compiler/tool failures, timeouts,
signals, inconsistent counters, internal oracle errors, and other unsupported
oracle combinations remain errors; they cannot satisfy one of these
expectations. Active fixture files omitted from the manifest are rejected.

Every expectation change fails the run, including an unexpected improvement or
a different error replacing a known blocker. To investigate locally:

```sh
python3 tests/random-testing/run.py --campaigns-only --output-dir /tmp/cn-campaign-results
```

Inspect `run-*/campaigns/summary.md`, `summary.json`, and the per-case reports and
logs. Confirm the new behavior, then edit the relevant engine expectation in
`campaigns.json` for review. Differences from the fixture's intent need a reason.
The runner never rewrites its baseline automatically. CI retains these artifacts
and publishes the outcome table even when the baseline comparison fails.

## Pending export and retained legacy fixtures

`fixtures/pending/mkm.pass.c` has migrated integer specifications, preserved
unsigned offset and signed-narrowing behavior, and Cerberus-compatible POSIX
type includes. Its predicates read values obtained from uninitialized ownership,
which the exporter cannot yet represent. Reproduce from the repository root:

```sh
dune build cn.install --profile=dev
case_dir=$(mktemp -d /tmp/cn-mkm-export.XXXXXX)
_build/default/bin/main.exe test export \
  tests/random-testing/fixtures/pending/mkm.pass.c -o "$case_dir/spec.json"
```

The current diagnostic is `AustenTest cannot represent a specification reading
an uninitialized Owned value`. Export currently warns and exits 0 without
creating `spec.json`; an exit code alone does not establish export success.
MKM stays outside `campaigns.json` until it produces a complete specification.

`legacy/src/` and `legacy/run-cn-test-gen.py` preserve the old test-generation
corpus and runner for migration. They are inactive and excluded from active
runner modes and CI. The old runner still needs `tqdm` and assumes retired engines,
options, and runtime behavior; it cannot validate the current Austen backend.
Its local memory cache now lives beside that script.

Four legacy fixtures remain unchanged:

| Fixture | Reason retained |
| --- | --- |
| `cast_equality.pass.c` | Its unsupported integer-to-`u64` cast is the regression's purpose; removing it would erase that coverage. |
| `learn_cast.special.c` | Tests learning across mixed-width casts; an integer-only rewrite would remove the behavior under test. |
| `array2d.buggy.c` | Preserves the existing known-bug exclusion. |
| `bitwise_mask.pass.c` | Targets the retired Lucas/tristate abstract-domain behavior; retained for a separate retirement decision. |

Flaky-fixture retries, retained option support, and CN's multi-function campaign
orchestration remain migration work. This runner selects each function
explicitly in a separate CN invocation.
