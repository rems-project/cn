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
explicit current expectations for Bennet and Darcy. The first batch moves 11
scalar fixtures from `legacy/src/` into `fixtures/campaigns/`; their C types and
implementations are preserved. CN specifications now use integer terms with
finite C bounds and the original overflow exclusions. Two no-argument controls
exercise successful dispatch and a confirmed postcondition failure.

The exporter gives Austen finite-width C arguments and owned values, then casts
them to mathematical integers for specification expressions. Logical structs
use records, with their C storage layouts retained for native calls and memory.
The width regressions exercise signed boundaries, `UINT64_MAX` with mathematical
addition, canonical Boolean values, scalar ownership, nested structs passed and
returned by value, ownership-only structs with arrays, uninitialized ownership,
and a confirmed postcondition failure.
The reviewed baseline contains 52 engine/function runs: 48 passes and four
confirmed findings. All 20 former scalar-width blockers now pass. Equivalent
raw-bitvector equality constraints supplement the original integer equalities
so finite draws can be solved without expensive mixed integer/bitvector search.

`no_args.pass.c:pred_no_args` executes with both engines.
`test_macro.fail.c:always_fail` also executes successfully: its legacy
finding depended on per-access ownership instrumentation, while Austen checks
entry/exit oracles. The manifest retains its intended finding and records this
current difference with a reason; filenames do not determine observed outcomes.

The array functions in `export_widths.pass.c` (`aggregate_echo`,
`owned_aggregate`, `fixed_array`, `quantified_array`, and `empty_array`) have
structural export coverage but are not yet selected by the passing campaign
baseline. Real runs exposed additional Austen 0.0.1 limitations: Bennet rejects
integer map indices, standalone owned arrays lack C-shape-aware generation, and
Darcy's dynamic quantified-array case dispatches no inputs. These failures are
not accepted as successful regressions. Reproductions and the required upstream
work are recorded in [the Austen handoff](../../docs/austen-integer-export-followups.md).

Only a completed report with dispatched inputs and supported oracle results can
count as a pass or finding. A known blocker must match its exact report stage,
error kind, blame, and message. Missing/malformed reports, compiler failures,
timeouts, signals, empty runs, and unsupported oracle results cannot satisfy a
known-blocker expectation. Active fixture files omitted from the manifest are
rejected.

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

## Remaining legacy corpus

`legacy/src/` and `legacy/run-cn-test-gen.py` preserve the old test-generation
corpus and runner for migration. They are inactive and excluded from active
runner modes and CI. The old runner still needs `tqdm` and assumes retired engines,
options, and runtime behavior; it cannot validate the current Austen backend.
Its local memory cache now lives beside that script.

`cast_equality.pass.c` remains there because its integer-to-`u64` cast is
unsupported; removing that cast would erase its regression coverage.
`bounds.pass.c` stays for the heap/array batch, where signed sizes and unsigned
index casts need a separate review. The known-bug exclusion and Lucas-specific
fixture are unchanged. Further fixture batches, flaky-fixture retries, retained
option support, and CN's multi-function campaign orchestration remain migration
work. This runner selects each function explicitly in a separate CN invocation.
