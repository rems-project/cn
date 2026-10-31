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
  combined with `--no-run`.
- Real Bennet and Darcy harness builds using `--no-run`, with macro/include
  forwarding and source/output paths containing spaces. These check the exported
  specification, prepared harness engine and schema version, executable, compiled
  coverage instrumentation, ready evaluation harness, and a
  result reporting zero inputs and crashes. They execute no testing campaigns.

To run just the OUnit suites without real Austen or its build dependencies:

```sh
python3 tests/random-testing/run.py --unit-only
```

`--builds-only` runs just the real harness builds, as the Austen CI job does.

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
real harness builds on OCaml 4.14.1. It installs and
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
fixtures and the real-Austen scalar fixture. Generic OUnit tests stay in
`../ounit/` and are included by the centralized runner.

Command output streams to the terminal and is saved in a fresh temporary
directory. Each external command has a 600-second timeout, configurable with
`--timeout SECONDS`. Timed-out or interrupted commands have their child processes
terminated as well.

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
The workflow also supports manual dispatch; a failed real-Austen setup does not
prevent the independent unit jobs from finishing.

## Legacy corpus

`legacy/src/` and `legacy/run-cn-test-gen.py` preserve the old test-generation
corpus and runner for migration. They are inactive and excluded from both runner
modes and active CI. The old runner still needs `tqdm` and assumes retired engines,
options, and runtime behavior; it cannot validate the current Austen backend.
Its local memory cache now lives beside that script.

Campaign execution and specification porting remain separate migration work.
