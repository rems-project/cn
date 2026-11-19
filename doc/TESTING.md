# CN Testing

CN has testing capabilities available via the `cn test` subcommand.

## Overview

Currently, CN supports only per-function tests, but additional types of testing may become available in the future.

`cn test` chooses an input generation engine via subcommand:

- `cn test bennet <filename.c>` uses randomized input generation (Bennet, the default).
- `cn test darcy <filename.c>` uses symbolic (SMT-based) input generation (Darcy). This replaces the deprecated `cn test --symbolic` flag.
- `cn test lucas <filename.c>` uses randomized refinement of abstract elements (Lucas), generalizing Bennet's randomized pipeline. It accepts Bennet's flags plus the abstract-domain flags.

Each engine subcommand only accepts the flags relevant to it (plus the common testing flags), so e.g. `--smt-logging` is rejected under `bennet`, and the abstract-domain flags (e.g. `--static-absint`) are only available under `lucas`.

Running `cn test <engine> <filename.c>` generates C files with the testing infrastructure, the instrumented program under test, and a build script named `run_tests.sh`.
This script compiles the C files and runs the tests.

By default, running `cn test <engine>` will automatically run `run_tests.sh`, which produces a test executable `tests.out`.
This can be disabled by using the `--no-run` flag.

The default behavior of testing is to rely on Fulminate for checking, which does not detect undefined behavior.
If you would like to also check for undefined behavior, you can use a sanitizer via `--sanitize=undefined`.

The output directory for these files can be set by using `--output-dir=<DIR>`.
If the directory does not already exist, it is created.

### Per-function tests

When testing, there are currently two types of tests, constant tests and generator-based tests.
For *each function with a body*, CN will create either a constant test or generator-based test.

If a function takes no arguments, does not use accesses on global variables, and is correct, it should always return the same value and free any memory it allocates.
In this case, a constant test is generated, which runs the function once and uses [Fulminate](FULMINATE_README.md) to check for post-condition violations.

In all other cases, it creates generator-based tests, which are in the style of property-based testing.
A "generator" is created, which randomly generates function arguments, values for globals accessed and heap states, all of which adhere to the given function's pre-condition.
It calls the function with this input and uses [Fulminate](FULMINATE_README.md) similar to the constant tests.

#### Understanding errors

By default, the tool will attempt to synthesize a C program which reproduces the failure.
However, due to non-determinism of `malloc`, if your code include complex relationships between pointer, we cannot guarantee it will consistently reproduce the failure.

If the C program provided does not reproduce the failure, `tests.out` can be run with the `--trap` flag in a debugger.
Since seeds are printed each time the tool runs, `--seed <seed>` can be used to reproduce the test case.
The debugger should automatically pause right before rerunning the failing test case.

#### Writing custom tests

There is currently no way to write custom property-based tests.
However, once lemmas can be tested, a lemma describing the desired property could be written to test it.

In terms of unit tests, one can simply define a function that performs the desired operations.
This function will get detected by `cn test <engine>` and turned into a constant test.
Any assertions that one would make about the result would have to be captured by the post-condition.
In the future, existing infrastructure like `cn_assert` might be adapted for general use.
