# Fulminate regression tests

One directory per Fulminate fix, one C file per case, run by [`run.sh`](run.sh) (from
`tests/`: `./run-cn-fulminate.sh`; the CI's Fulminate workflow runs it after
`run-cn-exec.sh`). Each case is instrumented with `cn instrument`, its output checked, then
compiled with the CN runtime and run. The expectations are `// expect KEY: VALUE` lines at the
top of each file, after the header comment; `run.sh`'s header lists the keys. A `no (reason)`
expectation records a known gap in mainline: the case is an expected failure (XFAIL) while the
gap is there and a failure once it is gone, so that the line gets removed and the later checks
apply.

| directory | fix | what the cases check |
|---|---|---|
| [`bump-alloc-unspecified-fns/`](bump-alloc-unspecified-fns) | [#681](https://github.com/rems-project/cn/pull/681), bump-allocate CN values for parameters and locals only in functions with a specification | which functions declare `_addr_cn` values, and that the bump allocator is flat over 10 calls of an unspecified function ([`driver.c`](bump-alloc-unspecified-fns/driver.c) measures it) |
| [`brace-single-statements/`](brace-single-statements) | [#683](https://github.com/rems-project/cn/pull/683), brace a single-statement body only where a statement-level injection lands next to it, by default | which bodies are braced (jumps and source labels) and which are not (expression statements, compound literals included), and that the result runs |

The XFAIL cases record three gaps unrelated to the fixes they sit beside: the `for` init-clause
re-printed as `int i = c_add_to_ghost_state(...), 0;`, `CN_LOAD` round a compound literal (its
commas split the macro's arguments), and CN's typing on a compound literal after a `case` label
(`Context.get_a: not found`).

Running by hand, with cn installed in the current opam switch:

```sh
cd tests && ./run-cn-fulminate.sh                    # every directory
./fulminate/run.sh fulminate/brace-single-statements  # one
CN=path/to/cn CC=clang RUNTIME_PREFIX=path/to/lib/cn/runtime ./run-cn-fulminate.sh   # another build
```
