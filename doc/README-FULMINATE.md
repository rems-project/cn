# Fulminate

Fulminate is a tool for translating CN specifications into C runtime assertions, which can then be checked using concrete test inputs.

## Installation 

Fulminate is installed as part of the CN toolchain - see [README.md](README.md) for instructions.

## Running Fulminate

### Generating executable specifications

To produce a file instrumented with CN runtime assertions, run:

```bash
cn instrument <your-file>.c
```

This will produce three files: 

* `<your-file>.exec.c`, the instrumented source
* `<your-file>.cn.h`, a header file containing various definitions and prototypes, including C struct definitions representing CN datatypes, structs and records, as well as function prototypes for the various translated top-level CN functions and predicates.
* `<your-file>.cn.c`, a file that includes `<your-file>.cn.h` and provides definitions for the aforementioned prototypes


These are all produced in the directory the command was run from. Alternatively, one can provide an output directory for these three files (after creating the directory) using the `--output-dir` argument:


```bash
cn instrument <your-file>.c --output-dir <path/to/output/dir>
```

The translation tool injects the executable precondition right before the source function body, at the start of the given function; the executable postcondition into a label called `cn_epilogue`, which gets jumped to via a `goto` wherever there is a return statement in the source; and the executable assertions inplace, wherever they were defined in the source.

### Compiling, linking and running executable CN specifications

To compile and link the output files described in the above section, and also to run these examples on some manually-produced concrete inputs (i.e. via a handwritten `main` function), one can run the following command:

```bash
cn instrument --run <your-file>.c
```

This generates the executable specification files, compiles and links these, and then runs the produced binary.

The compile command includes the `-g` flag for collecting debug information, which means gdb or lldb can be run on the produced binary for setting breakpoints, stepping in and out of functions in a given run, printing concrete variable values at specific points in the program run, etc.
Problems can be caused by gdb on Mac due to some certification-related issues, so we recommend that Mac users use lldb.

