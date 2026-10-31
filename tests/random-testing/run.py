#!/usr/bin/env python3
"""Run CN's random-testing regressions using the active OPAM environment."""

import argparse
import json
import math
import os
from pathlib import Path
import shlex
import shutil
import signal
import subprocess
import sys
import tempfile
import threading


REPO_ROOT = Path(__file__).resolve().parents[2]
FIXTURES = Path("tests/random-testing/fixtures/austen")


def stop_process(process):
    """Stop the whole command tree, including compiler children holding stdout."""
    def send(sig):
        try:
            if os.name == "posix":
                os.killpg(process.pid, sig)
            elif process.poll() is None:
                process.send_signal(sig)
        except ProcessLookupError:
            pass

    send(signal.SIGTERM)
    try:
        process.wait(timeout=1)
    except subprocess.TimeoutExpired:
        pass
    finally:
        # The parent may exit before a child that ignored SIGTERM.
        send(signal.SIGKILL)
        process.wait()


def run_command(command, log_path, timeout=600):
    """Stream output to terminal and log; bound execution and clean up children."""
    command = list(map(str, command))
    print("+ " + shlex.join(command), flush=True)
    with log_path.open("a", encoding="utf-8") as log:
        with subprocess.Popen(
            command,
            cwd=REPO_ROOT,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            text=True,
            encoding="utf-8",
            errors="replace",
            start_new_session=os.name == "posix",
        ) as process:
            timed_out = threading.Event()

            def expire():
                timed_out.set()
                stop_process(process)

            timer = threading.Timer(timeout, expire)
            timer.start()
            try:
                # Stopping the process group closes stdout, ending this loop.
                for line in process.stdout:
                    print(line, end="", flush=True)
                    log.write(line)
                    log.flush()
                status = process.wait()
            except BaseException:
                stop_process(process)
                raise
            finally:
                timer.cancel()
                timer.join()
    if timed_out.is_set():
        raise RuntimeError(f"{command[0]} timed out after {timeout:g} seconds; see {log_path}")
    if status != 0:
        raise RuntimeError(f"{command[0]} exited with status {status}; see {log_path}")


def require(condition, message):
    if not condition:
        raise RuntimeError(message)


def read_json_object(path):
    try:
        contents = json.loads(path.read_text())
    except (OSError, UnicodeError, ValueError) as error:
        raise RuntimeError(f"Cannot read JSON artifact {path}: {error}") from error
    require(isinstance(contents, dict), f"Expected a JSON object: {path}")
    return contents


def check_builds(output_root, timeout=600):
    for tool in ("rustc", "clang", "z3", "austen"):
        run_command([tool, "--version"], output_root / "versions.log", timeout)

    source = output_root / "scalar fixture.c"
    shutil.copyfile(REPO_ROOT / FIXTURES / "scalar.c", source)
    for engine in ("bennet", "darcy"):
        output = output_root / f"{engine} output"
        log = output_root / f"{engine}.log"
        run_command(
            [
                "dune", "exec", "--profile=dev", "cn", "--", "test", engine,
                "--no-run", "--only=decrement", "--print-steps",
                "--output-dir", str(output), str(source),
                "-D", "STEP=1", "-I", str(FIXTURES / "include"),
                "--include", str(FIXTURES / "include/forced.h"),
            ],
            log,
            timeout,
        )
        work = output / "work/decrement"
        spec = read_json_object(output / "spec.json")
        functions = spec.get("functions")
        require(
            isinstance(functions, list)
            and any(isinstance(fn, dict) and fn.get("name") == "decrement" for fn in functions),
            f"{engine}: exported specification does not include decrement",
        )
        prepared = read_json_object(work / "prepared-harness.json")
        require(
            type(prepared.get("version")) is int
            and prepared["version"] == 1
            and prepared.get("engine") == engine,
            f"{engine}: prepared harness has an unexpected version or engine",
        )
        campaign = prepared.get("campaign")
        require(
            isinstance(campaign, dict)
            and campaign.get("instrumented") is True
            and campaign.get("instrumentation") == "compiled"
            and type(campaign.get("coverage_guards")) is int
            and campaign["coverage_guards"] > 0,
            f"{engine}: prepared harness has no compiled coverage instrumentation",
        )
        evaluation = prepared.get("evaluation")
        require(
            isinstance(evaluation, dict) and evaluation.get("status") == "ready",
            f"{engine}: evaluation harness is not ready",
        )
        harness = work / "harness"
        require(
            harness.is_file() and os.access(harness, os.X_OK),
            f"Missing or non-executable harness: {harness}",
        )
        require(
            "-fsanitize-coverage=trace-pc-guard" in (work / "build.sh").read_text(),
            f"{engine}: build.sh does not enable coverage instrumentation",
        )
        require(
            "done: 0 inputs, 0 crashes" in log.read_text(),
            f"{engine}: expected a build-only result with zero inputs and crashes",
        )
        print(f"{engine}: build-only checks passed", flush=True)


def print_diagnostics(output_root):
    for path in sorted(output_root.rglob("*")):
        if path.is_file() and (
            path.suffix == ".log" or path.name in ("build.sh", "prepared-harness.json")
        ):
            print(f"\n--- {path} ---", flush=True)
            try:
                print(path.read_text(errors="replace"), flush=True)
            except OSError as error:
                print(f"Could not read diagnostic: {error}", file=sys.stderr)
    print(f"\nRetained test output: {output_root}", flush=True)


def positive_timeout(value):
    try:
        timeout = float(value)
    except ValueError:
        raise argparse.ArgumentTypeError("timeout must be a positive number") from None
    if not math.isfinite(timeout) or timeout <= 0:
        raise argparse.ArgumentTypeError("timeout must be a positive finite number")
    return timeout


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    suites = parser.add_mutually_exclusive_group()
    suites.add_argument(
        "--unit-only", action="store_true",
        help="run OUnit regressions without requiring real Austen",
    )
    suites.add_argument(
        "--builds-only", action="store_true",
        help="run only the real Austen harness builds",
    )
    parser.add_argument(
        "--timeout", type=positive_timeout, default=600,
        metavar="SECONDS", help="timeout for each external command (default: %(default)s)",
    )
    parser.add_argument(
        "--output-dir", type=Path,
        help="retain logs and artifacts in a fresh run-* directory beneath this path",
    )
    args = parser.parse_args(argv)
    output_root = None
    try:
        if args.output_dir is not None:
            args.output_dir.mkdir(parents=True, exist_ok=True)
        output_root = Path(tempfile.mkdtemp(
            prefix="cn-random-testing-" if args.output_dir is None else "run-",
            dir=args.output_dir,
        )).resolve()
        if not args.builds_only:
            run_command(
                [
                    "dune", "test", "tests/ounit", "tests/random-testing/ounit",
                    "--profile=dev",
                ],
                output_root / "ounit.log",
                args.timeout,
            )
        if not args.unit_only:
            check_builds(output_root, args.timeout)
    except (OSError, RuntimeError) as error:
        print(f"Random testing failed: {error}", file=sys.stderr, flush=True)
        if output_root is not None:
            print_diagnostics(output_root)
        return 1
    except KeyboardInterrupt:
        print("Random testing interrupted.", file=sys.stderr, flush=True)
        if output_root is not None:
            print_diagnostics(output_root)
        return 130
    if args.output_dir is None:
        shutil.rmtree(output_root)
    else:
        print(f"Retained test output: {output_root}", flush=True)
    print("Random-testing regressions passed.", flush=True)
    return 0


if __name__ == "__main__":
    sys.exit(main())
