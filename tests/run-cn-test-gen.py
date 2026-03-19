#!/usr/bin/env python3

import argparse
import multiprocessing
import os
import signal
import subprocess
import sys
import threading
import time
from collections import defaultdict
from concurrent.futures import ThreadPoolExecutor, as_completed
from pathlib import Path

from tqdm import tqdm


def red(text):
    """Wrap text in ANSI red escape codes."""
    return f"\033[91m{text}\033[0m"


# Track active subprocesses so we can kill them on interrupt
_active_procs = set()
_active_procs_lock = threading.Lock()


def _kill_active_procs():
    """Kill all tracked subprocesses by process group."""
    with _active_procs_lock:
        for proc in _active_procs:
            try:
                os.killpg(os.getpgid(proc.pid), signal.SIGKILL)
            except (ProcessLookupError, OSError):
                pass


def get_test_type(test_file, config):
    """Determine the expected test result type based on filename and config."""
    test_file = Path(test_file).name

    if (test_file.endswith('learn_cast.special.c')
        or test_file.endswith('learn_multiple.special.c')
            or test_file.endswith('pointer_ordering.special.c')):
        if '--symbolic' in config:
            return 'PASS'
        else:
            return 'SKIP'
    elif test_file.endswith('.pass.c'):
        return 'PASS'
    elif test_file.endswith('.fail.c'):
        return 'FAIL'
    elif test_file.endswith('.buggy.c'):
        return 'SKIP'
    elif test_file.endswith('.flaky.c'):
        return 'FLAKY'
    else:
        return 'UNKNOWN'


def run_cn_test(cn_path, test_file, config):
    """Run CN test with given configuration and return (return_code, elapsed_time, test_output)."""
    start_time = time.time()

    cmd = [cn_path, 'test', str(test_file)] + config.split()
    try:
        proc = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                text=True, start_new_session=True)
        with _active_procs_lock:
            _active_procs.add(proc)
        try:
            stdout, stderr = proc.communicate()
        finally:
            with _active_procs_lock:
                _active_procs.discard(proc)
        test_output = stdout + stderr
        return_code = proc.returncode
    except Exception as e:
        test_output = str(e)
        return_code = -1

    elapsed = time.time() - start_time
    return return_code, elapsed, test_output


def run_job(cn_path, test_file, full_config, test_type):
    """Run one test job. Returns (test_file, full_config, passed, elapsed, output)."""
    build_tool = "bash"
    for part in full_config.split():
        if part.startswith("--build-tool="):
            build_tool = part.split("=", 1)[1]

    if test_type == "PASS":
        ret_code, elapsed, output = run_cn_test(
            cn_path, test_file, full_config)
        passed = ret_code == 0
        return (test_file, full_config, passed, elapsed, output)

    elif test_type in ("FAIL", "BUGGY"):
        ret_code, elapsed, output = run_cn_test(
            cn_path, test_file, full_config)
        if ret_code == 0:
            passed = False
        elif build_tool == "bash" and ret_code != 1:
            passed = False
        else:
            passed = True
        return (test_file, full_config, passed, elapsed, output)

    elif test_type == "FLAKY":
        total_elapsed = 0
        for _ in range(3):
            ret_code, elapsed, output = run_cn_test(
                cn_path, test_file, full_config)
            total_elapsed += elapsed
            if ret_code != 0:
                if build_tool == "bash" and ret_code != 1:
                    return (test_file, full_config, False, total_elapsed, output)
                return (test_file, full_config, True, total_elapsed, output)
        return (test_file, full_config, False, total_elapsed, output)

    else:  # UNKNOWN
        return (test_file, full_config, False, 0.0, f"Unknown test type for {test_file}")


def main():
    parser = argparse.ArgumentParser(description='Run CN test generation')
    parser.add_argument('-s', '--symbolic', action='store_true',
                        help='Use symbolic execution configurations')
    parser.add_argument('-j', '--jobs', type=int, default=None,
                        help='Number of parallel workers (default: cpu_count)')
    parser.add_argument('--solver-type', choices=['z3', 'cvc5'],
                        help='SMT solver to use for the test run (default: solver executable in PATH)')
    parser.add_argument('--build-tool', choices=['bash', 'make', 'both'], default='both',
                        help='Build tool to use: bash, make, or both (default: both)')
    parser.add_argument('--only', type=str,
                        help='Comma-separated list of specific test files to run (e.g., "bst.pass.c,bst.fail.c")')
    parser.add_argument('test_file', nargs='?',
                        help='Single test file to run (optional)')

    args = parser.parse_args()

    # Get CN path from OPAM
    opam_prefix = os.environ.get('OPAM_SWITCH_PREFIX')
    if not opam_prefix:
        print("Error: OPAM_SWITCH_PREFIX not set", file=sys.stderr)
        sys.exit(1)

    cn_path = os.path.join(opam_prefix, 'bin', 'cn')

    # Get the directory where this script is located
    script_dir = Path(__file__).parent.resolve()

    # Change to the cn-test-gen directory
    target_dir = script_dir / "cn-test-gen"

    if not target_dir.exists():
        print(f"Error: Directory {target_dir} does not exist", file=sys.stderr)
        sys.exit(1)

    os.chdir(target_dir)

    # Set environment variables for stricter CI and sanitizers
    env = os.environ.copy()
    env['CPPFLAGS'] = ' '.join([env.get('CPPFLAGS', ''), '-Werror', '-Wall',
                               '-Wno-unused-value',
                                '-Wno-unused-variable',
                                '-Wno-unused-but-set-variable',
                                '-Wno-unused-label',
                                '-Wno-unused-function'])
    env['UBSAN_OPTIONS'] = 'halt_on_error=1'
    env['ASAN_OPTIONS'] = 'allocator_may_return_null=1:detect_leaks=0'
    os.environ.update(env)

    # Base configuration
    solver_config = f" --solver-type={args.solver_type}" if args.solver_type else ""

    base_config = (
        f"-I{opam_prefix}/lib/cerberus-lib/runtime/libc/include/posix "
        "--input-timeout=1000 "
        "--progress-level=function "
        "--sanitize=address,undefined "
        "--allow-split-magic-comments "
        "--print-seed"
        f"{solver_config}"
    )

    # Set configurations based on symbolic option
    if args.symbolic:
        alt_configs = [
            "--symbolic --coverage --print-backtrack-info --print-satisfaction-info --smt-pruning-at-runtime",
            "--symbolic --coverage --print-backtrack-info --print-satisfaction-info --smt-pruning-before-absint=slow"
        ]
    else:
        alt_configs = [
            "--coverage --sizing-strategy=quickcheck --inline=everything",
            "--coverage --experimental-learning --print-backtrack-info --print-size-info --static-absint=wrapped_interval --smt-pruning-after-absint=slow --runtime-assert-domain --local-iterations=15",
            "--sizing-strategy=uniform --lazy-gen --experimental-product-arg-destruction --experimental-return-pruning --experimental-arg-pruning --static-absint=interval --smt-pruning-before-absint=fast",
            "--experimental-learning --print-satisfaction-info --output-tyche=results.jsonl --inline=nonrec"
        ]

    # Set build tools based on argument
    if args.build_tool == 'bash':
        build_tools = ["bash"]
    elif args.build_tool == 'make':
        build_tools = ["make"]
    else:  # 'both'
        build_tools = ["bash", "make"]

    # Determine test files
    if args.test_file:
        test_file = Path(args.test_file)
        if not test_file.exists():
            print(
                f"Error: Test file {test_file} does not exist", file=sys.stderr)
            sys.exit(1)
        test_files = [test_file]
    else:
        src_dir = Path("./src")
        if not src_dir.exists():
            print(
                f"Error: Source directory {src_dir} does not exist", file=sys.stderr)
            sys.exit(1)

        if args.symbolic:
            smt_test_unsupported = [
                "ini_queue.pass.c",
                "mkm.pass.c",
                "range.fail.c",
                "range.pass.c",
                "sized_array.pass.c",
            ]
            all_test_files = list(src_dir.glob("**/*.c"))
            test_files = [
                tf for tf in all_test_files
                if tf.name not in smt_test_unsupported
            ]
        else:
            test_files = list(src_dir.glob("**/*.c"))

        # Filter to only requested files if --only is specified
        if args.only:
            only_files = [f.strip() for f in args.only.split(",")]
            available_files = {tf.name: tf for tf in test_files}

            filtered_test_files = []
            for test_name in only_files:
                if test_name in available_files:
                    filtered_test_files.append(available_files[test_name])
                else:
                    mode_str = "symbolic" if args.symbolic else "random"
                    print(
                        f"Error: Test file {test_name} not found in {mode_str} mode test set", file=sys.stderr)
                    sys.exit(1)

            test_files = filtered_test_files

    if not test_files:
        print("No test files found")
        return

    # Build flat job list
    jobs = []
    for tf in test_files:
        for alt_config in alt_configs:
            for build_tool in build_tools:
                full_config = f"{base_config} {alt_config} --build-tool={build_tool}"

                if args.symbolic and Path(tf).name == "ini_queue.fail.c":
                    full_config += ' --exit-fast'

                test_type = get_test_type(tf, full_config)
                if test_type == 'SKIP':
                    continue
                jobs.append((tf, full_config, test_type))

    if not jobs:
        print("No testable jobs after filtering")
        return

    mode_str = "symbolic" if args.symbolic else "random"
    print(
        f"Running {len(jobs)} jobs across {len(test_files)} test files ({mode_str} mode)")

    # Execute all jobs in parallel
    max_workers = args.jobs or max(1, multiprocessing.cpu_count() // 2)
    results = []
    completed_files = set()
    num_failed = 0

    try:
        with ThreadPoolExecutor(max_workers=max_workers) as executor:
            futures = {
                executor.submit(run_job, cn_path, tf, cfg, tt): (tf, cfg, tt)
                for tf, cfg, tt in jobs
            }

            with tqdm(total=len(jobs), unit="job") as pbar:
                for future in as_completed(futures):
                    result = future.result()
                    results.append(result)
                    tf, _, passed, _, _ = result
                    completed_files.add(str(tf))
                    if not passed:
                        num_failed += 1
                        pbar.set_postfix(failed=num_failed)
                    pbar.update(1)

    except KeyboardInterrupt:
        executor.shutdown(wait=False, cancel_futures=True)
        _kill_active_procs()

        incomplete = [str(tf)
                      for tf in test_files if str(tf) not in completed_files]
        if incomplete:
            print(red("\nInterrupted. Incomplete tests:"))
            for t in incomplete:
                print(red(f"  {t}"))

        # Still print results for what we have
        _print_results(results, test_files)
        os._exit(1)

    _print_results(results, test_files)

    # Exit 1 if any failures
    if any(not passed for _, _, passed, _, _ in results):
        sys.exit(1)


def _print_results(results, test_files):
    """Print aggregated results grouped by test file."""
    # Group results by test file
    by_file = defaultdict(list)
    for tf, cfg, passed, elapsed, output in results:
        by_file[str(tf)].append((cfg, passed, elapsed, output))

    failed_files = []

    print(f"\n{'='*60}")
    for tf in test_files:
        key = str(tf)
        if key not in by_file:
            continue

        file_results = by_file[key]
        all_passed = all(p for _, p, _, _ in file_results)
        total_time = sum(e for _, _, e, _ in file_results)

        if all_passed:
            print(f"PASSED: {tf} [{total_time:.1f}s]")
        else:
            failed_configs = [cfg for cfg, p, _, _ in file_results if not p]
            print(f"FAILED: {tf} [{total_time:.1f}s]")
            for cfg in failed_configs:
                # Extract the distinguishing parts (alt_config + build_tool)
                parts = cfg.split("--build-tool=")
                bt = parts[1].split()[0] if len(parts) > 1 else "?"
                print(f"         config: ...--build-tool={bt}")

            # Print output from failed jobs
            for cfg, p, _, output in file_results:
                if not p and output.strip():
                    print(output)
            failed_files.append(tf)

    print(f"{'='*60}")

    total = len(by_file)
    failed = len(failed_files)
    if failed:
        print(f"\n{failed}/{total} test file(s) failed:")
        for tf in failed_files:
            print(f"  {tf}")
    else:
        print(f"\nAll {total} test file(s) passed")


if __name__ == "__main__":
    main()
