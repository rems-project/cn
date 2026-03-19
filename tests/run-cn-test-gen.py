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


def parse_size(s):
    """Parse size string like '4g', '512m' to bytes."""
    s = s.strip().lower()
    multipliers = {'k': 1024, 'm': 1024**2, 'g': 1024**3}
    if s[-1] in multipliers:
        return int(s[:-1]) * multipliers[s[-1]]
    return int(s)


_use_systemd = None


def _has_systemd_run():
    """Check if systemd-run --scope is usable (Linux cgroup memory limits)."""
    try:
        result = subprocess.run(
            ["systemd-run", "--scope", "-q", "--", "true"],
            capture_output=True, timeout=5
        )
        return result.returncode == 0
    except (FileNotFoundError, subprocess.TimeoutExpired):
        return False


def _get_rss_bytes(pid):
    """Get RSS in bytes for a process. Returns None on failure."""
    try:
        if sys.platform == 'linux':
            with open(f'/proc/{pid}/statm') as f:
                return int(f.read().split()[1]) * os.sysconf('SC_PAGE_SIZE')
        else:
            out = subprocess.check_output(
                ['ps', '-o', 'rss=', '-p', str(pid)],
                text=True, stderr=subprocess.DEVNULL
            )
            return int(out.strip()) * 1024
    except (FileNotFoundError, ProcessLookupError, ValueError,
            subprocess.CalledProcessError, OSError):
        return None


def _drain_pipe(pipe, output_list):
    """Read all data from a pipe into output_list."""
    output_list.append(pipe.read())


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


def run_cn_test(cn_path, test_file, config, memory_limit=None):
    """Run CN test with given configuration and return (return_code, elapsed_time, test_output, oom)."""
    start_time = time.time()

    cmd = [cn_path, 'test', str(test_file)] + config.split()

    # Wrap with systemd-run for cgroup memory limits on Linux
    use_systemd = memory_limit is not None and _use_systemd
    if use_systemd:
        cmd = ["systemd-run", "--scope", "-q",
               f"--property=MemoryMax={memory_limit}", "--"] + cmd

    oom = False
    try:
        proc = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                text=True, start_new_session=True)
        with _active_procs_lock:
            _active_procs.add(proc)
        try:
            if memory_limit is not None and not use_systemd:
                # RSS polling fallback (macOS or no systemd)
                stdout_buf, stderr_buf = [], []
                t1 = threading.Thread(target=_drain_pipe, args=(proc.stdout, stdout_buf), daemon=True)
                t2 = threading.Thread(target=_drain_pipe, args=(proc.stderr, stderr_buf), daemon=True)
                t1.start()
                t2.start()

                while proc.poll() is None:
                    rss = _get_rss_bytes(proc.pid)
                    if rss is not None and rss > memory_limit:
                        try:
                            os.killpg(os.getpgid(proc.pid), signal.SIGKILL)
                        except (ProcessLookupError, OSError):
                            pass
                        oom = True
                        break
                    time.sleep(0.5)

                t1.join(timeout=5)
                t2.join(timeout=5)
                stdout = stdout_buf[0] if stdout_buf else ""
                stderr = stderr_buf[0] if stderr_buf else ""
                if proc.poll() is None:
                    proc.wait()
            else:
                stdout, stderr = proc.communicate()
                # systemd-run sends SIGKILL (exit 137) on cgroup OOM
                if use_systemd and proc.returncode == 137:
                    oom = True
        finally:
            with _active_procs_lock:
                _active_procs.discard(proc)
        test_output = stdout + stderr
        return_code = proc.returncode
    except Exception as e:
        test_output = str(e)
        return_code = -1

    elapsed = time.time() - start_time
    return return_code, elapsed, test_output, oom


def run_job(cn_path, test_file, full_config, test_type, memory_limit=None):
    """Run one test job. Returns (test_file, full_config, result, elapsed, output).

    result is one of: 'pass', 'fail', 'oom'.
    """
    build_tool = "bash"
    for part in full_config.split():
        if part.startswith("--build-tool="):
            build_tool = part.split("=", 1)[1]

    def _run():
        return run_cn_test(cn_path, test_file, full_config, memory_limit=memory_limit)

    def _check_oom(oom, ret_code, elapsed, output):
        if oom:
            limit_str = f"{memory_limit}" if memory_limit else "?"
            return (test_file, full_config, 'oom', elapsed,
                    output + f"\nOOM: exceeded {limit_str} byte memory limit\n")
        return None

    if test_type == "PASS":
        ret_code, elapsed, output, oom = _run()
        oom_result = _check_oom(oom, ret_code, elapsed, output)
        if oom_result:
            return oom_result
        result = 'pass' if ret_code == 0 else 'fail'
        return (test_file, full_config, result, elapsed, output)

    elif test_type in ("FAIL", "BUGGY"):
        ret_code, elapsed, output, oom = _run()
        oom_result = _check_oom(oom, ret_code, elapsed, output)
        if oom_result:
            return oom_result
        if ret_code == 0:
            result = 'fail'
        elif build_tool == "bash" and ret_code != 1:
            result = 'fail'
        else:
            result = 'pass'
        return (test_file, full_config, result, elapsed, output)

    elif test_type == "FLAKY":
        total_elapsed = 0
        for _ in range(3):
            ret_code, elapsed, output, oom = _run()
            total_elapsed += elapsed
            oom_result = _check_oom(oom, ret_code, total_elapsed, output)
            if oom_result:
                return oom_result
            if ret_code != 0:
                if build_tool == "bash" and ret_code != 1:
                    return (test_file, full_config, 'fail', total_elapsed, output)
                return (test_file, full_config, 'pass', total_elapsed, output)
        return (test_file, full_config, 'fail', total_elapsed, output)

    else:  # UNKNOWN
        return (test_file, full_config, 'fail', 0.0, f"Unknown test type for {test_file}")


def main():
    parser = argparse.ArgumentParser(description='Run CN test generation')
    parser.add_argument('-s', '--symbolic', action='store_true',
                        help='Use symbolic execution configurations')
    parser.add_argument('-j', '--jobs', type=int, default=None,
                        help='Number of parallel workers (default: cpu_count)')
    parser.add_argument('--solver-type', choices=['z3', 'cvc5'],
                        help='SMT solver to use for the test run (default: solver executable in PATH)')
    parser.add_argument('--memory-limit', type=str, default=None,
                        help='Per-test memory limit (e.g., 512m, 4g)')
    parser.add_argument('--build-tool', choices=['bash', 'make', 'both'], default='both',
                        help='Build tool to use: bash, make, or both (default: both)')
    parser.add_argument('--only', type=str,
                        help='Comma-separated list of specific test files to run (e.g., "bst.pass.c,bst.fail.c")')
    parser.add_argument('test_file', nargs='?',
                        help='Single test file to run (optional)')

    args = parser.parse_args()

    # Parse and cache memory limit settings
    global _use_systemd
    memory_limit = None
    if args.memory_limit is not None:
        memory_limit = parse_size(args.memory_limit)
        _use_systemd = _has_systemd_run()
        if _use_systemd:
            print(f"Memory limit: {args.memory_limit} (enforced via systemd-run)")
        else:
            print(f"Memory limit: {args.memory_limit} (enforced via RSS polling)")

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
    num_oom = 0

    try:
        with ThreadPoolExecutor(max_workers=max_workers) as executor:
            futures = {
                executor.submit(run_job, cn_path, tf, cfg, tt, memory_limit): (tf, cfg, tt)
                for tf, cfg, tt in jobs
            }

            with tqdm(total=len(jobs), unit="job") as pbar:
                for future in as_completed(futures):
                    result = future.result()
                    results.append(result)
                    tf, _, result_str, _, _ = result
                    completed_files.add(str(tf))
                    if result_str == 'oom':
                        num_oom += 1
                        pbar.set_postfix(failed=num_failed, oom=num_oom)
                    elif result_str == 'fail':
                        num_failed += 1
                        pbar.set_postfix(failed=num_failed, oom=num_oom)
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

    # Exit 1 if any failures or OOMs
    if any(r != 'pass' for _, _, r, _, _ in results):
        sys.exit(1)


def _print_results(results, test_files):
    """Print aggregated results grouped by test file."""
    # Group results by test file
    by_file = defaultdict(list)
    for tf, cfg, result_str, elapsed, output in results:
        by_file[str(tf)].append((cfg, result_str, elapsed, output))

    failed_files = []
    oom_files = []

    print(f"\n{'='*60}")
    for tf in test_files:
        key = str(tf)
        if key not in by_file:
            continue

        file_results = by_file[key]
        all_passed = all(r == 'pass' for _, r, _, _ in file_results)
        any_oom = any(r == 'oom' for _, r, _, _ in file_results)
        total_time = sum(e for _, _, e, _ in file_results)

        if all_passed:
            print(f"PASSED: {tf} [{total_time:.1f}s]")
        elif any_oom:
            print(f"OOM:    {tf} [{total_time:.1f}s] — exceeded memory limit")
            for cfg, r, _, _ in file_results:
                if r == 'oom':
                    parts = cfg.split("--build-tool=")
                    bt = parts[1].split()[0] if len(parts) > 1 else "?"
                    print(f"         config: ...--build-tool={bt}")
            for cfg, r, _, output in file_results:
                if r == 'oom' and output.strip():
                    print(output)
            oom_files.append(tf)
        else:
            failed_configs = [cfg for cfg, r, _, _ in file_results if r != 'pass']
            print(f"FAILED: {tf} [{total_time:.1f}s]")
            for cfg in failed_configs:
                parts = cfg.split("--build-tool=")
                bt = parts[1].split()[0] if len(parts) > 1 else "?"
                print(f"         config: ...--build-tool={bt}")

            for cfg, r, _, output in file_results:
                if r != 'pass' and output.strip():
                    print(output)
            failed_files.append(tf)

    print(f"{'='*60}")

    total = len(by_file)
    failed = len(failed_files)
    oom = len(oom_files)
    if failed or oom:
        parts = []
        if failed:
            parts.append(f"{failed} failed")
        if oom:
            parts.append(f"{oom} hit memory limit")
        print(f"\n{total} test file(s): {', '.join(parts)}")
        for tf in failed_files:
            print(f"  FAILED: {tf}")
        for tf in oom_files:
            print(f"  OOM:    {tf}")
    else:
        print(f"\nAll {total} test file(s) passed")


if __name__ == "__main__":
    main()
