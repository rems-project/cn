#!/usr/bin/env python3

import argparse
import hashlib
import json
import multiprocessing
import os
import re
import signal
import subprocess
import sys
import tempfile
import threading
import time
from collections import defaultdict, deque
from concurrent.futures import FIRST_COMPLETED, ThreadPoolExecutor, wait
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
    """Get total RSS in bytes for a process and all its descendants. Returns None on failure."""
    try:
        if sys.platform == 'linux':
            # Sum RSS for the entire process group (parent + children)
            page_size = os.sysconf('SC_PAGE_SIZE')
            total_rss = 0
            found = False
            # Read the main process
            try:
                with open(f'/proc/{pid}/statm') as f:
                    total_rss += int(f.read().split()[1]) * page_size
                    found = True
            except (FileNotFoundError, ProcessLookupError):
                pass
            # Read all children via /proc/[pid]/task/*/children recursively
            try:
                children = _get_descendant_pids_linux(pid)
                for cpid in children:
                    try:
                        with open(f'/proc/{cpid}/statm') as f:
                            total_rss += int(f.read().split()[1]) * page_size
                            found = True
                    except (FileNotFoundError, ProcessLookupError):
                        pass
            except OSError:
                pass
            return total_rss if found else None
        else:
            # macOS: use ps to get RSS for the entire process group
            out = subprocess.check_output(
                ['ps', '-o', 'rss=', '-g', str(pid)],
                text=True, stderr=subprocess.DEVNULL
            )
            total_rss = sum(int(line.strip()) * 1024
                            for line in out.strip().splitlines() if line.strip())
            return total_rss if total_rss > 0 else None
    except (FileNotFoundError, ProcessLookupError, ValueError,
            subprocess.CalledProcessError, OSError):
        return None


def _get_descendant_pids_linux(pid):
    """Get all descendant PIDs of a process on Linux."""
    descendants = []
    try:
        children_dir = Path(f'/proc/{pid}/task/{pid}/children')
        if children_dir.exists():
            child_pids = children_dir.read_text().split()
        else:
            # Fallback: scan /proc for processes with this parent
            child_pids = []
            for entry in os.listdir('/proc'):
                if not entry.isdigit():
                    continue
                try:
                    with open(f'/proc/{entry}/stat') as f:
                        stat = f.read()
                    # Field 4 is PPID (after pid, comm, state)
                    parts = stat.rsplit(')', 1)[-1].split()
                    if len(parts) >= 2 and parts[1] == str(pid):
                        child_pids.append(entry)
                except (FileNotFoundError, ProcessLookupError):
                    pass
        for cpid_str in child_pids:
            cpid = int(cpid_str)
            descendants.append(cpid)
            descendants.extend(_get_descendant_pids_linux(cpid))
    except (FileNotFoundError, ProcessLookupError, OSError):
        pass
    return descendants


def _get_total_memory_bytes():
    """Detect total system memory in bytes. Returns None on failure."""
    try:
        if sys.platform == 'linux':
            with open('/proc/meminfo') as f:
                for line in f:
                    if line.startswith('MemTotal:'):
                        # /proc/meminfo reports in kB
                        return int(line.split()[1]) * 1024
            # Fallback
            return os.sysconf('SC_PHYS_PAGES') * os.sysconf('SC_PAGE_SIZE')
        elif sys.platform == 'darwin':
            result = subprocess.run(
                ['sysctl', '-n', 'hw.memsize'],
                capture_output=True, text=True, timeout=5
            )
            if result.returncode == 0:
                return int(result.stdout.strip())
    except (FileNotFoundError, ValueError, OSError, subprocess.TimeoutExpired):
        pass
    return None


def format_size(nbytes):
    """Format bytes as human-readable string like '1.0g', '512.0m'."""
    if nbytes >= 1024**3:
        return f"{nbytes / 1024**3:.1f}g"
    elif nbytes >= 1024**2:
        return f"{nbytes / 1024**2:.1f}m"
    elif nbytes >= 1024:
        return f"{nbytes / 1024:.1f}k"
    return f"{nbytes}b"


CACHE_VERSION = 1


def _config_hash(config_str):
    """Short SHA-256 hash of config string for cache key."""
    return hashlib.sha256(config_str.encode()).hexdigest()[:12]


def _cache_key(test_file, config_str):
    """Generate cache key from test file and config."""
    return f"{Path(test_file).name}::{_config_hash(config_str)}"


def _load_cache(cache_path):
    """Load memory cache from JSON file. Returns entries dict."""
    try:
        with open(cache_path) as f:
            data = json.load(f)
        if data.get("version") != CACHE_VERSION:
            return {}
        entries = data.get("entries", {})
        if not isinstance(entries, dict):
            return {}
        return entries
    except (FileNotFoundError, json.JSONDecodeError, KeyError, TypeError):
        return {}


def _save_cache(cache_path, entries):
    """Save memory cache to JSON file atomically."""
    if not entries:
        return
    data = {"version": CACHE_VERSION, "entries": entries}
    tmp_path = None
    try:
        dir_path = os.path.dirname(os.path.abspath(cache_path))
        with tempfile.NamedTemporaryFile(
            mode='w', dir=dir_path, suffix='.tmp', delete=False
        ) as tmp:
            tmp_path = tmp.name
            json.dump(data, tmp, indent=2, sort_keys=True)
        os.replace(tmp_path, cache_path)
    except OSError as e:
        print(f"Warning: failed to save cache: {e}", file=sys.stderr)
        if tmp_path:
            try:
                os.unlink(tmp_path)
            except OSError:
                pass


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
                t1 = threading.Thread(target=_drain_pipe, args=(
                    proc.stdout, stdout_buf), daemon=True)
                t2 = threading.Thread(target=_drain_pipe, args=(
                    proc.stderr, stderr_buf), daemon=True)
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
    parser.add_argument('--cache-file', type=str, default=None,
                        help='Path to memory settings cache file (default: tests/.test-gen-cache.json)')
    parser.add_argument('--no-cache', action='store_true',
                        help='Disable loading and saving memory settings cache')
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
            print(
                f"Memory limit: {args.memory_limit} (enforced via systemd-run)")
        else:
            print(
                f"Memory limit: {args.memory_limit} (enforced via RSS polling)")

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
        "--max-generator-size=16 "
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

                if args.symbolic and Path(tf).name == "mkm.pass.c":
                    full_config += ' --max-array-length=1024'

                if args.symbolic and Path(tf).name == "no_args.pass.c":
                    full_config += ' --experimental-arg-pruning'

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

    # Execute jobs with budget-aware scheduling
    max_workers = args.jobs or max(1, multiprocessing.cpu_count() // 2)

    def _apply_reduced_knobs(cfg):
        """Reduce --num-samples, --max-bump-blocks, and --max-generator-size for OOM retry."""
        m = re.search(r'--num-samples=(\d+)', cfg)
        if m:
            cfg = cfg.replace(
                m.group(), f'--num-samples={int(m.group(1)) // 2}')
        else:
            cfg += ' --num-samples=50'
        if '--max-bump-blocks=' not in cfg:
            cfg += ' --max-bump-blocks=64'
        m = re.search(r'--max-generator-size=(\d+)', cfg)
        if m:
            cfg = cfg.replace(
                m.group(), f'--max-generator-size={int(m.group(1)) // 2}')
        else:
            cfg += ' --max-generator-size=16'
        return cfg

    # Budget initialization
    if memory_limit is not None:
        total_budget = memory_limit
    else:
        detected = _get_total_memory_bytes()
        # Reserve 1 GiB for OS/overhead when auto-detecting
        total_budget = max(detected - 1024**3, 1024**3) if detected else None

    per_job_limit = total_budget // max_workers if total_budget else None

    # Load memory settings cache
    cache_path = None
    cache_entries = {}
    cache_hits = 0
    cache_applied = 0
    if not args.no_cache:
        if args.cache_file is not None:
            cache_path = args.cache_file
        else:
            cache_path = str(script_dir / '.test-gen-cache.json')
        cache_entries = _load_cache(cache_path)

    # Build pending jobs with per-job memory limits, applying cached settings
    # Tuple: (tf, cfg, tt, jml, san_disabled, knobs_reduced, orig_cfg)
    pending = deque()
    for tf, cfg, tt in jobs:
        orig_cfg = cfg
        ck = _cache_key(tf, cfg)
        cached = cache_entries.get(ck)
        if cached:
            cache_hits += 1
            jml = per_job_limit
            san_disabled = cached.get("san_disabled", False)
            knobs_reduced = cached.get("knobs_reduced", False)
            cached_mem = cached.get("memory_limit")
            if cached_mem is not None and total_budget is not None:
                jml = min(cached_mem, total_budget)
            elif cached_mem is not None:
                jml = cached_mem
            if san_disabled or knobs_reduced or (jml != per_job_limit):
                cache_applied += 1
            if san_disabled:
                cfg = cfg.replace(
                    '--sanitize=address,undefined', '--sanitize=undefined')
            if knobs_reduced:
                cfg = _apply_reduced_knobs(cfg)
        else:
            jml = per_job_limit
            san_disabled = False
            knobs_reduced = False
        pending.append(
            (tf, cfg, tt, jml, san_disabled, knobs_reduced, orig_cfg))

    if cache_hits:
        print(f"Cache: {cache_hits} hits ({cache_applied} applied), "
              f"{len(jobs) - cache_hits} misses")

    # future -> (tf, cfg, tt, job_mem_limit, san_disabled, knobs_reduced, orig_cfg)
    running = {}
    retry_history = {}  # (str(tf), cfg) -> [(limit_bytes, result_str)]
    cache_updates = {}  # cache_key -> {memory_limit, san_disabled, knobs_reduced}

    results = []
    completed_files = set()
    num_failed = 0
    num_oom = 0
    num_retries = 0

    try:
        with ThreadPoolExecutor(max_workers=max_workers) as executor:
            with tqdm(total=len(jobs), unit="job") as pbar:
                available_budget = total_budget

                while pending or running:
                    # Dispatch phase: fill up to max_workers slots
                    submitted_this_round = True
                    while submitted_this_round and pending and len(running) < max_workers:
                        submitted_this_round = False
                        for i in range(len(pending)):
                            tf, cfg, tt, jml, san_disabled, knobs_reduced, orig_cfg = pending[i]
                            if total_budget is None or jml <= available_budget:
                                del pending[i]
                                if total_budget is not None:
                                    available_budget -= jml
                                fut = executor.submit(
                                    run_job, cn_path, tf, cfg, tt, jml)
                                running[fut] = (
                                    tf, cfg, tt, jml, san_disabled, knobs_reduced, orig_cfg)
                                submitted_this_round = True
                                break

                    if not running:
                        break  # Nothing running, nothing dispatchable

                    # Wait for at least one completion
                    done, _ = wait(running.keys(), return_when=FIRST_COMPLETED)
                    for fut in done:
                        tf, cfg, tt, jml, san_disabled, knobs_reduced, orig_cfg = running.pop(
                            fut)
                        result = fut.result()
                        _, _, result_str, elapsed, output = result

                        # Return budget
                        if total_budget is not None and jml is not None:
                            available_budget += jml

                        key = (str(tf), cfg)

                        # OOM retry logic
                        if result_str == 'oom' and total_budget is not None:
                            retry_history.setdefault(
                                key, []).append((jml, 'oom', san_disabled, knobs_reduced))
                            new_limit = min(jml * 2, total_budget)
                            if new_limit > jml:
                                # Re-queue with doubled limit
                                pending.append(
                                    (tf, cfg, tt, new_limit, san_disabled, knobs_reduced, orig_cfg))
                                num_retries += 1
                                pbar.set_postfix(failed=num_failed, oom=num_oom,
                                                 retries=num_retries)
                                continue  # Don't record as final result yet
                            elif not san_disabled:
                                # Retry without address sanitizer (reduces memory overhead)
                                new_cfg = cfg.replace(
                                    '--sanitize=address,undefined', '--sanitize=undefined')
                                if new_cfg != cfg:
                                    pending.append(
                                        (tf, new_cfg, tt, jml, True, knobs_reduced, orig_cfg))
                                    num_retries += 1
                                    pbar.set_postfix(failed=num_failed, oom=num_oom,
                                                     retries=num_retries)
                                    continue
                            elif not knobs_reduced:
                                # Last resort: reduce test workload
                                new_cfg = _apply_reduced_knobs(cfg)
                                pending.append(
                                    (tf, new_cfg, tt, jml, san_disabled, True, orig_cfg))
                                num_retries += 1
                                pbar.set_postfix(failed=num_failed, oom=num_oom,
                                                 retries=num_retries)
                                continue

                        # Final result (pass, fail, or final OOM)
                        if key in retry_history:
                            retry_history[key].append(
                                (jml, result_str, san_disabled, knobs_reduced))

                        # Track settings for cache (only non-default or retried jobs)
                        ck = _cache_key(tf, orig_cfg)
                        if result_str != 'oom' and (
                            san_disabled or knobs_reduced
                            or jml != per_job_limit
                            or ck in cache_entries
                        ):
                            cache_updates[ck] = {
                                "memory_limit": jml,
                                "san_disabled": san_disabled,
                                "knobs_reduced": knobs_reduced,
                            }

                        results.append(result)
                        completed_files.add(str(tf))
                        if result_str == 'oom':
                            num_oom += 1
                        elif result_str == 'fail':
                            num_failed += 1
                        pbar.set_postfix(failed=num_failed, oom=num_oom,
                                         retries=num_retries)
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

        # Save cache with partial results
        if cache_path and cache_updates:
            merged = dict(cache_entries)
            merged.update(cache_updates)
            _save_cache(cache_path, merged)

        # Still print results for what we have
        _print_results(results, test_files, retry_history)
        os._exit(1)

    # Save cache
    if cache_path and cache_updates:
        merged = dict(cache_entries)
        merged.update(cache_updates)
        _save_cache(cache_path, merged)
        print(f"Cache: saved {len(cache_updates)} entries to {cache_path}")

    _print_results(results, test_files, retry_history)

    # Exit 1 if any failures or OOMs
    if any(r != 'pass' for _, _, r, _, _ in results):
        sys.exit(1)


def _print_results(results, test_files, retry_history=None):
    """Print aggregated results grouped by test file."""
    if retry_history is None:
        retry_history = {}

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
                    print(f"         config: {cfg}")
            for cfg, r, _, output in file_results:
                if r == 'oom' and output.strip():
                    print(output)
            oom_files.append(tf)
        else:
            failed_configs = [cfg for cfg, r, _,
                              _ in file_results if r != 'pass']
            print(f"FAILED: {tf} [{total_time:.1f}s]")
            for cfg in failed_configs:
                print(f"         config: {cfg}")

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

    # Print retry history summary
    if retry_history:
        print(f"\nMemory limit adjustments:")
        for (tf_str, cfg), history in retry_history.items():
            steps = []
            for limit_bytes, result_str, san_off, knobs_red in history:
                size_str = format_size(limit_bytes) if limit_bytes else "?"
                tags = []
                if san_off:
                    tags.append("no ASan")
                if knobs_red:
                    tags.append("reduced")
                suffix = f", {', '.join(tags)}" if tags else ""
                steps.append(f"{size_str} ({result_str.upper()}{suffix})")
            print(f"  {Path(tf_str).name} (config: {cfg}):")
            print(f"    {' -> '.join(steps)}")


if __name__ == "__main__":
    main()
