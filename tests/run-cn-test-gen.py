#!/usr/bin/env python3

import argparse
import concurrent.futures
import multiprocessing
import os
import subprocess
import sys
import time
from pathlib import Path


def get_test_type(test_file, config):
    """Determine the expected test result type based on filename and config."""
    test_file = Path(test_file).name

    if test_file.endswith('.pass.c'):
        return 'PASS'
    elif test_file.endswith('.fail.c'):
        return 'FAIL'
    elif test_file.endswith('.buggy.c'):
        return 'BUGGY'
    elif test_file.endswith('.flaky.c'):
        return 'FLAKY'
    elif (test_file.endswith('learn_cast.special.c')
          or test_file.endswith('learn_multiple.special.c')
          or test_file.endswith('pointer_ordering.special.c')):
        if '--symbolic' in config:
            return 'PASS'
        else:
            return 'SKIP'
    else:
        return 'UNKNOWN'


def separator():
    """Return a separator string for output formatting."""
    return '\n===========================================================\n\n'


def run_cn_test(cn_path, test_file, config):
    """Run CN test with given configuration and return (return_code, elapsed_time, test_output)."""
    start_time = time.time()

    cmd = [cn_path, 'test', str(test_file)] + config.split()
    try:
        result = subprocess.run(cmd, capture_output=True,
                                text=True, timeout=None)
        test_output = result.stdout + result.stderr
        return_code = result.returncode
    except subprocess.CalledProcessError as e:
        test_output = e.stdout + e.stderr if e.stdout or e.stderr else str(e)
        return_code = e.returncode
    except Exception as e:
        test_output = str(e)
        return_code = -1

    end_time = time.time()
    elapsed = end_time - start_time

    return return_code, elapsed, test_output


def run_single_test(test_file, cn_path, base_config, alt_configs, build_tools, _symbolic):
    """Run a single test file with all configurations."""
    # Track failures and times for this test
    num_failed = 0
    failed_configs = []
    times = []
    output_buffer = ""

    # Run tests for each configuration
    for alt_config in alt_configs:
        for build_tool in build_tools:
            full_config = f"{base_config} {alt_config} --build-tool={build_tool}"

            output_buffer += separator()
            output_buffer += f'Running CI with CLI config "{full_config}"\n'
            output_buffer += separator()

            test_type = get_test_type(test_file, full_config)

            if test_type == "PASS":
                ret_code, elapsed, test_output = run_cn_test(
                    cn_path, test_file, full_config)
                times.append(f"{elapsed:.3f}s")
                output_buffer += test_output

                if ret_code != 0:
                    output_buffer += f"\n{test_file} -- Tests failed unexpectedly\n"
                    num_failed += 1
                    failed_configs.append(
                        f"({alt_config} --build-tool={build_tool})")
                else:
                    output_buffer += f"\n{test_file} -- Tests passed successfully\n"

            elif test_type in ["FAIL", "BUGGY"]:
                ret_code, elapsed, test_output = run_cn_test(
                    cn_path, test_file, full_config)
                times.append(f"{elapsed:.3f}s")

                if ret_code == 0:
                    output_buffer += f"\n{test_file} -- Tests passed unexpectedly\n"
                    num_failed += 1
                    failed_configs.append(f"({alt_config})")
                elif build_tool == "bash" and ret_code != 1:
                    output_buffer += test_output
                    output_buffer += f"\n{test_file} -- Tests failed unnaturally\n"
                    num_failed += 1
                    failed_configs.append(f"({alt_config})")
                else:
                    output_buffer += f"\n{test_file} -- Tests failed successfully\n"

            elif test_type == "FLAKY":
                ret_code, elapsed, test_output = run_cn_test(
                    cn_path, test_file, full_config)

                # Run twice since flaky
                if ret_code == 0:
                    ret_code, elapsed_extra, test_output = run_cn_test(
                        cn_path, test_file, full_config)
                    elapsed += elapsed_extra

                times.append(f"{elapsed:.3f}s")

                if ret_code == 0:
                    output_buffer += f"\n{test_file} -- Tests passed unexpectedly\n"
                    num_failed += 1
                    failed_configs.append(f"({alt_config})")
                elif build_tool == "bash" and ret_code != 1:
                    output_buffer += test_output
                    output_buffer += f"\n{test_file} -- Tests failed unnaturally\n"
                    num_failed += 1
                    failed_configs.append(f"({alt_config})")
                else:
                    output_buffer += f"\n{test_file} -- Tests failed successfully"

            elif test_type == "SKIP":
                # Skip this test configuration
                continue

            elif test_type == "UNKNOWN":
                return test_file, False, f"{test_file} -- Unknown test type"

            output_buffer += separator()

    # Format times
    times_str = ", ".join(times)

    # Report results
    if not failed_configs:
        return test_file, True, f"{test_file} - all configs passed. ({times_str})"
    else:
        output_buffer += f"{test_file} - {num_failed} configs failed:\n  {' '.join(failed_configs)}"
        output_buffer += f"\nTimes: ({times_str})"
        return test_file, False, output_buffer


def main():
    parser = argparse.ArgumentParser(description='Run CN test generation')
    parser.add_argument('-s', '--symbolic', action='store_true',
                        help='Use symbolic execution configurations')
    parser.add_argument('--mode', choices=['testing', 'benchmarking'], default='testing',
                        help='Execution mode: testing (parallel, minimal output) or benchmarking (sequential, detailed timing)')
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
    env['CPPFLAGS'] = env.get('CPPFLAGS', '') + ' -Werror'
    env['UBSAN_OPTIONS'] = 'halt_on_error=1'
    env['ASAN_OPTIONS'] = 'allocator_may_return_null=1:detect_leaks=0'
    os.environ.update(env)

    # Base configuration
    base_config = (
        f"-I{opam_prefix}/lib/cerberus-lib/runtime/libc/include/posix "
        "--input-timeout=1000 "
        "--progress-level=function "
        "--sanitize=address,undefined "
        "--allow-split-magic-comments "
        "--print-seed"
    )

    # Set configurations based on symbolic option
    if args.symbolic:
        alt_configs = [
            "--symbolic --coverage --print-backtrack-info --print-satisfaction-info --smt-pruning-at-runtime",
            "--symbolic --coverage --print-backtrack-info --print-satisfaction-info --smt-pruning-before-absint=slow"
        ]
    else:
        alt_configs = [
            "--coverage --sizing-strategy=quickcheck",
            "--coverage --experimental-learning --print-backtrack-info --print-size-info --static-absint=wrapped_interval --smt-pruning-after-absint=slow",
            "--sizing-strategy=uniform --random-size-splits --experimental-product-arg-destruction --static-absint=interval --smt-pruning-before-absint=fast",
            "--random-size-splits --experimental-learning --print-satisfaction-info --output-tyche=results.jsonl"
        ]

    # Set build tools based on argument
    if args.build_tool == 'bash':
        build_tools = ["bash"]
    elif args.build_tool == 'make':
        build_tools = ["make"]
    else:  # 'both'
        build_tools = ["bash", "make"]

    # If a single test file is specified, run just that test
    if args.test_file:
        test_file = Path(args.test_file)
        if not test_file.exists():
            print(
                f"Error: Test file {test_file} does not exist", file=sys.stderr)
            sys.exit(1)

        mode_str = "symbolic" if args.symbolic else "random"
        print(
            f"Running single test: {test_file} ({mode_str} mode, {args.mode} mode)")

        if args.mode == 'benchmarking':
            import time as time_module
            test_start_time = time_module.time()

        _, success, output = run_single_test(
            test_file, cn_path, base_config, alt_configs, build_tools, args.symbolic)

        if args.mode == 'benchmarking':
            test_end_time = time_module.time()
            test_elapsed = test_end_time - test_start_time
            status = "PASSED" if success else "FAILED"
            print(f"{status}: {test_file} (took {test_elapsed:.3f}s)")

        print(output)
        sys.exit(0 if success else 1)

    # Otherwise, find test files to run
    src_dir = Path("./src")
    if not src_dir.exists():
        print(
            f"Error: Source directory {src_dir} does not exist", file=sys.stderr)
        sys.exit(1)

    # First, determine the normal set of test files based on mode
    if args.symbolic:
        # For symbolic mode, use a specific subset of tests
        smt_test_files = [
            "abs_mem.fail.c",
            "abs_mem.pass.c",
            "abs.pass.c",
            "array_shift.pass.c",
            "between.pass.c",
            "bin_tree.pass.c",
            "bounds.pass.c",
            "cast_equality.pass.c",
            "counter.pass.c",
            "delete_main.pass.c",
            "dll.pass.c",
            "enum1.pass.c",
            "enum2.pass.c",
            "func_apply.pass.c",
            "learn_cast.special.c",
            "learn_equality.pass.c",
            "learn_multiple.special.c",
            "let.pass.c",
            "list_rev.fail.c",
            "list_rev.pass.c",
            "list_seg.pass.c",
            "member_shift.pass.c",
            "memcpy.pass.c",
            "mps_1.pass.c",
            "neg100.pass.c",
            "neq.pass.c",
            "non_access_global.pass.c",
            "pointer_ordering.special.c",
            "record.pass.c",
            "runway.pass.c",
            "smt_pruning.pass.c",
            "sorted_list.cons.fail.c",
            "sorted_list.insert.pass.c",
            "sorted_list.sum.pass.c",
            "sorted_list_alt.insert.flaky.c",
            "sorted_list_alt.insert.pass.c",
            "sorted_list_alt3.pass.c",
            "test_macro.fail.c",
            "void_pred.pass.c",
        ]
        test_files = []
        for test_name in smt_test_files:
            test_path = src_dir / test_name
            if test_path.exists():
                test_files.append(test_path)
            else:
                print(
                    f"Error: SMT test file {test_path} not found", file=sys.stderr)
                sys.exit(1)
    else:
        # For non-symbolic mode, use all .c files
        test_files = list(src_dir.glob("**/*.c"))

    # Filter to only requested files if --only is specified
    if args.only:
        only_files = [f.strip() for f in args.only.split(",")]
        # Convert test_files to a set of filenames for quick lookup
        available_files = {
            test_file.name: test_file for test_file in test_files}

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

    mode_str = "symbolic" if args.symbolic else "random"
    print(
        f"Found {len(test_files)} test files for {mode_str} mode ({args.mode} mode)")

    failed_tests = []

    if args.mode == 'testing':
        # Testing mode: Run tests in parallel with minimal output
        max_workers = multiprocessing.cpu_count()
        with concurrent.futures.ThreadPoolExecutor(max_workers=max_workers) as executor:
            # Submit all test jobs
            future_to_test = {
                executor.submit(run_single_test, test_file, cn_path, base_config, alt_configs, build_tools, args.symbolic): test_file
                for test_file in test_files
            }

            # Collect results
            for future in concurrent.futures.as_completed(future_to_test):
                test_file, success, output = future.result()
                if not success:
                    failed_tests.append(test_file)
                    print(f"FAILED: {test_file}")
                    if output.strip():
                        print(output)
                else:
                    print(f"PASSED: {test_file}")

    elif args.mode == 'benchmarking':
        # Benchmarking mode: Run tests sequentially with detailed timing
        import time as time_module
        total_start_time = time_module.time()

        for i, test_file in enumerate(test_files, 1):
            print(f"\n[{i}/{len(test_files)}] Running test: {test_file}")
            test_start_time = time_module.time()

            test_file_result, success, output = run_single_test(
                test_file, cn_path, base_config, alt_configs, build_tools, args.symbolic)

            test_end_time = time_module.time()
            test_elapsed = test_end_time - test_start_time

            if not success:
                failed_tests.append(test_file_result)
                print(f"FAILED: {test_file} (took {test_elapsed:.3f}s)")
                if output.strip():
                    print(output)
            else:
                print(f"PASSED: {test_file} (took {test_elapsed:.3f}s)")
                print(output)

        total_end_time = time_module.time()
        total_elapsed = total_end_time - total_start_time
        print(f"\nTotal benchmarking time: {total_elapsed:.3f}s")

    # Exit with error if any tests failed
    if failed_tests:
        print(f"\n{len(failed_tests)} test(s) failed:")
        for test_file in failed_tests:
            print(f"  {test_file}")
        sys.exit(1)
    else:
        print(f"\nAll {len(test_files)} test(s) passed")


if __name__ == "__main__":
    main()
