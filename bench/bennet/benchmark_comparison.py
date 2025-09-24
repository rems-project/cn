#!/usr/bin/env python3

import argparse
import json
import os
import re
import subprocess
import sys
import tempfile
from datetime import datetime
from pathlib import Path

import numpy as np
from scipy import stats


def run_command(cmd, cwd=None, check=True):
    """Run a command and return the result."""
    print(f"Running: {' '.join(cmd) if isinstance(cmd, list) else cmd}")
    result = subprocess.run(cmd, shell=isinstance(cmd, str), cwd=cwd,
                            capture_output=True, text=True, check=check)
    if result.returncode != 0 and check:
        print(f"Command failed with code {result.returncode}")
        print(f"stdout: {result.stdout}")
        print(f"stderr: {result.stderr}")
        raise subprocess.CalledProcessError(result.returncode, cmd)
    return result


def get_current_commit():
    """Get the current git commit hash."""
    result = run_command(["git", "rev-parse", "HEAD"])
    return result.stdout.strip()


def get_current_branch():
    """Get the current git branch."""
    result = run_command(["git", "rev-parse", "--abbrev-ref", "HEAD"])
    return result.stdout.strip()


def parse_benchmark_output(output):
    """Parse the output of run-cn-test-gen.py --mode=benchmarking to extract timing data."""
    times_data = {}
    current_test = None

    lines = output.split('\n')
    for line in lines:
        # Look for test start lines: "[1/26] Running test: src/test.c"
        test_match = re.match(r'\[\d+/\d+\] Running test: (.+)', line)
        if test_match:
            current_test = test_match.group(1)
            continue

        # Look for result lines: "PASSED: src/test.c (took 4.110s)"
        result_match = re.match(
            r'(PASSED|FAILED): (.+) \(took ([\d.]+)s\)', line)
        if result_match:
            status, test_file, total_time = result_match.groups()
            if current_test:
                test_file = current_test

            times_data[test_file] = {
                'status': status,
                'total_time': float(total_time),
                'config_times': []
            }
            continue

        # Look for config timing lines: "test.c - all configs passed. (0.717s, 0.650s, 0.827s, 0.770s, 0.349s, 0.330s, 0.247s, 0.219s)"
        config_match = re.search(
            r'- all configs (?:passed|failed)\. \(([^)]+)\)', line)
        if config_match and current_test:
            times_str = config_match.group(1)

            # Parse individual times
            time_values = []
            for time_part in times_str.split(', '):
                time_match = re.match(r'([\d.]+)s', time_part.strip())
                if time_match:
                    time_values.append(float(time_match.group(1)))

            if current_test in times_data:
                times_data[current_test]['config_times'] = time_values

    return times_data


def run_benchmarks(symbolic=False):
    """Run the benchmark tests and return timing data."""
    cmd = [sys.executable, "tests/run-cn-test-gen.py",
           "--mode=benchmarking", "--build-tool=make"]
    if symbolic:
        cmd.append("-s")

    print(f"Running benchmarks with command: {' '.join(cmd)}")
    result = run_command(cmd, check=False)  # Don't fail on test failures

    if result.returncode not in [0, 1]:  # 0 = all passed, 1 = some failed
        print(
            f"Benchmark command failed unexpectedly with code {result.returncode}")
        print(f"stdout: {result.stdout}")
        print(f"stderr: {result.stderr}")
        return None

    return parse_benchmark_output(result.stdout)


def run_benchmarks_in_dir(directory, symbolic=False):
    """Run the benchmark tests in a specific directory and return timing data."""
    cmd = [sys.executable, "tests/run-cn-test-gen.py",
           "--mode=benchmarking", "--build-tool=make"]
    if symbolic:
        cmd.append("-s")

    print(f"Running benchmarks in {directory} with command: {' '.join(cmd)}")
    # Don't fail on test failures
    result = run_command(cmd, cwd=directory, check=False)

    if result.returncode not in [0, 1]:  # 0 = all passed, 1 = some failed
        print(
            f"Benchmark command failed unexpectedly with code {result.returncode}")
        print(f"stdout: {result.stdout}")
        print(f"stderr: {result.stderr}")
        return None

    return parse_benchmark_output(result.stdout)


def save_results(results, filename):
    """Save benchmark results to a JSON file."""
    with open(filename, 'w') as f:
        json.dump(results, f, indent=2)
    print(f"Results saved to {filename}")


def load_results(filename):
    """Load benchmark results from a JSON file."""
    if not os.path.exists(filename):
        return None
    with open(filename, 'r') as f:
        return json.load(f)


def perform_paired_ttest(current_times, upstream_times):
    """Perform paired t-test on config times."""
    if len(current_times) != len(upstream_times) or len(current_times) < 2:
        return None

    try:
        # Perform paired t-test
        statistic, p_value = stats.ttest_rel(current_times, upstream_times)

        # Calculate effect size (Cohen's d for paired samples)
        differences = np.array(current_times) - np.array(upstream_times)
        effect_size = np.mean(differences) / np.std(differences,
                                                    ddof=1) if np.std(differences, ddof=1) > 0 else 0

        return {
            'statistic': statistic,
            'p_value': p_value,
            'effect_size': effect_size,
            'n_configs': len(current_times),
            'mean_diff': np.mean(differences),
            'std_diff': np.std(differences, ddof=1)
        }
    except Exception as e:
        print(f"Error in t-test: {e}")
        return None


def compare_results(current_results, upstream_results):
    """Compare current results with upstream results."""
    if not current_results or not upstream_results:
        print("Cannot compare: missing results")
        return

    print("\n" + "="*80)
    print("PERFORMANCE COMPARISON")
    print("="*80)

    current_data = current_results.get('benchmark_data', {})
    upstream_data = upstream_results.get('benchmark_data', {})

    # Compare total times
    print(
        f"\n{'Test File':<40} {'Current':<10} {'Upstream':<10} {'Diff':<10} {'Change'}")
    print("-" * 80)

    total_current_time = 0
    total_upstream_time = 0

    for test_file in sorted(set(current_data.keys()) | set(upstream_data.keys())):
        current_time = current_data.get(test_file, {}).get('total_time', 0)
        upstream_time = upstream_data.get(test_file, {}).get('total_time', 0)

        total_current_time += current_time
        total_upstream_time += upstream_time

        if upstream_time > 0:
            diff = current_time - upstream_time
            percent_change = (diff / upstream_time) * 100
            change_str = f"{percent_change:+.1f}%"

            # Color coding for significant changes
            if abs(percent_change) > 10:
                if percent_change > 0:
                    change_str = f"🔴 {change_str}"  # Slower
                else:
                    change_str = f"🟢 {change_str}"  # Faster
        else:
            diff = current_time
            change_str = "NEW"

        print(
            f"{test_file:<40} {current_time:<10.3f} {upstream_time:<10.3f} {diff:<+10.3f} {change_str}")

    # Overall summary
    if total_upstream_time > 0:
        total_diff = total_current_time - total_upstream_time
        total_percent_change = (total_diff / total_upstream_time) * 100

        print("-" * 80)
        print(f"{'TOTAL':<40} {total_current_time:<10.3f} {total_upstream_time:<10.3f} {total_diff:<+10.3f} {total_percent_change:+.1f}%")

        if total_percent_change > 5:
            print(
                f"\n⚠️  PERFORMANCE REGRESSION: {total_percent_change:.1f}% slower than upstream")
        elif total_percent_change < -5:
            print(
                f"\n🎉 PERFORMANCE IMPROVEMENT: {abs(total_percent_change):.1f}% faster than upstream")
        else:
            print(
                f"\n✅ Performance is similar to upstream ({total_percent_change:+.1f}%)")

    # Statistical analysis of config times - analyze by configuration across all tests
    print("\n" + "="*80)
    print("STATISTICAL ANALYSIS (Per-Config Analysis)")
    print("="*80)

    # Reorganize data by config position instead of by test file
    # Determine the number of configs by looking at the config_times arrays
    max_configs = 0
    common_tests = set(current_data.keys()) & set(upstream_data.keys())

    for test_file in common_tests:
        current_config_times = current_data.get(
            test_file, {}).get('config_times', [])
        upstream_config_times = upstream_data.get(
            test_file, {}).get('config_times', [])
        if len(current_config_times) > 0 and len(upstream_config_times) > 0:
            max_configs = max(max_configs, len(
                current_config_times), len(upstream_config_times))

    if max_configs == 0:
        print("No config timing data available for statistical analysis")
    else:
        print(
            f"\n{'Config':<30} {'p-value':<10} {'Effect':<15} {'Significance'}")
        print("-" * 80)

        # Define config names based on the test runner configurations
        if 'symbolic' in current_results.get('mode', ''):
            config_names = [
                "symbolic+coverage+pruning-runtime+make",
                "symbolic+coverage+pruning-before-absint+make"
            ]
        else:
            config_names = [
                "coverage+quickcheck+make",
                "coverage+learning+wrapped_interval+slow_prune+make",
                "uniform+intervals+fast_prune+destruction+make",
                "learning+satisfaction+tyche_output+make"
            ]

        significant_configs = []

        # For each configuration position, collect times across all test files
        for config_idx in range(max_configs):
            current_config_times_across_tests = []
            upstream_config_times_across_tests = []

            for test_file in common_tests:
                current_config_times = current_data.get(
                    test_file, {}).get('config_times', [])
                upstream_config_times = upstream_data.get(
                    test_file, {}).get('config_times', [])

                # Only include if both have this config index
                if (len(current_config_times) > config_idx and
                        len(upstream_config_times) > config_idx):
                    current_config_times_across_tests.append(
                        current_config_times[config_idx])
                    upstream_config_times_across_tests.append(
                        upstream_config_times[config_idx])

            if len(current_config_times_across_tests) >= 2:  # Need at least 2 pairs
                ttest_result = perform_paired_ttest(
                    current_config_times_across_tests,
                    upstream_config_times_across_tests
                )

                # Use config name if available, otherwise use index
                config_name = config_names[config_idx] if config_idx < len(
                    config_names) else f"Config {config_idx}"

                if ttest_result:
                    p_val = ttest_result['p_value']
                    effect = ttest_result['effect_size']
                    mean_diff = ttest_result['mean_diff']
                    n_tests = len(current_config_times_across_tests)

                    # Interpret significance
                    if p_val < 0.001:
                        sig_str = "*** p<0.001"
                    elif p_val < 0.01:
                        sig_str = "** p<0.01"
                    elif p_val < 0.05:
                        sig_str = "* p<0.05"
                    else:
                        sig_str = "n.s."

                    # Interpret effect size
                    if abs(effect) < 0.2:
                        effect_str = f"{effect:+.2f} (small, n={n_tests})"
                    elif abs(effect) < 0.5:
                        effect_str = f"{effect:+.2f} (medium, n={n_tests})"
                    elif abs(effect) < 0.8:
                        effect_str = f"{effect:+.2f} (large, n={n_tests})"
                    else:
                        effect_str = f"{effect:+.2f} (very large, n={n_tests})"

                    # Color code significant results
                    if p_val < 0.05:
                        if mean_diff > 0:
                            # Significantly slower
                            sig_str = f"🔴 {sig_str}"
                        else:
                            # Significantly faster
                            sig_str = f"🟢 {sig_str}"
                        significant_configs.append(
                            (config_name, p_val, effect, mean_diff))

                    print(
                        f"{config_name:<30} {p_val:<10.4f} {effect_str:<15} {sig_str}")
                else:
                    print(
                        f"{config_name:<30} {'N/A':<10} {'N/A':<15} {'insufficient data'}")

        # Summary of statistical findings
        if significant_configs:
            print(f"\n📊 STATISTICAL SUMMARY:")
            print(
                f"Configs with significant differences (p < 0.05): {len(significant_configs)}")

            # mean_diff < 0
            faster_configs = [c for c in significant_configs if c[3] < 0]
            # mean_diff > 0
            slower_configs = [c for c in significant_configs if c[3] > 0]

            if faster_configs:
                print(
                    f"🟢 Significantly faster: {len(faster_configs)} configs")
            if slower_configs:
                print(
                    f"🔴 Significantly slower: {len(slower_configs)} configs")

            # Show most significant changes
            significant_configs.sort(key=lambda x: x[1])  # Sort by p-value
            print(f"\nMost significant config changes:")
            for config_name, p_val, effect, mean_diff in significant_configs[:3]:
                direction = "faster" if mean_diff < 0 else "slower"
                print(
                    f"  {config_name}: {direction} (p={p_val:.4f}, effect={effect:.2f})")
        else:
            print(f"\n📊 No statistically significant differences found (p < 0.05)")


def main():
    parser = argparse.ArgumentParser(
        description='Benchmark CN test performance against upstream/main')
    parser.add_argument('-s', '--symbolic', action='store_true',
                        help='Run symbolic tests only (SMT subset)')
    parser.add_argument('--no-upstream', action='store_true',
                        help='Skip upstream comparison (only run current version)')
    parser.add_argument('--results-dir', default='.',
                        help='Directory to store results (default: current directory)')

    args = parser.parse_args()

    # Ensure we're in the project root
    project_root = Path(__file__).parent.parent.parent
    os.chdir(project_root)

    print(f"Working directory: {os.getcwd()}")
    print(f"Results directory: {args.results_dir}")

    # Create results directory if it doesn't exist
    os.makedirs(args.results_dir, exist_ok=True)

    # Get current version info
    current_commit = get_current_commit()
    current_branch = get_current_branch()

    timestamp = datetime.now().isoformat()
    mode = "symbolic" if args.symbolic else "random"

    print(f"Current commit: {current_commit}")
    print(f"Current branch: {current_branch}")
    print(f"Benchmark mode: {mode}")

    # Run benchmarks on current version
    print("\n" + "="*60)
    print("RUNNING BENCHMARKS ON CURRENT VERSION")
    print("="*60)

    current_benchmark_data = run_benchmarks(args.symbolic)
    if not current_benchmark_data:
        print("Failed to run current version benchmarks")
        sys.exit(1)

    current_results = {
        'timestamp': timestamp,
        'commit': current_commit,
        'branch': current_branch,
        'mode': mode,
        'benchmark_data': current_benchmark_data
    }

    current_filename = os.path.join(
        args.results_dir, f'benchmark_current_{mode}_{timestamp[:19].replace(":", "-")}.json')
    save_results(current_results, current_filename)

    if args.no_upstream:
        print("Skipping upstream comparison")
        return

    # Clone upstream/main into a temporary directory
    print("\n" + "="*60)
    print("CLONING UPSTREAM/MAIN TO TEMPORARY DIRECTORY")
    print("="*60)

    with tempfile.TemporaryDirectory() as temp_dir:
        upstream_dir = os.path.join(temp_dir, "upstream_cn")

        # Get the remote URL for upstream
        try:
            upstream_url_result = run_command(
                ["git", "remote", "get-url", "upstream"])
            upstream_url = upstream_url_result.stdout.strip()
        except subprocess.CalledProcessError:
            print("No 'upstream' remote found, using 'origin'")
            upstream_url_result = run_command(
                ["git", "remote", "get-url", "origin"])
            upstream_url = upstream_url_result.stdout.strip()

        print(f"Cloning from: {upstream_url}")
        print(f"Clone directory: {upstream_dir}")

        # Clone the repository
        run_command(["git", "clone", upstream_url, upstream_dir])

        # Get upstream commit
        upstream_commit = run_command(
            ["git", "rev-parse", "HEAD"], cwd=upstream_dir).stdout.strip()
        print(f"Upstream commit: {upstream_commit}")

        # Build upstream version
        print("\nBuilding upstream version...")
        run_command(["make", "install"], cwd=upstream_dir)

        # Run benchmarks on upstream version
        print("\n" + "="*60)
        print("RUNNING BENCHMARKS ON UPSTREAM/MAIN")
        print("="*60)

        # We need to run the benchmark from the upstream directory
        upstream_benchmark_data = run_benchmarks_in_dir(
            upstream_dir, args.symbolic)
        if not upstream_benchmark_data:
            print("Failed to run upstream version benchmarks")
            return

        upstream_results = {
            'timestamp': timestamp,
            'commit': upstream_commit,
            'branch': 'upstream/main',
            'mode': mode,
            'benchmark_data': upstream_benchmark_data
        }

        upstream_filename = os.path.join(
            args.results_dir, f'benchmark_upstream_{mode}_{timestamp[:19].replace(":", "-")}.json')
        save_results(upstream_results, upstream_filename)

        # Compare results
        compare_results(current_results, upstream_results)

    # Reinstall current version after upstream benchmarks
    print("\n" + "="*60)
    print("REINSTALLING CURRENT VERSION")
    print("="*60)
    run_command(["make", "install"])
    print("Current version reinstalled successfully")

    print("\nTemporary directory cleaned up automatically")


if __name__ == "__main__":
    main()
