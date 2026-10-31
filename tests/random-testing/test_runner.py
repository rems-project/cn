"""Regressions for the CI entry point; requires only Python's standard library."""

import contextlib
import functools
import io
import json
import os
from pathlib import Path
import signal
import sys
import tempfile
import threading
import time
import unittest
from unittest import mock


import run as runner


class RunnerTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="cn runner tests ")
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.stdout = io.StringIO()
        self.stderr = io.StringIO()
        self.enterContext(contextlib.redirect_stdout(self.stdout))
        self.enterContext(contextlib.redirect_stderr(self.stderr))

    def run_python(self, code, timeout=5):
        runner.run_command([sys.executable, "-c", code], self.root / "command.log", timeout)

    def kill_pid_file_on_cleanup(self, pid_file):
        def cleanup():
            if pid_file.exists():
                try:
                    os.kill(int(pid_file.read_text()), signal.SIGKILL)
                except ProcessLookupError:
                    pass

        self.addCleanup(cleanup)

    def wait_until_gone(self, pid, message):
        state = Path(f"/proc/{pid}/stat")
        # An orphan may briefly remain a zombie until the container's init reaps it.
        deadline = time.monotonic() + 2
        while state.exists():
            try:
                if state.read_text().split()[2] == "Z":
                    break
            except FileNotFoundError:
                break
            self.assertLess(time.monotonic(), deadline, message)
            time.sleep(0.01)

    def test_combines_output_and_preserves_arguments_with_spaces(self):
        runner.run_command(
            [sys.executable, "-c", "import sys; print(sys.argv[1]); print('stderr', file=sys.stderr)",
             "one argument with spaces; $(exit 1)"],
            self.root / "command.log",
        )
        log = (self.root / "command.log").read_text()
        self.assertIn("one argument with spaces; $(exit 1)", log)
        self.assertIn("stderr", log)
        self.assertIn(log, self.stdout.getvalue())

    def test_invalid_utf8_is_retained_as_diagnostics(self):
        self.run_python("import os; os.write(1, b'bad: \\xff\\n')")
        self.assertIn("bad: \ufffd", (self.root / "command.log").read_text())

    def test_nonzero_exit_cannot_be_hidden_by_a_success_message(self):
        with self.assertRaisesRegex(RuntimeError, "exited with status 2"):
            self.run_python("print('done: 0 inputs, 0 crashes'); raise SystemExit(2)")
        self.assertIn("done:", (self.root / "command.log").read_text())

    @unittest.skipUnless(os.name == "posix", "POSIX signal status")
    def test_signal_is_a_failure(self):
        with self.assertRaisesRegex(RuntimeError, f"exited with status {-signal.SIGTERM}"):
            self.run_python("import os, signal; os.kill(os.getpid(), signal.SIGTERM)")

    def test_missing_command_is_a_failure(self):
        with self.assertRaises(FileNotFoundError):
            runner.run_command([self.root / "nonexistent-tool"], self.root / "command.log")

    @unittest.skipUnless(sys.platform.startswith("linux"), "Linux process state inspection")
    def test_timeout_stops_descendants_even_if_parent_exited(self):
        ready = self.root / "descendant ready"

        self.kill_pid_file_on_cleanup(ready)
        child = (
            "import os, signal, time; from pathlib import Path; "
            "signal.signal(signal.SIGINT, signal.SIG_IGN); "
            "signal.signal(signal.SIGTERM, signal.SIG_IGN); "
            f"Path({str(ready)!r}).write_text(str(os.getpid())); "
            "print('child:' + str(os.getpid()), flush=True); time.sleep(60)"
        )
        code = f"import subprocess, sys; subprocess.Popen([sys.executable, '-c', {child!r}])"
        start = time.monotonic()
        with self.assertRaisesRegex(RuntimeError, "timed out after 2 seconds"):
            self.run_python(code, timeout=2)
        self.assertLess(time.monotonic() - start, 8)
        line = (self.root / "command.log").read_text().strip()
        self.assertTrue(line.startswith("child:"), line)
        pid = int(line.split(":")[1])
        self.wait_until_gone(pid, "descendant survived timeout cleanup")

    @unittest.skipUnless(sys.platform.startswith("linux"), "Linux process state inspection")
    def test_timeout_allows_cn_to_interrupt_and_reap_detached_child(self):
        ready = self.root / "detached child ready"
        interrupted = self.root / "detached child interrupted"
        child = (
            "import os, signal, time; from pathlib import Path\n"
            "def interrupt(signum, frame):\n"
            f"    Path({str(interrupted)!r}).write_text('interrupted')\n"
            "    raise SystemExit(0)\n"
            "signal.signal(signal.SIGINT, interrupt)\n"
            f"Path({str(ready)!r}).write_text(str(os.getpid()))\n"
            "time.sleep(60)\n"
        )
        parent = (
            "import signal, subprocess, sys, time\n"
            f"child = subprocess.Popen([sys.executable, '-c', {child!r}], start_new_session=True)\n"
            "def interrupt(signum, frame):\n"
            "    child.send_signal(signal.SIGINT)\n"
            "    child.wait()\n"
            "    raise SystemExit(130)\n"
            "signal.signal(signal.SIGINT, interrupt)\n"
            "time.sleep(60)\n"
        )
        with self.assertRaisesRegex(RuntimeError, "timed out"):
            self.run_python(parent, timeout=1)
        self.assertEqual(interrupted.read_text(), "interrupted")
        pid = int(ready.read_text())
        self.assertFalse(Path(f"/proc/{pid}").exists(), "detached child was not reaped")

    @unittest.skipUnless(sys.platform.startswith("linux"), "Linux process state inspection")
    def test_timeout_kills_detached_child_that_ignores_interrupt_and_term(self):
        ready = self.root / "ignoring detached child ready"
        child = (
            "import os, signal, time; from pathlib import Path; "
            "signal.signal(signal.SIGINT, signal.SIG_IGN); "
            "signal.signal(signal.SIGTERM, signal.SIG_IGN); "
            f"Path({str(ready)!r}).write_text(str(os.getpid())); "
            "time.sleep(60)"
        )
        parent = (
            "import signal, subprocess, sys, time; "
            "signal.signal(signal.SIGINT, signal.SIG_IGN); "
            f"subprocess.Popen([sys.executable, '-c', {child!r}], start_new_session=True); "
            "time.sleep(60)"
        )

        self.kill_pid_file_on_cleanup(ready)
        start = time.monotonic()
        with self.assertRaisesRegex(RuntimeError, "timed out"):
            self.run_python(parent, timeout=1)
        self.assertLess(time.monotonic() - start, 6)
        pid = int(ready.read_text())
        self.wait_until_gone(pid, "detached child survived cleanup")

    @unittest.skipUnless(sys.platform.startswith("linux"), "Linux process state inspection")
    def test_keyboard_interrupt_stops_the_running_command(self):
        ready = self.root / "interrupt ready"
        stop = threading.Event()

        def interrupt_when_ready():
            while not stop.wait(0.01):
                if ready.exists() and ready.stat().st_size > 0:
                    os.kill(os.getpid(), signal.SIGINT)
                    return

        interrupter = threading.Thread(target=interrupt_when_ready, daemon=True)
        interrupter.start()
        try:
            code = (
                "import os, signal, time; from pathlib import Path; "
                "signal.signal(signal.SIGTERM, signal.SIG_IGN); "
                f"Path({str(ready)!r}).write_text(str(os.getpid())); time.sleep(60)"
            )
            with self.assertRaises(KeyboardInterrupt):
                self.run_python(code)
        finally:
            stop.set()
            interrupter.join(timeout=1)
        pid = int(ready.read_text())
        state = Path(f"/proc/{pid}/stat")
        self.assertFalse(state.exists(), "interrupted command was not terminated and reaped")

    def test_timeout_still_applies_after_stdout_closes(self):
        with self.assertRaisesRegex(RuntimeError, "timed out"):
            self.run_python("import os, time; os.close(1); os.close(2); time.sleep(60)", timeout=0.1)

    def fake_ounit(self, command, log_path, timeout):
        self.assertEqual(command[0:2], ["dune", "test"])
        log_path.write_text("OUnit passed\n")

    def test_unit_only_retains_separate_successful_runs_without_austen(self):
        output = self.root / "CI artifacts"
        with mock.patch.object(runner, "run_command", side_effect=self.fake_ounit), \
             mock.patch.object(runner, "check_builds") as builds:
            for _ in range(2):
                self.assertEqual(runner.main(["--unit-only", "--output-dir", str(output)]), 0)
        builds.assert_not_called()
        runs = list(output.glob("run-*"))
        self.assertEqual(len(runs), 2)
        self.assertTrue(all((run / "ounit.log").read_text() == "OUnit passed\n" for run in runs))

    def test_builds_only_skips_ounit(self):
        with mock.patch.object(runner, "run_command") as command, \
             mock.patch.object(runner, "check_builds") as builds:
            self.assertEqual(runner.main(["--builds-only"]), 0)
        command.assert_not_called()
        builds.assert_called_once()

    def test_campaigns_only_uses_status_runner_and_skips_build_checks(self):
        with mock.patch.object(runner, "run_command") as command, \
             mock.patch.object(runner, "check_builds") as builds, \
             mock.patch.object(runner, "check_campaigns") as campaigns:
            self.assertEqual(runner.main(["--campaigns-only"]), 0)
        command.assert_not_called()
        builds.assert_not_called()
        campaigns.assert_called_once()
        self.assertIs(campaigns.call_args.args[2], runner.run_status)

    def test_austen_only_runs_builds_and_campaigns_without_ounit(self):
        with mock.patch.object(runner, "run_command") as command, \
             mock.patch.object(runner, "check_builds") as builds, \
             mock.patch.object(runner, "check_campaigns") as campaigns:
            self.assertEqual(runner.main(["--austen-only"]), 0)
        command.assert_not_called()
        builds.assert_called_once()
        campaigns.assert_called_once()

    def test_status_runner_preserves_finding_and_error_exits(self):
        for status in (1, 2):
            with self.subTest(status=status):
                actual = runner.run_status(
                    [sys.executable, "-c", f"raise SystemExit({status})"],
                    self.root / "status.log", 5,
                )
                self.assertEqual(actual, status)

    def test_status_runner_does_not_convert_timeouts_to_campaign_errors(self):
        with mock.patch.object(runner, "run_command", side_effect=RuntimeError("timed out")):
            with self.assertRaisesRegex(RuntimeError, "timed out"):
                runner.run_status(["cn"], self.root / "status.log", 5)

    def test_failure_retains_diagnostics_and_cannot_reuse_stale_output(self):
        output = self.root / "CI artifacts"
        stale = output / "run-stale"
        stale.mkdir(parents=True)
        (stale / "ounit.log").write_text("old success\n")

        def fail(command, log_path, timeout):
            log_path.write_text("current failure\n")
            raise RuntimeError("compiler failed")

        with mock.patch.object(runner, "run_command", side_effect=fail), \
             mock.patch.object(runner, "check_builds") as builds:
            self.assertEqual(runner.main(["--output-dir", str(output)]), 1)
        builds.assert_not_called()
        self.assertIn("current failure", self.stdout.getvalue())
        self.assertNotIn("old success", self.stdout.getvalue())
        self.assertEqual(len(list(output.glob("run-*"))), 2)

    def test_interruption_returns_130_and_retains_output(self):
        with mock.patch.object(runner, "run_command", side_effect=KeyboardInterrupt):
            self.assertEqual(runner.main(["--output-dir", str(self.root)]), 130)
        self.assertEqual(len(list(self.root.glob("run-*"))), 1)
        self.assertIn("interrupted", self.stderr.getvalue())

    def test_invalid_timeouts_are_rejected_before_running_anything(self):
        for value in ("0", "-1", "nan", "inf", "word"):
            with self.subTest(value=value), mock.patch.object(runner, "run_command") as command:
                with self.assertRaises(SystemExit) as error:
                    runner.main([f"--timeout={value}"])
                self.assertEqual(error.exception.code, 2)
                command.assert_not_called()

    def fake_build(self, mutation, command, log_path, timeout):
        if command[-1] == "--version":
            log_path.write_text("tool version\n")
            return
        engine = command[command.index("test") + 1]
        output = Path(command[command.index("--output-dir") + 1])
        work = output / "work/decrement"
        work.mkdir(parents=True, exist_ok=True)
        (output / "spec.json").write_text(json.dumps({"functions": [{"name": "decrement"}]}))
        (work / "prepared-harness.json").write_text(json.dumps({
            "version": 1, "engine": engine,
            "campaign": {"instrumented": True, "instrumentation": "compiled", "coverage_guards": 4},
            "evaluation": {"status": "ready"},
        }))
        (work / "harness").write_text("harness")
        (work / "harness").chmod(0o755)
        (work / "build.sh").write_text("clang -fsanitize-coverage=trace-pc-guard")
        log_path.write_text("done: 0 inputs, 0 crashes\n")
        if mutation:
            mutation(output, work, log_path)

    def check_synthetic_build(self, mutation=None):
        fake = functools.partial(self.fake_build, mutation)
        with mock.patch.object(runner, "run_command", side_effect=fake):
            runner.check_builds(self.root)

    def test_accepts_both_real_build_contracts(self):
        self.check_synthetic_build()
        self.assertIn("bennet: build-only checks passed", self.stdout.getvalue())
        self.assertIn("darcy: build-only checks passed", self.stdout.getvalue())

    def test_rejects_missing_and_malformed_artifacts(self):
        for name, contents in (("spec.json", "{"), ("prepared-harness.json", "[]")):
            with self.subTest(name=name):
                def corrupt(output, work, log, name=name, contents=contents):
                    (output if name == "spec.json" else work).joinpath(name).write_text(contents)
                with self.assertRaisesRegex(RuntimeError, "JSON"):
                    self.check_synthetic_build(corrupt)
        with self.assertRaisesRegex(RuntimeError, "Cannot read JSON artifact"):
            self.check_synthetic_build(lambda output, work, log: (work / "prepared-harness.json").unlink())

    def test_rejects_export_missing_the_selected_function(self):
        with self.assertRaisesRegex(RuntimeError, "does not include decrement"):
            self.check_synthetic_build(
                lambda output, work, log: (output / "spec.json").write_text('{"functions": []}')
            )

    def test_rejects_wrong_engine_or_uninstrumented_harness(self):
        for field, value in (("engine", "lucas"), ("campaign", {}), ("evaluation", {"status": "failed"})):
            with self.subTest(field=field):
                def corrupt(output, work, log, field=field, value=value):
                    path = work / "prepared-harness.json"
                    metadata = json.loads(path.read_text())
                    metadata[field] = value
                    path.write_text(json.dumps(metadata))
                with self.assertRaises(RuntimeError):
                    self.check_synthetic_build(corrupt)

    def test_rejects_nonexecutable_harness_and_accidental_campaign(self):
        for mutate in (
            lambda output, work, log: (work / "harness").chmod(0o644),
            lambda output, work, log: log.write_text("done: 1 inputs, 0 crashes\n"),
        ):
            with self.subTest(mutation=mutate), self.assertRaises(RuntimeError):
                self.check_synthetic_build(mutate)


if __name__ == "__main__":
    unittest.main()
