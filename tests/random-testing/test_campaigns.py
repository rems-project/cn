"""Synthetic report and baseline regressions; no Austen installation required."""

import contextlib
import copy
import io
import json
from pathlib import Path
import tempfile
import unittest
from unittest import mock

import campaigns


BLOCKER = {
    "outcome": "blocked", "stage": "generate", "kind": "driver.unrunnable", "blame": "spec",
    "message": "the generator is structurally unrunnable: no scalar width for base type Integer",
    "reason": "Austen 0.0.1 cannot generate unbounded CN Integer arguments.",
}


def successful_report(engine="bennet", confirmed=0):
    return {
        "schema_version": 1, "function": "target", "engine": engine,
        "budget": "seconds", "duration": 1, "seed": 0,
        "check_heap_ownership": True, "effective_check_heap_ownership": True,
        "status": "ok", "stage": "complete", "error": None,
        "exit": 1 if confirmed else 0, "seen": 5, "selected": 5, "dispatched": 3,
        "crashes": confirmed,
        "findings": {"unit": "crashing-executions", "confirmed": confirmed,
                     "post_failed": confirmed, "capped": False},
        "oracle": {"parse_rejected": 1, "pre_passed": 3, "pre_failed": 1,
                   "pre_unsupported": 0, "pre_internal_error": 0, "post_unsupported": 0},
    }


def blocked_report(engine="bennet"):
    report = successful_report(engine)
    report.update(status="error", stage="generate", exit=2, seen=0, selected=0, dispatched=0,
                  oracle=None, error={key: BLOCKER[key] for key in ("kind", "blame", "message")})
    return report


class CampaignTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="cn campaign tests ")
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.fixture = self.root / "fixtures/campaigns/fixture.pass.c"
        self.fixture.parent.mkdir(parents=True)
        self.fixture.write_text("int target(void) { return 0; }\n")
        self.manifest = self.root / "campaigns.json"
        self.contents = {
            "version": 1, "duration_seconds": 1, "seed": 0,
            "cases": [{"id": "fixture", "source": "fixtures/campaigns/fixture.pass.c",
                       "function": "target", "intent": "pass",
                       "expected": {engine: {"outcome": "pass"} for engine in campaigns.ENGINES}}],
        }
        self.manifest.write_text(json.dumps(self.contents))
        self.output = self.root / "output"
        self.output.mkdir()
        self.stdout = io.StringIO()
        self.enterContext(contextlib.redirect_stdout(self.stdout))
        self.commands = []

    def write_manifest(self):
        self.manifest.write_text(json.dumps(self.contents))

    def expect(self, outcome, *, intent="pass"):
        self.contents["cases"][0]["intent"] = intent
        self.contents["cases"][0]["expected"] = {engine: copy.deepcopy(outcome) for engine in campaigns.ENGINES}
        self.write_manifest()

    def fake_command(self, reports=None, *, status=None, contents=None):
        def run(command, log_path, timeout):
            self.commands.append(command)
            log_path.write_text("command output\n")
            if command[0] == "dune":
                return 0
            engine = command[2]
            if reports is not None:
                report = reports(engine)
                (log_path.parent / "report.json").write_text(json.dumps(report))
                return report["exit"] if status is None else status
            if contents is not None:
                (log_path.parent / "report.json").write_text(contents)
            return 0 if status is None else status
        return run

    def run_campaigns(self, run, **kwargs):
        return campaigns.check_campaigns(self.output, 30, run, manifest_path=self.manifest, **kwargs)

    def summary(self):
        return json.loads((self.output / "campaigns/summary.json").read_text())

    def classify(self, report, status=None):
        return campaigns.classify_report(report, report["exit"] if status is None else status,
                                         fn="target", engine="bennet", duration=1, seed=0)

    def test_runs_both_engines_with_explicit_seed_and_budget(self):
        self.run_campaigns(self.fake_command(successful_report))
        self.assertEqual(len(self.commands), 3)
        self.assertEqual(self.commands[0], ["dune", "build", "bin/main.exe", "--profile=dev"])
        for command in self.commands[1:]:
            self.assertIn("--seed=0", command)
            self.assertIn("--until-timeout=1", command)
            self.assertIn("--only=target", command)
            self.assertEqual(command[-1], str(self.fixture))
        self.assertEqual([row["actual"] for row in self.summary()], ["pass", "pass"])
        self.assertTrue(all(not row["changed"] for row in self.summary()))
        self.assertTrue((self.output / "campaigns/summary.md").is_file())

    def test_engine_filter_keeps_both_expected_engines_in_manifest(self):
        self.run_campaigns(self.fake_command(successful_report), engine="darcy")
        self.assertEqual(len(self.commands), 2)
        self.assertEqual(self.summary()[0]["engine"], "darcy")

    def test_runs_the_binary_in_the_selected_dune_build_directory(self):
        build = self.root / "isolated build"
        with mock.patch.dict(campaigns.os.environ, {"DUNE_BUILD_DIR": str(build)}):
            self.run_campaigns(self.fake_command(successful_report))
        self.assertEqual(self.commands[1][0], str(build / "default/bin/main.exe"))

    def test_accepts_report_backed_finding(self):
        self.expect({"outcome": "finding"}, intent="finding")
        self.run_campaigns(self.fake_command(lambda engine: successful_report(engine, 1)))
        self.assertTrue(all(row["confirmed"] == 1 for row in self.summary()))

    def test_accepts_only_exact_reviewed_blocker(self):
        self.expect(BLOCKER)
        self.run_campaigns(self.fake_command(blocked_report))
        self.assertTrue(all(row["actual"] == "blocked" and not row["changed"] for row in self.summary()))

    def test_unexpected_improvement_is_failure_and_preserves_summary(self):
        self.expect(BLOCKER)
        with self.assertRaisesRegex(RuntimeError, "baseline changed"):
            self.run_campaigns(self.fake_command(successful_report))
        self.assertEqual(len(self.summary()), 2)
        self.assertTrue(all(row["changed"] for row in self.summary()))
        self.assertTrue(all(row["actual"] == "pass" for row in self.summary()))

    def test_pass_to_finding_regression_is_failure(self):
        with self.assertRaisesRegex(RuntimeError, "baseline changed"):
            self.run_campaigns(self.fake_command(lambda engine: successful_report(engine, 1)))
        self.assertTrue(all(row["actual"] == "finding" for row in self.summary()))

    def test_different_blocker_message_is_failure(self):
        self.expect(BLOCKER)
        def changed(engine):
            report = blocked_report(engine)
            report["error"]["message"] = "a different unsupported type"
            return report
        with self.assertRaisesRegex(RuntimeError, "baseline changed"):
            self.run_campaigns(self.fake_command(changed))

    def test_missing_report_never_satisfies_blocked_expectation(self):
        self.expect(BLOCKER)
        with self.assertRaisesRegex(RuntimeError, "Cannot read JSON artifact"):
            self.run_campaigns(self.fake_command(status=2))
        self.assertEqual(self.summary()[0]["actual"], "infrastructure-error")
        self.assertEqual(len(self.commands), 2)

    def test_malformed_report_never_satisfies_success(self):
        with self.assertRaisesRegex(RuntimeError, "Cannot read JSON artifact"):
            self.run_campaigns(self.fake_command(contents="{broken"))
        self.assertIn("error", self.summary()[0])

    def test_compiler_error_is_infrastructure_even_with_report(self):
        self.expect(BLOCKER)
        def failed(engine):
            report = blocked_report(engine)
            report.update(stage="compile-target")
            report["error"].update(kind="build.cc", blame="environment", message="clang failed")
            return report
        with self.assertRaisesRegex(RuntimeError, "infrastructure"):
            self.run_campaigns(self.fake_command(failed))
        self.assertEqual(len(self.commands), 2)

    def test_signal_and_unexpected_exit_are_infrastructure(self):
        for status in (-15, -2, 3):
            with self.subTest(status=status):
                with tempfile.TemporaryDirectory(dir=self.root) as output:
                    self.output = Path(output)
                    with self.assertRaisesRegex(RuntimeError, "unexpected campaign"):
                        self.run_campaigns(self.fake_command(status=status))

    def test_interruption_remains_interruption(self):
        with self.assertRaises(KeyboardInterrupt):
            self.run_campaigns(self.fake_command(status=130))
        self.assertEqual(self.summary()[0]["error"], "interrupted")

    def test_build_failure_aborts_before_campaigns(self):
        def failed(command, log_path, timeout):
            self.commands.append(command)
            return 2
        with self.assertRaisesRegex(RuntimeError, "CN campaign build"):
            self.run_campaigns(failed)
        self.assertEqual(len(self.commands), 1)

    def test_report_contract_rejects_inconclusive_and_mismatched_runs(self):
        mutations = [
            ("wrong function", lambda report: report.update(function="other")),
            ("wrong engine", lambda report: report.update(engine="darcy")),
            ("unknown schema", lambda report: report.update(schema_version=2)),
            ("bool schema", lambda report: report.update(schema_version=True)),
            ("wrong seed", lambda report: report.update(seed=1)),
            ("wrong budget", lambda report: report.update(budget="unbounded")),
            ("wrong duration", lambda report: report.update(duration=10)),
            ("disabled ownership", lambda report: report.update(effective_check_heap_ownership=False)),
            ("zero inputs", lambda report: report.update(dispatched=0)),
            ("negative inputs", lambda report: report.update(dispatched=-1)),
            ("bool inputs", lambda report: report.update(dispatched=True)),
            ("float inputs", lambda report: report.update(dispatched=3.0)),
            ("inconsistent tally", lambda report: report.update(selected=1)),
            ("missing oracle", lambda report: report.update(oracle=None)),
            ("no pre pass", lambda report: report["oracle"].update(pre_passed=0)),
            ("unsupported pre", lambda report: report["oracle"].update(pre_unsupported=1)),
            ("unsupported post", lambda report: report["oracle"].update(post_unsupported=1)),
            ("internal oracle error", lambda report: report["oracle"].update(pre_internal_error=1)),
            ("oracle tally", lambda report: report["oracle"].update(pre_failed=100)),
            ("bad crash count", lambda report: report.update(crashes=1)),
            ("no findings", lambda report: report.update(findings=None)),
            ("wrong exit", lambda report: report.update(exit=1)),
            ("wrong stage", lambda report: report.update(stage="generate")),
            ("error object", lambda report: report.update(error={})),
        ]
        for name, mutate in mutations:
            with self.subTest(name=name):
                report = successful_report()
                mutate(report)
                with self.assertRaises(RuntimeError):
                    self.classify(report)

    def test_unbacked_finding_exit_is_rejected(self):
        report = successful_report()
        report["exit"] = 1
        with self.assertRaisesRegex(RuntimeError, "confirmed findings"):
            self.classify(report)

    def test_json_duplicate_fields_and_nonstandard_numbers_are_rejected(self):
        for contents in ('{"schema_version":1,"schema_version":1}', '{"number":NaN}'):
            self.manifest.write_text(contents)
            with self.assertRaisesRegex(RuntimeError, "Cannot read JSON artifact"):
                campaigns.read_json(self.manifest)

    def test_inventory_and_manifest_validation(self):
        original = copy.deepcopy(self.contents)
        mutations = [
            ("version", lambda contents: contents.update(version=2)),
            ("duration", lambda contents: contents.update(duration_seconds=0)),
            ("seed", lambda contents: contents.update(seed=True)),
            ("empty", lambda contents: contents.update(cases=[])),
            ("duplicate id", lambda contents: contents["cases"].append(copy.deepcopy(contents["cases"][0]))),
            ("duplicate selection", lambda contents: contents["cases"].append(dict(contents["cases"][0], id="other"))),
            ("path escape", lambda contents: contents["cases"][0].update(source="../elsewhere.c")),
            ("missing file", lambda contents: contents["cases"][0].update(source="fixtures/campaigns/missing.c")),
            ("missing engine", lambda contents: contents["cases"][0]["expected"].pop("darcy")),
            ("bad intent", lambda contents: contents["cases"][0].update(intent="skip")),
            ("unknown outcome", lambda contents: contents["cases"][0]["expected"]["bennet"].update(outcome="skip")),
            ("no differing reason", lambda contents: contents["cases"][0]["expected"]["bennet"].update(outcome="finding")),
            ("arbitrary blocker", lambda contents: contents["cases"][0]["expected"].update(bennet={
                "outcome": "blocked", "stage": "compile-target", "kind": "build.cc", "blame": "environment",
                "message": "compiler failed", "reason": "not supported",
            })),
        ]
        for name, mutate in mutations:
            with self.subTest(name=name):
                self.contents = copy.deepcopy(original)
                mutate(self.contents)
                self.write_manifest()
                with self.assertRaises(RuntimeError):
                    campaigns.load_manifest(self.manifest)
        self.contents = original
        self.write_manifest()
        (self.fixture.parent / "unlisted.pass.c").write_text("int unlisted(void) { return 0; }\n")
        with self.assertRaisesRegex(RuntimeError, "omitted from manifest"):
            campaigns.load_manifest(self.manifest)


if __name__ == "__main__":
    unittest.main()
