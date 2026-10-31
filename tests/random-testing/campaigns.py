"""Run migrated fixtures against a reviewed, report-backed outcome baseline."""

import json
import os
from pathlib import Path
import re


TEST_ROOT = Path(__file__).resolve().parent
REPO_ROOT = TEST_ROOT.parents[1]
ENGINES = ("bennet", "darcy")
UINT64_MAX = (1 << 64) - 1
ORACLE_COUNTERS = (
    "parse_rejected", "pre_passed", "pre_failed", "pre_unsupported",
    "pre_internal_error", "post_unsupported",
)
# A compiler/tool failure must never satisfy a known migration blocker.
SPEC_BLOCKERS = {
    ("generate", "driver.unrunnable", "spec"),
    ("convert", "driver.convert", "spec"),
}
# Counters a conversion failure reports as null because no campaign ran.
CONVERSION_NULL_FIELDS = (
    "engine", "seen", "selected", "dispatched", "crashes", "findings",
    "oracle", "inputs", "rejected", "rejections",
)


def require(condition, message):
    if not condition:
        raise RuntimeError(message)


def _unique_object(pairs):
    result = {}
    for key, value in pairs:
        if key in result:
            raise ValueError(f"duplicate JSON field: {key}")
        result[key] = value
    return result


def read_json(path):
    def invalid_constant(value):
        raise ValueError(f"invalid JSON constant: {value}")

    try:
        value = json.loads(
            path.read_text(encoding="utf-8"),
            object_pairs_hook=_unique_object,
            parse_constant=invalid_constant,
        )
    except (OSError, UnicodeError, ValueError) as error:
        raise RuntimeError(f"Cannot read JSON artifact {path}: {error}") from error
    require(type(value) is dict, f"Expected a JSON object: {path}")
    return value


def counter(obj, field):
    value = obj.get(field)
    require(
        type(value) is int and 0 <= value <= UINT64_MAX,
        f"{field} must be a nonnegative 64-bit integer",
    )
    return value


def _expect(obj, field, value):
    require(
        field in obj and type(obj[field]) is type(value) and obj[field] == value,
        f"{field}: expected {value!r}, got {obj.get(field)!r}",
    )


def load_manifest(path):
    manifest = read_json(path)
    _expect(manifest, "version", 1)
    duration = counter(manifest, "duration_seconds")
    require(duration > 0, "duration_seconds must be positive")
    counter(manifest, "seed")
    cases = manifest.get("cases")
    require(type(cases) is list and cases, "manifest cases must be a nonempty list")
    fixture_root = (path.parent / "fixtures/campaigns").resolve()
    ids, selections, sources = set(), set(), set()
    for case in cases:
        require(type(case) is dict, "each manifest case must be an object")
        case_id = case.get("id")
        require(
            type(case_id) is str and re.fullmatch(r"[A-Za-z0-9][A-Za-z0-9_.-]*", case_id),
            f"invalid campaign case id: {case_id!r}",
        )
        require(case_id not in ids, f"duplicate campaign case id: {case_id}")
        ids.add(case_id)
        name = case.get("function")
        require(
            type(name) is str and re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]*", name),
            f"{case_id}: invalid function name",
        )
        relative = case.get("source")
        require(type(relative) is str and relative, f"{case_id}: missing fixture source")
        source = (path.parent / relative).resolve()
        require(
            not Path(relative).is_absolute() and fixture_root in source.parents,
            f"{case_id}: fixture path escapes fixtures/campaigns",
        )
        require(source.is_file() and source.suffix == ".c", f"{case_id}: missing C fixture: {source}")
        selection = (source, name)
        require(selection not in selections, f"duplicate fixture/function selection: {relative}:{name}")
        selections.add(selection)
        sources.add(source)
        intent = case.get("intent")
        require(intent in ("pass", "finding"), f"{case_id}: invalid fixture intent")
        expected = case.get("expected")
        require(
            type(expected) is dict and set(expected) == set(ENGINES),
            f"{case_id}: expected outcomes must name both engines",
        )
        for engine, outcome in expected.items():
            require(type(outcome) is dict, f"{case_id}/{engine}: expected outcome must be an object")
            kind = outcome.get("outcome")
            require(kind in ("pass", "finding", "blocked", "inconclusive", "unsupported"),
                    f"{case_id}/{engine}: invalid expected outcome")
            allowed = {"outcome", "reason"}
            if kind == "blocked":
                allowed |= {"stage", "kind", "blame", "message"}
                signature = tuple(outcome.get(key) for key in ("stage", "kind", "blame"))
                require(all(type(value) is str for value in signature) and signature in SPEC_BLOCKERS,
                        f"{case_id}/{engine}: unsupported blocker signature")
                require(
                    type(outcome.get("message")) is str and outcome["message"],
                    f"{case_id}/{engine}: blocker message must be explicit",
                )
            require(not (set(outcome) - allowed), f"{case_id}/{engine}: unknown expectation fields")
            if kind != intent or "reason" in outcome:
                require(
                    type(outcome.get("reason")) is str and outcome["reason"].strip(),
                    f"{case_id}/{engine}: a differing expectation needs a reason",
                )
    inventory = {source.resolve() for source in fixture_root.rglob("*.c")}
    omitted = sorted(str(source) for source in inventory - sources)
    require(not omitted, "active fixtures omitted from manifest: " + ", ".join(omitted))
    return manifest


def _oracle_counts(report):
    oracle = report.get("oracle")
    require(type(oracle) is dict, "campaign report has no oracle counters")
    return {key: counter(oracle, key) for key in ORACLE_COUNTERS}


def validate_oracle(report, *, dispatched, selected):
    counts = _oracle_counts(report)
    require(not any(counts[key] for key in ("pre_unsupported", "pre_internal_error", "post_unsupported")),
            "campaign oracle is unsupported or has internal errors")
    require(dispatched <= counts["pre_passed"], "dispatch exceeds successful preconditions")
    require(sum(counts[key] for key in ("parse_rejected", "pre_passed", "pre_failed")) <= selected,
            "oracle counters exceed selected inputs")


def _blocked(report):
    _expect(report, "status", "error")
    error = report.get("error")
    require(type(error) is dict, "failed campaign report has no error object")
    signature = (report.get("stage"), error.get("kind"), error.get("blame"))
    require(signature in SPEC_BLOCKERS, f"infrastructure or unrecognized campaign error: {signature!r}")
    require(type(error.get("message")) is str and error["message"], "campaign error has no message")
    return {
        "outcome": "blocked", "stage": signature[0], "kind": signature[1],
        "blame": signature[2], "message": error["message"],
    }


def classify_report(report, status, *, fn, engine, duration, seed):
    """Classify executed campaigns and narrowly validated migration failures."""
    require(type(report) is dict, "campaign report must be an object")
    require(type(status) is int and status in (0, 1, 2), f"unexpected campaign exit status {status}")
    for field, value in (
        ("schema_version", 1), ("function", fn),
        ("budget", "seconds"), ("duration", duration), ("seed", seed),
        ("check_heap_ownership", True), ("effective_check_heap_ownership", True),
    ):
        _expect(report, field, value)
    if report.get("stage") == "convert":
        require(status == 2, "conversion blocker must exit with status 2")
        _expect(report, "exit", 2)
        blocked = _blocked(report)
        for field in CONVERSION_NULL_FIELDS:
            _expect(report, field, None)
        provenance = report.get("provenance")
        require(type(provenance) is dict, "conversion report has no provenance object")
        configuration = provenance.get("configuration")
        require(type(configuration) is dict, "conversion report has no configuration object")
        for field, value in (("engine", engine), ("duration_seconds", duration),
                             ("seed", seed), ("mode", "campaign"),
                             ("check_heap_ownership", True)):
            _expect(configuration, field, value)
        return blocked
    _expect(report, "engine", engine)
    seen, selected, dispatched = (counter(report, key) for key in ("seen", "selected", "dispatched"))
    require(dispatched <= selected <= seen, "inconsistent campaign dispatch counters")
    findings = report.get("findings")
    require(type(findings) is dict, "campaign report has no findings object")
    _expect(findings, "unit", "crashing-executions")
    confirmed, post_failed = counter(findings, "confirmed"), counter(findings, "post_failed")
    require("capped" in findings and (findings["capped"] is None or type(findings["capped"]) is bool),
            "findings.capped must be a boolean or null")
    require(counter(report, "crashes") == confirmed, "crashes and confirmed findings disagree")
    require(post_failed <= confirmed <= dispatched, "inconsistent campaign findings counters")
    if status == 2 and report.get("exit") == 0:
        _expect(report, "status", "ok")
        _expect(report, "stage", "complete")
        _expect(report, "error", None)
        require(dispatched == confirmed == post_failed == 0,
                "incomplete campaign has nonzero execution counters")
        rejections = report.get("rejections")
        require(type(rejections) is dict, "incomplete campaign has no rejection counters")
        for key in rejections:
            require(type(key) is str and key, "invalid campaign rejection name")
            counter(rejections, key)
        if report.get("oracle") is not None:
            counts = _oracle_counts(report)
            require(set(report["oracle"]) == set(ORACLE_COUNTERS),
                    "unsupported campaign has invalid oracle counters")
            require(counts["pre_unsupported"] > 0 and counts["pre_unsupported"] == selected,
                    "unsupported preconditions must account for every selected input")
            require(not any(value for key, value in counts.items() if key != "pre_unsupported"),
                    "unsupported campaign has mixed oracle results or internal errors")
            return {"outcome": "unsupported"}
        _expect(report, "oracle", None)
        require(seen == selected == 0, "inconclusive campaign has nonzero input counters")
        return {"outcome": "inconclusive"}
    _expect(report, "exit", status)
    if status == 2:
        blocked = _blocked(report)
        require(confirmed == 0, "unrunnable generator report has confirmed findings")
        if dispatched:
            validate_oracle(report, dispatched=dispatched, selected=selected)
        else:
            _expect(report, "oracle", None)
        return blocked
    _expect(report, "status", "ok")
    _expect(report, "stage", "complete")
    require("error" in report and report["error"] is None, "completed campaign has an error")
    require(dispatched > 0, "campaign dispatched zero inputs")
    validate_oracle(report, dispatched=dispatched, selected=selected)
    require(status == (1 if confirmed else 0), "process exit does not match confirmed findings")
    return {"outcome": "finding" if confirmed else "pass"}


def _write_summary(root, rows):
    (root / "summary.json").write_text(json.dumps(rows, indent=2) + "\n", encoding="utf-8")
    lines = ["| Case | Engine | Intent | Expected | Actual | Changed | Rejections |",
             "|---|---|---|---|---|---|---|"]
    for row in rows:
        lines.append("| " + " | ".join(str(row[key]).replace("|", "\\|").replace("\n", " ")
                                      for key in ("id", "engine", "intent", "expected", "actual", "changed", "rejections")) + " |")
    (root / "summary.md").write_text("\n".join(lines) + "\n", encoding="utf-8")


def check_campaigns(output_root, timeout, run_command, *, manifest_path=None, engine=None):
    """Run each selected function/engine once and fail on any baseline change."""
    manifest_path = Path(manifest_path) if manifest_path is not None else TEST_ROOT / "campaigns.json"
    manifest = load_manifest(manifest_path)
    require(engine is None or engine in ENGINES, f"unknown campaign engine: {engine}")
    root = output_root / "campaigns"
    root.mkdir()
    rows = []
    _write_summary(root, rows)
    status = run_command(["dune", "build", "bin/main.exe", "--profile=dev"], root / "build.log", timeout)
    require(status == 0, f"CN campaign build exited with status {status}")
    build_directory = Path(os.environ.get("DUNE_BUILD_DIR", "_build"))
    executable = (REPO_ROOT / build_directory / "default/bin/main.exe").resolve()
    duration, seed = manifest["duration_seconds"], manifest["seed"]
    changes = []
    for case in manifest["cases"]:
        for selected_engine in ((engine,) if engine else ENGINES):
            expected = case["expected"][selected_engine]
            output = root / case["id"] / selected_engine
            output.mkdir(parents=True)
            row = {
                "id": case["id"], "engine": selected_engine, "intent": case["intent"],
                "expected": expected["outcome"], "actual": "infrastructure-error", "changed": True,
                "rejections": None,
            }
            rows.append(row)
            try:
                status = run_command([
                    str(executable), "test", selected_engine,
                    "--only=" + case["function"], "--until-timeout=" + str(duration),
                    "--seed=" + str(seed), "--print-steps", "--output-dir", str(output),
                    str((manifest_path.parent / case["source"]).resolve()),
                ], output / "campaign.log", timeout)
                if status == 130:
                    raise KeyboardInterrupt
                require(status in (0, 1, 2), f"unexpected campaign process exit: {status}")
                report = read_json(output / "report.json")
                actual = classify_report(report, status, fn=case["function"], engine=selected_engine,
                                         duration=duration, seed=seed)
                row["actual"] = actual["outcome"]
                row["observed"] = actual
                row["dispatched"] = report["dispatched"]
                row["confirmed"] = report["findings"]["confirmed"] if report["findings"] else None
                row["rejections"] = report.get("rejections")
                fingerprint = {key: value for key, value in expected.items() if key != "reason"}
                row["changed"] = actual != fingerprint
                if row["changed"]:
                    changes.append(f"{case['id']}/{selected_engine}: expected {fingerprint!r}, got {actual!r}")
                print(f"{case['id']}/{selected_engine}: {actual['outcome']} "
                      f"(expected {expected['outcome']}; {report['dispatched']} dispatched)", flush=True)
            except (OSError, RuntimeError, KeyboardInterrupt) as error:
                row["error"] = str(error) or "interrupted"
                raise
            finally:
                _write_summary(root, rows)
    require(not changes, "Campaign outcome baseline changed:\n" + "\n".join(changes))
