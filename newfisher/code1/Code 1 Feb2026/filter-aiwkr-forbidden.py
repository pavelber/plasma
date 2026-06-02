from __future__ import annotations

import argparse
import shutil
from collections import Counter
from dataclasses import dataclass
from pathlib import Path


NFAI = 201
DEFAULT_QSS = "QSsKr.inp"
DEFAULT_AIW = "AIwKr.inp"
DEFAULT_BACKUP = "AIwKr-temp.inp"


@dataclass
class StageInfo:
    ss: int
    n_qs: int
    n_ai: int
    pi: float
    qs_start: int = 0
    ai_start: int | None = None


@dataclass
class FilterResult:
    kept_lines: list[str]
    removed_lines: list[tuple[int, str, str]]
    reason_counts: Counter[str]
    added_stop_marker: bool


def parse_qss_summary(qss_path: Path) -> dict[int, StageInfo]:
    stages: dict[int, StageInfo] = {}

    with qss_path.open() as file:
        lines = file.readlines()

    for line in lines[2:]:
        tokens = line.split()
        if len(tokens) < 4:
            continue
        try:
            ss = int(tokens[0])
            n_qs = int(tokens[1])
            n_ai = int(tokens[2])
            pi = float(tokens[3].replace("D", "E").replace("d", "e"))
        except ValueError:
            break
        stages[ss] = StageInfo(ss=ss, n_qs=n_qs, n_ai=n_ai, pi=pi)

    if not stages:
        raise ValueError(f"Could not parse stage summary from {qss_path}")

    qs_start = 1
    for ss in sorted(stages):
        stages[ss].qs_start = qs_start
        qs_start += stages[ss].n_qs

    nnu = qs_start
    ai_start = nnu + 1
    for ss in sorted(stages):
        if stages[ss].n_ai > 0:
            stages[ss].ai_start = ai_start
            ai_start += stages[ss].n_ai

    return stages


def parse_aiw_data_line(line: str) -> tuple[int, int, int, int, float, float] | None:
    tokens = line.split()
    if len(tokens) < 6:
        return None
    try:
        return (
            int(tokens[0]),
            int(tokens[1]),
            int(tokens[2]),
            int(tokens[3]),
            float(tokens[4].replace("D", "E").replace("d", "e")),
            float(tokens[5].replace("D", "E").replace("d", "e")),
        )
    except ValueError:
        return None


def forbidden_reason(
    record: tuple[int, int, int, int, float, float],
    stages: dict[int, StageInfo],
) -> str | None:
    iss1, iel1, iss2, iel2, _aiw, _tren = record

    if iss1 == 111:
        return None

    initial_stage = stages.get(iss1)
    final_stage = stages.get(iss2)

    if initial_stage is None:
        return "unknown initial ion stage"
    if final_stage is None:
        return "unknown final ion stage"

    if iel1 < NFAI:
        return "initial level is not AI"
    if iel2 >= NFAI:
        return "final level is AI"
    if iel2 < 1:
        return "final level is not positive"

    if initial_stage.n_ai <= 0 or initial_stage.ai_start is None:
        return "initial ion stage has no AI levels"
    if iel1 > NFAI + initial_stage.n_ai - 1:
        return "initial AI level is out of range"

    if iel2 > final_stage.n_qs:
        return "final non-AI level is out of range"

    if iss2 != iss1 + 1:
        return "final ion stage is not initial stage plus one"

    return None


def filter_aiw(aiw_path: Path, stages: dict[int, StageInfo]) -> FilterResult:
    kept_lines: list[str] = []
    removed_lines: list[tuple[int, str, str]] = []
    reason_counts: Counter[str] = Counter()
    has_stop_marker = False

    with aiw_path.open() as file:
        lines = file.readlines()

    for line_number, line in enumerate(lines, start=1):
        record = parse_aiw_data_line(line)
        if record is None:
            kept_lines.append(line)
            continue

        if record[0] == 111:
            has_stop_marker = True

        reason = forbidden_reason(record, stages)
        if reason is None:
            kept_lines.append(line)
            continue

        removed_lines.append((line_number, reason, line.rstrip("\r\n")))
        reason_counts[reason] += 1

    added_stop_marker = False
    if not has_stop_marker:
        if kept_lines and kept_lines[-1].strip():
            kept_lines.append("\n")
        kept_lines.append("  111  201   32    1    0.0000E+00        0.000\n")
        added_stop_marker = True

    return FilterResult(
        kept_lines=kept_lines,
        removed_lines=removed_lines,
        reason_counts=reason_counts,
        added_stop_marker=added_stop_marker,
    )


def write_removed_log(aiw_path: Path, removed_lines: list[tuple[int, str, str]]) -> Path:
    log_path = aiw_path.with_name(aiw_path.stem + "-removed-forbidden.log")
    with log_path.open("w") as file:
        for line_number, reason, line in removed_lines:
            file.write(f"line {line_number}: {reason}: {line}\n")
    return log_path


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Remove AIwKr transitions forbidden by the Fortran AI reader."
    )
    parser.add_argument("--qss", default=DEFAULT_QSS, help=f"default: {DEFAULT_QSS}")
    parser.add_argument("--aiw", default=DEFAULT_AIW, help=f"default: {DEFAULT_AIW}")
    parser.add_argument(
        "--backup",
        default=DEFAULT_BACKUP,
        help=f"backup created before overwrite; default: {DEFAULT_BACKUP}",
    )
    parser.add_argument(
        "--apply",
        action="store_true",
        help="overwrite AIwKr.inp; without this flag only prints a dry-run report",
    )
    args = parser.parse_args()

    script_dir = Path(__file__).resolve().parent
    qss_path = script_dir / args.qss
    aiw_path = script_dir / args.aiw
    backup_path = script_dir / args.backup

    stages = parse_qss_summary(qss_path)
    result = filter_aiw(aiw_path, stages)

    print(f"Scanned {aiw_path.name}.")
    print(f"Forbidden transitions found: {len(result.removed_lines)}")
    for reason, count in result.reason_counts.most_common():
        print(f"  {count:6d}  {reason}")
    if result.added_stop_marker:
        print("Will add AIw stop marker line with iSS1=111.")
    else:
        print("AIw stop marker line already exists.")

    if result.removed_lines:
        log_path = write_removed_log(aiw_path, result.removed_lines)
        print(f"Removed-lines log: {log_path.name}")

    if not args.apply:
        print("Dry run only. Use --apply to overwrite the AIw file.")
        return

    if not backup_path.exists():
        shutil.copy2(aiw_path, backup_path)
        print(f"Copied original {aiw_path.name} to {backup_path.name}.")
    else:
        print(f"Backup already exists: {backup_path.name}; not overwriting it.")

    aiw_path.write_text("".join(result.kept_lines))
    print(f"Overwrote {aiw_path.name} with forbidden transitions removed.")


if __name__ == "__main__":
    main()
