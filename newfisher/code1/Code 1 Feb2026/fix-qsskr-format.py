from __future__ import annotations

import argparse
import re
import shutil
from dataclasses import dataclass
from pathlib import Path


DEFAULT_INPUT = "QSsKr.inp"
DEFAULT_BACKUP = "QSsKr-temp.inp"

NUMERIC_RE = re.compile(
    r"^[+-]?(?:\d+(?:\.\d*)?|\.\d+)(?:[EeDd][+-]?\d+)?$"
)
ORBITAL_RE = re.compile(r"^\d+[A-Za-z]+\d+$")


@dataclass
class ConvertStats:
    converted_level_lines: int = 0
    removed_separator_lines: int = 0


def is_numeric(token: str) -> bool:
    if ORBITAL_RE.match(token):
        return False
    return bool(NUMERIC_RE.match(token))


def to_float(token: str) -> float:
    return float(token.replace("D", "E").replace("d", "e"))


def to_int(token: str) -> int:
    return int(round(to_float(token)))


def split_newline(line: str) -> tuple[str, str]:
    if line.endswith("\r\n"):
        return line[:-2], "\r\n"
    if line.endswith("\n"):
        return line[:-1], "\n"
    return line, ""


def is_separator_line(body: str) -> bool:
    stripped = body.strip()
    return len(stripped) >= 3 and set(stripped) == {"-"}


def first_numeric_index(tokens: list[str]) -> int | None:
    for index, token in enumerate(tokens):
        if is_numeric(token):
            return index
    return None


def format_level_line(tokens: list[str]) -> str | None:
    number_index = first_numeric_index(tokens)
    if number_index is None or number_index == 0:
        return None

    electron_tokens = tokens[:number_index]
    if not electron_tokens or not all(ORBITAL_RE.match(token) for token in electron_tokens):
        return None

    numbers = tokens[number_index:]
    if len(numbers) < 4:
        return None

    name_tokens = electron_tokens[-2:]
    if len(name_tokens) == 1:
        name1, name2 = "", name_tokens[0]
    else:
        name1, name2 = name_tokens

    if len(name1) > 5 or len(name2) > 5:
        raise ValueError(f"electron label is longer than 5 columns: {name1!r} {name2!r}")

    g0 = to_int(numbers[0])
    energy = to_float(numbers[1])

    if len(numbers) >= 5 and "." in numbers[2]:
        uncertainty = to_float(numbers[2])
        local_index = to_int(numbers[3])
        global_index = to_int(numbers[4])
        tail = numbers[5:]
    else:
        uncertainty = 0.0
        local_index = to_int(numbers[2])
        global_index = to_int(numbers[3])
        tail = numbers[4:]

    formatted = (
        f"{name1:<5}{name2:<5}"
        f"{g0:3d}"
        f"{energy:11.3f}"
        f"{uncertainty:11.3f}"
        f"{local_index:5d}"
        f"{global_index:5d}"
    )

    if tail:
        formatted += "   " + " ".join(tail)

    return formatted.rstrip()


def convert_text(text: str) -> tuple[str, ConvertStats]:
    converted_lines: list[str] = []
    stats = ConvertStats()

    for line_number, line in enumerate(text.splitlines(keepends=True), start=1):
        body, newline = split_newline(line)
        if line_number > 2 and is_separator_line(body):
            print(f"Removed separator line {line_number}: {body}")
            stats.removed_separator_lines += 1
            continue

        formatted = format_level_line(body.split())
        if formatted is None:
            converted_lines.append(line)
        else:
            converted_lines.append(formatted + newline)
            stats.converted_level_lines += 1

    return "".join(converted_lines), stats


def convert_file(input_path: Path, backup_path: Path, dry_run: bool) -> int:
    source_path = backup_path if backup_path.exists() else input_path
    source_text = source_path.read_text()
    converted_text, stats = convert_text(source_text)

    if dry_run:
        print(f"Dry run source: {source_path.name}")
        print(f"Dry run: would convert {stats.converted_level_lines} level lines.")
        print(f"Dry run: would remove {stats.removed_separator_lines} separator lines.")
        return stats.converted_level_lines

    if backup_path.exists():
        print(f"Using existing backup {backup_path.name} as source; not overwriting it.")
    else:
        shutil.copy2(input_path, backup_path)
        print(f"Copied original {input_path.name} to {backup_path.name}.")

    input_path.write_text(converted_text)

    print(f"Converted {stats.converted_level_lines} level lines in {input_path.name}.")
    print(f"Removed {stats.removed_separator_lines} separator lines from {input_path.name}.")
    return stats.converted_level_lines


def main() -> None:
    parser = argparse.ArgumentParser(
        description=(
            "Back up QSsKr.inp and rewrite Kr level rows to the fixed-column "
            "two-electron-label format read by Code1_Feb2026.for."
        )
    )
    parser.add_argument(
        "input",
        nargs="?",
        default=DEFAULT_INPUT,
        help=f"input file to convert; default: {DEFAULT_INPUT}",
    )
    parser.add_argument(
        "--backup",
        default=DEFAULT_BACKUP,
        help=f"backup file to create before overwriting input; default: {DEFAULT_BACKUP}",
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="count converted rows without copying or overwriting files",
    )
    args = parser.parse_args()

    script_dir = Path(__file__).resolve().parent
    input_path = script_dir / args.input
    backup_path = script_dir / args.backup

    if not input_path.exists():
        raise FileNotFoundError(input_path)

    convert_file(input_path, backup_path, args.dry_run)


if __name__ == "__main__":
    main()
