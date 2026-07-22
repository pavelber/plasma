import argparse
import os
import sys
from collections import Counter, defaultdict


REQUIRED_FILES = ("IN1.INP", "EXCIT.INP", "SPECTR.INP", "RREC.INP", "BCFP.INP")


class Reporter:
    def __init__(self, max_examples):
        self.max_examples = max_examples
        self.errors = []
        self.warnings = []

    def error(self, message):
        self.errors.append(message)

    def warning(self, message):
        self.warnings.append(message)

    def print_group(self, title, items):
        print(title + ": " + str(len(items)))
        for item in items[:self.max_examples]:
            print("  - " + item)
        if len(items) > self.max_examples:
            print("  ... %d more" % (len(items) - self.max_examples))

    def print_summary(self):
        self.print_group("ERRORS", self.errors)
        self.print_group("WARNINGS", self.warnings)


def is_int(value):
    try:
        int(value)
        return True
    except ValueError:
        return False


def int_sort_key(value):
    if is_int(value):
        return 0, int(value)
    return 1, value


def transition_sort_key(values):
    return tuple(int_sort_key(value) for value in values)


def level_sort_key(value):
    if is_int(value):
        int_value = int(value)
        if int_value > 0:
            return 0, int_value
        if int_value == 0:
            return 1, 0
        return 2, -int_value
    return 3, value


def make_sort_key(key_types):
    converters = {
        "species": int_sort_key,
        "level": level_sort_key,
    }

    def sort_key(values):
        return tuple(converters[key_type](value) for key_type, value in zip(key_types, values))

    return sort_key


def parse_float(value):
    return float(value.replace("D", "E").replace("d", "e"))


def parse_in1(path, reporter):
    levels = defaultdict(set)
    header_counts = {}
    header_rows = {}
    sections = set()
    level_counts = defaultdict(lambda: {"regular": 0, "auto": 0})
    duplicates = []

    with open(path, encoding="utf-8") as infile:
        lines = infile.readlines()

    if len(lines) < 14:
        reporter.error("IN1.INP has fewer than 14 lines")
        return levels, header_counts

    current_sp = None
    in_sections = False
    for line_num, line in enumerate(lines[13:], start=14):
        parts = line.split()
        if not parts:
            continue

        if len(parts) == 1 and is_int(parts[0]):
            in_sections = True
            current_sp = parts[0]
            sections.add(current_sp)
            continue

        if not in_sections:
            if len(parts) >= 5 and is_int(parts[0]):
                sp_num = parts[0]
                try:
                    header_counts[sp_num] = {
                        "regular": int(parts[1]),
                        "auto": int(parts[2]),
                    }
                    header_rows[sp_num] = parts
                except ValueError:
                    reporter.error("IN1 header line %d has invalid counts: %s" % (line_num, line.rstrip()))
            continue

        if current_sp is None:
            reporter.error("IN1 level line %d appears before a section marker" % line_num)
            continue

        if "autoion" in line.lower():
            continue

        level = parts[-1]
        key = (current_sp, level)
        if level in levels[current_sp]:
            duplicates.append("%s/%s at IN1 line %d" % (current_sp, level, line_num))
        levels[current_sp].add(level)

        if is_int(level) and int(level) < 0:
            level_counts[current_sp]["auto"] += 1
        else:
            level_counts[current_sp]["regular"] += 1

    for sp_num in sorted(header_counts, key=int_sort_key):
        expected = header_counts[sp_num]
        if sp_num not in sections:
            if expected["regular"] == 1 and expected["auto"] == 0:
                levels[sp_num].add("1")
                level_counts[sp_num]["regular"] = 1
                reporter.warning(
                    "IN1 species %s has a header row but no section; treating level 1 as synthetic top-edge nucleus" %
                    sp_num
                )
            else:
                reporter.error("IN1 species %s has a header row but no level section" % sp_num)
            continue

        actual = level_counts[sp_num]
        if actual != expected:
            reporter.error(
                "IN1 species %s count mismatch: header regular/auto=%d/%d, actual=%d/%d" %
                (sp_num, expected["regular"], expected["auto"], actual["regular"], actual["auto"])
            )

    for sp_num in sorted(sections - set(header_counts), key=int_sort_key):
        reporter.error("IN1 species %s has a level section but no header row" % sp_num)

    for duplicate in duplicates:
        reporter.error("Duplicate IN1 level " + duplicate)

    return levels, header_counts


def check_level(levels, sp_num, level, context, reporter):
    if sp_num not in levels:
        reporter.error("%s references missing species %s" % (context, sp_num))
        return False
    if level not in levels[sp_num]:
        reporter.error("%s references missing level %s/%s" % (context, sp_num, level))
        return False
    return True


def read_transition_rows(path, min_parts):
    rows = []
    duplicates = Counter()
    with open(path, encoding="utf-8") as infile:
        for line_num, line in enumerate(infile, start=1):
            parts = line.split()
            if len(parts) < min_parts or not parts[0].lstrip("-").isdigit():
                continue
            rows.append((line_num, parts))
    return rows, duplicates


def report_order_errors(name, items, reporter, sort_key=transition_sort_key):
    for index in range(1, len(items)):
        previous_line, previous_key = items[index - 1]
        line_num, key = items[index]
        if sort_key(key) < sort_key(previous_key):
            reporter.error(
                "%s numeric order error at line %d: %s follows %s from line %d" %
                (name, line_num, key, previous_key, previous_line)
            )


def validate_in1_order(base_dir, reporter):
    path = os.path.join(base_dir, "IN1.INP")
    with open(path, encoding="utf-8") as infile:
        lines = infile.readlines()

    header_items = []
    section_items = []
    current_sp = None
    current_level_items = []
    in_sections = False

    def flush_levels():
        if current_sp is not None:
            report_order_errors(
                "IN1 levels for species " + current_sp,
                current_level_items,
                reporter,
                make_sort_key(("level",)),
            )

    for line_num, line in enumerate(lines[13:], start=14):
        parts = line.split()
        if not parts:
            continue

        if len(parts) == 1 and is_int(parts[0]):
            if not in_sections:
                report_order_errors("IN1 header species", header_items, reporter, make_sort_key(("species",)))
                in_sections = True
            flush_levels()
            current_sp = parts[0]
            current_level_items = []
            section_items.append((line_num, (current_sp,)))
            continue

        if not in_sections:
            if len(parts) >= 5 and is_int(parts[0]):
                header_items.append((line_num, (parts[0],)))
            continue

        if "autoion" in line.lower():
            continue
        if current_sp is not None:
            current_level_items.append((line_num, (parts[-1],)))

    if not in_sections:
        report_order_errors("IN1 header species", header_items, reporter, make_sort_key(("species",)))
    else:
        flush_levels()
        report_order_errors("IN1 section species", section_items, reporter, make_sort_key(("species",)))


def validate_transition_order(base_dir, filename, min_parts, key_indexes, key_types, reporter):
    path = os.path.join(base_dir, filename)
    rows, _ = read_transition_rows(path, min_parts)
    items = [
        (line_num, tuple(parts[index] for index in key_indexes))
        for line_num, parts in rows
    ]
    report_order_errors(filename, items, reporter, make_sort_key(key_types))


def validate_numeric_order(base_dir, reporter):
    validate_in1_order(base_dir, reporter)
    validate_transition_order(base_dir, "EXCIT.INP", 11, (0, 1, 2), ("species", "level", "level"), reporter)
    validate_transition_order(base_dir, "SPECTR.INP", 6, (0, 1, 2), ("species", "level", "level"), reporter)
    validate_transition_order(base_dir, "RREC.INP", 14, (0, 1, 2), ("species", "level", "level"), reporter)
    validate_transition_order(
        base_dir,
        "BCFP.INP",
        8,
        (0, 1, 2, 3),
        ("species", "level", "species", "level"),
        reporter,
    )


def validate_excit(base_dir, levels, reporter):
    path = os.path.join(base_dir, "EXCIT.INP")
    rows, _ = read_transition_rows(path, 11)
    keys = Counter()
    for line_num, parts in rows:
        sp_num, from_level, to_level = parts[0], parts[1], parts[2]
        key = (sp_num, from_level, to_level)
        keys[key] += 1
        context = "EXCIT line %d" % line_num
        check_level(levels, sp_num, from_level, context, reporter)
        check_level(levels, sp_num, to_level, context, reporter)
        for index in range(4, 11):
            try:
                parse_float(parts[index])
            except ValueError:
                reporter.error("%s has non-numeric value in column %d: %s" % (context, index + 1, parts[index]))

    report_duplicate_keys("EXCIT", keys, reporter)
    return len(rows)


def validate_spectr(base_dir, levels, reporter):
    path = os.path.join(base_dir, "SPECTR.INP")
    rows, _ = read_transition_rows(path, 6)
    keys = Counter()
    for line_num, parts in rows:
        sp_num, up_level, low_level = parts[0], parts[1], parts[2]
        key = (sp_num, up_level, low_level)
        keys[key] += 1
        context = "SPECTR line %d" % line_num
        check_level(levels, sp_num, up_level, context, reporter)
        check_level(levels, sp_num, low_level, context, reporter)
        for index in range(3, min(len(parts), 7)):
            try:
                parse_float(parts[index])
            except ValueError:
                reporter.error("%s has non-numeric value in column %d: %s" % (context, index + 1, parts[index]))

    report_duplicate_keys("SPECTR", keys, reporter)
    return len(rows)


def validate_rrec(base_dir, levels, reporter):
    path = os.path.join(base_dir, "RREC.INP")
    rows, _ = read_transition_rows(path, 14)
    keys = Counter()
    for line_num, parts in rows:
        sp_num, from_level, to_level = parts[0], parts[1], parts[2]
        target_sp = str(int(sp_num) + 1)
        key = (sp_num, from_level, to_level)
        keys[key] += 1
        context = "RREC line %d" % line_num
        check_level(levels, sp_num, from_level, context, reporter)
        check_level(levels, target_sp, to_level, context, reporter)
        for index in range(4, 14):
            try:
                parse_float(parts[index])
            except ValueError:
                reporter.error("%s has non-numeric value in column %d: %s" % (context, index + 1, parts[index]))

    report_duplicate_keys("RREC", keys, reporter)
    return len(rows)


def validate_bcfp(base_dir, levels, reporter):
    path = os.path.join(base_dir, "BCFP.INP")
    rows, _ = read_transition_rows(path, 8)
    keys = Counter()
    format_kinds = set()
    for line_num, parts in rows:
        from_sp, from_level, to_sp, to_level = parts[0], parts[1], parts[2], parts[3]
        key = (from_sp, from_level, to_sp, to_level)
        keys[key] += 1
        context = "BCFP line %d" % line_num
        check_level(levels, from_sp, from_level, context, reporter)
        check_level(levels, to_sp, to_level, context, reporter)
        for index in range(4, 8):
            try:
                parse_float(parts[index])
            except ValueError:
                reporter.error("%s has non-numeric value in column %d: %s" % (context, index + 1, parts[index]))

        if "coefficient" in open(path, encoding="utf-8").readline().lower():
            format_kinds.add("branching-ratio")
        else:
            format_kinds.add("fit-coefficients")

    if len(format_kinds) > 1:
        reporter.warning("BCFP appears to contain mixed formats: " + ", ".join(sorted(format_kinds)))

    report_duplicate_keys("BCFP", keys, reporter)
    return len(rows)


def report_duplicate_keys(name, keys, reporter):
    for key, count in keys.items():
        if count > 1:
            reporter.error("%s duplicate transition %s appears %d times" % (name, key, count))


def validate_database(base_dir, max_examples):
    reporter = Reporter(max_examples)

    for filename in REQUIRED_FILES:
        path = os.path.join(base_dir, filename)
        if not os.path.exists(path):
            reporter.error("Missing required file: " + path)

    if reporter.errors:
        reporter.print_summary()
        return 1

    levels, header_counts = parse_in1(os.path.join(base_dir, "IN1.INP"), reporter)

    stats = {
        "IN1 species": len(levels),
        "IN1 levels": sum(len(per_sp) for per_sp in levels.values()),
        "EXCIT transitions": validate_excit(base_dir, levels, reporter),
        "SPECTR transitions": validate_spectr(base_dir, levels, reporter),
        "RREC transitions": validate_rrec(base_dir, levels, reporter),
        "BCFP transitions": validate_bcfp(base_dir, levels, reporter),
    }
    validate_numeric_order(base_dir, reporter)

    print("Validated database: " + os.path.abspath(base_dir))
    for key, value in stats.items():
        print("%s: %s" % (key, value))
    print("Species: " + ", ".join(sorted(levels.keys(), key=int_sort_key)))
    reporter.print_summary()

    return 1 if reporter.errors else 0


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Validate NOMAD plasma database INP files.")
    parser.add_argument("database_dir", help="Directory containing IN1/EXCIT/SPECTR/RREC/BCFP INP files")
    parser.add_argument("--max-examples", type=int, default=25, help="Maximum errors/warnings to print")
    args = parser.parse_args()

    sys.exit(validate_database(args.database_dir, args.max_examples))
