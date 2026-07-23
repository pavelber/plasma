import argparse
import copy
import csv
import os
import tempfile

from lib.bcfp import BCFP
from lib.excit import EXCIT
from lib.in1 import IN1
from lib.merge_checks import run_post_merge_checks
from lib.rrec import RREC
from lib.spectr import SPECTR


DEFAULT_BOUNDARY_SP = "6"
DEFAULT_TARGET_ENERGY_COLUMN = "Peter (NIST) energy"


def _int_sort(values):
    return sorted(values, key=lambda value: int(value))


def _pick_column(fieldnames, requested, candidates):
    if requested in fieldnames:
        return requested

    normalized = {field: field.lower().replace(" ", "") for field in fieldnames}
    for field, normalized_field in normalized.items():
        if all(candidate in normalized_field for candidate in candidates):
            return field

    raise ValueError(
        "Cannot find CSV column. Requested '%s', available columns: %s" %
        (requested, ", ".join(fieldnames))
    )


def read_level_map(csv_path, fac_column, target_column):
    with open(csv_path, newline='', encoding='utf-8-sig') as csv_file:
        reader = csv.DictReader(csv_file)
        if reader.fieldnames is None:
            raise ValueError("CSV file has no header: " + csv_path)

        fac_column = _pick_column(reader.fieldnames, fac_column, ["fac"])
        target_column = _pick_column(reader.fieldnames, target_column, ["nist"])

        level_map = {}
        for row in reader:
            fac_level = row[fac_column].strip()
            target_level = row[target_column].strip()
            if not fac_level or not target_level:
                continue
            level_map[fac_level] = target_level

    if not level_map:
        raise ValueError("CSV mapping is empty: " + csv_path)
    return level_map


def read_target_energy_map(csv_path, target_column, target_energy_column):
    with open(csv_path, newline='', encoding='utf-8-sig') as csv_file:
        reader = csv.DictReader(csv_file)
        if reader.fieldnames is None:
            raise ValueError("CSV file has no header: " + csv_path)

        target_column = _pick_column(reader.fieldnames, target_column, ["nist"])
        target_energy_column = _pick_column(reader.fieldnames, target_energy_column, ["peter", "energy"])

        energy_map = {}
        for row in reader:
            target_level = row[target_column].strip()
            target_energy = row[target_energy_column].strip()
            if not target_level or not target_energy:
                continue
            energy_map[target_level] = float(target_energy)

    if not energy_map:
        raise ValueError("CSV energy mapping is empty: " + csv_path)
    return energy_map


def _in1_section_numbers(lines):
    section_numbers = set()
    in_sections = False
    for line in lines[13:]:
        parts = line.split()
        if len(parts) == 1:
            in_sections = True
            section_numbers.add(parts[0])
        elif in_sections and "Nucleus" in line:
            continue
    return section_numbers


def _read_in1(path):
    lines = open(path, encoding='utf-8').readlines()
    section_numbers = _in1_section_numbers(lines)
    sanitized_lines = lines[:13]

    for line in lines[13:]:
        parts = line.split()
        # Some database-built IN1 files keep a synthetic final-species header row
        # (for example Ti has a 23/1 nucleus row) without a matching levels section.
        # The merge only needs species that have sections, so drop header-only rows locally.
        if len(parts) > 1 and parts[0].isdigit() and parts[0] not in section_numbers:
            continue
        sanitized_lines.append(line)

    with tempfile.NamedTemporaryFile('w', delete=False, encoding='utf-8', suffix='.INP') as temp_file:
        temp_file.writelines(sanitized_lines)
        temp_path = temp_file.name

    try:
        return IN1(temp_path)
    finally:
        os.remove(temp_path)


def _clear_in1_sp(in1_data, sp_num):
    for level in in1_data._levels_per_sp_num.get(sp_num, []):
        key = (sp_num, level)
        in1_data._energy_table.pop(key, None)
        in1_data._configs.pop(key, None)
        in1_data._stat_weight.pop(key, None)
    in1_data._levels_per_sp_num.pop(sp_num, None)
    in1_data._branching_ratio.pop(sp_num, None)
    in1_data._ionization_potential.pop(sp_num, None)
    in1_data._header_fifth_column.pop(sp_num, None)


def _copy_in1_sp(in1_data, source_in1, sp_num):
    _clear_in1_sp(in1_data, sp_num)

    in1_data._ionization_potential[sp_num] = source_in1._ionization_potential[sp_num]
    in1_data._header_fifth_column[sp_num] = source_in1._header_fifth_column[sp_num]
    in1_data._levels_per_sp_num[sp_num] = source_in1._levels_per_sp_num[sp_num].copy()

    for level in in1_data._levels_per_sp_num[sp_num]:
        key = (sp_num, level)
        in1_data._energy_table[key] = source_in1._energy_table[key]
        in1_data._configs[key] = source_in1._configs[key].copy()
        in1_data._stat_weight[key] = source_in1._stat_weight[key]

    in1_data._branching_ratio[sp_num] = {
        config: levels.copy()
        for config, levels in source_in1._branching_ratio.get(sp_num, {}).items()
    }


def _copy_filtered_in1_sp(in1_data, source_in1, sp_num, level_map, target_energy_map):
    _clear_in1_sp(in1_data, sp_num)

    in1_data._ionization_potential[sp_num] = source_in1._ionization_potential[sp_num]
    in1_data._header_fifth_column[sp_num] = source_in1._header_fifth_column[sp_num]
    in1_data._levels_per_sp_num[sp_num] = _int_sort(level_map.values())
    in1_data._branching_ratio[sp_num] = {}

    for old_level, new_level in level_map.items():
        old_key = (sp_num, old_level)
        new_key = (sp_num, new_level)
        if old_key not in source_in1._configs:
            raise ValueError("FAC IN1 has no mapped level %s/%s" % (sp_num, old_level))

        in1_data._energy_table[new_key] = target_energy_map.get(new_level, source_in1._energy_table[old_key])
        in1_data._configs[new_key] = source_in1._configs[old_key].copy()
        in1_data._stat_weight[new_key] = source_in1._stat_weight[old_key]

    for config, old_levels in source_in1._branching_ratio.get(sp_num, {}).items():
        new_levels = [level_map[level] for level in old_levels if level in level_map]
        if new_levels:
            in1_data._branching_ratio[sp_num][config] = new_levels


def merge_in1(piter_in1, fac_in1, boundary_sp, level_map, target_energy_map):
    merged = copy.deepcopy(piter_in1)

    for sp_num in list(merged._ionization_potential.keys()):
        if int(sp_num) >= int(boundary_sp):
            _clear_in1_sp(merged, sp_num)

    _copy_filtered_in1_sp(merged, fac_in1, boundary_sp, level_map, target_energy_map)
    for sp_num in fac_in1.get_sp_numbers():
        if int(sp_num) > int(boundary_sp):
            _copy_in1_sp(merged, fac_in1, sp_num)

    return merged


def _translate_boundary_level(sp_num, level, boundary_sp, level_map):
    if sp_num != boundary_sp:
        return level
    return level_map.get(level)


def merge_same_species_transitions(piter_obj, fac_obj, boundary_sp, level_map):
    merged = copy.deepcopy(piter_obj)
    new_transitions = {}
    new_spectroscopic_numbers = set()

    for key, data in piter_obj._transitions.items():
        sp_num = key[0]
        if int(sp_num) < int(boundary_sp):
            new_transitions[key] = data.copy()
            new_spectroscopic_numbers.add(sp_num)

    for key, data in fac_obj._transitions.items():
        sp_num = key[0]
        if int(sp_num) == int(boundary_sp):
            translated_levels = [
                _translate_boundary_level(sp_num, level, boundary_sp, level_map)
                for level in key[1:]
            ]
            if all(level is not None for level in translated_levels):
                new_key = (sp_num, *translated_levels)
                new_transitions[new_key] = data.copy()
                new_spectroscopic_numbers.add(sp_num)
        elif int(sp_num) > int(boundary_sp):
            new_transitions[key] = data.copy()
            new_spectroscopic_numbers.add(sp_num)

    merged._transitions = new_transitions
    merged._spectroscopic_numbers = new_spectroscopic_numbers
    return merged


def merge_rrec(piter_rrec, fac_rrec, boundary_sp, level_map, valid_species):
    merged = copy.deepcopy(piter_rrec)
    new_transitions = {}
    new_spectroscopic_numbers = set()

    for key, data in piter_rrec._transitions.items():
        sp_num = key[0]
        if int(sp_num) < int(boundary_sp):
            new_transitions[key] = data.copy()
            new_spectroscopic_numbers.add(sp_num)

    for (sp_num, from_level, to_level), data in fac_rrec._transitions.items():
        target_sp = str(int(sp_num) + 1)
        if target_sp not in valid_species:
            # The requested merged database stops at the highest generated species.
            # Drop top-edge recombination rows that would target a missing next species.
            continue
        if int(sp_num) == int(boundary_sp):
            new_from_level = level_map.get(from_level)
            if new_from_level is not None:
                new_transitions[(sp_num, new_from_level, to_level)] = data.copy()
                new_spectroscopic_numbers.add(sp_num)
        elif int(sp_num) > int(boundary_sp):
            new_transitions[(sp_num, from_level, to_level)] = data.copy()
            new_spectroscopic_numbers.add(sp_num)

    merged._transitions = new_transitions
    merged._spectroscopic_numbers = new_spectroscopic_numbers
    return merged


def merge_bcfp(piter_bcfp, fac_bcfp, boundary_sp, level_map, valid_species):
    merged = copy.deepcopy(piter_bcfp)
    new_transitions = {}
    new_spectroscopic_numbers = set()

    for key, data in piter_bcfp._transitions.items():
        from_sp, from_level, to_sp, to_level = key
        if from_sp not in valid_species or to_sp not in valid_species:
            continue
        if int(from_sp) < int(boundary_sp) and int(to_sp) <= int(boundary_sp):
            new_transitions[key] = data.copy()
            new_spectroscopic_numbers.update([from_sp, to_sp])

    for key, data in fac_bcfp._transitions.items():
        from_sp, from_level, to_sp, to_level = key
        if from_sp not in valid_species or to_sp not in valid_species:
            # The merged Ti database intentionally stops at species 13.
            # Drop ionization rows that would target missing top-edge species 14.
            continue
        if int(from_sp) < int(boundary_sp) and int(to_sp) <= int(boundary_sp):
            continue

        new_from_level = _translate_boundary_level(from_sp, from_level, boundary_sp, level_map)
        new_to_level = _translate_boundary_level(to_sp, to_level, boundary_sp, level_map)
        if new_from_level is None or new_to_level is None:
            continue

        new_key = (from_sp, new_from_level, to_sp, new_to_level)
        new_transitions[new_key] = data.copy()
        new_spectroscopic_numbers.update([from_sp, to_sp])

    merged._transitions = new_transitions
    merged._spectroscopic_numbers = new_spectroscopic_numbers
    return merged


def merge_titanium(piter_dir, fac_dir, output_dir, mapping_csv, boundary_sp,
                   fac_level_column, target_level_column, target_energy_column,
                   run_checks=True):
    level_map = read_level_map(mapping_csv, fac_level_column, target_level_column)
    target_energy_map = read_target_energy_map(mapping_csv, target_level_column, target_energy_column)

    piter_in1 = _read_in1(os.path.join(piter_dir, "IN1.INP"))
    fac_in1 = _read_in1(os.path.join(fac_dir, "IN1.INP"))
    piter_excit = EXCIT(os.path.join(piter_dir, "EXCIT.INP"))
    fac_excit = EXCIT(os.path.join(fac_dir, "EXCIT.INP"))
    piter_bcfp = BCFP(os.path.join(piter_dir, "BCFP.INP"))
    fac_bcfp = BCFP(os.path.join(fac_dir, "BCFP.INP"))
    piter_spectr = SPECTR(os.path.join(piter_dir, "SPECTR.INP"))
    fac_spectr = SPECTR(os.path.join(fac_dir, "SPECTR.INP"))
    piter_rrec = RREC(os.path.join(piter_dir, "RREC.INP"))
    fac_rrec = RREC(os.path.join(fac_dir, "RREC.INP"))

    os.makedirs(output_dir, exist_ok=True)

    merged_in1 = merge_in1(piter_in1, fac_in1, boundary_sp, level_map, target_energy_map)
    merged_in1.dump_to_file(os.path.join(output_dir, "IN1.INP"))
    merge_same_species_transitions(piter_excit, fac_excit, boundary_sp, level_map).dump_to_file(
        os.path.join(output_dir, "EXCIT.INP")
    )
    valid_species = set(merged_in1.get_sp_numbers())
    merged_bcfp = merge_bcfp(piter_bcfp, fac_bcfp, boundary_sp, level_map, valid_species)
    # Piter BCFP rows use branching ratios, while FAC rows already use NOMAD fit coefficients.
    # Convert the retained Piter rows before dumping so the merged file has one consistent format.
    merged_bcfp.create_fac_fitting_params(in1_data=merged_in1, remove_bad_fits=False)
    merged_bcfp.dump_to_file(os.path.join(output_dir, "BCFP.INP"))
    merge_same_species_transitions(piter_spectr, fac_spectr, boundary_sp, level_map).dump_to_file(
        os.path.join(output_dir, "SPECTR.INP")
    )
    merge_rrec(piter_rrec, fac_rrec, boundary_sp, level_map, valid_species).dump_to_file(
        os.path.join(output_dir, "RREC.INP")
    )
    if run_checks:
        run_post_merge_checks(output_dir)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Merge Titanium Piter and FAC databases using a Ti6 level mapping CSV."
    )
    parser.add_argument("piter_dir", help="Directory with Piter/database INP files")
    parser.add_argument("fac_dir", help="Directory with FAC INP files")
    parser.add_argument("output_dir", help="Directory for merged INP files")
    parser.add_argument("mapping_csv", help="CSV with Ti6 level mapping")
    parser.add_argument("--boundary-sp", default=DEFAULT_BOUNDARY_SP, help="Boundary spectroscopic number")
    parser.add_argument("--fac-level-column", default="NOMAD from Fac №", help="CSV column with FAC level numbers")
    parser.add_argument("--target-level-column", default="NIST №", help="CSV column with target/Piter level numbers")
    parser.add_argument(
        "--target-energy-column",
        default=DEFAULT_TARGET_ENERGY_COLUMN,
        help="CSV column with target/Piter level energies",
    )
    parser.add_argument("--skip-check", action="store_true", help="Do not run legacy post-merge checks")

    args = parser.parse_args()
    merge_titanium(
        args.piter_dir,
        args.fac_dir,
        args.output_dir,
        args.mapping_csv,
        args.boundary_sp,
        args.fac_level_column,
        args.target_level_column,
        args.target_energy_column,
        run_checks=not args.skip_check,
    )
