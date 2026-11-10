"""Fit Bernstam tabulation with Lotz/Fisher formula #4 and assemble Fisher files."""
import shutil
from pathlib import Path

from lib.bcfp import BCFP, METHOD
from lib.fit_parameters import create_fits
from lib.fisher import run_for_fisher
from lib.in1 import IN1

AIW_HEADER = "  SSi AIQS#  SSf  QSf#       WAI          DE(eV)\n"


def read_tabulation(path):
    points = []
    for line in path.read_text().splitlines():
        stripped = line.strip()
        if not stripped or stripped.startswith("#"):
            continue
        parts = stripped.split()
        points.append((float(parts[0]), float(parts[1])))
    return points


def fit_tables(src_dir, work_dir, tabulation_dir):
    in1 = IN1(str(src_dir / "IN1.INP"))
    bcfp = BCFP(str(src_dir / "BCFP.INP"))
    fitted = 0
    missing = 0
    failed = 0
    params = None

    for key in list(bcfp._transitions):
        from_sp, from_level, to_sp, to_level = key
        table_path = tabulation_dir / ("%s_%s_%s_%s.txt" % (from_sp, from_level, to_sp, to_level))
        if not table_path.exists():
            bcfp._transitions[key] = {
                "coefficients": [0.0, 0.0, 0.0, 0.0],
                "comment": "# Can't create fitting params",
            }
            missing += 1
            continue

        table = read_tabulation(table_path)
        if len(table) < 2:
            bcfp._transitions[key] = {
                "coefficients": [0.0, 0.0, 0.0, 0.0],
                "comment": "# Can't create fitting params",
            }
            missing += 1
            continue

        result, _square = create_fits(
            table=table,
            approximation_fun=BCFP.approximation_fun,
            ionization_potential=in1.get_ionization_potential(from_sp),
            stat_weight=in1.get_stat_weight(from_sp, from_level),
            initial_params=params,
            method=METHOD,
            reject_bad_fits=False,
        )
        if result is None:
            bcfp._transitions[key] = {
                "coefficients": [0.0, 0.0, 0.0, 0.0],
                "comment": "# Can't create fitting params",
            }
            failed += 1
            continue

        params = result
        bcfp._transitions[key] = {
            "coefficients": result,
            "comment": " # lotz from bernstam table",
        }
        fitted += 1

    out_bcfp = work_dir / "BCFP.INP.fisher"
    bcfp.dump_to_file(str(out_bcfp))
    return fitted, missing, failed


def assemble(src_dir, work_dir, dest_dir):
    work_dir.mkdir(parents=True, exist_ok=True)
    dest_dir.mkdir(parents=True, exist_ok=True)
    for name in ("IN1.INP", "EXCIT.INP", "RREC.INP", "SPECTR.INP"):
        shutil.copy2(src_dir / name, work_dir / name)

    fitted, missing, failed = fit_tables(src_dir, work_dir, src_dir / "tabulation")
    run_for_fisher(False, 1, 6, "C", str(work_dir), "BCFP.INP.fisher", False)

    fisher_dir = work_dir / "fisher"
    qss = next(fisher_dir.glob("QSsC*.inp"))
    inz = next(fisher_dir.glob("InzC*.INP"))
    suffix = qss.name[len("QSsC"):-len(".inp")]

    shutil.copy2(qss, dest_dir / qss.name)
    shutil.copy2(inz, dest_dir / inz.name)
    shutil.copy2(work_dir / "EXCIT.INP", dest_dir / ("ExcC%s.inp" % suffix))
    (dest_dir / ("AIwC%s.inp" % suffix)).write_text(AIW_HEADER)
    return fitted, missing, failed, dest_dir


if __name__ == "__main__":
    repo = Path(__file__).resolve().parents[1]
    src = repo / "out" / "bernstam" / "C"
    work = repo / "out" / "C-lotz4-from-bernstam"
    dest = repo / "out" / "fisher-C"
    if work.exists():
        shutil.rmtree(work)
    if dest.exists():
        shutil.rmtree(dest)
    fitted, missing, failed, dest_dir = assemble(src, work, dest)
    print("fitted", fitted, "no table", missing, "fit failed", failed)
    print("fisher dir", dest_dir)
    for path in sorted(dest_dir.iterdir()):
        print(path.name, path.stat().st_size)
