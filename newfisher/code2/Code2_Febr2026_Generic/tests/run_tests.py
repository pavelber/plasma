#!/usr/bin/env python3
"""Regression tests of Code2 (Generic version). Every comparison is bit for bit.

  python tests/run_tests.py            T1 + T2 (unit + one step on Kr and Ge), ~3 min
  python tests/run_tests.py unit       T1 only (seconds)
  python tests/run_tests.py step       T2 only
  python tests/run_tests.py e2e        T3: the real program, 3 time steps of Kr and Ge, ~8 min
  python tests/run_tests.py all        T1 + T2 + T3
  ... --threads N                      run the programs with OMP_NUM_THREADS=N
  ... --numeric                        compare with tolerances (for changes that move the last
                                       digits, e.g. a new D02EAF): T1/T2 values within rtol 1e-9,
                                       T3 numbers within 1 unit of the last printed digit
                                       (golden data: golden/e2e_<case>_dat.zip)
  python tests/run_tests.py d02        new D02EAF against the previous algorithm (seconds)
  ... --update                         write the golden files instead of comparing
                                       (only after deciding that a change of results is wanted)

Build first:  mingw32-make -f Makefile.gnu tests
Run from the folder Code2_Febr2026_Generic (the folder of Makefile.gnu).
"""
import hashlib
import os
import re
import shutil
import subprocess
import sys
import time

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)                       # Code2_Febr2026_Generic
GOLD = os.path.join(HERE, 'golden')
WORK = os.path.join(HERE, '_work')
EXE = '.exe' if os.name == 'nt' else ''
def _first_case_dir(*dirs):
    for d in dirs:
        if os.path.exists(os.path.join(d, 'Params0.inp')):
            return d
    return dirs[0]


CASES = {   # input files of the test cases: examples/ in the zip, or the repository layout
    'kr': _first_case_dir(os.path.join(os.path.dirname(ROOT), 'examples', 'Kr'), ROOT),
    'ge': _first_case_dir(os.path.join(os.path.dirname(ROOT), 'examples', 'Ge'),
                          os.path.join(os.path.dirname(ROOT), 'code2Feb2026-case16-Ge')),
}
INPUT_RE = re.compile(r'.*\.(inp|INP)$')
E2E_STOP = '7.6d-12'      # Params2.inp line 5: StopTime -> 3 time steps (the third one has BB emission)


THREADS = None            # --threads N: OMP_NUM_THREADS for the programs under test
NUMERIC = False           # --numeric: compare with tolerances instead of bit for bit
RTOL = 1e-9               # relative tolerance of T1/T2 values in --numeric mode
UNITS = 1                 # T3 in --numeric mode: allowed difference, in units of the last printed digit
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import numcompare         # noqa: E402


def env():
    e = dict(os.environ)
    e['GFORTRAN_UNBUFFERED_PRECONNECTED'] = 'y'
    if THREADS:
        e['OMP_NUM_THREADS'] = str(THREADS)
    return e


def prepare(case, name):
    d = os.path.join(WORK, f'{name}_{case}')
    if os.path.isdir(d):
        shutil.rmtree(d)
    os.makedirs(d)
    for f in os.listdir(CASES[case]):
        if INPUT_RE.match(f):
            shutil.copy2(os.path.join(CASES[case], f), d)
    return d


def parse_value_line(line):
    """T1/T2 line -> (name, kind, values): scalar 'name HEX value', array
    'name HASH sum= S max= M', or integer 'name N'."""
    t = line.split()
    if 'sum=' in t:
        i = t.index('sum=')
        return ' '.join(t[:i - 1]), 'arr', (float(t[i + 1]), float(t[i + 3]))
    if len(t) >= 3 and re.fullmatch(r'[0-9A-F]{16}', t[-2]):
        return ' '.join(t[:-2]), 'val', (float(t[-1]),)
    return ' '.join(t[:-1]), 'int', (int(t[-1]),)


def compare_numeric(name, got_file, gold_file):
    got = open(got_file).read().splitlines()
    gold = open(gold_file).read().splitlines()
    if len(gold) != len(got):
        print(f'  {name}: FAIL, {len(got)} lines, golden has {len(gold)}')
        return False
    nbit = nbad = 0
    worst, wl = 0.0, ''
    for g, h in zip(gold, got):
        if g == h:
            nbit += 1
            continue
        ng, kg, vg = parse_value_line(g)
        nh, kh, vh = parse_value_line(h)
        if ng != nh or kg != kh:
            print(f'  {name}: FAIL, lines do not match:')
            print(f'     golden: {g}')
            print(f'     now:    {h}')
            return False
        for a, b in zip(vg, vh):
            if kg == 'int':
                rel = 0.0 if a == b else 1.0
            else:
                rel = 0.0 if a == b else abs(a - b) / max(abs(a), abs(b))
            if rel > worst:
                worst, wl = rel, ng
            if rel > RTOL:
                nbad += 1
    status = 'OK' if nbad == 0 else 'FAIL'
    where = f' ({wl})' if wl else ''
    print(f'  {name}: {status} (numeric, rtol {RTOL:g}): {nbit} of {len(gold)} values bit for bit, '
          f'max rel. difference {worst:.1e}{where}, {nbad} outside')
    return nbad == 0


def compare_lines(name, got_file, gold_file, update):
    if NUMERIC and not update:
        return compare_numeric(name, got_file, gold_file)
    got = open(got_file).read().splitlines()
    if update:
        shutil.copy2(got_file, gold_file)
        print(f'  {name}: golden written ({len(got)} lines)')
        return True
    if not os.path.exists(gold_file):
        print(f'  {name}: FAIL, no golden file {gold_file}')
        return False
    gold = open(gold_file).read().splitlines()
    bad = [(g, h) for g, h in zip(gold, got) if g != h]
    if len(gold) != len(got):
        print(f'  {name}: FAIL, {len(got)} lines, golden has {len(gold)}')
        return False
    if bad:
        print(f'  {name}: FAIL, {len(bad)} of {len(gold)} values differ, first ones:')
        for g, h in bad[:10]:
            print(f'     golden: {g}\n     now:    {h}')
        return False
    print(f'  {name}: OK ({len(gold)} values bit for bit)')
    return True


def run(cmd, cwd, log):
    with open(os.path.join(cwd, log), 'w') as out:
        return subprocess.Popen(cmd, cwd=cwd, stdout=out, stderr=subprocess.STDOUT,
                                stdin=subprocess.DEVNULL, env=env())


def test_unit(update):
    d = os.path.join(WORK, 'unit')
    os.makedirs(d, exist_ok=True)
    p = run([os.path.join(ROOT, 'test_unit' + EXE), 'unit_out.txt'], d, 'screen.log')
    p.wait()
    if p.returncode != 0:
        print(f'  T1 unit: FAIL, exit code {p.returncode}, see {d}')
        return False
    return compare_lines('T1 unit', os.path.join(d, 'unit_out.txt'),
                         os.path.join(GOLD, 'unit.txt'), update)


def test_step(update):
    procs = {}
    for case in CASES:
        d = prepare(case, 'step')
        procs[case] = (d, run([os.path.join(ROOT, 'test_step' + EXE), 'step_out.txt'], d, 'screen.log'))
    ok = True
    for case, (d, p) in procs.items():
        p.wait()
        if p.returncode != 0:
            print(f'  T2 step {case}: FAIL, exit code {p.returncode}, see {d}')
            ok = False
            continue
        ok &= compare_lines(f'T2 step {case}', os.path.join(d, 'step_out.txt'),
                            os.path.join(GOLD, f'step_{case}.txt'), update)
    return ok


def test_d02(update):
    """New D02EAF against the previous algorithm D02EAF_TAYLOR (no golden file)."""
    d = os.path.join(WORK, 'd02')
    os.makedirs(d, exist_ok=True)
    p = run([os.path.join(ROOT, 'test_d02' + EXE), 'd02_out.txt'], d, 'screen.log')
    p.wait()
    if p.returncode != 0:
        print(f'  T1 D02EAF: FAIL, exit code {p.returncode}, see {d}')
        return False
    ok = True
    worst = 0.0
    rows = [r for r in open(os.path.join(d, 'd02_out.txt')).read().splitlines() if r.strip()]
    print('     n   ||A||   new-old   err new   err old  (err: vs quadruple precision)')
    for r in rows:
        t = r.split()
        n, nrm, rel, dsum, ymin = int(t[0]), float(t[1]), float(t[2]), float(t[3]), float(t[4])
        f1, f2 = int(t[5]), int(t[6])
        errn, erro, dsumo = float(t[7]), float(t[8]), float(t[9])
        worst = max(worst, rel)
        if errn >= 0:   # exact reference: the new one must not be less accurate than the old one
            good = errn <= 10 * max(erro, 1e-14)
            print(f'   {n:4d} {nrm:7.0e} {rel:9.1e} {errn:9.1e} {erro:9.1e}')
        else:
            good = rel <= 1e-8
        good = good and dsum <= 10 * max(dsumo, 1e-14) and ymin >= -1e-12 and f1 == 0 and f2 == 0
        if not good:
            print(f'  T1 D02EAF: FAIL for n={n}, ||A||={nrm:g}: new-old {rel:.1e}, err new {errn:.1e}, '
                  f'err old {erro:.1e}, |sum-1| new {dsum:.1e} old {dsumo:.1e}, min {ymin:.1e}, '
                  f'ifail {f1} {f2}')
            ok = False
    if ok:
        print(f'  T1 D02EAF: OK ({len(rows)} cases): not less accurate than the previous algorithm; '
              f'max new-old difference {worst:.1e}')
    return ok


def set_stop_time(params2):
    lines = open(params2, newline='').read().split('\n')
    lines[4] = re.sub(r'^(\s*)\S+', lambda m: m.group(1) + E2E_STOP, lines[4], count=1)
    open(params2, 'w', newline='').write('\n'.join(lines))


def test_e2e(update):
    procs = {}
    for case in CASES:
        d = prepare(case, 'e2e')
        set_stop_time(os.path.join(d, 'Params2.inp'))
        procs[case] = (d, run([os.path.join(ROOT, 'Co2feb26_Generic' + EXE)], d, 'screen.log'))
    ok = True
    for case, (d, p) in procs.items():
        p.wait()
        if p.returncode != 0:
            print(f'  T3 e2e {case}: FAIL, exit code {p.returncode}, see {d}')
            ok = False
            continue
        if NUMERIC and not update:
            import zipfile
            ref = os.path.join(WORK, f'e2e_{case}_ref')
            if os.path.isdir(ref):
                shutil.rmtree(ref)
            zipfile.ZipFile(os.path.join(GOLD, f'e2e_{case}_dat.zip')).extractall(ref)
            files = sorted(f for f in os.listdir(ref) if f.endswith('.dat')) + ['screen.log']
            print(f'  T3 e2e {case} (numeric, up to {UNITS:g} unit(s) of the last printed digit):')
            good = numcompare.compare_dirs(ref, d, files, UNITS, quiet=True)
            print(f'  T3 e2e {case}: {"OK" if good else "FAIL"}')
            ok &= good
            continue
        out = os.path.join(d, 'e2e_sha256.txt')
        with open(out, 'w') as f:
            for name in sorted(os.listdir(d)):
                if name.endswith('.dat') or name == 'screen.log':
                    h = hashlib.sha256(open(os.path.join(d, name), 'rb').read()).hexdigest()
                    f.write(f'{name} {h}\n')
        ok &= compare_lines(f'T3 e2e {case}', out, os.path.join(GOLD, f'e2e_{case}.txt'), update)
    return ok


def main():
    args = sys.argv[1:]
    global THREADS
    update = '--update' in args
    args = [a for a in args if a != '--update']
    if '--threads' in args:
        i = args.index('--threads')
        THREADS = int(args[i + 1])
        del args[i:i + 2]
        print(f'OMP_NUM_THREADS = {THREADS}')
    global NUMERIC
    if '--numeric' in args:
        NUMERIC = True
        args = [a for a in args if a != '--numeric']
        print('numeric comparison (tolerances), not bit for bit')
    what = args[0] if args else 'quick'
    tests = {'unit': [test_unit], 'step': [test_step], 'e2e': [test_e2e], 'd02': [test_d02],
             'quick': [test_unit, test_d02, test_step],
             'all': [test_unit, test_d02, test_step, test_e2e]}[what]
    os.makedirs(WORK, exist_ok=True)
    os.makedirs(GOLD, exist_ok=True)
    ok = True
    for t in tests:
        t0 = time.time()
        ok &= t(update)
        print(f'     ({time.time() - t0:.0f} s)')
    print('ALL TESTS PASSED' if ok else 'SOME TESTS FAILED')
    sys.exit(0 if ok else 1)


if __name__ == '__main__':
    main()
