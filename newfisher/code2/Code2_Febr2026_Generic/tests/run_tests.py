#!/usr/bin/env python3
"""Regression tests of Code2 (Generic version). Every comparison is bit for bit.

  python tests/run_tests.py            T1 + T2 (unit + one step on Kr and Ge), ~3 min
  python tests/run_tests.py unit       T1 only (seconds)
  python tests/run_tests.py step       T2 only
  python tests/run_tests.py e2e        T3: the real program, 3 time steps of Kr and Ge, ~8 min
  python tests/run_tests.py all        T1 + T2 + T3
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
CASES = {
    'kr': ROOT,                                                   # Kr inputs next to the sources
    'ge': os.path.join(os.path.dirname(ROOT), 'code2Feb2026-case16-Ge'),
}
INPUT_RE = re.compile(r'.*\.(inp|INP)$')
E2E_STOP = '7.6d-12'      # Params2.inp line 5: StopTime -> 3 time steps (the third one has BB emission)


def env():
    e = dict(os.environ)
    e['GFORTRAN_UNBUFFERED_PRECONNECTED'] = 'y'
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


def compare_lines(name, got_file, gold_file, update):
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
    update = '--update' in args
    args = [a for a in args if a != '--update']
    what = args[0] if args else 'quick'
    tests = {'unit': [test_unit], 'step': [test_step], 'e2e': [test_e2e],
             'quick': [test_unit, test_step], 'all': [test_unit, test_step, test_e2e]}[what]
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
