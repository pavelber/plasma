#!/usr/bin/env python3
"""Numeric comparison of Code2 output files (Nov 2026).

Used when a change is expected to move only the last digits (e.g. a new D02EAF).
Every number of every line is compared with the reference. A number "agrees" if it is
equal as printed, or differs by at most `units` units of its last printed digit
(the printed precision of the output files, mostly 4 significant digits).

  python tests/numcompare.py REF_DIR NEW_DIR [--units N] [--files a.dat,b.dat]

Prints, per file, the number of values, how many differ, how many are outside the
limit, and the largest relative difference; exit code 1 if any value is outside.
"""
import os
import re
import sys

NUM = re.compile(r'[-+]?(?:\d+\.?\d*|\.\d+)(?:[EeDd][-+]?\d+)?')


def last_digit_unit(tok):
    """Size of one unit in the last printed digit of a number written as text."""
    t = tok.replace('D', 'E').replace('d', 'e')
    mant, _, exp = t.partition('e') if 'e' in t else t.partition('E')
    e = int(exp) if exp else 0
    if '.' in mant:
        decimals = len(mant.split('.')[1])
    else:
        decimals = 0
    return 10.0 ** (e - decimals)


def compare_text(ref_lines, new_lines, units):
    nval = ndiff = nbad = 0
    worst = 0.0
    worst_txt = ''
    if len(ref_lines) != len(new_lines):
        return None
    for a, b in zip(ref_lines, new_lines):
        if a == b:
            nval += len(NUM.findall(a))
            continue
        ta, tb = NUM.findall(a), NUM.findall(b)
        if len(ta) != len(tb):
            return None
        for x, y in zip(ta, tb):
            nval += 1
            if x == y:
                continue
            fx = float(x.replace('D', 'E').replace('d', 'e'))
            fy = float(y.replace('D', 'E').replace('d', 'e'))
            if fx == fy:
                continue
            ndiff += 1
            unit = max(last_digit_unit(x), last_digit_unit(y))
            if abs(fx - fy) > units * unit * 1.0001:
                nbad += 1
            rel = abs(fx - fy) / max(abs(fx), abs(fy))
            if rel > worst:
                worst, worst_txt = rel, f'{x} vs {y}'
    return nval, ndiff, nbad, worst, worst_txt


def compare_dirs(ref, new, files=None, units=1, quiet=False):
    """Returns True if every number is within `units` of the last printed digit."""
    if files is None:
        files = sorted(f for f in os.listdir(ref) if f.endswith('.dat'))
    ok = True
    tot = [0, 0, 0]
    for f in files:
        pa, pb = os.path.join(ref, f), os.path.join(new, f)
        if not os.path.exists(pb):
            print(f'    {f}: missing in {new}')
            ok = False
            continue
        ra = open(pa, errors='replace').read().splitlines()
        rb = open(pb, errors='replace').read().splitlines()
        res = compare_text(ra, rb, units)
        if res is None:
            print(f'    {f}: different structure (number of lines or of values)')
            ok = False
            continue
        nval, ndiff, nbad, worst, wt = res
        tot[0] += nval; tot[1] += ndiff; tot[2] += nbad
        if nbad:
            ok = False
        if (ndiff and not quiet) or nbad:
            print(f'    {f:22s} values {nval:8d}  differ {ndiff:6d}  outside {nbad:4d}'
                  f'  max rel {worst:.1e}  ({wt})')
    print(f'    total: {tot[0]} values, {tot[1]} differ in the last digit(s), '
          f'{tot[2]} outside {units} unit(s) of the last printed digit')
    return ok


def main():
    a = sys.argv[1:]
    units = 1
    files = None
    if '--units' in a:
        i = a.index('--units'); units = float(a[i + 1]); del a[i:i + 2]
    if '--files' in a:
        i = a.index('--files'); files = a[i + 1].split(','); del a[i:i + 2]
    ok = compare_dirs(a[0], a[1], files, units)
    print('AGREE' if ok else 'DIFFERENT')
    sys.exit(0 if ok else 1)


if __name__ == '__main__':
    main()
