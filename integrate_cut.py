import os
import sys

from lib.utils import error

################## MAIN ######################
if len(sys.argv) < 2:
    error('\nUsage: ' + sys.argv[
        0] + ' out-dir element-name')

out_dir = os.path.abspath(sys.argv[1])
elem = sys.argv[2]

elem_dir = os.path.join(out_dir, elem)

rrec_path = os.path.join(elem_dir, "RREC.INP")


def fun(a, b, c, d, x):
    return (a + b / x + c / (x * x)) * (1 / (pow(x, (7.0 / 2.0 + d))))


def integrate(a, b, c, d, start, end, step):
    sum = 0
    i = start
    while i < end + step:
        i1 = fun(a, b, c, d, i)
        i2 = fun(a, b, c, d, i + step)
        sum += (i1 + i2) / 2.0
        i += step
    return sum


with open(rrec_path, "r") as rrec:
    for line in rrec:
        parts = line.split()
        sp = parts[0]
        level1 = parts[1]
        level2 = parts[2]
        a = float(parts[3])
        b = float(parts[4])
        c = float(parts[5])
        d = float(parts[6])
        cut = integrate(a, b, c, d, 1.0, 1000.0, 1.0)
        print("%4s%4s%4s%10.2f" % (sp, level1, level2, cut))

# TODO: for removed create a graph using rrec and for this formula and RREC and compare
