import os
import sys

from lib.utils import error


def nist_strip(s):
    if s.startswith('"=""'):
        return s[4:-3].strip()
    else:
        return s.strip()


def clean_num(s):
    return s.rstrip("?")


def format_configuration(configuration, max_len):
    configuration_split = configuration.split(".")
    s = " ".join(configuration_split)
    if len(s) > max_len:
        return " ".join(filter(lambda c: c[0:1] != '(', configuration_split))
    else:
        return " ".join(configuration_split)


def format_term(s):
    p = s.split(" ")
    return p[-1]


def write_section(elem, outf, spec_num, spec_num_file):
    with open(spec_num_file, "rb") as inf:
        n = 1
        outf.write(spec_num + '\n')
        for line in inf:
            parts = line.strip().split(',')
            configuration = nist_strip(parts[0])
            config = format_configuration(configuration, 10)
            term = format_term(nist_strip(parts[1]))
            g = nist_strip(parts[2])
            level = float(clean_num(nist_strip(parts[3])))
            if configuration.startswith(elem):
                outf.write("\n")
                return level
            else:
                outf.write(" %-10s%8s%3s%14.3f    0.00e+00 0.00e+00% 6d\n" % (config, term, g, level, n))
                print(" %-10s%8s%3s%14.3f    0.00e+00 0.00e+00% 6d\n" % (config, term, g, level, n))
                n = n + 1


################## MAIN ######################
if len(sys.argv) < 3:
    error('\nUsage: ' + sys.argv[
        0] + 'directory-with-nist-csv out-dir element-name')

in_dir = os.path.abspath(sys.argv[1])
out_dir = os.path.abspath(sys.argv[2])
elem = sys.argv[3]

if not os.path.isdir(in_dir) or not os.path.exists(in_dir):
    error(in_dir + " does not exists or is not a directory")

if not os.path.exists(out_dir):
    os.makedirs(out_dir)

with open(os.path.join(out_dir, "IN1.INP"), 'wb') as in1_inp:
    i_spectro = map(lambda x: str(x),
                    sorted(map(lambda x: int(os.path.splitext(x)[0]),
                               filter(lambda f:
                                      os.path.splitext(
                                          os.path.basename(f))[0].isdigit() and os.path.splitext(os.path.basename(f))[
                                          1] == '.csv',
                                      os.listdir(in_dir)))))
    for f in i_spectro:
        write_section(elem, in1_inp, f, os.path.join(in_dir, f + '.csv'))
