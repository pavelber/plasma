import os
import re
import sys

from lib.utils import error, skip_n_lines, rreplace


def add_transitions(configs):
    pass


def format_configuration(configuration):
    configuration_split = configuration.split(".")
    s = " ".join(configuration_split)
    return " ".join(filter(lambda c: c[0:1] != '(', configuration_split))


def get_in_parenthesis(s):
    if '(' in s:
        return re.search(r'\((.*?)\)', s).group(1).replace('*', "")
    else:
        return ''


def create_conf(p1, p2):
    return (format_configuration(p1) + " " + p2.replace('*', ''), get_in_parenthesis(p1))


def find_transitions_from_file(configs, spn, fname):
    trs = {}
    with open(fname) as nist_f:
        skip_n_lines(nist_f, 5)
        for l in nist_f:
            parts = l.split("|")
            if parts[2].strip() != '':
                if parts[1].strip() != '':
                    confs_1 = parts[1]
                confs_2 = parts[2]
                energy = parts[8].strip()
                osc = parts[11].strip()
                configurations_1 = confs_1.split(" - ")
                configurations_2 = confs_2.split(" - ")
                low = create_conf(configurations_1[0].strip(), configurations_2[0].strip())
                high = create_conf(configurations_1[1].strip(), configurations_2[1].strip())
                matched = next((x for x in configs if
                                (x[0] in low[0] and x[2] == low[1]) or (x[0] in high[0] and x[2] == high[1])), 0)
                if matched != 0 and osc != "":
                    print("%12s|%6s|%41s|%16s|%30s|%30s" % (os.path.basename(fname), parts[0], confs_1, parts[2], parts[8], parts[11]))
    return trs


def find_transitions(nist, configs):
    return find_transitions_from_file(configs, "2",
                                      os.path.join(nist, "ArII.txt")) \
        .update(find_transitions_from_file(configs, "3", os.path.join(nist, "ArIII.txt")))


def rewrite_in1_inp(inp_in, inp_out):
    inside_levels = False
    take_count = False
    replacements = {}

    for l in inp_in:
        parts = l.split()
        if len(parts) == 1:  # new spectroscopic number
            take_count = False
            inside_levels = True
            spn = parts[0]
            inp_out.write(l)
        elif len(parts) == 2:  # new spectroscopic number
            inp_out.write(l)
        elif inside_levels:
            if len(parts) > 7:
                level = parts[7]
            if level == 'N':
                take_count = True
                count = count + 1
                inp_out.write(l.replace('N ', str(count)))
            elif take_count:
                count += 1
                scount = str(count)
                if l.count(level) > 1:
                    inp_out.write(rreplace(l, level, scount, 1))
                    replacements[(spn, level)] = scount
                else:
                    inp_out.write(l.replace(level, scount))
                    replacements[(spn, level)] = scount
            else:
                try:
                    count = int(level)
                except:
                    pass
                inp_out.write(l)
        else:
            inp_out.write(l)
    return replacements


def replace_bcfp(infile, outfile, replace):
    for l in infile:
        parts = l.split()
        if len(parts) != 8:
            outfile.write(l)
        else:
            spn = parts[0]
            low = parts[1]
            high = parts[3]
            if (spn, low) in replace:
                low = replace[(spn, low)]
            if (spn, high) in replace:
                high = replace[(spn, high)]
            outfile.write(
                "%5s%5s%5s%5s%15s%7s%7s%6s\n" % (spn, low, parts[2], high, parts[4], parts[5], parts[6], parts[7]))


def replace_rrec(infile, outfile, replace):
    for l in infile:
        parts = l.split()
        if len(parts) != 13:
            outfile.write(l)
        else:
            spn = parts[0]
            low = parts[2]
            high = parts[1]
            if (spn, low) in replace:
                low = replace[(spn, low)]
            if (spn, high) in replace:
                high = replace[(spn, high)]
            outfile.write(
                "%2s%3s%4s%12s%12s%12s%12s%12s%12s%12s%12s%12s%12s\n"
                % (spn, high, low, parts[3], parts[4], parts[5], parts[6], parts[7], parts[8], parts[9], parts[10],
                   parts[11], parts[12]))


def replace_excit(infile, outfile, replace):
    for l in infile:
        parts = l.split()
        if len(parts) != 11:
            outfile.write(l)
        else:
            spn = parts[0]
            low = parts[1]
            high = parts[2]
            if (spn, low) in replace:
                low = replace[(spn, low)]
            if (spn, high) in replace:
                high = replace[(spn, high)]
            outfile.write(
                "%4s%4s%4s%4s%11s%11s%11s%11s%11s%11s%11s\n"
                % (spn, low, high, parts[3], parts[4], parts[5], parts[6], parts[7], parts[8], parts[9], parts[10]))
    ################## MAIN ######################


def replace_spectr(infile, outfile, replace):
    for l in infile:
        parts = l.split()
        if len(parts) < 6:
            outfile.write(l)
        else:
            spn = parts[0]
            low = parts[2]
            high = parts[1]
            if (spn, low) in replace:
                low = replace[(spn, low)]
            if (spn, high) in replace:
                high = replace[(spn, high)]
            if len(parts) == 6:
                comment = ""
            else:
                comment = parts[6]
            outfile.write(
                "%2s%6s%4s%9s%10s%10s %s\n"
                % (spn, high, low, parts[3], parts[4], parts[5], comment))

    ################## MAIN ######################


if len(sys.argv) < 3:
    error('\nUsage: ' + sys.argv[
        0] + ' directory-inp-files out-dir element-name')

in_dir = os.path.abspath(sys.argv[1])
out_dir = os.path.abspath(sys.argv[2])
elem = sys.argv[3]
nist_dir = sys.argv[4]

if not os.path.isdir(in_dir) or not os.path.exists(in_dir):
    error(in_dir + " does not exists or is not a directory")

if not os.path.exists(out_dir):
    os.makedirs(out_dir)

with open(os.path.join(in_dir, "IN1.INP"), 'rb') as inf:
    with open(os.path.join(out_dir, "IN1.INP"), 'wb') as outf:
        replace = rewrite_in1_inp(inf, outf)

with open(os.path.join(in_dir, "BCFP.INP"), 'rb') as inf:
    with open(os.path.join(out_dir, "BCFP.INP"), 'wb') as outf:
        replace_bcfp(inf, outf, replace)

with open(os.path.join(in_dir, "RREC.INP"), 'rb') as inf:
    with open(os.path.join(out_dir, "RREC.INP"), 'wb') as outf:
        replace_rrec(inf, outf, replace)

with open(os.path.join(in_dir, "EXCIT.INP"), 'rb') as inf:
    with open(os.path.join(out_dir, "EXCIT.INP"), 'wb') as outf:
        replace_excit(inf, outf, replace)

with open(os.path.join(in_dir, "SPECTR.INP"), 'rb') as inf:
    with open(os.path.join(out_dir, "SPECTR.INP"), 'wb') as outf:
        replace_spectr(inf, outf, replace)

find_transitions(nist_dir,
                 [
                     ("3p4 4p 2P", 23.7575, '1S'),
                     ("3p4 4s 2S", 20.7435, '1S'),
                     ("3p3 4p 3D", 29.720, '2P'),
                     ("3p3 4p 3P", 30.095, '2P')
                 ]
                 )
#
# with open(os.path.join(in_dir, "EXCIT.INP.tmp"), 'rb') as inf:
#     with open(os.path.join(out_dir, "EXCIT.INP"), 'wb') as outf:
#         add_transitions(inf, outf, transitions)
