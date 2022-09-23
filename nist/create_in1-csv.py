import os
import sys

from lib.utils import error

################## MAIN ######################
if len(sys.argv) < 2:
    error('\nUsage: ' + sys.argv[
        0] + ' directory-with-nist-csv out-dir')

in_dir = os.path.abspath(sys.argv[1])
out_dir = os.path.abspath(sys.argv[2])

if not os.path.isdir(in_dir) or not os.path.exists(in_dir):
    error(in_dir + " does not exists or is not a directory")

if not os.path.exists(out_dir):
    os.makedirs(out_dir)

with open(os.path.join(out_dir, "IN1.INP"), 'rb') as in1_inp:
    with open(os.path.join(out_dir, "IN1.csv"), 'wb') as in1_csv:
        sp_nums_started = False
        for l in in1_inp:
            parts = l.split()
            if len(parts) == 1:
                sp_nums_started = True
                spec_num = parts[0]
            else:
                if sp_nums_started and len(parts) > 2 and len(parts) > 7:
                    n = parts[7]
                    config = parts[0] + "." + parts[1]
                    config_1 = parts[2]
                    energy_str = parts[4]
                    in1_csv.write("%s,%s,%s,%s,%s\n" % (spec_num, n, energy_str, config, config_1))
