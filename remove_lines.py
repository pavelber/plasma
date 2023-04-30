import os
import sys


from lib.remove_lines_and_renumenrate import read_used_lines, remove_lines_from_in1_inp, renumerate_in1_inp, \
    replace_in_file, sp_num_fun, remove_unused_lines_and_renumerate
from lib.utils import error

################## MAIN ######################
if len(sys.argv) < 3:
    error('\nUsage: ' + sys.argv[
        0] + ' out-dir element-name')

out_dir = os.path.abspath(sys.argv[1])
elem = sys.argv[2]
elem_dir = os.path.join(out_dir, elem)


# copy_checks(my_dir, elem_dir)
# check_and_fix_rr(elem_dir)
# check_and_fix_old_rr(elem_dir)


