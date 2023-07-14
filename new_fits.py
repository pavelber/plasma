import os
import sys

from lib.remove_lines_and_renumenrate import remove_large
from lib.update_fits import create_new_fits_for_rrec3, create_new_fits_for_rrec4
from lib.utils import error

################## MAIN ######################
if len(sys.argv) < 3:
    error('\nUsage: ' + sys.argv[
        0] + ' out-dir element-name energy-limits')

out_dir = os.path.abspath(sys.argv[1])
elem = sys.argv[2]

if not os.path.exists(out_dir):
    os.makedirs(out_dir)

elem_dir = os.path.join(out_dir, elem)
create_new_fits_for_rrec4(elem_dir)
file_name = os.path.join(elem_dir, "RREC-fits.INP")
removed = remove_large(file_name, 0, [4, 5], 1.0e-4)
print("Removed " + str(removed) + "from " + file_name)
