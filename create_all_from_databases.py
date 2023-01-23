import os
import sys

from lib.create_in1_from_databases import create_in1_from_databases, parse_energy_limits
from lib.create_rrec_from_in1 import create_rrec_from_in1
from lib.utils import error

################## MAIN ######################
if len(sys.argv) < 3:
    error('\nUsage: ' + sys.argv[
        0] + ' out-dir element-name energy-limits')

out_dir = os.path.abspath(sys.argv[1])
elem = sys.argv[2]

energy_limits = parse_energy_limits(sys.argv[3])

download = True
if len(sys.argv) == 5 and sys.argv[4] == 'False':
    download = False

sp_nums = create_in1_from_databases(out_dir, elem, energy_limits,download)
create_rrec_from_in1(os.path.join(out_dir, elem, "IN1.INP"), out_dir, sp_nums)
