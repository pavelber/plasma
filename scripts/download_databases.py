import os
from os.path import dirname, abspath

from lib.download_parse_pa_uky_levels import download_piter_levels
from lib.download_parse_pa_uky_lines import download_piter_lines
from lib.env import get_pathes, env
from lib.utils import error, read_table

################## MAIN ######################


(name_to_table, num_to_table) = read_table()

env_errors = env()
if env_errors is not None:
    error(env_errors)

old_path, fit_path, exc_fac_path, ph_fac_path, my_dir = get_pathes()

(name_to_table, num_to_table) = read_table()
my_dir = dirname(abspath(__file__))

for elem_num in range(1, 37):
    elem = num_to_table[str(elem_num)]['Symbol']
    nmax = 10
    osc = 1e-8
    min_sp_num = 1
    max_sp_num = min(int(num_to_table[str(elem_num)]['AtomicNumber']), 8)
    print("DOWNLOADING " + elem)
    elem_dir = os.path.join(my_dir, "db", elem)
    levels_downloaded = os.path.join(my_dir, "db", elem, "levels")
    lines_downloaded = os.path.join(my_dir, "db", elem, "lines")

    if not os.path.exists(elem_dir):
        os.mkdir(elem_dir)

    if not os.path.exists(levels_downloaded):
        os.mkdir(levels_downloaded)

    if not os.path.exists(lines_downloaded):
        os.mkdir(lines_downloaded)

    download_piter_levels(elem, levels_downloaded, nmax)
    download_piter_lines(elem, lines_downloaded, nmax, osc)
