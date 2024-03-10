import os

from lib.env import get_pathes, env
from lib.strsbrg_db import download_levels_to_file, download_cuts_to_file
from lib.utils import error, read_table

################## MAIN ######################


(name_to_table, num_to_table) = read_table()

env_errors = env()
if env_errors is not None:
    error(env_errors)

old_path, fit_path, exc_fac_path, ph_fac_path, my_dir = get_pathes()

(name_to_table, num_to_table) = read_table()

for elem_num in range(1, 37):
    elem = num_to_table[str(elem_num)]['Symbol']
    min_sp_num = 1
    max_sp_num = min(int(num_to_table[str(elem_num)]['AtomicNumber']), 8)
    print("DOWNLOADING " + elem)
    elem_dir = os.path.join(my_dir, "db", elem)
    levels_downloaded = os.path.join(my_dir, "db", elem, "strasbg-levels")
    cuts_downloaded = os.path.join(my_dir, "db", elem, "strasbg-cuts")

    if not os.path.exists(elem_dir):
        os.mkdir(elem_dir)

    if not os.path.exists(levels_downloaded):
        os.mkdir(levels_downloaded)

    if not os.path.exists(cuts_downloaded):
        os.mkdir(cuts_downloaded)

    #download_levels_to_file(elem, levels_downloaded, min_sp_num, max_sp_num)
    download_cuts_to_file(elem, cuts_downloaded, min_sp_num, max_sp_num)
