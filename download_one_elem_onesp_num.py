import os
import sys
from os.path import dirname, abspath

from lib.download_parse_pa_uky_levels import download_piter_levels
from lib.download_parse_pa_uky_lines import download_piter_lines, download_piter_lines_one_spnum
from lib.env import get_pathes, env
from lib.roman import roman_to_int
from lib.utils import error, read_table

################## MAIN ######################


elem = "Na"
nmax = 10
osc = 1e-8
min_sp_num = 1
max_sp_num = 8

(name_to_table, num_to_table) = read_table()

env_errors = env()
if env_errors is not None:
    error(env_errors)

old_path, fit_path, exc_fac_path, ph_fac_path, my_dir = get_pathes()

my_dir = dirname(abspath(__file__))

elem_dir = os.path.join(my_dir, "db", elem)
levels_downloaded = os.path.join(my_dir, "db", elem, "levels")
lines_downloaded = os.path.join(my_dir, "db", elem, "lines")

if not os.path.exists(elem_dir):
    os.mkdir(elem_dir)

if not os.path.exists(levels_downloaded):
    os.mkdir(levels_downloaded)

if not os.path.exists(lines_downloaded):
    os.mkdir(lines_downloaded)

sp_num = "I"

outf = os.path.join(lines_downloaded, str(roman_to_int(sp_num)) + '.txt')
download_piter_lines_one_spnum(outf, elem, sp_num, nmax, osc)