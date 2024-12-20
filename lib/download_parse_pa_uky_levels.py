# https://linelist.pa.uky.edu/newpage/levels.html
# elmion: C I
# erange: 1000
# ener: eV
# nmax: 8
# jrange:
# auto: Show
# jval: useg
# sort: energy
# oddeven: mixed
# mode: Plain
import os
from urllib.parse import urlencode
from urllib.request import Request, urlopen

from lib.current_data import SPNUMS_TO_USE
from lib.roman import roman_to_int

LINES_SKIP = 10




def download_piter_levels(elem, piter_dir, nmax):
    if not os.path.exists(piter_dir):
        os.mkdir(piter_dir)
    for sp_num in SPNUMS_TO_USE:
        outf = os.path.join(piter_dir, str(roman_to_int(sp_num)) + '.txt')
        download_piter_levels_one_spnum(outf, elem, nmax, sp_num)


def download_piter_levels_one_spnum(file, elem, nmax, sp_num_roman):
    with open(file, "wb") as piter:
        values = {
            'elmion': elem + ' ' + sp_num_roman,
            'erange': '1000',
            'ener': 'eV',
            'nmax': nmax,
            'jrange': '',
            'auto': 'Suppress',
            'jval': 'useg',
            'sort': 'energy',
            'oddeven': 'mixed',
            'mode': 'Plain'
        }
        data = urlencode(values, doseq=True).encode()
        headers = {"Content-type": "application/x-www-form-urlencoded", "Accept": "text/plain"}
        req = Request("https://linelist.pa.uky.edu/newpage/cgi-bin/qlevels.cgi", data=data,
                      headers=headers, method="POST")
        with urlopen(req, timeout=5) as response:
            for i in range(LINES_SKIP):
                response.readline()
            piter.write(response.read())

