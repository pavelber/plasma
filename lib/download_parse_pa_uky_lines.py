# https://linelist.pa.uky.edu/newpage/lines.html
# wavl: 1-9000
# wave: Angstrom
# air: Vacuum
# radv:
# rvtp: vrad
# wacc:
# elmion: Fe I
# akival:
# akitype: Aki
# akizero: incl
# abun:
# depl:
# elo: 100000
# ener: eV
# ehi: 100000
# nmax:
# type: All
# auto: Suppress
# form: spec
# form: type
# form: term
# form: angm
# jval: usej
# tptype: as_a
# form: ener
# mode: Plain
# mlin: 5000
import os
from urllib.parse import urlencode
from urllib.request import Request, urlopen

from lib.current_data import SPNUMS_TO_USE
from lib.roman import roman_to_int

LINES_SKIP = 18


def download_piter_lines(elem, piter_dir, nmax, osc):
    if not os.path.exists(piter_dir):
        os.mkdir(piter_dir)
    for sp_num in SPNUMS_TO_USE:
        outf = os.path.join(piter_dir, str(roman_to_int(sp_num)) + '.txt')
        download_piter_lines_one_spnum(outf, elem, sp_num, nmax, osc)


# curl 'https://www.pa.uky.edu/~peter/newpage/cgi-bin/qlines.cgi'   -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9'   -H 'Accept-Language: en-US,en;q=0.9,he;q=0.8,ru;q=0.7,uk;q=0.6'   -H 'Cache-Control: max-age=0'   -H 'Connection: keep-alive'   -H 'Content-Type: application/x-www-form-urlencoded'   -H 'Origin: https://www.pa.uky.edu'   -H 'Referer: https://www.pa.uky.edu/~peter/newpage/index.html'   -H 'Sec-Fetch-Dest: document'   -H 'Sec-Fetch-Mode: navigate'   -H 'Sec-Fetch-Site: same-origin'   -H 'Sec-Fetch-User: ?1'   -H 'Upgrade-Insecure-Requests: 1'   -H 'User-Agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/104.0.0.0 Safari/537.36'   -H 'dnt: 1'   -H 'sec-ch-ua: "Chromium";v="104", " Not A;Brand";v="99", "Google Chrome";v="104"'   -H 'sec-ch-ua-mobile: ?0'   -H 'sec-ch-ua-platform: "Windows"'   -H 'sec-gpc: 1'   --data-raw 'wavl=1-9000&wave=Angstrom&air=Vacuum&radv=&rvtp=vrad&wacc=&elmion=Fe+VI&akival=&akitype=Aki&akizero=incl&abun=&depl=&elo=100000&ener=eV&ehi=100000&nmax=&type=All&auto=Show&form=spec&form=type&form=term&form=angm&jval=usej&tptype=as_a&form=ener&mode=Plain&mlin=50000'   --compressed > /mnt/c/work4/plasma/pa-uky-data/Fe/6.txt


def download_piter_lines_one_spnum(file, elem, sp_num_roman, nmax, osc):
    start_wave = 1
    end_wave = 20000
    step_wave = 2000
    with open(file, "wb") as piter:
        while start_wave < end_wave:
            download_piter_lines_one_spnum_wavelengts(piter, elem, sp_num_roman, nmax, osc, start_wave, start_wave + step_wave)
            start_wave += step_wave


def download_piter_lines_one_spnum_wavelengts(piter, elem, sp_num_roman, nmax, osc, wavel_from, wavel_to):
    values = {
        'elmion': elem + ' ' + sp_num_roman,
        'wavl': '%d-%d' % (wavel_from, wavel_to),
        'wave': 'Angstrom',
        'air': 'Vacuum',
        'radv': '',
        'rvtp': 'vrad',
        'wacc': '',
        'akival': osc,
        'akitype': 'f_ik',
        'akizero': 'excl',
        'abun': '',
        'depl': '',
        'elo': '100000',
        'ener': 'eV',
        'ehi': '100000',
        'nmax': nmax,
        'type': 'All',
        'auto': 'Suppress',
        'jval': 'useg',
        'tptype': 'as_a',
        'mode': 'Plain',
        'mlin': '50000',
        "form": ['spec', 'type', 'term', 'angm', 'ener', 'prob','conf'],
    }
    data = urlencode(values, doseq=True).encode()
    headers = {"Content-type": "application/x-www-form-urlencoded", "Accept": "text/plain"}
    req = Request("https://linelist.pa.uky.edu/newpage/cgi-bin/qlines.cgi", data=data,
                  headers=headers, method="POST")
    with urlopen(req, timeout=5) as response:
        print(sp_num_roman, wavel_from, wavel_to, response.status, response.reason)

        for i in range(LINES_SKIP):
            response.readline()
        piter.write(response.read())
