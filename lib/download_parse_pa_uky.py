import httplib
# https://www.pa.uky.edu/~peter/newpage/index.html
# https://www.pa.uky.edu/~peter/newpage/cgi-bin/qlines.cgi
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
# auto: Show
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
import urllib

from lib.roman import roman_to_int

sp_nums_to_use = ['I', 'II', 'III', 'IV', 'V', 'VI', 'VII']


def download_piter(elem, dir):
    for sp_num in sp_nums_to_use:
        outf = os.path.join(os.path.join(dir, elem), str(roman_to_int(sp_num)) + '.txt')
        download_piter_one_spnum(outf, elem, sp_num)


# https://curlconverter.com/
# curl 'https://www.pa.uky.edu/~peter/newpage/cgi-bin/qlines.cgi'   -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9'   -H 'Accept-Language: en-US,en;q=0.9,he;q=0.8,ru;q=0.7,uk;q=0.6'   -H 'Cache-Control: max-age=0'   -H 'Connection: keep-alive'   -H 'Content-Type: application/x-www-form-urlencoded'   -H 'Origin: https://www.pa.uky.edu'   -H 'Referer: https://www.pa.uky.edu/~peter/newpage/index.html'   -H 'Sec-Fetch-Dest: document'   -H 'Sec-Fetch-Mode: navigate'   -H 'Sec-Fetch-Site: same-origin'   -H 'Sec-Fetch-User: ?1'   -H 'Upgrade-Insecure-Requests: 1'   -H 'User-Agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/104.0.0.0 Safari/537.36'   -H 'dnt: 1'   -H 'sec-ch-ua: "Chromium";v="104", " Not A;Brand";v="99", "Google Chrome";v="104"'   -H 'sec-ch-ua-mobile: ?0'   -H 'sec-ch-ua-platform: "Windows"'   -H 'sec-gpc: 1'   --data-raw 'wavl=1-9000&wave=Angstrom&air=Vacuum&radv=&rvtp=vrad&wacc=&elmion=Fe+VI&akival=&akitype=Aki&akizero=incl&abun=&depl=&elo=100000&ener=eV&ehi=100000&nmax=&type=All&auto=Show&form=spec&form=type&form=term&form=angm&jval=usej&tptype=as_a&form=ener&mode=Plain&mlin=50000'   --compressed > /mnt/c/work4/plasma/pa-uky-data/Fe/6.txt


def download_piter_one_spnum(file, elem, sp_num_roman):
    values = {
        'elmion': elem + ' ' + sp_num_roman,
        'wavl': '1-9000',
        'wave': 'Angstrom',
        'air': 'Vacuum',
        'radv': '',
        'rvtp': 'vrad',
        'wacc': '',
        'akival': '1e5',
        'akitype': 'Aki',
        'akizero': 'incl',
        'abun': '',
        'depl': '',
        'elo': '100000',
        'ener': 'eV',
        'ehi': '100000',
        'nmax': '',
        'type': 'All',
        'auto': 'Show',
        'jval': 'usej',
        'tptype': 'as_a',
        'mode': 'Plain',
        'mlin': '50000'
    }

    params = urllib.urlencode(values) + '&form=spec&form=type&form=term&form=angm&form=ener&form=prob'
    headers = {"Content-type": "application/x-www-form-urlencoded", "Accept": "text/plain"}
    conn = httplib.HTTPSConnection("www.pa.uky.edu")
    conn.request("POST", "/~peter/newpage/cgi-bin/qlines.cgi", params, headers)
    response = conn.getresponse()
    print response.status, response.reason

    data = response.read()
    with open(file, "wb") as piter:
        piter.write(data)
    conn.close()