import os
from urllib.parse import urlencode
from urllib.request import urlopen, Request

# For IN1.INP
# curl 'https://physics.nist.gov/cgi-bin/ASD/energy1.pl?de=0&spectrum=Fe+I&units=1&format=2&output=0&page_size=100&multiplet_ordered=1&conf_out=on&term_out=on&level_out=on&j_out=on&g_out=on&temp=&submit=Retrieve+Data' \
#   -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9' \
#   -H 'Accept-Language: en-US,en;q=0.9,he;q=0.8,ru;q=0.7,uk;q=0.6' \
#   -H 'Connection: keep-alive' \
#   -H 'Referer: https://physics.nist.gov/PhysRefData/ASD/levels_form.html' \
#   -H 'Sec-Fetch-Dest: document' \
#   -H 'Sec-Fetch-Mode: navigate' \
#   -H 'Sec-Fetch-Site: same-origin' \
#   -H 'Sec-Fetch-User: ?1' \
#   -H 'Upgrade-Insecure-Requests: 1' \
#   -H 'User-Agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/105.0.0.0 Safari/537.36' \
#   -H 'dnt: 1' \
#   -H 'sec-ch-ua: "Google Chrome";v="105", "Not)A;Brand";v="8", "Chromium";v="105"' \
#   -H 'sec-ch-ua-mobile: ?0' \
#   -H 'sec-ch-ua-platform: "Windows"' \
#   -H 'sec-gpc: 1' \
#   --compressed
from lib.roman import roman_to_int, dec_to_roman

sp_nums_to_use = ['I', 'II', 'III', 'IV', 'V', 'VI']


# sp_nums_to_use = ['I', 'II', 'III', 'IV', 'V', 'VI', 'VII']

def clean_num(n):
    n.replace('"=""', '').replace('"""', '')


def download_nist_for_in1(element, nist_dir):
    if not os.path.exists(nist_dir):
        os.mkdir(nist_dir)

    for sp_num in sp_nums_to_use:
        download_one_sp_num(element, nist_dir, sp_num)


def download_one_sp_num(element, nist_dir, sp_num):
    values = {
        'spectrum': element + ' ' + sp_num
    }
    params = urlencode(
        values) + '&de=0&units=1&format=2&output=0&page_size=100&multiplet_ordered=1&conf_out=on&term_out=on&level_out=on&j_out=on&g_out=on&temp=&submit=Retrieve+Data'
    headers = {
        'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9',
        'Accept-Language': 'en-US,en;q=0.9,he;q=0.8,ru;q=0.7,uk;q=0.6',
        'Connection': 'keep-alive',
        'Referer': 'https://physics.nist.gov/PhysRefData/ASD/levels_form.html',
        'Sec-Fetch-Dest': 'document',
        'Sec-Fetch-Mode': 'navigate',
        'Sec-Fetch-Site': 'same-origin',
        'Sec-Fetch-User': '?1',
        'Upgrade-Insecure-Requests': '1',
        'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/105.0.0.0 Safari/537.36',
        'dnt': '1',
        'sec-ch-ua': '"Google Chrome";v="105", "Not)A;Brand";v="8", "Chromium";v="105"',
        'sec-ch-ua-mobile': '?0',
        'sec-ch-ua-platform': '"Windows"',
        'sec-gpc': '1',

    }
    req = Request("https://physics.nist.gov/cgi-bin/ASD/energy1.pl?" + params,
                  headers=headers, )
    with urlopen(req, timeout=5) as f:
        print(f.status, f.reason)

        data = f.read()
        outf = os.path.join(nist_dir, str(roman_to_int(sp_num)) + '.csv')
        print(outf)
        with open(outf, 'wb') as out:
            out.write(data)


def get_ionization_energy_ev(element, sp_num):
    values = {
        'spectra': element + ' ' + dec_to_roman(sp_num)
    }
    params = urlencode(
        values) + '&submit=Retrieve+Data&units=1&format=2&order=0&at_num_out=on&sp_name_out=on&ion_charge_out=on&el_name_out=on&seq_out=on&shells_out=on&level_out=on&ion_conf_out=on&e_out=0&unc_out=on&biblio=on'
    headers = {
        'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9',
        'Accept-Language': 'en-US,en;q=0.9,he;q=0.8,ru;q=0.7,uk;q=0.6',
        'Connection': 'keep-alive',
        'Sec-Fetch-Dest': 'document',
        'Sec-Fetch-Mode': 'navigate',
        'Sec-Fetch-Site': 'same-origin',
        'Sec-Fetch-User': '?1',
        'Upgrade-Insecure-Requests': '1',
        'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/105.0.0.0 Safari/537.36',
        'dnt': '1',
        'sec-ch-ua': '"Google Chrome";v="105", "Not)A;Brand";v="8", "Chromium";v="105"',
        'sec-ch-ua-mobile': '?0',
        'sec-ch-ua-platform': '"Windows"',
        'sec-gpc': '1',

    }
    req = Request("https://physics.nist.gov/cgi-bin/ASD/ie.pl?" + params,
                  headers=headers, )
    with urlopen(req, timeout=5) as f:
        print(f.status, f.reason)

        data = f.read().decode('utf-8')
        response_lines = data.split("\n")
        header = response_lines[0].split(",")
        fields = response_lines[1].split(",")
        index = header.index("Ionization Energy (eV)")
        eV = fields[index]
        return float(eV.replace('"""', '').replace('"=""', ''))
