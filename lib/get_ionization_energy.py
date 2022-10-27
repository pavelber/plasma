import httplib
import re

from lib.utils import dec_to_roman


def get_ionization_energy_ev(elem, sp_num):
    params = 'spectra=' + elem + '+' + dec_to_roman(
        sp_num) + '&submit=Retrieve+Data&units=1&format=1&order=3&at_num_out=on&sp_name_out=on&ion_charge_out=on&el_name_out=on&seq_out=on&shells_out=on&level_out=on&ion_conf_out=on&e_out=0&unc_out=on&biblio=on'
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
    conn = httplib.HTTPSConnection("physics.nist.gov")
    conn.request("GET", "/cgi-bin/ASD/ie.pl" + '?' + params, "", headers)
    response = conn.getresponse()
    print response.status, response.reason

    table_lines = 0
    response_lines = response.read().split("\n")
    for line in response_lines:
        if table_lines > 0:
            table_lines += 1

        if line.startswith("At. num"):
            table_lines = 1
        if table_lines == 3:
            parts = line.split("|")
            eV = parts[8].strip()
            if eV.startswith("<"):
                eV = re.sub(r'<.*?>', '', eV).replace("[","").replace("]","")
    conn.close()
    return float(eV)
