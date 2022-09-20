import httplib
import os
import urllib


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
from lib.roman import roman_to_int


def download_nist_for_in1(element, dir):
    for sp_num in ['I', 'II', 'III', 'IV', 'V', 'VI', 'VII']:
        values = {
            'spectrum': element + ' ' + sp_num
        }

        params = urllib.urlencode(
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
        conn = httplib.HTTPSConnection("physics.nist.gov")
        conn.request("GET", "/cgi-bin/ASD/energy1.pl" + '?' + params, "", headers)
        response = conn.getresponse()
        print response.status, response.reason

        data = response.read()
        outf = os.path.join(os.path.join(dir,element), str(roman_to_int(sp_num)) + '.csv')
        print(outf)
        with open(outf, 'wb') as out:
            out.write(data)
        conn.close()


#download_nist_for_in1("Fe", 'C:\\work4\\plasma\\nist-data\\')
