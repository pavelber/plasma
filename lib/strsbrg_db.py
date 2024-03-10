import re
from collections import defaultdict
from os.path import join
from urllib.request import Request, urlopen

from lib.utils import read_table, add_one_to_config_in_missing

(name_to_table, num_to_table) = read_table()
URL_LINES_SKIP = 8
FILE_LINES_SKIP = 3


def extract_config(config):
    config = re.sub(r'\([^)]*\)', ' ', config)
    return list(map(lambda c: add_one_to_config_in_missing(c), config.split()))
    pass


def download_levels(elem, sp_num_dec):
    nz = int(name_to_table[elem]["AtomicNumber"])
    ne = nz + 1 - sp_num_dec

    # https://cdsweb.u-strasbg.fr/cgi-bin/topbase/topbase.sh?com=dt&ent=e&nz1=8&nz2=8&ne1=7&ne2=7&is1=1&is2=9&il1=0&il2=9&ip1=0&ip2=1&lv1=0&lv2=0&en1=0.0&en2=0.0&so=s2&iconf=on&e=on&te=on&gi=on

    url = "https://cdsweb.u-strasbg.fr/cgi-bin/topbase/topbase.sh?com=dt&ent=e&nz1=%s&nz2=%s&ne1=%s&ne2=%s&is1=1&is2=9&il1=0&il2=9&ip1=0&ip2=1&lv1=0&lv2=0&en1=0.0&en2=0.0&so=s2&iconf=on&e=on&te=on&gi=on" \
          % (nz, nz, ne, ne)
    headers = {"Content-type": "application/x-www-form-urlencoded", "Accept": "text/plain"}
    req = Request(url,
                  headers=headers, method="GET")
    with urlopen(req, timeout=5) as response:
        for i in range(URL_LINES_SKIP):
            response.readline()
        levels = defaultdict(list)
        for line in response:
            parts = line.decode('utf-8').split()
            num_parts = len(parts)
            num_i = parts[0]
            if num_parts < 8:
                continue

            energy = parts[num_parts - 2]
            if num_parts == 9:
                config = extract_config(parts[5])
            else:
                config = extract_config(parts[5] + " " + parts[6])
            stat_weight = parts[num_parts - 1]
            levels[str(config)].append((config, stat_weight, num_i, energy))
    return levels


def parse_levels(f_name):
    with open(f_name, "r") as f:
        for i in range(FILE_LINES_SKIP):
            f.readline()
        levels = defaultdict(list)
        for line in f:
            parts = line.split()
            num_parts = len(parts)
            num_i = parts[0]
            if num_parts < 8:
                continue

            energy = parts[num_parts - 2]
            if num_parts == 9:
                config = extract_config(parts[5])
            else:
                config = extract_config(parts[5] + " " + parts[6])
            stat_weight = parts[num_parts - 1]
            levels[str(config)].append((config, stat_weight, num_i, energy))
    return levels


def download_levels_to_file(elem, dir, min_sp, max_sp):
    for n in range(min_sp, max_sp + 1):
        download_one_level_to_file(elem, n, join(dir, str(n) + ".txt"))


def download_one_level_to_file(elem, sp_num_dec, file):
    nz = int(name_to_table[elem]["AtomicNumber"])
    ne = nz + 1 - sp_num_dec

    url = "https://cdsweb.u-strasbg.fr/cgi-bin/topbase/topbase.sh?com=dt&ent=e&nz1=%s&nz2=%s&ne1=%s&ne2=%s&is1=1&is2=9&il1=0&il2=9&ip1=0&ip2=1&lv1=0&lv2=0&en1=0.0&en2=0.0&so=s2&iconf=on&e=on&te=on&gi=on" \
          % (nz, nz, ne, ne)
    headers = {"Content-type": "application/x-www-form-urlencoded", "Accept": "text/plain"}
    req = Request(url,
                  headers=headers, method="GET")
    with urlopen(req, timeout=5) as response:
        for i in range(URL_LINES_SKIP):
            response.readline()
        with open(file, "wb") as f:
            f.write(response.read())


def normalize_config(conf):
    m = list(map(lambda c: add_one_to_config_in_missing(c), filter(lambda c: c[0:1] != '(', conf.split("."))))
    if len(m) == 4:
        m.pop(-1)
    if len(m) == 3:
        m.pop(0)
    return m


def parse_piter_levels(fil):
    levels = defaultdict(list)
    with open(fil, "r") as inf:
        n = 1
        prev_energy_str = None
        for line in inf:
            parts = line.split()
            if ".nd" in parts[0]:
                continue
            if len(parts) == 5:
                configs = normalize_config(parts[0])
                configs_copy_for_csv = configs.copy()
                if configs_copy_for_csv[0] == "":
                    configs_copy_for_csv.pop(0)
                if len(configs_copy_for_csv) == 3:
                    configs_copy_for_csv.pop(0)

                if len(configs) == 1:
                    configs.insert(0, "")

                term = parts[1]
                g = parts[2]
                energy_str = parts[3]

                levels[str(configs_copy_for_csv)].append((term, n, g, energy_str))
                n += 1
    return levels


def download_cuts_to_file(elem, dir, min_sp, max_sp):
    for n in range(min_sp, max_sp + 1):
        download_one_level_cut_to_file(elem, n, join(dir, str(n) + ".txt"))


def download_one_level_cut_to_file(elem, sp_num_dec, file):
    nz = int(name_to_table[elem]["AtomicNumber"])
    ne = nz + 1 - sp_num_dec

    url = "https://cdsweb.u-strasbg.fr/cgi-bin/topbase/topbase.sh?com=dt&ent=p&nz1=%s&nz2=%s&ne1=%s&ne2=%s&is1=1&is2=9&il1=0&il2=9&ip1=0&ip2=1&lv1=0&lv2=0&en1=0.0&en2=0.0&so=s2" \
          % (nz, nz, ne, ne)
    headers = {"Content-type": "application/x-www-form-urlencoded", "Accept": "text/plain"}
    req = Request(url,
                  headers=headers, method="GET")
    with urlopen(req, timeout=5) as response:
        for i in range(URL_LINES_SKIP):
            response.readline()
        with open(file, "wb") as f:
            f.write(response.read())


def normalize_config(conf):
    m = list(map(lambda c: add_one_to_config_in_missing(c), filter(lambda c: c[0:1] != '(', conf.split("."))))
    if len(m) == 4:
        m.pop(-1)
    if len(m) == 3:
        m.pop(0)
    return m


def parse_piter_levels(fil):
    levels = defaultdict(list)
    with open(fil, "r") as inf:
        n = 1
        prev_energy_str = None
        for line in inf:
            parts = line.split()
            if ".nd" in parts[0]:
                continue
            if len(parts) == 5:
                configs = normalize_config(parts[0])
                configs_copy_for_csv = configs.copy()
                if configs_copy_for_csv[0] == "":
                    configs_copy_for_csv.pop(0)
                if len(configs_copy_for_csv) == 3:
                    configs_copy_for_csv.pop(0)

                if len(configs) == 1:
                    configs.insert(0, "")

                term = parts[1]
                g = parts[2]
                energy_str = parts[3]

                levels[str(configs_copy_for_csv)].append((term, n, g, energy_str))
                n += 1
    return levels

#
# strsbrg_levels = download_levels("O", 2)
#
# piter_levels = parse_piter_levels("..\\db\\O\\levels\\2.txt")
#
# for config in piter_levels:
#     per_config = piter_levels[config]
#     grouped_dict = {}
#     for item in per_config:
#         term = item[0]
#         if term in grouped_dict:
#             grouped_dict[term].append(item)
#         else:
#             grouped_dict[term] = [item]
#     piter_levels[config] = grouped_dict
# #
# for config in piter_levels:
#     per_term = piter_levels[config]
#     strsb = strsbrg_levels[config]
#     st_weight = int(strsb[1])
#     result = list(filter(lambda levels: True, per_term.values()))
#     if not result:
#         print(config + " not found")
