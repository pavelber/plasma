import re
from collections import defaultdict
from dataclasses import dataclass
from os.path import join, dirname, abspath
from typing import List
from urllib.request import Request, urlopen

from lib.utils import read_table, add_one_to_config_in_missing, energy_ryd_to_ev

(name_to_table, num_to_table) = read_table()
URL_LINES_SKIP = 8
FILE_LINES_SKIP = 3
FILE_CUTS_SKIP = 3

base_dir = dirname(abspath(__file__))
strsbrg_levels = {}
strsbrg_cuts = {}


@dataclass
class Transition:
    level_from: int
    level_to: int
    islp: str
    energy: float
    cuts: dict


@dataclass
class Level:
    config: List[str]
    stat_weight: int
    num_i: int
    energy: float
    islp: str


def extract_config(config):
    config = re.sub(r'\([^)]*\)', ' ', config)
    return list(map(lambda c: add_one_to_config_in_missing(c), config.split()))
    pass


def parse_cuts(f_name):
    with open(f_name, "r") as f:
        for i in range(FILE_CUTS_SKIP):
            f.readline()
        transitions = {}

        for line in f:
            parts = line.split()
            num_parts = len(parts)
            if num_parts == 7:
                level_from = int(parts[0])
                level_to = int(parts[4])
                islp = parts[3]
                energy = energy_ryd_to_ev(float(parts[5]))
                cuts = {}
                transition = Transition(level_from, level_to, islp, energy, cuts)
                transitions[level_from] = transition
            elif num_parts == 2:
                cuts[parts[0]] = parts[1]

    return transitions


def parse_levels(f_name):
    with open(f_name, "r") as f:
        for i in range(FILE_LINES_SKIP):
            f.readline()
        levels = defaultdict(list)
        for line in f:
            parts = line.split()
            num_parts = len(parts)
            if num_parts < 8:
                continue
            num_i = int(parts[0])
            islp = parts[3]
            energy = energy_ryd_to_ev(float(parts[num_parts - 3]))
            if num_parts == 9:
                config = extract_config(parts[5])
            else:
                config = extract_config(parts[5] + " " + parts[6])
            stat_weight = int(float(parts[num_parts - 1]))
            levels[str(config)].append(Level(config, stat_weight, num_i, energy, islp))
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


def read_strsbrg_db(elem, sp_nums, nucleus):
    for s_n in sp_nums:
        if s_n != nucleus:
            levels_file = join(base_dir, "..", "db", elem, "strasbg-levels", "%s.txt" % s_n)
            cuts_file = join(base_dir, "..", "db", elem, "strasbg-cuts", "%s.txt" % s_n)
            strsbrg_levels[s_n] = parse_levels(levels_file)
            strsbrg_cuts[s_n] = parse_cuts(cuts_file)

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
