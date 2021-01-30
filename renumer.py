import os
import sys


def get_ion_potential(spectral_num):
    fac_lev = out_dir + os.path.sep + spectral_num + os.path.sep + 'fac.lev'
    with open(fac_lev, 'rb') as inf:
        return read_ion_potential_from_fac_lev(inf)


def read_ion_potential_from_fac_lev(fac_lev):
    ion_num = 0
    for line in fac_lev:
        parts = line.split()
        if len(parts) > 0 and parts[0] == 'ILEV':
            ion_num += 1
        else:
            if ion_num == 2:
                return float(parts[2])


def create_translation_table(spectral_num, potential):
    fac_lev = out_dir + os.path.sep + spectral_num + os.path.sep + 'fac.lev'
    table = {}
    count_autoion = -1
    with open(fac_lev, 'rb') as fac_lev:
        take_lines = False
        for line in fac_lev:
            parts = line.split()
            if len(parts) > 0 and parts[0] == 'ILEV':
                take_lines = True
            else:
                if take_lines:
                    if len(parts) == 0:
                        return table
                    n = parts[0]
                    energy = float(parts[2])
                    if energy < potential:
                        table[n] = n
                    else:
                        table[n] = count_autoion
                        count_autoion -= 1



out_dir = os.path.abspath(sys.argv[1])
i_spectro = map(lambda x: str(x), sorted(map(lambda x: int(x), filter(lambda f: f.isdigit(), os.listdir(out_dir)))))

ionization_potential = {}

for num in i_spectro:
    ionization_potential[num] = get_ion_potential(num)

print(ionization_potential)

translation_table = {}

for num in i_spectro:
    translation_table[num] = create_translation_table(num, ionization_potential[num])

print(translation_table)