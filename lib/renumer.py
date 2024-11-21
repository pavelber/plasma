import os


def get_ion_potential(out_dir, spectral_num):
    fac_lev = out_dir + os.path.sep + spectral_num + os.path.sep + 'fac.lev'
    with open(fac_lev, 'r') as inf:
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


def create_translation_table(out_dir, spectral_num, potential):
    # print("+++++++++++++++++++"+spectral_num)
    fac_lev = out_dir + os.path.sep + spectral_num + os.path.sep + 'fac.lev'
    table = {}
    count_autoion = -1
    with open(fac_lev, 'r') as fac_lev:
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
                    # print(line)
                    if energy < potential:
                        table[str(int(n) + 1)] = str(int(n) + 1)
                        # print(n+" "+str(int(n)+1))
                    else:
                        table[str(int(n) + 1)] = str(count_autoion)
                        count_autoion -= 1
    return table


def create_tables(out_dir):
    i_spectro = list(
        map(lambda x: str(x), sorted(map(lambda x: int(x), filter(lambda f: f.isdigit(), os.listdir(out_dir))))))

    ionization_potential = {}

    for num in i_spectro:
        ionization_potential[num] = 100000.0

    translation_table = {}

    for num in i_spectro:
        if ionization_potential[num] or True:
            translation_table[num] = create_translation_table(out_dir, num, ionization_potential[num])

    return ionization_potential, translation_table
