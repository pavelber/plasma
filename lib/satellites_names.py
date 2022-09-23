import os

from lib.utils import read_table


def read_satellites_names(el_num):
    table = read_table()
    el_name = table[1][str(el_num)]['Element']
    reading = False
    path = os.path.dirname(__file__)
    if path == "" or path is None:
        path = ".."
    names_file_path = path + os.path.sep + "satellites-names.csv"
    names = {}
    with open(names_file_path, "rb") as name_file:
        name_file.readline()
        for line in name_file:
            striped = line.strip()
            parts = striped.split(",")
            if len(parts) < 3 and reading:
                return names
            if len(striped) == 0:
                continue
            elif len(parts) == 1:
                if el_name == parts[0]:
                    reading = True
            elif reading:
                wavelength = "%.4f" % float(parts[0])
                names[wavelength] = parts


print(read_satellites_names(12))
