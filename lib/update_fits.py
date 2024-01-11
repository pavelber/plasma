import os

import numpy as np
from scipy.optimize import minimize


def fun(a, b, c, d, x):
    return (a + b / x + c / (x * x)) * (1 / (pow(x, (7.0 / 2.0 + d))))


def create_new_fits_for_rrec2(elem_dir, method, from_new_tol_old, new_filts_filenae, old_fits_filename):
    file_name = os.path.join(elem_dir, new_filts_filenae)
    original_file = os.path.join(elem_dir, old_fits_filename)
    # shutil.copyfile(file_name, )
    with open(file_name, "w") as fwrite:
        with open(original_file, "r") as f:
            for l in f:
                parts = l.split()
                sp_num = parts[0]
                level_from = parts[1]
                level_to = parts[2]
                av = float(parts[4])
                bv = float(parts[5])
                cv = float(parts[6])
                dv = float(parts[7])
                new_level_from = from_new_tol_old[sp_num][level_from]
                new_level_to = from_new_tol_old[str(int(sp_num) + 1)][level_to]
                data_file_name = os.path.join(elem_dir, sp_num, "%s_%s_%s.txt" % (sp_num, new_level_from, new_level_to))
                sigma_map = {}
                if os.path.exists(data_file_name):
                    with open(data_file_name, "r") as df:
                        for dl in df:
                            dparts = dl.split(",")
                            relative_e = int(float(dparts[0]))
                            sigma = float(dparts[1])
                            sigma_map[relative_e] = sigma
                else:
                    print("o", end="")
                    continue

                def data_fun(e):
                    return sigma_map[e]

                def def_fun_to_minimize(x):
                    a = x[0]
                    b = x[1]
                    c = x[2]
                    d = x[3]

                    start_e = 2
                    end_e = 100
                    e = start_e
                    sum = 0
                    while e <= end_e:
                        v = fun(a, b, c, d, float(e))
                        expected_v = data_fun(e)
                        sum += (v - expected_v) * (v - expected_v)
                        e += 2
                    return sum

                if av > 10e-6:
                    x0 = np.array([newa, newb, newc, newd])
                else:
                    x0 = np.array([av, bv, cv, dv])
                res = minimize(def_fun_to_minimize, x0, method=method)
                if res.success:
                    newa = res.x[0]
                    newb = res.x[1]
                    newc = res.x[2]
                    newd = res.x[3]
                    fwrite.write(
                        "%s%11.3e %11.3e %11.3e %11.3e   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00\n" % (
                            l[0:22], newa, newb, newc, newd))
                    print(".", end="")
                else:
                    fwrite.write(l)
                    print("X", end="")
