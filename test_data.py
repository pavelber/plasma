import os
import sys
from math import log

from lib.bcfp import BFCP_From_databases
from lib.consts import ry
from lib.cross_section import get_constants_for_bernshtam_ralchenko
from lib.in1 import IN1
from lib.utils import read_table
from test_data_for_kr_data_o import test_data


def get_fisher_om(xw, a, b, c, d):
    yw = 1.0 - 1 / xw
    return a * log(xw) + b * yw * yw + c * yw / xw + d * yw / xw / xw


def get_lotz_om(e, num_of_electrons):
    return 4.5 * 10E-14 * num_of_electrons * log(e) / e


def get_lotz_om2(e, num_of_electrons, ionization_energy):
    if e < 1:  # Ensure E >= Ei
        return 0.0
    a = 4.5e-14  # cm² eV²
    return (a * num_of_electrons * log(e) / (e * ionization_energy * ionization_energy))


def get_bp_om(e, c_l, delta_l, num_of_electrons, branching_ration, ionization_energy):
    return c_l * pow(ry / ionization_energy, 2 - delta_l) * num_of_electrons * branching_ration * log(e) / e


################## MAIN ######################


param_num = 1
elem_dir = os.path.abspath(sys.argv[param_num])
param_num += 1
bfcp = os.path.join(elem_dir, "BFCP.INP")

(name_to_table, num_to_table) = read_table()
path_to_in1_inp = os.path.join(elem_dir, "IN1.INP")
in1_data = IN1(path_to_in1_inp)
##############################

from_sp = "8"
from_level = "1"
to_sp = "9"
to_level = "1"
########################
#e_gr = [0.48164, 3.1515, 6.5252, 10.788,16.172,22.973,31.56,42.4,56.076,73.324,95.061,122.43,156.87,200.13,254.4]
#om = [3.8934E-18,2.2207E-17,4.04E-17,5.7947E-17,7.3529E-17,8.5767E-17,9.3753E-17,9.722E-17,9.6545E-17,9.2533E-17,8.6151E-17,7.8336E-17,6.9875E-17,6.137E-17,5.3232E-17]
#e_gr = [1.7871,9.1016,17.836,28.262,40.705,55.549,73.251,94.349,119.48,149.4,184.98,227.25,277.43,336.9,407.27]
#om = [8.6002E-19,4.3367E-18,7.748E-18,1.0607E-17,1.2734E-17,1.412E-17,1.4846E-17,1.5028E-17,1.4786E-17,1.4233E-17,1.3464E-17,1.256E-17,1.1582E-17,1.0581E-17,9.5906E-18]
#e_gr = [4.6302,21.088,40.302,62.722,88.871,119.35,154.84,196.15,244.16,299.92,364.58,439.45,526,625.86,740.83]
#om = [9.0635E-19,3.5657E-18,5.4772E-18,6.653E-18,7.264E-18,7.4742E-18,7.4105E-18,7.1655E-18,6.8054E-18,6.3774E-18,5.9151E-18,5.4423E-18,4.9754E-18,4.5256E-18,4.1E-18]
#e_gr = [2.9603,15.755,31.165,49.714,72.031,98.863,131.1,169.79,216.17,271.71,338.1,417.31,511.63,623.64,756.27]
#om = [1.7246E-19,8.1461E-19,1.3832E-18,1.8406E-18,2.1746E-18,2.3899E-18,2.5004E-18,2.524E-18,2.4789E-18,2.3824E-18,2.2497E-18,2.0939E-18,1.9256E-18,1.7534E-18,1.5836E-18]
#e_gr = [3.8152,20.244,40.011,63.78,92.34,126.63,167.76,217.03,275.97,346.36,430.27,530.07,648.43,788.4,953.33]
#om = [1.5511E-19,7.0521E-19,1.1763E-18,1.558E-18,1.8461E-18,2.0433E-18,2.1571E-18,2.1982E-18,2.179E-18,2.1125E-18,2.0109E-18,1.8853E-18,1.7454E-18,1.5991E-18,1.4525E-18]
#e_gr = [6.5479,45.678,95.743,159.67,241.11,344.52,475.33,640.02,846.18,1102.5,1418.5,1804.6,2271.3,2828.7,3486.2]
#om = [8.0541E-20,4.1197E-19,6.2177E-19,7.3329E-19,7.718E-19,7.5982E-19,7.1569E-19,6.5337E-19,5.8307E-19,5.1174E-19,4.4385E-19,3.819E-19,3.2707E-19,2.7957E-19,2.3907E-19]
#e_gr = [56.412,297.99,582.84,916.65,1304.8,1752.7,2265.4,2847.1,3501.3,4230.8,5036.9,5920.3,6880.5,7916.2,9025.9]
#om = [8.0269E-21,3.3238E-20,4.8715E-20,5.677E-20,5.9926E-20,6.0022E-20,5.8278E-20,5.5485E-20,5.2161E-20,4.8637E-20,4.5121E-20,4.174E-20,3.8564E-20,3.5625E-20,3.2933E-20]
e_gr = [43.57,222.78,433.76,680.88,968.8,1302.1,1685.5,2123.3,2619.6,3177.8,3800.9,4490.9,5249.1,6075.9,6971.3]
om = [2.1607E-21,9.3158E-21,1.4765E-20,1.8494E-20,2.0774E-20,2.1929E-20,2.2255E-20,2.1995E-20,2.1338E-20,2.0427E-20,1.9371E-20,1.8245E-20,1.7106E-20,1.599E-20,1.4921E-20]
###############################
from_config = in1_data.get_config(from_sp, from_level)
to_config = in1_data.get_config(to_sp, to_level)
energy = in1_data.get_ionization_energy(from_sp, from_level,
                                        to_sp, to_level)

(c_l, delta_l, num_of_electrons) = get_constants_for_bernshtam_ralchenko(
    from_config,
    to_config)
bfcp_data = BFCP_From_databases(bfcp)
key = (from_sp, from_level, to_sp, to_level)
coefficient = bfcp_data.get_branching_coefficient(key)
###################################

print( "EGrid,x,Сечение у Олега,Сечение у Паши (Бернштам/Ралченко),Сечение у Паши (Лотц),Cl,Deltal, количество электронов в субоболочке, коэффициент ветвления")
for i in range(0, len(e_gr)):
    e_gr[i] = e_gr[i]
    e = (e_gr[i]+energy)/energy
    bp_om = get_bp_om(e, c_l, delta_l, num_of_electrons, coefficient, energy)
    lotz_om = get_lotz_om2(e, num_of_electrons, energy)
    print( "%13.3e,%13.3e,%13.3e,%13.3e,%13.3e,%13.3e,%13.3e, %d, %13.3e"% (e_gr[i],e,om[i], bp_om, lotz_om, c_l, delta_l,num_of_electrons,coefficient))
    #e += STEP_E
