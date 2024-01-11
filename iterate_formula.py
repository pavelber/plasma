def fun(a, b, c, d, x):
    return (a + b / x + c / (x * x)) * (1 / (pow(x, (7.0 / 2.0 + d))))


def iterate(a, b, c, d, start, end, step):
    i = start
    while i < end + step:
        v = fun(a, b, c, d, i)
#        print("%e,%e" % (i, v))
        print("%e" % ( v))
        i += step
    return sum


# 1,106,8
#iterate(2.000e-03, -2.000e-03, 8.691E-17, -2.452E-01, 2.0, 100.0, 2.0)
print ("--------------------------")
# 1,106,9
#ralchenko iterate(1.623E-16, -7.671E-16, 9.206E-16, -8.216E-02, 2.0, 100.0, 2.0)
#iterate( 5.25026710e-17, -1.80287407e-16 , 1.78105938e-16, -4.61471272e-01, 2.0, 100.0, 2.0)
iterate( 1.57043124e-16, -7.15028143e-16,  8.30604398e-16,  1.85677177e-01, 2.0, 100.0, 2.0)

