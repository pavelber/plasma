import os


def fun(a, b, c, d, x):
    return (a + b / x + c / (x * x)) * (1 / (pow(x, (7.0 / 2.0 + d))))


def integrate(a, b, c, d, start, end, step):
    sum = 0
    i = start
    while i < end + step:
        i1 = fun(a, b, c, d, i)
        i2 = fun(a, b, c, d, i + step)
        sum += (i1 + i2) / 2.0
        i += step
    return sum


def add_cut_comment(out_dir, out_file_name):
    rrec_path = os.path.join(out_dir, "RREC-fits.INP")
    out_path = os.path.join(out_dir, out_file_name)
    with open(rrec_path, "r") as rrec:
        with open(out_path, "w") as rrec_out:
            for line in rrec:
                parts = line.split()
                sp = parts[0]
                level1 = parts[1]
                level2 = parts[2]
                a = float(parts[4])
                b = float(parts[5])
                c = float(parts[6])
                d = float(parts[7])
                cut = integrate(a, b, c, d, 1.0, 100.0, 1.0)
                rrec_out.write("%s # %10.8e\n" % (line[:-1], cut))


def mark_large_cuts(out_dir, in_file_name, out_file_name):
    in_path = os.path.join(out_dir, in_file_name)
    out_path = os.path.join(out_dir, out_file_name)
    n = {}
    sum = {}
    with open(in_path, "r") as rrec_in:
        for line in rrec_in:
            parts = line.split()
            sp = parts[0]
            cut = float(parts[15])
            if sp not in sum:
                sum[sp] = 0.0
                n[sp] = 0
            sum[sp] += cut
            n[sp] += 1

    with open(in_path, "r") as rrec_in:
        with open(out_path, "w") as rrec_out:
            for line in rrec_in:
                parts = line.split()
                sp = parts[0]
                cut = float(parts[15])
                if cut / (sum[sp] / n[sp]) > 5:
                    comment = " LARGE"
                else:
                    comment = ""
                rrec_out.write("%s # %s\n" % (line[:-1], comment))


add_cut_comment("C:\\work4\\db\\O", "RREC-tmp.INP")
mark_large_cuts("C:\\work4\\db\\O", "RREC-tmp.INP", "RREC-cut.INP")
