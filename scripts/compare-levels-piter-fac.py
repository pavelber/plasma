from lib.in1 import IN1

if __name__ == "__main__":

    fac_in1_path = "C:\\work4\\tmp\\O-fac\\IN1.INP"
    fac_in1 = IN1(fac_in1_path)
    piter_in1_path = "../db/O/IN1.INP"
    piter_in1 = IN1(piter_in1_path)

    sp_nums = fac_in1.get_sp_numbers()
    for sp_num in sp_nums:
        fac_levels = fac_in1.get_levels(sp_num)
        piter_levels = piter_in1.get_levels(sp_num)
        if fac_levels is None or piter_levels is None:
            continue
        count = 0
        for level in fac_levels:
            if not fac_in1.contains_energy(sp_num, level):
                continue
            energy = fac_in1.get_energy(sp_num, level)
            stat_weight = fac_in1.get_stat_weight(sp_num, level)
            config = fac_in1.get_config(sp_num, level)
            piter_level = piter_in1.find_level_by_energy_statweight_config(sp_num, energy, stat_weight, config)
            if piter_level is not None:
                count += 1
        print(f"Spectroscopic number {sp_num} has {len(fac_levels)} levels in FAC and {len(piter_levels)} levels in PITER, match: {count}")
