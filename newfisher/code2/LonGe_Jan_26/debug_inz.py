
import os

def debug_inz(directory):
    # Constants based on QSs.inp headers in LonGe-nov-mth16
    # SS: (nuQS, nuAS)
    qs_data = {
        20: (2, 0),
        21: (23, 296),
        22: (5, 237),
        23: (37, 10),
        24: (108, 67),
        25: (86, 120),
        26: (215, 158),
        27: (160, 144),
        28: (68, 83),
        29: (48, 40),
        30: (15, 82),
        31: (31, 46),
        32: (16, 0)
    }
    
    FSS = 20
    HSS = 32
    Nnu = 815
    
    # Calculate nuGS and kAI1/kAI2
    nuGS = {}
    kAI1 = {}
    
    # nuGS (Non-AI start indices)
    current_idx = 1
    for ss in range(FSS, HSS + 2): # +1 for nucl
        nuGS[ss] = current_idx
        if ss <= HSS:
             nuQS = qs_data[ss][0]
             current_idx += nuQS
        else:
             # Nucl
             pass
             
    # kAI1 (AI start indices)
    # Code logic:
    # if(nuAS.gt.0)
    #   if(prev_kAI1 == 0) kAI1 = Nnu + 1
    #   else kAI1 = prev_kAI1 + prev_nuAS
    
    current_ai_idx = Nnu + 1
    
    sorted_ss = sorted(qs_data.keys())
    
    for ss in sorted_ss:
        nuAS = qs_data[ss][1]
        if nuAS > 0:
            kAI1[ss] = current_ai_idx
            current_ai_idx += nuAS
        else:
            kAI1[ss] = 0

    print("Calculated Tables:")
    print(f"nuGS: {nuGS}")
    print(f"kAI1: {kAI1}")
    
    inz_path = os.path.join(directory, "Inz.inp")
    print(f"Scanning {inz_path}...")
    
    try:
        with open(inz_path, 'r') as f:
            lines = f.readlines()
            # Skip title
            start_line = 1
            
            for line_num, line in enumerate(lines[start_line:], start=start_line+1):
                parts = line.split()
                if not parts: continue
                # iSS1, iQS1, iSS2, iQS2, ...
                try:
                    iSS1 = int(parts[0])
                    iQS1 = int(parts[1])
                    iSS2 = int(parts[2])
                    iQS2 = int(parts[3])
                    
                    # Compute ki
                    if iQS1 > 0:
                        ki = nuGS[iSS1] - 1 + iQS1
                    else:
                        ki = kAI1[iSS1] - 1 - iQS1
                        
                    # Compute kf
                    if iQS2 > 0:
                        kf = nuGS[iSS2] - 1 + iQS2
                    else:
                        kf = kAI1[iSS2] - 1 - iQS2
                        
                    if ki == 1425 or kf == 1544:
                    # if True:
                        if ki == 1425 and kf == 1544:
                            print(f"MATCH FOUND at Line {line_num}:")
                            print(f"Content: {line.strip()}")
                            print(f"Parsing: SS1={iSS1}, QS1={iQS1} -> ki={ki}")
                            print(f"Parsing: SS2={iSS2}, QS2={iQS2} -> kf={kf}")
                            return
                except ValueError:
                    continue
    except Exception as e:
        print(f"Error: {e}")

if __name__ == "__main__":
    import sys
    debug_inz(sys.argv[1])
