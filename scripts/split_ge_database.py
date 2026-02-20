#!/usr/bin/env python3
"""
split_ge_database.py
====================
Split the "long" Ge database (spectroscopic numbers 20–32) in LonGe-nov-mth16/
into two sub-databases:

  Lo  (LoGe-nov-mth16/)  : SpS 20–25  +  ground states (QS #1) of SpS 26–32
  Hi  (HiGe-nov-mth16/)  : SpS 26–32  (full)

Physical motivation
-------------------
Co2nov25.for requires the full ionisation ladder, so the Lo database must
include the ground states of all higher ions (26–32) plus the ionisation
transitions that connect them.

File formats handled
--------------------
QSs.inp  – summary header (lines 1-16), then per-ion blocks
Exc.INP  – excitation:   SS  #1  #2  ...
Inz.INP  – ionisation:   iSS iQS fSS fQS ...
AIW.INP  – autoionisation: SSi AIQS# SSf QSf# ...

Usage
-----
    python scripts/split_ge_database.py [input_dir]

Default input_dir: newfisher/data/LonGe-nov-mth16  (relative to repo root)
Output dirs are created as siblings of input_dir.
"""

import glob
import os
import re
import shutil
import sys

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
REPO_ROOT  = os.path.dirname(SCRIPT_DIR)

DEFAULT_INPUT_DIR = os.path.join(REPO_ROOT, "newfisher", "data", "LonGe-nov-mth16")

LO_RANGE = range(20, 26)   # SpS 20..25  (inclusive)
HI_RANGE = range(26, 33)   # SpS 26..32  (inclusive) — 33 = nucleus, kept in Hi

LO_EXTRA = range(26, 33)   # ions whose ground state is included in Lo

ALL_RANGE = range(20, 34)  # everything (including nucleus 33)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def first_int(s):
    """Return the first integer token in a string, or None."""
    m = re.match(r'\s*(-?\d+)', s)
    return int(m.group(1)) if m else None


def ensure_dir(path):
    os.makedirs(path, exist_ok=True)


# ===========================================================================
# QSs.inp
# ===========================================================================

def parse_qss(path):
    """
    Parse QSs.inp.

    Returns
    -------
    header_lines : list of str  – lines 1-16 (summary table + separators)
    ions         : dict  ss -> {'header': str, 'states': [str], 'ais': [str]}
                   'header'  – the "NN [Name]  ..." line
                   'states'  – ordinary state lines (QS# >= 1)
                   'ais'     – inner-shell / AI state lines (QS# < 0)
    """
    header_lines = []
    ions = {}
    current_ss = None

    # The header consists of:
    #   line 1 : "SpS  QSs  AI  ..."
    #   line 2 : short dash separator  (--...--)
    #   lines 3-N : summary table rows  "NN  nQS  nAI  PI  [Name]"
    #   line N+1 : long dash separator (--------...--------)
    # We collect everything up to and INCLUDING the long separator.
    # Strategy: the first dash-line we see is the SHORT one; the SECOND
    # dash-line is the LONG one that ends the header.
    dash_count = 0
    in_summary = True

    with open(path, 'r') as f:
        for raw in f:
            line = raw.rstrip('\n').rstrip('\r')

    # ions[ss] = [ block1, block2, ... ]
    # block = {'header': str, 'lines': [str], 'is_ai': bool}
    
    current_block = None

    with open(path, 'r') as f:
        for raw in f:
            line = raw.rstrip('\n').rstrip('\r')

            # ---- summary header ----------------------------------------
            if in_summary:
                header_lines.append(raw)
                if re.match(r'\s*-{10,}', line):
                    dash_count += 1
                    if dash_count == 2:   # second dash-line = end of header
                        in_summary = False
                continue

            # ---- ion-section header line ---------------------------------
            # e.g. "20 [Al] ..."  or  "20 Al-like AIs"
            # The nucleus line might just be "33" (no space, or spaces only)
            m = re.match(r'^(\d+)(\s|$)', line)
            if m:
                ss = int(m.group(1))
                is_ai_header = "AI" in line
                
                if ss not in ions:
                    ions[ss] = []
                
                current_block = {'header': raw, 'lines': [], 'is_ai': is_ai_header}
                ions[ss].append(current_block)
                continue

            # ---- empty / blank line at end of file -----------------------
            if line.strip() == '':
                # If we have a current block, we might want to append blank lines 
                # to preserve spacing, but typically re-writing handles newlines.
                # We'll ignore blank lines to avoid duplication.
                continue

            # ---- state line ----------------------------------------------
            if current_block is not None:
                current_block['lines'].append(raw)

    return header_lines, ions


def rebuild_summary_table(header_lines, ions, ss_list, extra_gs_list=None):
    """
    Rebuild the top summary table for the given ss_list.
    """
    extra_gs_list = extra_gs_list or []

    new_lines = []
    for raw in header_lines:
        line = raw.rstrip('\n').rstrip('\r')

        # Lines starting with a SpS number in the summary table
        m = re.match(r'^(\s*)(\d+)(\s+)(\d+)(\s+)(\d+)(.*)$', line)
        if m:
            ss = int(m.group(2))
            if ss in ss_list:
                if ss in extra_gs_list:
                    # Only ground state (1 QS, 0 AIs)
                    n_qs = 1
                    n_ai = 0
                elif ss in ions:
                    # Sum up counts from all blocks
                    n_qs = 0
                    n_ai = 0
                    for block in ions[ss]:
                        # Check lines to see if they are GS or AI
                        # (Usually blocks are either all-GS or all-AI, 
                        #  but let's check per line to be safe/consistent)
                        for l in block['lines']:
                            parts = l.split()
                            if len(parts) >= 4:
                                try:
                                    qs = int(parts[-2])
                                    if qs < 0: n_ai += 1
                                    else:      n_qs += 1
                                except: pass
                else:
                    continue
                # Reconstruct line preserving column widths
                rest = m.group(7)   # everything after the AI count
                new_line = "%s%s%s%3d%s%3d%s\n" % (
                    m.group(1), m.group(2), m.group(3),
                    n_qs, m.group(5),
                    n_ai, rest)
                new_lines.append(new_line)
            # else: skip ions not in this sub-database
        else:
            new_lines.append(raw)

    return new_lines


def write_qss_lo(header_lines, ions, out_path):
    """Write QSs.inp for the Lo database."""
    lo_ions  = [ss for ss in sorted(ions.keys()) if ss in LO_RANGE]
    hi_gs    = [ss for ss in sorted(ions.keys()) if ss in LO_EXTRA]
    nucleus  = [ss for ss in sorted(ions.keys()) if ss == 33]

    all_ss_in_file = lo_ions + hi_gs + nucleus

    new_header = rebuild_summary_table(
        header_lines, ions,
        ss_list=all_ss_in_file,
        extra_gs_list=hi_gs)

    global_counter = 0  # will renumber the ## column

    with open(out_path, 'w') as f:
        for line in new_header:
            f.write(line)

        # 1. Normal States for Lo ions (20-25)
        for ss in lo_ions:
            for block in ions[ss]:
                if not block['is_ai']:
                    f.write(block['header'])
                    for raw in block['lines']:
                        global_counter += 1
                        f.write(_renumber_global(raw, global_counter))
            f.write('\n') # separator between ions

        # 2. Normal States (Ground State only) for Hi ions (26-32)
        for ss in hi_gs:
            if ss not in ions: continue
            # Find the FIRST normal block
            for block in ions[ss]:
                if not block['is_ai']:
                    f.write(block['header'])
                    # Only the first state (ground state)
                    if block['lines']:
                        global_counter += 1
                        f.write(_renumber_global(block['lines'][0], global_counter))
                    break # stop after first normal block
            f.write('\n')

        # 3. Nucleus (SpS 33)
        nnu_index = 0
        for ss in nucleus:
            if ss in ions:
                for block in ions[ss]:
                    f.write(block['header'])
                    for raw in block['lines']:
                        global_counter += 1
                        f.write(_renumber_global(raw, global_counter))
                        # The nucleus is typically a single line, so global_counter here is Nnu
                        nnu_index = global_counter
                # No newline after Nucleus block, AI headers start immediately


        # 4. AI States for Lo ions (20-25)
        for ss in lo_ions:
            for block in ions[ss]:
                if block['is_ai']:
                    f.write(block['header'])
                    for raw in block['lines']:
                        global_counter += 1
                        f.write(_renumber_global(raw, global_counter))
            
    total_states = global_counter
    print(f"  QSs.inp Lo: Nnu={nnu_index}, Total={total_states} states written -> {out_path}")
    return nnu_index, total_states


def write_qss_hi(header_lines, ions, out_path):
    """Write QSs.inp for the Hi database."""
    hi_ions = [ss for ss in sorted(ions.keys()) if ss in HI_RANGE or ss == 33]

    new_header = rebuild_summary_table(
        header_lines, ions,
        ss_list=hi_ions)

    global_counter = 0

    nucleus = [ss for ss in sorted(ions.keys()) if ss == 33]
    hss = max(HI_RANGE)

    with open(out_path, 'w') as f:
        for line in new_header:
            f.write(line)

        # 1. Normal States for Hi ions (26-32)
        for ss in hi_ions:
            if ss == 33: continue # Nucleus handled separately
            c_local = 0
            for block in ions[ss]:
                if not block['is_ai']:
                    f.write(block['header'])
                    for raw in block['lines']:
                        global_counter += 1
                        c_local += 1
                        f.write(_renumber_global(raw, global_counter))
            f.write('\n')

        # 2. Nucleus (SpS 33)
        nnu_index = 0
        for ss in nucleus:
            if ss in ions:
                c_local = 0
                for block in ions[ss]:
                    f.write(block['header'])
                    for raw in block['lines']:
                        global_counter += 1
                        c_local += 1
                        f.write(_renumber_global(raw, global_counter))
                        nnu_index = global_counter
                # No newline after Nucleus block

        # 3. AI States for Hi ions (26-31, H-like 32 usually has no AI)
        for ss in hi_ions:
            if ss == 33: continue
            for block in ions[ss]:
                if block['is_ai']:
                    f.write(block['header'])
                    for raw in block['lines']:
                        global_counter += 1
                        f.write(_renumber_global(raw, global_counter))

    print(f"  QSs.inp Hi: Nnu={nnu_index}, Total={global_counter} states written -> {out_path}")
    return nnu_index, global_counter


def _renumber_global(raw, new_num):
    """
    Replace the last integer column (## = global QS number) on a state line.
    Preserves the line formatting as much as possible.
    """
    # We match the last integer field at the end of the line (ignoring trailing whitespace)
    m = re.match(r'^(.*\s+)(-?\d+)(\s*)$', raw.rstrip('\r\n'))
    if m:
        return m.group(1) + str(new_num) + m.group(3) + '\n'
    return raw


# ===========================================================================
# Exc.INP
# ===========================================================================

def split_exc(in_path, lo_path, hi_path):
    """
    Exc.INP format:
        SS  #1  #2  Method  A  B  C  D  E  F  OscStrength

    SS is the spectroscopic number of the ion.
    Excitation is always within the same ion, so splitting is straightforward.
    """
    lo_count = hi_count = 0

    with open(in_path, 'r') as f, \
         open(lo_path, 'w') as flo, \
         open(hi_path, 'w') as fhi:

        for raw in f:
            line = raw.rstrip('\n').rstrip('\r')
            parts = line.split()
            if not parts:
                continue

            # Header line (starts with letters)
            if not parts[0].lstrip('-').isdigit():
                flo.write(raw)
                fhi.write(raw)
                continue

            ss = int(parts[0])
            if ss in LO_RANGE:
                flo.write(raw)
                lo_count += 1
            elif ss in HI_RANGE:
                fhi.write(raw)
                hi_count += 1
            # else: skip (e.g. nucleus 33 has no excitation rows)
            


    print(f"  Exc.INP  Lo: {lo_count} rows, Hi: {hi_count} rows")
    return lo_count, hi_count


# ===========================================================================
# Inz.INP
# ===========================================================================

def split_inz(in_path, lo_path, hi_path):
    """
    Inz.INP format:
        iSS  iQS  fSS  fQS  ...

    Rules for Lo database (SpS 20-25 + GS of 26-32):
      * Keep rows where iSS ∈ 20-25  (all ionisations from the Lo active set)
      * Also keep rows where iSS ∈ 26-32 AND iQS == 1 AND fSS ∈ 27-33 AND fQS == 1
        (ground-state-to-ground-state ionisation ladder within the "bookend" ions)

    Rules for Hi database (SpS 26-32):
      * Keep rows where iSS ∈ 26-32
    """
    lo_count = hi_count = 0

    with open(in_path, 'r') as f, \
         open(lo_path, 'w') as flo, \
         open(hi_path, 'w') as fhi:

        for raw in f:
            line = raw.rstrip('\n').rstrip('\r')
            parts = line.split()
            if not parts:
                continue

            # Header line
            if not parts[0].lstrip('-').isdigit():
                flo.write(raw)
                fhi.write(raw)
                continue

            if len(parts) < 4:
                continue

            i_ss  = int(parts[0])
            i_qs  = int(parts[1])
            f_ss  = int(parts[2])
            f_qs  = int(parts[3])

            # --- Lo ---
            in_lo = False
            if i_ss in LO_RANGE:
                # Full ionisations from Lo active set.
                # For fSS in 26-32: only keep if fQS==1 (target ground state)
                # (because Lo only has the ground state of 26-32)
                if f_ss in LO_RANGE or f_ss == 33:
                    in_lo = True
                elif f_ss in LO_EXTRA:
                    if f_qs == 1:
                        in_lo = True
                    # fQS < 0 means autoionising state of next ion — skip
                    # (those AI states are not in the Lo QSs.inp)
            elif i_ss in LO_EXTRA:
                # Ground-state-to-ground-state ladder among the bookend ions
                if i_qs == 1 and f_qs == 1:
                    in_lo = True

            if in_lo:
                flo.write(raw)
                lo_count += 1

            # --- Hi ---
            if i_ss in HI_RANGE:
                fhi.write(raw)
                hi_count += 1

    print(f"  Inz.INP  Lo: {lo_count} rows, Hi: {hi_count} rows")
    return lo_count, hi_count


# ===========================================================================
# AIW.INP
# ===========================================================================

def split_aiw(in_path, lo_path, hi_path):
    """
    AIW.INP format:
        SSi  AIQS#  SSf  QSf#  WAI  DE(eV)

    SSi is the ion that autoionises; AIQS# < 0 refers to its AI states.
    SSf is the resulting (more ionised) ion.

    Lo: keep rows where SSi ∈ 20-25.
    Hi: keep rows where SSi ∈ 26-32.

    Note: rows where SSf ∈ Lo_extra (26-32) but SSi ∈ Lo_range could exist,
    but their QSf# references states only in Lo or GS of Hi, so they are fine.
    """
    lo_count = hi_count = 0

    with open(in_path, 'r') as f, \
         open(lo_path, 'w') as flo, \
         open(hi_path, 'w') as fhi:
        
        header_seen = False

        for raw in f:
            line = raw.rstrip('\n').rstrip('\r')
            parts = line.split()
            if not parts:
                continue

            # 1. Header Line (Assume first non-empty line is header)
            if not header_seen:
                flo.write(raw)
                fhi.write(raw)
                lo_count += 1 # Header is counted as a line for StrAIw
                hi_count += 1 # Header is counted as a line for StrAIw
                header_seen = True
                continue

            # 2. Parse SSi. If fails, it is a TAIL line (parameters like FrL).
            try:
                ssi = int(parts[0])
            except ValueError:
                # Tail line -> Write to both, but DO NOT COUNT as transition
                flo.write(raw)
                fhi.write(raw)
                continue
            
            if len(parts) < 4:
                # Should not happen if int(parts[0]) succeeded, but check anyway
                continue
                
            # Parse full transition
            # SSi AIQS# SSf QSf# ...
            # 25  -1    26  2    ...
            ssi = int(parts[0])
            qsi = int(parts[1])
            ssf = int(parts[2])
            qsf = int(parts[3])

            # --- Lo ---
            # Rule: Keep SSi in 20-25.
            # AND: If SSf is 26-32, QSf MUST be 1.
            in_lo = False
            if ssi in LO_RANGE:
                # Check destination
                if ssf in LO_RANGE or ssf == 33:
                    in_lo = True
                elif ssf in LO_EXTRA:
                    if qsf == 1:
                        in_lo = True
                    # else: excited state of boundary ion -> skip
                # else: ssf > 33? (Should not happen)
                
            if in_lo:
                flo.write(raw)
                lo_count += 1

            # --- Hi ---
            # Rule: Keep SSi in 26-32.
            # (Note: transitions from 25 to 26 are NOT in Hi, because SSi must be in Hi)
            if ssi in HI_RANGE:
                fhi.write(raw)
                hi_count += 1

    print(f"  AIW.INP  Lo: {lo_count} rows, Hi: {hi_count} rows")
    return lo_count, hi_count


# ===========================================================================
# Params files
# ===========================================================================

# Files to copy verbatim from the original directory
PARAM_FILES = [
    "Params1.inp",
    "QSsC.inp",
    "QSsHe.inp",
    "QSsD.inp",
]


def count_data_lines(path):
    """Count non-empty lines in a file (for StrExc / StrInz / StrAIw)."""
    n = 0
    with open(path, 'r') as f:
        for line in f:
            if line.strip():
                n += 1
    return n


def copy_params(in_dir, out_dir, qss_total_states, qss_nucleus_num,
                fss, hss, str_exc, str_inz, str_aiw):
    """
    Copy all parameter files from in_dir to out_dir, then rewrite
    Params0.inp with the correct line counts and state indices.

    Parameters
    ----------
    in_dir          : source directory
    out_dir         : destination directory
    qss_total_states: NST  – total number of QS entries (including AIs)
    qss_nucleus_num : Nnu  – global serial number of the nucleus entry
    fss             : first SpS in this sub-database
    hss             : H-like SpS   (same for both Lo and Hi: 32)
    str_exc         : Line count for Exc.inp (Header+Transitions)
    str_inz         : Line count for Inz.inp (Header+Transitions)
    str_aiw         : Line count for AIw.inp (Header+Transitions, NO TAIL)
    """
    # 1. Copy verbatim files ---------------------------------------------------
    for fname in PARAM_FILES:
        src = os.path.join(in_dir, fname)
        if os.path.isfile(src):
            shutil.copy2(src, os.path.join(out_dir, fname))

    # Also copy InpResp*.inp
    for src in glob.glob(os.path.join(in_dir, "InpResp*.inp")):
        shutil.copy2(src, os.path.join(out_dir, os.path.basename(src)))

    # 2. Read original Params0.inp to keep its structure ---------------------
    params0_src = os.path.join(in_dir, "Params0.inp")
    with open(params0_src, 'r') as f:
        orig_lines = f.readlines()


    # 4. Rebuild Params0.inp --------------------------------------------------
    # Original layout (ignoring trailing comment text after the numbers):
    #   line 0: FSS(1) FSS(2) FSS(3) FSS(4)   <- first SpS per element
    #   line 1: HSS(1) HSS(2) HSS(3) HSS(4)   <- H-like SpS
    #   line 2: Nnu(1) Nnu(2) Nnu(3) Nnu(4)   <- nucleus serial #
    #   line 3: NST(1) NST(2) NST(3) NST(4)   <- total states
    #   line 4: ----  separator
    #   line 5: StrExc   comment
    #   line 6: StrInz   comment
    #   line 7: StrAIw   comment
    #
    # We keep columns 2-4 (for C, He, D) unchanged from the original.
    def _replace_first_col(orig_line, new_val):
        """Replace the first integer token on a line, keep rest verbatim."""
        m = re.match(r'^(\s*)(\d+)(\s+.*$|\s*$)', orig_line.rstrip('\r\n'))
        if m:
            return "%s%d%s\n" % (m.group(1), new_val, m.group(3))
        return orig_line

    def _replace_first_col_comment(orig_line, new_val):
        """Replace value + keep the descriptive comment that follows."""
        m = re.match(r'^(\s*)(\d+)(\s+.*$|\s*$)', orig_line.rstrip('\r\n'))
        if m:
            return "%s%d%s\n" % (m.group(1), new_val, m.group(3))
        return orig_line

    new_lines = list(orig_lines)  # start from a copy
    new_lines[0] = _replace_first_col(orig_lines[0], fss)
    new_lines[1] = _replace_first_col(orig_lines[1], hss)
    new_lines[2] = _replace_first_col(orig_lines[2], qss_nucleus_num)
    new_lines[3] = _replace_first_col(orig_lines[3], qss_total_states)
    # line 4 is separator – unchanged
    new_lines[5] = _replace_first_col_comment(orig_lines[5], str_exc)
    new_lines[6] = _replace_first_col_comment(orig_lines[6], str_inz)
    new_lines[7] = _replace_first_col_comment(orig_lines[7], str_aiw)

    out_params0 = os.path.join(out_dir, "Params0.inp")
    with open(out_params0, 'w') as f:
        f.writelines(new_lines)

    print(f"  Params0.inp  FSS={fss} HSS={hss} Nnu={qss_nucleus_num} "
          f"NST={qss_total_states}  StrExc={str_exc} StrInz={str_inz} "
          f"StrAIw={str_aiw}  -> {out_params0}")


# ===========================================================================
# Main
# ===========================================================================

def main():
    in_dir = sys.argv[1] if len(sys.argv) > 1 else DEFAULT_INPUT_DIR
    in_dir = os.path.abspath(in_dir)

    if not os.path.isdir(in_dir):
        print(f"ERROR: input directory not found: {in_dir}", file=sys.stderr)
        sys.exit(1)

    parent_dir = os.path.dirname(in_dir)
    lo_dir = os.path.join(parent_dir, "LoGe-nov-mth16")
    hi_dir = os.path.join(parent_dir, "HiGe-nov-mth16")
    ensure_dir(lo_dir)
    ensure_dir(hi_dir)

    print(f"Input  : {in_dir}")
    print(f"Lo dir : {lo_dir}")
    print(f"Hi dir : {hi_dir}")
    print()

    # --- QSs.inp ---
    print("Processing QSs.inp ...")
    qss_in   = os.path.join(in_dir, "QSs.inp")
    qss_lo   = os.path.join(lo_dir, "QSs.inp")
    qss_hi   = os.path.join(hi_dir, "QSs.inp")
    header_lines, ions = parse_qss(qss_in)
    
    lo_nnu, lo_total = write_qss_lo(header_lines, ions, qss_lo)
    hi_nnu, hi_total = write_qss_hi(header_lines, ions, qss_hi)

    # Compute QSs metadata for Params0.inp
    # Lo: states for SpS 20-25 (full) + 1 GS per SpS 26-32 + nucleus(33)
    lo_fss = min(LO_RANGE)   # 20
    lo_hss = max(HI_RANGE)   # 32  (H-like is still 32 in Lo)

    # Hi: states for SpS 26-32 (full) + nucleus(33)
    hi_fss = min(HI_RANGE)   # 26
    hi_hss = max(HI_RANGE)   # 32

    # --- Exc.INP ---
    print("Processing Exc.INP ...")
    exc_in = os.path.join(in_dir, "Exc.inp")
    exc_lo = os.path.join(lo_dir, "Exc.inp")
    exc_hi = os.path.join(hi_dir, "Exc.inp")
    exc_count_lo, exc_count_hi = split_exc(exc_in, exc_lo, exc_hi)
    
    # --- Params0.inp generation (Partial) ---
    # We need StrExc, StrInz, StrAIw. We will call copy_params at the end.

    # --- Inz.INP ---
    print("Processing Inz.INP ...")
    inz_in = os.path.join(in_dir, "Inz.inp")
    inz_lo = os.path.join(lo_dir, "Inz.inp")
    inz_hi = os.path.join(hi_dir, "Inz.inp")
    inz_count_lo, inz_count_hi = split_inz(inz_in, inz_lo, inz_hi)

    # --- AIW.INP ---
    print("Processing AIW.INP ...")
    aiw_in = os.path.join(in_dir, "AIw.inp")
    aiw_lo = os.path.join(lo_dir, "AIw.inp")
    aiw_hi = os.path.join(hi_dir, "AIw.inp")
    aiw_count_lo, aiw_count_hi = split_aiw(aiw_in, aiw_lo, aiw_hi)

    # --- Params0.inp ---
    print("Processing Params0.inp ...")

    # 1. Lo
    copy_params(in_dir, lo_dir,
                qss_total_states = lo_total,
                qss_nucleus_num  = lo_nnu,
                fss = lo_fss,
                hss = lo_hss,
                str_exc = exc_count_lo,
                str_inz = inz_count_lo,
                str_aiw = aiw_count_lo)

    # 2. Hi
    copy_params(in_dir, hi_dir,
                qss_total_states = hi_total,
                qss_nucleus_num  = hi_nnu,
                fss = hi_fss,
                hss = hi_hss,
                str_exc = exc_count_hi,
                str_inz = inz_count_hi,
                str_aiw = aiw_count_hi)
    
    print("Done.")


if __name__ == "__main__":
    main()
