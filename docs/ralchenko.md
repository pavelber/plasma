# Ralchenko's Fortran programs

This document describes the standalone Fortran programs written by
Yu. Ralchenko that this repository uses to turn FAC cross sections into fitted
`EXCIT.INP` and `RREC.INP` records, and to check the fitted coefficients in
`EXCIT.INP`, `RREC.INP` and `BCFP.INP`. The name "Ralchenko" appears only in
the directory name `ralchenko/` and in some binary names. The sources carry no
author notice, but the main fit function is called `FCN_SASHA`.

Everything here was worked out from the sources, the Perl/Python callers, git
history, and the strings and debug symbols embedded in the binaries. Nothing
was disassembled. Statements about the old binary that are inferences, not
facts read from code, are marked as such.

## Contents

- [Inventory](#inventory)
- [Where they sit in the pipeline](#where-they-sit-in-the-pipeline)
- [`exc_fac`: excitation cross-section fit (`fit_spline.f`)](#exc_fac-excitation-cross-section-fit-fit_splinef)
- [Old `exc_fac.exe` (without formula 16)](#old-exc_facexe-without-formula-16)
- [`ph_fac`: photoionization cross-section fit (`ph_fac_new.f`)](#ph_fac-photoionization-cross-section-fit-ph_fac_newf)
- [Checkers: `check`, `check_bcfp`, `check_rr`](#checkers-check-check_bcfp-check_rr)
- [Fixers and orchestration](#fixers-and-orchestration)
- [Formula reference](#formula-reference)
- [Building](#building)
- [Known quirks and bugs](#known-quirks-and-bugs)

## Inventory

### Sources

| File | Program | Purpose |
|---|---|---|
| `ralchenko/src/fit_spline.f` | `PROGRAM FIT` → `exc_fac` | Fits excitation cross sections, formulas 16 and 5 (15 defined, unused) |
| `ralchenko/src/ph_fac_new.f` | `PROGRAM PHOTOFIT` → `ph_fac` | Fits photoionization cross sections, formula 4 |
| `ralchenko/src/check.f` | `PROGRAM CHECK` → `check.exe` | Looks for negative σ in `EXCIT.INP` (methods 5, 8, 11, 16) |
| `ralchenko/src/check_bcfp.f` | `PROGRAM CHECK` → `check_bcfp.exe` | Looks for negative values in `BCFP.INP` |
| `ralchenko/src/check_rr.f` | `PROGRAM CHECK` → `check_rr.exe` | Looks for negative σ in `RREC.INP` (method 4) |
| `ralchenko/src/lmdif1.f` | library | MINPACK `lmdif1` (Levenberg–Marquardt, forward-difference Jacobian) |
| `ralchenko/src/Makefile` | | macOS gfortran build (see [Building](#building)) |
| `ralchenko/oldsrc/check_rr.f` | | Older `check_rr` with a different x grid |
| `ralchenko/oldsrc/fix_rr.pl` | | Older copy of `check/fix_rr.pl` (same as `check/old_fix_rr.pl` apart from messages) |
| `ralchenko/oldsrc/lmdif1.f`, `Makefile` | | Identical to the `src/` copies |

All sources entered git in two commits: `2f17a36` (2021-08-28) for `src/` and
`091de7d` (2021-11-05) for `oldsrc/`. None has been modified since, so git holds
no older version of `fit_spline.f`.

### Binaries

| Binary | In git since | Built from | Toolchain (from embedded strings) |
|---|---|---|---|
| `win32/exc_fac.exe` | `b9f7753`, 2021-01-17 | **`exc_fac.f`, not in the repo** | g77, mingw32 / `crtdll.dll`, with STABS debug info |
| `win32/ph_fac.exe` | `b9f7753`, 2021-01-17 | `ph_fac.f`, not in the repo | g77, mingw32 |
| `win32/ralchenko_exc_fac.exe` | `a418f79`, 2021-07-21 | `fit_spline.f` | MinGW-W64 gcc 8.1.0 |
| `win32/ralchenko_ph_fac.exe` | `a418f79`, 2021-07-21 | `ph_fac_new.f` | MinGW-W64 gcc 8.1.0 |
| `win32/exc_fac_static.exe` | `6fa9445`, 2021-08-07 | `fit_spline.f` | Strawberry Perl gcc 8.3.0, static |
| `win32/ph_fac_static.exe` | `6fa9445`, 2021-08-07 | `ph_fac_new.f` | Strawberry Perl gcc 8.3.0, static |
| `check/check.exe`, `check/check_bcfp.exe` | `2f17a36` | `check.f`, `check_bcfp.f` | Strawberry Perl gcc 8.3.0 |
| `check/check_rr.exe` | `2f17a36`, rebuilt locally | `src/check_rr.f` | gcc 16.1.0 (working tree, uncommitted) |
| `check/old_check_rr.exe` | `c85d0a6`, rebuilt locally | `oldsrc/check_rr.f` | gcc 16.1.0 (working tree, uncommitted) |

`win32/ralchenko.txt` holds the gfortran and ld command lines used for these
builds.

**On Windows, the pipeline uses `exc_fac_static.exe` and `ph_fac_static.exe`**
([lib/env.py](../lib/env.py)). On other platforms it expects `exc_fac` and
`ph_fac` in `<sys.platform>/`.

## Where they sit in the pipeline

```
FAC output (fac.lev, fac.tr, fac.ce, fac.rr, ...)
        │
        ▼  fac_IN1.pl  (called by run.py / run-ui.py with -exc <exe> -ph <exe>)
        │    writes EXC/excit  and  REC/rrec
        │
        ├─► EXC/:  echo <spch> | exc_fac   → outpp.dat, outpp_da.dat, badout.dat, outinf.dat
        │          fac_IN1.pl merges them, renumbers levels   → EXCIT.INP
        │
        └─► REC/:  echo <spch> | ph_fac    → output_ph.dat, info_ph_bad.dat
                   sort output_ph.dat                          → RREC.INP

EXCIT.INP / BCFP.INP  ─►  check_all.pl  ─►  check.exe / check_bcfp.exe + fix_excit.pl / fix_bcfp.pl
RREC.INP              ─►  lib/check_and_fix.py, lib/merge_checks.py  ─►  check_rr.exe + fix
```

Entry points:

- **FAC flow**: [run.py](../run.py) → `run_fac_in1()` runs
  `perl fac_IN1.pl -exc <exc_fac> -ph <ph_fac>` in each spectroscopic-number
  directory. `fac_IN1.pl -d` also runs `check_all.pl`. `-norun` skips both fits.
  `-16` forces formula 16 for every transition (no formula-5 fit).
- **Database flow**:
  [create_all_from_databases.py](../create_all_from_databases.py) and
  [ui-databases.py](../ui-databases.py) call
  `lib/check_and_fix.create_rrec_inp()`. That runs
  `ph_fac_static.exe <spch>` directly in each `<elem>/<sn>/` directory,
  followed by `type output_ph.dat | sort > RREC.INP`, and then
  `check_fix()` (the `check_rr` loop). `exc_fac` is not used in this flow.
  Excitation fits there come from the Python re-implementation in
  [lib/create_cross_section_fits.py](../lib/create_cross_section_fits.py)
  (`nomad_5`, `nomad_11` and `nomad_16` in
  [lib/cross_section.py](../lib/cross_section.py)).
- **Post-merge checks**: [lib/merge_checks.py](../lib/merge_checks.py)
  compiles `check.f`, `check_bcfp.f` and `check_rr.f` from `ralchenko/src` with
  `gfortran` if the `.exe` is missing, runs `check_all.pl`, and then loops
  `check_rr.exe` + `fix_rr.pl` up to 100 times.

## `exc_fac`: excitation cross-section fit (`fit_spline.f`)

### Invocation

Run it in the `EXC/` directory. It reads the spectroscopic charge from stdin:

```
echo <spch> | exc_fac_static.exe
```

### Input file `excit`

`fac_IN1.pl` writes this file from FAC's `fac.ce`. It has one block per
transition:

```
  NLOW      NUPP      INDEX    OSC            ← free format: %6d    %6d    %d    %10.3e
 x1          σ1          E1                   ← format (1PE10.3,2X,1PE10.3,2X,1PE10.3)
 x2          σ2          E2
 ...
--                                            ← block terminator
```

- `NLOW` and `NUPP` are FAC level indices + 1. `fac_IN1.pl` renumbers them to
  IN1 numbering afterwards.
- `INDEX` is 0 for "dipole-allowed" and 2 otherwise. `fac_IN1.pl` sets 0 when the
  first number of the second header line of the FAC record is > 0 (probably the
  Bethe coefficient) **and** f > 1e-3. The `-16` option forces 2 for every
  transition.
- `OSC` is the absorption oscillator strength f.
- `x = E_in / ΔE` is the incident electron energy in threshold units.
  `fac_IN1.pl` computes it as `(E_FAC + ΔE)/ΔE`.
- σ is in cm² (FAC value × 1e-20). E is the absolute energy in eV. The fit reads
  E but does not use it.
- In `fac_IN1.pl`, points with σ < 0 are dropped. If the first point is
  negative, the whole transition is dropped. If `x2/x1 > 2`, three
  log-interpolated points are inserted between the first two.

### Point selection (per transition)

1. Read points until `--`. Keep track of σ_max (`CS0`).
2. Stop taking points (skip to `--`) when either condition holds:
   - more than 8 points are read and σ rises compared with the previous point, or
   - more than 7 points are read and the local log-log slope
     `ln(σ_i/σ_{i-2}) / ln(x_{i-2}/x_i)` changes by more than 2 from the previous
     slope.
3. Drop the last point, up to twice, if it is higher than the one before it.
4. If 5 or fewer points remain (M ≤ 5), skip the transition silently. It is
   written to no output file.

### Variables actually fitted

```
s_j = σ_j · x_j / σ_max                                  (scaled "collision strength")
y_j = α · sqrt( (x_j − 1) / (x_j + x_M) ),   α = 0.7 · sqrt( 2 x_M / (x_M − 1) )
```

`x_M` is the x of the last point kept. y maps [1, ∞) onto [0, α), and
y(x_M) = 0.7. All fits minimise the relative residuals
`r_j = (model_j − s_j)/s_j` with `lmdif1` (TOL = 1e-5).
Fit quality is the RMS of these residuals: `SIGMA = sqrt(Σ r_j² / M)`.

### Formula 16 (always fitted)

```
s(y) = X1² e^{−y²/X5²} + X2² e^{−(y−1/3)²/X5²} + X3² e^{−(y−2/3)²/X5²} + X4² e^{−(y−1)²/X5²}
```

The model is four Gaussians with fixed centres 0, 0.333, 0.666 and 1 and a
common width. Squared parameters keep the amplitudes and the width positive. In
the original units:

```
σ(x) = [ P1 e^{−y²/P5} + P2 e^{−(y−0.333)²/P5} + P3 e^{−(y−0.666)²/P5} + P4 e^{−(y−1)²/P5} ] / x
P1..P4 = Xi² · σ_max,   P5 = X5²,   P6 = x_M
```

Every σ(x) from this formula is ≥ 0 by construction.

Fitting procedure:

- **Pass 1** starts from X = (0.9, 0.2, 0.1, 0.2, 0.12). **Passes 2–10** start
  from random X1..X4 in [0, 1) with X5 = 0.12.
- Within a pass, if `SIGMA > 0.1` the program changes the parameters with
  `X_k ← sqrt(|X_k / k|)` and refits, up to 100 times (`MAXCHK`).
- The fit is **good** if `SIGMA < 0.3` (3 × `vari`). A good transition is marked
  in `GOOD_FIT(NLOW,NUPP)` and skipped in later passes.
- The program makes up to 10 passes over the file (`NUMB_RUNS`). It stops early
  once every transition has a good fit.

### Formula 5 (only when `INDEX = 0`)

This fit is done once, before formula 16, in `FCN_SASHA`:

```
s(x) = X1 + X2/x + X3/x² + X4/x³ + X5² ln x
σ(x) = P1/x + P2/x² + P3/x³ + P4/x⁴ + P5 ln(x)/x ,   Pi = Xi·σ_max (i≤4),  P5 = X5²·σ_max
```

The model has Bethe-like asymptotics: the ln-term coefficient is forced ≥ 0.
The start point is X = (1, 1, −1, 1, 0.1). The source writes `10**POW` with
`POW = 0.28`, but `POW` is declared `INTEGER`, so the factor is 1. The fit
makes one `lmdif1` call with no retries, and **its quality is never checked**.

### Formula 15 (defined, not called)

`FCN_15` has the same four centres and the same y as formula 16, with
Lorentzians in place of Gaussians:

```
s(y) = X1²/(y²+X5²) + X2²/((y−0.333)²+X5²) + X3²/((y−0.666)²+X5²) + X4²/((y−1)²+X5²)
```

No checker or Python code knows method 15.

### Output files

Every record uses format `96`: `(" ",I2,"  ",I6," ",I6," ",I2," ",7(1PE11.3))`:

```
 SPCH  NLOW  NUPP  METHOD  P1  P2  P3  P4  P5  P6  −f
```

| File | Content |
|---|---|
| `outpp.dat` | Good formula-16 fits (`METHOD = 16`, P6 = x_M) |
| `outpp_da.dat` | For `INDEX = 0` transitions whose formula-16 fit is good: the formula-5 fit (`METHOD = 5`, P6 = 0) |
| `badout.dat` | Formula-16 fits with SIGMA ≥ 0.3, same format. Written again on each pass, so duplicates are possible |
| `outinf.dat` | `NLOW NUPP INDEX SIGMA` for the same bad fits (format `(I6,3X,I6,5X,I10,"  ",F8.3)`) |

On stdout the program prints `N good out of M total`.

### Post-processing in `fac_IN1.pl`

1. Concatenate `outpp_da.dat` with those lines of `outpp.dat` whose
   (NLOW, NUPP) pair is not already in `outpp_da.dat`. **For dipole-allowed
   transitions, formula 5 therefore replaces formula 16.**
2. Drop method-16 lines where P4 > 1e10 · P1 ("outrageously bad fits").
3. Renumber NLOW and NUPP from FAC index to IN1 level number.
4. Add method-0 lines (all coefficients zero, only −f) for radiatively
   connected pairs that FAC did not calculate at all.
5. `sort` the result into `../EXCIT.INP`, then `gzip -k excit out*`.

Transitions that end up in `badout.dat`, or that were skipped for M ≤ 5, are
**not** in `EXCIT.INP`, and step 4 does not add them back either.

## Old `exc_fac.exe` (without formula 16)

No source for this version exists in git or anywhere on the machine
(`C:\work2`, `C:\work`, the user profile and `Windows.old` were searched). The
binary is g77-compiled with STABS debug info, which gives the following facts
without disassembly:

- **Source file**: `exc_fac.f`, linked with `lmdif1.all.f`.
- **Routines**: `MAIN__`, `fcn_sasha`, `fcn_sasha4`, `fcn_sfd`, `fcn_sfd1`, and
  MINPACK (`lmdif1`, `lmdif`, `lmpar`, `qrfac`, `qrsolv`, `fdjac2`, `enorm`,
  `dpmpar`). **There is no `fcn_15` or `fcn_16`.**
- **Array sizes**: `x(5)`, `fvec(22)`, `e(100)`, `p(5)`, `wa(1000)`. Fits
  therefore have 5 parameters and at most 22 points per transition.
  `/BLOCKA/` is 1600 bytes, i.e. two `REAL*8(100)` arrays.
- **Local variables**: `mode`, `pow`, `nlow`, `nupp`, `spch`, `index`, `osc`,
  `cs0`, `ime`, `sigma`, `method` (CHARACTER*5), `filename`, `inf`,
  `out_inf`, `out_data`, `bad_out`.
- **Prompts**: `Enter the spectroscopic charge:`,
  `Enter the power for initial guess`, `Enter the name of file:`,
  `Enter type of transition:`, `Enter number of points:`,
  `Enter the X points :`, `Enter the Y points :`.
- **Files**: `info_ex.dat`, `outinf.dat`, `outpp.dat`, `badout.dat`. There is no
  `outpp_da.dat` and no `excit`.
- **Printed tags**: `sasha`, `sfdo`, `Sigma =`, `Loops made:`.
- **Format statements** are the same as in `fit_spline.f` (100, 99, 97, 96, 95,
  94, 14, 93, 13, 92). Several of those are left over, unused, in
  `fit_spline.f`, which confirms that `fit_spline.f` grew out of `exc_fac.f`.

Inferences (not verified):

- The program has the same three-mode structure that is still visible in
  `ph_fac_new.f`: (1) automatic mode driven by an info file (`info_ex.dat`),
  (2) a single data file, and (3) points typed at the keyboard. It also asks
  for the initial-guess power (`pow`) interactively. It therefore **cannot be
  driven by `echo <spch> | exc_fac`** the way `fac_IN1.pl` drives the current
  version.
- The fit functions correspond to the formulas that `check.f` still knows by
  name: `SASHA` → **method 5**, `SFD` → **method 8**, `SFDO` → **method 11**
  (see [Formula reference](#formula-reference)). `fcn_sasha4` and `fcn_sfd1`
  are variants whose exact form cannot be read from the binary. By name,
  `sasha4` is probably the formula-5 polynomial without the log term, and
  `sfd1` a variant of the spin-forbidden formula. So the old program most
  likely fitted dipole-allowed transitions with method 5 and forbidden ones
  with method 8 or 11, instead of today's method 16.

To see which method numbers it writes, run it on a small `info_ex.dat` / data
file and read `outpp.dat`. To recover `fcn_sasha4` and `fcn_sfd1` exactly, the
functions have to be disassembled; they are small.

The old `win32/ph_fac.exe` is also a g77 build, of `ph_fac.f`, with the single
fit function `fcn_photo` and the same prompts and file names as
`ph_fac_new.f`. It appears to be the same program as `ph_fac_new.f`.

## `ph_fac`: photoionization cross-section fit (`ph_fac_new.f`)

### Invocation

Run it in `REC/` (FAC flow) or `<elem>/<sn>/` (database flow). It reads the
spectroscopic charge from stdin. `MODE` is hard-wired to 1, the automatic mode.
Modes 2 (single file) and 3 (keyboard input) are still in the source but
unreachable.

### Input file `rrec`

`fac_IN1.pl` writes this file from FAC's `fac.rr`:

```
NLOW  NUPP                    ← %5s  %5s : IN1 number of the bound level, index of the ion level (1 = ground)
x   σ   E                     ← 3(1PE10.3,2X), x = photon energy / threshold, σ = PI cross section [cm²]
...
--
```

`fac_IN1.pl` takes σ from `b[-2]`, the photoionization column of `fac.rr`,
× 1e-20, and keeps only positive values. The database flow writes the same
format from database cross sections.

Reading a block stops at the first line that fails the E-format read, which is
the `--` line. At most 100 points are read per block.

### Formula 4

The data is normalised to s = σ/σ_max and fitted with `lmdif1` (TOL = 1e-20):

```
s(x) = [ X1² + X2/x + (X3² − X2 − X1²)/x² ] · x^−(3.5 + X4)
σ(x) = ( P1 + P2/x + P3/x² ) · x^−(3.5 + P4)
P1 = X1²·σ_max,  P2 = X2·σ_max,  P3 = (X3² − X1² − X2)·σ_max,  P4 = X4
```

This parametrisation forces P1 ≥ 0 and σ(1) = X3²·σ_max ≥ 0. σ can still turn
negative at intermediate x, which is why `check_rr` exists. The start point is
X = (4, 1, 0.1, rand).

### Quality and output

- The quality measure is `SIGMA = Σ max(|r_j|, |r_j/(1−|r_j|)|)`. If
  `SIGMA ≥ M`, the transition goes to `info_ph_bad.dat` as `NLOW -> NUPP SIGMA`.
- **Every fit, good or bad, goes to `output_ph.dat`**, in format
  `(" ",I2," ",I6," ",I6,"  4",10(1P,E11.3,1X))`:
  `SPCH NLOW NUPP 4 P1 P2 P3 P4 0 0 0 0 0 0`.
  The caller sorts this file into `RREC.INP`.
- `output_ph_bad.dat` is opened but never written.

## Checkers: `check`, `check_bcfp`, `check_rr`

All three checkers work the same way. Each reads its `.INP` file, skipping 2
header lines, and evaluates every line's formula on a fixed x grid plus random
x values. **A line is bad if the value is negative at any x.** For each bad
line, the checker prints and writes `idx1 idx2 idx3 METHOD m value x`, and at
the end prints `There are N lines with K bad lines`. The callers parse that
summary line.

| Program | Input | Output | Grid | Formulas checked |
|---|---|---|---|---|
| `check.exe` | `EXCIT.INP`: 4 ints + 6 coefficients | `check_out` | 40 fixed x in [1, 1e5] + 40 random `x = 10^(√5·h)` ∈ [1, ~172] | 5, 8, 11, 16 (others are not checked) |
| `check_bcfp.exe` | `BCFP.INP`: 4 ints + 4 coefficients | `check_bcfp` | 40 fixed x in [1, 1e5] + 60 random `x = 18^(2h²)` ∈ [1, 324] | see below. Lines with C2 = C3 = C4 = 0 are skipped |
| `check_rr.exe` (`src`) | `RREC.INP`: 4 ints + 10 coefficients | `check_rout` | 38 fixed x in [1, 1e4] + 462 random `x = 1000^h` ∈ [1, 1000] | 4 |
| `old_check_rr.exe` (`oldsrc`) | same | `check_rout` | 38 fixed x in [1, 1e5] + 462 random `x = 10^(1.5h)` ∈ [1, ~32] | 4 |

`h` is uniform in [0, 1). The `oldsrc` version also prints level numbers with
`I6` instead of `I8`.

`BCFP.INP` formula as evaluated by `check_bcfp`:

```
if C2 < 0:  y = 1 − 1/x;   f(x) = −C2 ln x + C3 y² + C4 y/x + C1 y/x²
else:       f(x) = C2 / (1 + C3/x² + C4/x)
```

The first branch is the usual Burgess–Chidichimo/Younger-type
electron-impact-ionization form. `check.f` also contains the functions `SASHA`,
`SFD`, `SFDO` and `S16`. They duplicate the formulas in the `SELECT CASE`
and are never called.

## Fixers and orchestration

All fixers live in `check/`, are copied into each working directory by
`copy_checks()`, and rewrite the `.INP` file in place. The previous version is
saved as `*.old` or `RREC.INP.BACK`, and the result is re-sorted.

### `check_all.pl` (EXCIT, BCFP)

- **IN1.INP**: warns about equal or decreasing energies. It does not fix
  anything.
- **EXCIT.INP** loop, at most 10 iterations:
  1. Run `check.exe`. Stop if there are 0 bad lines.
  2. Run `fix_excit.pl -1`, which removes bad transitions whose two level numbers
     are both negative (autoionizing levels).
  3. After the first iteration, if this is iteration 10 or the bad fraction is
     below 0.1 %, run `fix_excit.pl 1` and stop. Otherwise run `fix_excit.pl`
     with no argument and repeat.
- **BCFP.INP**: run `check_bcfp.exe` once, then `fix_bcfp.pl`.

### `fix_excit.pl <a>` (reads `check_out`)

| `a` | Effect on bad lines |
|---|---|
| none / 0 | Every negative coefficient `-d.ddd E±nn` gets its mantissa reduced by 0.1 (made less negative) |
| < 0 | Remove bad lines between two autoionizing levels (both indices < 0) |
| > 0 | Method 5 → method 0 (the fit is dropped and only f is kept); any other method → line removed |
| `d` | Remove every bad line |

### `fix_bcfp.pl`

Reads `check_bcfp` and removes ionization lines whose key (idx1, idx2, idx4)
was flagged, when idx3 = idx1 + 1. It only does anything if `check_bcfp.exe`
is present in the current directory.

### `fix_rr.pl [keep]` (reads `check_rout`)

Without an argument:

- a bad line whose last coefficient is 0 is removed;
- otherwise the method field becomes `-5` if |last| ≤ 1e-3, else `0`.

With an argument, the mantissa of every negative number is reduced by 0.02 and
the line is kept. `old_fix_rr.pl` does the same as the no-argument case. With
an argument it adds 0.002 to the first negative mantissa, which makes it more
negative, and then drops the line, because of a `next` placed before `print`.

### Python RREC loop (`lib/check_and_fix.py`)

`check_fix()` → `check_and_fix_rr_version2()` runs 10 iterations of
`check_rr.exe`. For each bad line it adds 0.002·10^exp to P1 and subtracts
0.002·10^exp from P2, which pushes σ up at large x. After the last iteration
it removes the lines that are still bad. It repeats once, and raises an
exception if bad lines remain. `check_and_fix_rr()` and
`check_and_fix_old_rr*()` are the older Perl-fixer based variants.

## Formula reference

x = E/ΔE (EXCIT) or E/I (RREC). The coefficients are the numbers that follow
the 4 integer columns of a line.

| File | Method | Formula σ(x) | Written by | Checked by |
|---|---|---|---|---|
| EXCIT | 0 | no fit, only −f in the last column (Van Regemorter-type rate from f in the kinetics code) | `fac_IN1.pl`, `fix_excit.pl 1` | — |
| EXCIT | 5 | C1/x + C2/x² + C3/x³ + C4/x⁴ + C5 ln(x)/x | `exc_fac` (`outpp_da.dat`), old `exc_fac` | `check` |
| EXCIT | 8 | (C5 x + C2 + C3 x²) / (C1 + x²) / x^C4 | old `exc_fac` (inferred) | `check` |
| EXCIT | 11 | (C1 x² + C2 x + C3) / (x + C4)⁴ / x^C5 | old `exc_fac` (inferred), Python fits | `check` |
| EXCIT | 15 | four Lorentzians in y (see above) | nothing (dead code) | — |
| EXCIT | 16 | [C1 e^{−y²/C5} + C2 e^{−(y−0.333)²/C5} + C3 e^{−(y−0.666)²/C5} + C4 e^{−(y−1)²/C5}] / x, y = 0.7·sqrt(2C6/(C6−1))·sqrt((x−1)/(x+C6)) | `exc_fac` (`outpp.dat`), Python fits | `check` |
| RREC | 4 | (C1 + C2/x + C3/x²) · x^−(3.5+C4) | `ph_fac` | `check_rr` |
| RREC | 0 / −5 | method disabled by `fix_rr.pl` | `fix_rr.pl` | — |
| BCFP | — | see [Checkers](#checkers-check-check_bcfp-check_rr) | other tools | `check_bcfp` |

In every EXCIT line the last column holds **−f**, and the kinetics code uses f
from there.

The Python `nomad_5` in [lib/cross_section.py](../lib/cross_section.py)
uses `e·ln(x)/x⁵` for the log term, while `check.f` and `exc_fac` use
`C5·ln(x)/x`. `nomad_16` matches formula 16 (`0.9899495 = 0.7·√2`).

## Building

`ralchenko/src/Makefile` targets macOS gfortran (`-O -mcmodel=large`, static
libgfortran). It only compiles object files: `make all` produces `*.o`, not
executables. The link commands are in `win32/ralchenko.txt`. For a plain build
anywhere:

```bash
gfortran -O -mcmodel=large -o exc_fac fit_spline.f lmdif1.f
gfortran -O -o ph_fac ph_fac_new.f lmdif1.f
gfortran -O -o check check.f
gfortran -O -o check_bcfp check_bcfp.f
gfortran -O -o check_rr check_rr.f
```

`-mcmodel=large` is needed for `fit_spline.f` because of the
`INTEGER GOOD_FIT(50000,50000)` array, about 10 GB of address space, only
touched sparsely. `lib/merge_checks.py` builds the checkers on the fly with
plain `gfortran -o`.

## Known quirks and bugs

`fit_spline.f`:

- The formula-5 fit is never quality-checked, yet it replaces the checked
  formula-16 fit for dipole-allowed transitions (`fac_IN1.pl` prefers
  `outpp_da.dat`).
- The quality of the formula-5 fit does not decide whether it is written: it is
  written only when the *formula-16* fit is good.
- `POW` is `INTEGER`, so `0.28` becomes 0 and the "initial guess power" has no
  effect.
- `DO_RAND` computes a seed and never uses it: it calls `RANDOM_SEED()` with no
  arguments on every call.
- Level indices must be ≤ 50000 (`GOOD_FIT` bounds are not checked).
- Transitions with M ≤ 5 usable points disappear without any message.
- `badout.dat` and `outinf.dat` get one line per pass for transitions that stay
  bad.

`ph_fac_new.f`:

- `FVEC` is declared `(20)` but up to 100 points are read. Blocks with more than
  20 points overrun the array.
- `SIGMA` only decides whether a line is logged in `info_ph_bad.dat`; bad fits
  are still written to `output_ph.dat`.
- The initial X4 is random, so results are not reproducible between runs.

Checkers:

- They sample x randomly, so a line can pass one run and fail the next. That is
  why the callers loop.
- `check.f` silently accepts every method other than 5, 8, 11 and 16.
- `fix_bcfp.pl` does nothing unless `check_bcfp.exe` is present in the current
  directory. `check_all.pl` calls `fix_excit.pl 1` without `perl`.
