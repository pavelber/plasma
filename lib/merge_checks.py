import os
import re
import shutil
import subprocess

from lib.exceptions import GenericPlasmaException


REPO_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
CHECK_DIR = os.path.join(REPO_ROOT, "check")
FORTRAN_SRC_DIR = os.path.join(REPO_ROOT, "ralchenko", "src")


def _run(args, cwd):
    proc = subprocess.run(
        args,
        cwd=cwd,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    if proc.stdout:
        print(proc.stdout)
    if proc.stderr:
        print(proc.stderr)
    if proc.returncode != 0:
        raise GenericPlasmaException(
            "Command failed in %s: %s\n%s\n%s" %
            (cwd, " ".join(args), proc.stdout, proc.stderr)
        )
    return proc.stdout


def _copy_check_scripts(output_dir):
    for filename in os.listdir(CHECK_DIR):
        src = os.path.join(CHECK_DIR, filename)
        if os.path.isfile(src):
            shutil.copy(src, output_dir)


def _compile_check_binary(output_dir, source_name, exe_name):
    exe_path = os.path.join(output_dir, exe_name)
    if os.path.exists(exe_path) and os.path.getsize(exe_path) > 0:
        return

    source_path = os.path.join(FORTRAN_SRC_DIR, source_name)
    if not os.path.exists(source_path):
        raise GenericPlasmaException("Missing legacy check source: " + source_path)

    _run(["gfortran", "-o", exe_path, source_path], output_dir)


def _parse_bad_lines(output):
    match = re.search(r"with\s+(\d+)\s+bad lines", output)
    if not match:
        raise GenericPlasmaException("Cannot parse check_rr output:\n" + output)
    return int(match.group(1))


def _run_rrec_check(output_dir, max_iterations):
    if not os.path.exists(os.path.join(output_dir, "RREC.INP")):
        return

    _compile_check_binary(output_dir, "check_rr.f", "check_rr.exe")
    for _ in range(max_iterations):
        output = _run([os.path.join(output_dir, "check_rr.exe")], output_dir)
        bad_lines = _parse_bad_lines(output)
        if bad_lines == 0:
            return
        _run(["perl", "fix_rr.pl"], output_dir)

    raise GenericPlasmaException(
        "RREC check still reports bad lines after %d iterations" % max_iterations
    )


def run_post_merge_checks(output_dir, max_rrec_iterations=100):
    output_dir = os.path.abspath(output_dir)
    _copy_check_scripts(output_dir)

    _compile_check_binary(output_dir, "check.f", "check.exe")
    _compile_check_binary(output_dir, "check_bcfp.f", "check_bcfp.exe")
    _run(["perl", "check_all.pl"], output_dir)

    _run_rrec_check(output_dir, max_rrec_iterations)
