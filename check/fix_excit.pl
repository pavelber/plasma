#!/usr/bin/perl

#
# fix EXCIT.INP; uses check_out
#

use IO::CaptureOutput qw/capture_exec/;


$remove = shift;

if ($remove =~ /^-?-h/i) {
    print "Usage: $0 <a>\n  a < 0: remove bad between autoioniz\n  a > 0: remove all bad\n";
    exit 0;
}
print "starting fix_excit.pl\n";


open CHK,'check_out.exe' or die "check_out: $!\n";
print "opened  check excit\n";
while (<CHK>) {
    my @a = split;
    $bad{$a[0].'_'.$a[1].'_'.$a[2]} = $a[4];
}

close CHK;

open EXC, 'EXCIT.INP' or die "EXCIT.INP: $!\n";
open OUT, ">ee";

print OUT "\n\n";

while (<EXC>) {
    next if /Z|(\-\-\-)/;
	print OUT $_, next if /^\s*$/;
    my @a = split;
    if ($bad{$a[0].'_'.$a[1].'_'.$a[2]}) {
        if ($remove eq 'd') {
            next;
        } elsif ($remove < 0 and $a[1] < 0 and $a[2] < 0) {
            next;
        } elsif ($remove > 0) {
            if ($a[3] == 5) {
                s/^(\s*\S+\s+\S+\s+\S+\s+)5\b/${1}0/;
            } else {
                next;
            }
        } else {
            s/-(\d\.\d\d\d)(E.{3}) /sprintf "-%4.3f%s ",$1-0.1,$2/egi;
        }
    }
    print OUT $_;
}

close EXC;
close OUT;
print "before rename  check excit\n";
run_exec "mv -f EXCIT.INP EXCIT.INP.old";
run_exec("sort ee > EXCIT.INP");
unlink 'ee';
exit;


sub run_exec() {
    my $cmd = shift;
    print "Running $cmd\n";
    my ($stdout, $stderr, $success, $exit_code) = capture_exec($cmd);
    print "($stdout, $stderr, $success, $exit_code)\n";
    if ($exit_code != 0) {
        die "BAD EXIT CODE"
    }
}