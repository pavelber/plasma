#!/usr/bin/perl

#
# fix EXCIT.INP; uses check_out
#

use IO::CaptureOutput qw/capture_exec/;


if ($nonremove = shift) {
    print "Do you REALLY want to keep the bad lines [y/n]: ";
    $_ = <STDIN>;
    undef $nonremove if $_ !~ /^y/i;
}
print "starting old_fix_rr.pl\n";
open CHK,'check_rout' or die "check_rout: $!\n";

while (<CHK>) {
    my @a = split;
    $bad{$a[0].'_'.$a[1].'_'.$a[2]} = $a[4];
}

close CHK;

open EXC, 'RREC.INP' or die "RREC.INP: $!\n";
open OUT, ">rr";

while (<EXC>) {
        print OUT $_, next if /^\s*$/;
    my @a = split;
    if ($bad{$a[0].'_'.$a[1].'_'.$a[2]} != 0) {
                print "$a[1] -> $a[2]\n" if $a[1] < 10 and $a[1] > 0;
        if ($nonremove) {
            s/-(\d\.\d\d\d)/sprintf "-%4.3f",$1+0.002/e;    
            next;
        }
        undef $_ if $a[-1] == 0;
        if (abs($a[-1]) <= 1e-3) {
            s/((\S+\s+){3})( |\d)\d\b/$1-5/;
        } else {
            s/((\S+\s+){3})( |\d)\d\b/$1 0/;
        }
    }
#    if ($bad{$a[0].'_'.$a[1].'_'.$a[2]} == 5) {
#        s/((\S+\s+){6,7})-(\d\.\d\d\d)/sprintf "%s-%4.3f",$1,$3-0.002/e;
#        undef $_ if $remove;
#    } elsif ($bad{$a[0].'_'.$a[1].'_'.$a[2]} == 11) {
#        s/((\S+\s+){5})( |-)(\d\.\d\d\d)/sprintf "%s%s%4.3f",$1,$3,$4-0.002/e;
#        undef $_ if $remove;
#    }
    print OUT $_;
}

close EXC;
close OUT;
print "rename in old fix_rr.pl\n";
rename 'RREC.INP', 'RREC.INP.old';
run_exec("sort rr > RREC.INP");
unlink 'rr';
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