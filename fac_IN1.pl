use File::Path;
use IO::CaptureOutput qw/capture_exec/;

sub usage {

    print <<EOP;
usage:

$0  [-t <levs>] [-e <excfile>] [-i <ionfile>] [-a <autofile>] [-p <recfile>] [-q <first> <shift>]
    [-r <trans>] [-al <autolimit>] [-reg] [-norun] [<file>] [-s <low upp lowA>] [-d] [-h] [-F]

Options:

INPUT FILES
<file>                FAC levels file name (default: fac.lev)
-t <levs>             total number of levels
-r <trans>            FAC transitions file name (default: fac.tr)
-i <ionfile>          FAC ionization file name (default: fac.ci)
-a <autofile>         FAC autoionization file name (default: fac.ai)
-p <recfile>          FAC photoionization file name (default: fac.rr)

OTHER OPTIONS
-F                    start 'sfac exc.py' first
-al <autolimit>       lower limit for autoionization rate
-d                    check with check_all.pl
-reg                  van Regemorter EXCIT0.INP
-norun                does not start exc_fac and ph_fac
-norad                skip radiative transitions
-noion                skip ionization
-noexc                skip excitation
-norec                skip recombination
-n                    only produces IN1.INP
-nonauto              only nonautoionizing states are included
-s <low upp lowA>     lower and upper wl's and lower A limit for SPECTR.INP
-q                    level number shift
-f                    output goes to file
        
EOP
}

print "INC!!!\n";
print "@INC\n";

# ------------------------------------------

# defaults
$file = 'fac.lev';
$tranfile = 'fac.tr';
$excfile = 'fac.ce';
$recfile = 'fac.rr';
$ionfile = 'fac.ci';
$autofile = 'fac.ai';

$numshift = 0;
$numshift0 = 1e10;

while ($_ = shift) {
    /^-16$/i && ($only16 = 1) && next; # only 16 fit, no 5
    /^-t$/i && ($total = shift) && next;
    /^-r$/i && ($tranfile = shift) && next;
    /^-F$/i && ($sfac = 1) && next;
    /^-e$/i && ($excfile = shift) && next;
    /^-exc$/i && ($exc_exe = shift) && next;
    /^-ph$/i && ($ph_exe = shift) && next;
    /^-i$/i && ($ionfile = shift) && next;
    /^-a$/i && ($autofile = shift) && next;
    /^-p$/i && ($recfile = shift) && next;
    /^-al$/i && ($autolimit = shift) && next;
    /^-reg$/i && ($regemorter = 1) && next;
    /^-q$/ && ($numshift0 = shift) && (($numshift = shift) || 1) && (print "$numshift0 -- $numshift\n") && next;
    /^-d$/i && ($do_check = 1) && next;
    /^-norun$/i && ($norun = 1) && &inform('No exc_fac or ph_fac will be executed') && next;
    /^-noexc$/i && ($noexc = 1) && next;
    /^-noion$/i && ($noion = 1) && next;
    /^-norec$/i && ($norec = 1) && next;
    /^-norad$/i && ($norad = 1) && next;
    /^-nonauto$/i && ($nonauto = 1) && next;
    /^-n$/i && ($only_IN1 = 1) && next;
    /^-f/ && (open STDOUT, ">fac_inp.out" or die "Can't open fac_inp.out: $!\n") && next;
    if (/^-s$/) {
        $low_wl = shift;
        $upp_wl = shift;
        $lowA = shift if $ARGV[0] =~ /^\d+(\.(\d+)?)?(e[+-]?\d+)?$/;
        next;
    }
    /^-?-h/i && &usage && exit;
    $file = $_;
}

# first delete unnecessary *.b files

unlink "fac.*.b";

# ------------------------------------------

system "sfac exc.py" if $sfac;

my $current_dir = `pwd`;

print "--------------\nCurrent directory: $current_dir--------------\n";

foreach my $f ($file, $tranfile, $excfile, $recfile, $ionfile, $autofile) {
    next if $only_IN1 and $f ne $file;
    unlink $f . '.gz' if -e $f and -e "$f.gz";
    system "gunzip -f $f.gz" if -e "$f.gz";
}

print "Working on IN1.INP...\n";

#system 'gunzip -f fac.lev.gz' if $file eq 'fac.lev' and -e 'fac.lev.gz';

open INP, $file or die "Can't open $file: $!\n";

# get spectroscopic number

while (<INP>) {
    $Z = $1 if /\s+Z\s+=\s+(\d+)\.0/;
    if (/NELE\s+=\s+(\d+)/) {
        $spch = $Z - $1 + 1;
        last;
    }
}

$spch = &spch if !defined $spch;


# look for the next ionization state

$_ = <INP> until /NLEV\s*=\s*(\d+)/;
while (<INP>) {
    next unless /NLEV\s*=\s*(\d+)/;
    $_ = <INP>;
    $_ = <INP>;
    ($first_ion, $IP) = (/(\d+)\s+\S+\s+(\S+)/);
    last;
}
seek(INP, 0, 0);

$_ = <INP> until /NLEV\s*=\s*(\d+)/;
$levs = $1;
!defined $total && ($total = $levs);
$_ = <INP>;

open OUT, ">IN1.INP" or die "Can't open IN1.INP: $!\n";

my @out_in1;

push @out_in1, "$spch\n";

$old_enrg = -1;

for (1 .. $levs) {
    $_ = <INP>;
    $ind++;

    my @a = split;
    next if $total < $a[0];

    my $enrg = $a[2];
    if ($enrg == $old_enrg) {
        $enrg += 1e-5;
    }
    $old_enrg = $enrg;

    if ($a[2] < $IP) {
        $last_below_IP = $a[0];
        $level_number[$a[0]] = $a[0] + 1;
        $level_number[$a[0]] += $numshift if $level_number[$a[0]] > $numshift0;
    }
    else {
        if ($nonauto) {
            $total = $a[0] - 1;
            last;
        }
        ($a[0] == $last_below_IP + 1) && (push @out_in1, "Autoionizing states\n") && ($autostates = 1);
        $level_number[$a[0]] = $last_below_IP - $a[0];
    }

    $sw[$ind] = $a[5] + 1;

    my $shls = substr $_, 63, 22;

    $shls =~ s/^\s+//;
    $shls =~ s/\s+$//;

    $shls =~ s/\[(\d+)\]/chr($1 + 44)/eg;

    my @shls = (split /\s+/, $shls);
    my ($sh2a, $sh2b, $sh2c) = ($shls[-2] =~ /^(\d+)([\D])(\d+)/);
    $sh2b = ' ' unless $shls[-2];
    my ($sh1a, $sh1b, $sh1c) = ($shls[-1] =~ /^(\d+)([\D])(\d+)/);

    #    printf OUT "%2s%s%-2s%2s%s%-2s %4da %5d%14.5f   0.00e+00 0.00e+00\n",$sh2a,$sh2b,$sh2c,$sh1a,$sh1b,$sh1c,$ind,$sw[$ind],$a[2];
    push @out_in1, (sprintf "%2s%s%-2s%2s%s%-2s%6d %6d%13.5f   0.00e+00 0.00e+00\n", $sh2a, $sh2b, $sh2c, $sh1a, $sh1b, $sh1c, $level_number[$a[0]], $sw[$ind], $enrg);
}

printf OUT "%2d %5d %5d    0 %10.2f    0   0   0   0   0\n", $spch, $last_below_IP + 1, $levs - 1 - $last_below_IP, $IP;
print OUT @out_in1;

close OUT;
close INP;

exit 0 if $only_IN1;

# ---------- transitions -----------

if (-e $tranfile and not $norad) {

    print "Working on radiative transitions...\n";

    #    print system "ps v $$";

    open TRANS, $tranfile or die "Can't open $tranfile: $!\n";

    while (<TRANS>) {
        my @a = split;
        next if $#a < 7;

        my $uta = ($#a == 8 ? 1 : 0);

        next if $total > 0 and ($a[2] > $total or $a[0] > $total);

        $osc->[$a[2]]->[$a[0]] += $a[5 + $uta] / ($a[3] + 1);
        if ($low_wl) {
            $A->[$a[0]]->[$a[2]] += $a[6 + $uta];
            $wl->[$a[0]]->[$a[2]] = 1e8 / (8065.545 * $a[4]);
        }
    }

    close TRANS;

    #    my $rad_name = 'rad_trans';
    #    open OUTTRANS, ">$rad_name" or die "Can't open $rad_name: $!\n";
    #    foreach my $i (0..$total-1) {
    #        map {printf OUTTRANS "%5s%5s%11.3e\n",
    #                $i+1,$_+1,-$osc->[$i]->[$_]}
    #                ($i+1..$total-1);
    #    }
    #    close OUTTRANS;
    #    system "gzip $rad_name";
}

# ---------- SPECTR.INP -------------

if ($low_wl) {

    print "Working on SPECTR.INP...\n";

    $spch = &spch if !defined $spch;
    open SPECTR, ">SPECTR.INP" or die "Can't open file SPECTR.INP: $!\n";

    print "Enter atomic mass: ";
    my $mass = <STDIN>;
    chomp $mass;

    print SPECTR $mass, '  ', $low_wl, '  ', $upp_wl, "  5000\n";

    foreach my $ind1 (0 .. $total - 1) {
        foreach my $ind2 ($ind1 + 1 .. $total) {
            next if $wl->[$ind2]->[$ind1] < $low_wl or $wl[$ind2]->[$ind1] > $upp_wl;
            next if $A->[$ind2]->[$ind1] < $lowA;
            printf SPECTR "%s %5s %5s  1.000 %10.4f %10.3e\n", $spch, $level_number[$ind2], $level_number[$ind1], -$wl->[$ind2]->[$ind1], $A->[$ind2]->[$ind1];
        }
    }
    close SPECTR;

}

undef $A;
undef $wl;
undef @A;
undef @wl;


# ---------- excitations ------------

if (-e $excfile and not $noexc) {

    print "Working on excitations...\n";

    open EXC, $excfile or die "Can't open $excfile: $!\n";

    mkdir 'EXC' unless -e -d 'EXC';
    system "rm -f EXC/*";

    open OUTEXC, ">EXC/excit" or die "Can't open file EXC/excit: $!\n";

    EXC:
    while (<EXC>) {
        $negrid = $1 if /^NEGRID\s+=\s+(\d+)/;

        my @lev = split;
        next unless $#lev == 5;

        next if $total and $lev[2] > $total;

        $_ = <EXC>;
        my @trans = split;

        $exc_calc->[$lev[0]]->[$lev[2]] = 1; # this excitation WAS calculated

        my $outexc = sprintf "%6d    %6d    %d    %10.3e\n",
            $lev[0] + 1, $lev[2] + 1,
            ($only16 ? 2 :
                (($trans[0] > 0 && $osc->[$lev[0]]->[$lev[2]] > 1e-3) ? 0 : 2)),
            $osc->[$lev[0]]->[$lev[2]];

        my ($old_x, $old_y);
        for my $ii (1 .. $negrid) {
            $_ = <EXC>;
            chomp;
            my @b = split;
            if ($b[-1] >= 0) {
                my $x = ($b[0] + $lev[4]) / $lev[4];
                my $y = $b[-1] * 1e-20;
                if ($ii == 2 and $x / $old_x > 2) {
                    my $loldx = log($old_x);
                    my $lox = log($x);
                    my $lody = log($old_y);
                    my $loy = log($y);
                    $outexc .= sprintf "%10.3e  %10.3e  %10.3e\n", exp($loldx + ($lox - $loldx) / 5),
                        exp($lody + ($loy - $lody) / 5), exp($loldx + ($lox - $loldx) / 5) * $lev[4];
                    $outexc .= sprintf "%10.3e  %10.3e  %10.3e\n", exp($loldx + ($lox - $loldx) / 4),
                        exp($lody + ($loy - $lody) / 4), exp($loldx + ($lox - $loldx) / 4) * $lev[4];
                    $outexc .= sprintf "%10.3e  %10.3e  %10.3e\n", exp($loldx + 2 * ($lox - $loldx) / 3),
                        exp($lody + 2 * ($loy - $lody) / 3), exp($loldx + 2 * ($lox - $loldx) / 3) * $lev[4];
                }
                $outexc .= sprintf "%10.3e  %10.3e  %10.3e\n", $x, $y, $b[0] + $lev[4];
                $ii == 1 and $old_x = $x and $old_y = $y;
            }
            elsif ($ii == 1) {
                # exclude this transition if sigma < 0 for the first point
                next EXC;
            }

        }

        print OUTEXC $outexc, "--\n";

    }
    close OUTEXC;

    close EXC;
}

# van Regemorter output

if ($regemorter) {
    $spch = &spch if !defined $spch;
    open REGEM, ">EXCIT0.INP" or die "Can't open EXCIT0.INP: $!\n";
    #	print REGEM "\n\n";
    foreach my $i (0 .. $total - 1) {
        foreach my $j ($i + 1 .. $total) {
            #            next if $om[$i]->[$j] == 0 and $osc->[$i]->[$j] == 0;
            next unless ($level_number[$j] != 0 and $level_number[$i] != 0);
            next if $osc->[$i]->[$j] == 0;
            printf REGEM "  %3i  %6d  %6d   0   0.000e+00  0.000e+00  0.000e+00  0.000e+00  0.000e+00  0.000e+00 -%9.3e\n",
                $spch, $level_number[$i], $level_number[$j], $osc->[$i]->[$j];

        }
    }

    close REGEM;

}


#undef %osc;


# ----- photoionization ------

# $levs is the number of ALL CALCULATED levels for the given charge state so that
# $levs+1 is the next ground state

if (-e $recfile and not $norec) {

    print "Working on photorecombination...\n";

    open RREC, 'fac.rr' or die "Can't open file fac.rr: $!\n";

    my $recdir = "REC";
    mkdir $recdir unless -e -d $recdir;

    open OUTREC, ">$recdir/rrec" or die "Can't open file $recdir/rrec: $!\n";
    while (<RREC>) {

        $negrid = $1 if /^NEGRID\s+=\s+(\d+)/;
        my @lev = split;
        next unless $#lev == 5;

        #	    next if $level_number[$lev[0]] <= 0 or ($total and $lev[0] > $total);

        $_ = <RREC>;
        my $outrec;
        for (1 .. $negrid) {
            $_ = <RREC>;
            #            print;
            chomp;
            my @b = split;
            if ($b[-2] > 0) {
                $outrec .= sprintf "%10.3e  %10.3e  %10.3e\n", ($b[0] + $lev[4]) / $lev[4], $b[-2] * 1e-20, $b[0] + $lev[4];
            }
        }

        printf OUTREC "%5s  %5s\n", $level_number[$lev[0]], $lev[2] - $levs + 1;
        print OUTREC $outrec, "--\n";
    }
    close OUTREC;
    close RREC;
}

# ---------- ionization or autoionization ------------

if ((-e $ionfile or -e $autofile) and not $noion) {

    print "Working on (auto)ionization...\n";

    $spch = &spch if !defined $spch;

    open OUTION, ">bb";
    print OUTION "\n\n";

    if (-e $ionfile) {

        open ION, $ionfile or die "Can't open $ionfile: $!\n";

        while (<ION>) {
            $negrid = $1 if /^NEGRID\s+=\s+(\d+)/;

            my @lev = split;
            next unless $#lev == 5;

            next if $total and $lev[0] > $total;

            $_ = <ION>;
            chomp;
            my @trans = split;
            next unless $#trans == 3;
            printf OUTION "%5d %7d %5d %7d    " . (("    %11.4e ") x 4) . "\n", $spch, $level_number[$lev[0]], $spch + 1, $lev[2] - $first_ion + 1, $trans[-1], -$trans[0], @trans[1, 2];
        }

        close ION;
    }

    if (-e $autofile) {
        open AUTO, $autofile or die "Can't open $autofile: $!\n";

        while (<AUTO>) {
            chomp;
            my @lev = split;
            next unless $#lev == 6;
            next if $autolimit and $lev[5] < $autolimit;

            next if $total and $lev[0] > $total;
            printf OUTION "%5d %7d %5d %7d        %11.4e   0  0  0\n", $spch, $level_number[$lev[0]], $spch + 1, $lev[2] - $first_ion + 1, $lev[5];

        }
        close AUTO;
    }

    close OUTION;

    my $output = `sort bb > BCFP.INP`;
    print "Output:   $output\n";

    if ($?) {
        exit $? >> 8;
    }

    unlink 'bb';

}

# now run the exc_fac and ph_fac

exit 0 if $norun;

if (-d 'EXC' and not $noexc) {
    print "Working on EXC...\n";

    chdir 'EXC';

    #    system "sort info_ex.dat > ii; mv -f ii info_ex.dat";

    print "Running exc_fac...\n";

    run_exec("echo $spch | $exc_exe");

    #    system "sort outpp.dat > ../EXCIT.INP";

    if (-e 'outpp_da.dat') {
        my %found5;
        open OUTDA, 'outpp_da.dat' or die "Can't open file outpp_da.dat: $!\n";
        open TMPOUT, ">tmpout" or die "Can't open file tmpout: $!\n";
        while (<OUTDA>) {
            next if /^\s*$/;
            my @a = split;
            $found5{$a[1] . '_' . $a[2]} = 1;
            print TMPOUT;
        }
        close OUTDA;

        open OUTPP, 'outpp.dat' or die "Can't open file outpp.dat: $!\n";
        while (<OUTPP>) {
            (print TMPOUT), next if /^\s*$/;
            my @a = split;
            print TMPOUT unless defined $found5{$a[1] . '_' . $a[2]};
        }
        close OUTPP;
        close TMPOUT;
    }
    else {
        run_exec("cp -f outpp.dat tmpout");
    }

    # fixing autoionization numbers

    open OUTPP, 'tmpout' or die "Can't open tmpout: $!\n";
    open EXCITINP, ">../ee" or die "Can't open ../ee: $!\n";

    EXCIT:
    while (<OUTPP>) {
        if (/\S/) {
            my @a = split;
            next EXCIT if $a[3] == 16 and $a[7] > $a[4] * 1e10; # remove outrageously bad fits
            s/^\s*(\S+)\s+(\S+)\s+(\S+)/sprintf "  %3i  %6d  %6d ", $1, $level_number[$2 - 1], $level_number[$3 - 1]/e;
        }
        print EXCITINP;
    }
    close OUTPP;
    unlink 'tmpout';

    # adding optically-allowed transitions that were not caluclate
    foreach my $i (0 .. $total - 1) {
        foreach my $j ($i + 1 .. $total) {
            #            next if $om[$i]->[$j] == 0 and $osc->[$i]->[$j] == 0;
            next unless ($level_number[$j] != 0 and $level_number[$i] != 0);
            next if $exc_calc->[$i]->[$j] == 1;
            next if $osc->[$i]->[$j] == 0;
            printf EXCITINP "  %3i  %6d  %6d   0   0.000e+00  0.000e+00  0.000e+00  0.000e+00  0.000e+00  0.000e+00 -%9.3e\n",
                $spch, $level_number[$i], $level_number[$j], $osc->[$i]->[$j];
        }
    }
    close EXCITINP;

    run_exec("sort ../ee > ../EXCIT.INP");
    unlink "../ee";
    run_exec("gzip -k -f excit out*");

    chdir "..";

    #    system "tar cfz rec_exc.tgz EXC";
    #    rmtree('EXC');

}

if (-d 'REC' and not $norec) {
    $spch = &spch if !defined $spch;
    print "Working on REC...\n";
    chdir "REC";

    my $output = `echo $spch | $ph_exe`;
    print "Output: $spch | $ph_exe \n $output\n";

    if ($?) {
        exit $? >> 8;
    }
    $output = `sort output_ph.dat > ../RREC.INP`;
    print "Output: sort output_ph.dat > ../RREC.INP \n$output\n";

    if ($?) {
        exit $? >> 8;
    }

    chdir '..';
    system "tar cfz rec_exc.tgz REC";
    #exit 0;
    # rmtree('REC');

}

# check_all

run_exec ("perl check_all.pl") if $do_check;

# final zip of all files


foreach ($file, $tranfile, $excfile, $recfile, $ionfile, $autofile) {
    system "gzip -k -f $_" if -e $_;
};

exit 0;

# ----------------------------------
sub spch {

    print "Enter spectroscopic charge: ";
    my $spch = <STDIN>;
    chomp($spch);
    return $spch;

}
# ----------------------------------
sub inform {
    my $message = shift;
    print "  --- $message\n";
}

sub run_exec() {
    my $cmd = shift;
    print "Running $cmd\n";
    my ($stdout, $stderr, $success, $exit_code) = capture_exec($cmd);
    print "($stdout, $stderr, $success, $exit_code)\n";
    if ($exit_code != 0) {
        die "BAD EXIT CODE"
    }
}