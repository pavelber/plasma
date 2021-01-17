#!c:\Perl64

sub usage {

print <<EOP;
usage:

$0  [-t <levs>] [-e <excfile>] [-i <ionfile>] [-a <autofile>] [-p <recfile>] [-r <trans>] [-h] [<file>] 

  where <levs>     -- total number of levels
        <file>     -- FAC levels file name (default: fac.lev)
        <excfile>    -- FAC excitations file name (default: fac.ce)
        <trans>    -- FAC transitions file name (default: fac.tr)
        <ionfile>  -- FAC ionization file name (default: fac.ci)
        <autofile> -- FAC autoionization file name (default: fac.ai)
        <recfile>  -- FAC photoionization file name (default: fac.rr)
        
EOP
}

# ------------------------------------------
$file = 'fac.lev';
$tranfile = 'fac.tr';
$excfile = 'fac.ce';
$recfile = 'fac.rr';
$ionfile = 'fac.ci';
$autofile = 'fac.ai';

while ($_ = shift) {
    /^-t/   && ($total = shift) && next;
    /^-r/   && ($tranfile = shift) && next;
    /^-e/   && ($excfile = shift) && next;
    /^-i/   && ($ionfile = shift) && next;
    /^-a/   && ($autofile = shift) && next;
    /^-p/   && ($recfile = shift) && next;
    /^-?-h/ && &usage && exit;
    $file = $_;

}

open INP, $file or die "Can't open $file: $!\n";

$_ = <INP> until /NLEV\s*=\s*(\d+)/;
$levs = $1;
$_ = <INP>;

open OUT, ">IN1.INP" or die "Can't open IN1.INP: $!\n";

for (1..$levs) {
    $_ = <INP>;
    $ind++;

    my @a = split;
    next if $total < $a[0];
    $sw[$ind] = $a[5]+1;
    
    my $shls = substr $_,63,14;
    
    $shls =~ s/^\s+//;
    $shls =~ s/\s+$//;
    
    my @shls = (split /\s+/,$shls);
    
    printf OUT " %-4s %-4s %4da %5d%12.3f     0.00e+00 0.00e+00\n",$shls[-2],$shls[-1],$ind,$sw[$ind],$a[2];
}

# checking ionization

while (<INP>) {
    if (/^\s+(\d+)\s+/) {
        $first_ion = $1;
        last;
    }
}

close OUT;
close INP;
	

!defined $total && ($total = $ind);

# ---------- transitions -----------
if (-e $tranfile) {
	print "start transitions\n";	
	
    open TRANS, $tranfile or die "Can't open $tranfile: $!\n";


    while (<TRANS>) {
        my @a = split;
        next unless $#a == 7;
    
        next if $total > 0 and ($a[2] > $total or $a[0] > $total);
    
        $osc[$a[2]]->[$a[0]] += $a[5]/($a[3]+1);
    }

    close TRANS;
    print "end transitions\n";	
}
# ---------- EXCITATION ------------

if (-e $excfile) {

	print "start excitations\n";	
	
    open EXC, $excfile or die "Can't open $excfile: $!\n";

    mkdir 'EXC';

    open INFO, ">EXC/info_ex.dat";


    while (<EXC>) {
        $negrid = $1 if /^NEGRID\s+=\s+(\d+)/;
    
        my @lev = split;
        next unless $#lev == 5;
    
	    next if $total and $lev[2] > $total;
	    
	    my $outfile = sprintf "%4.4d%4.4d.%s",$lev[0]+1,$lev[2]+1,'ex';
	    
	    open OUTEXC, ">EXC/$outfile" or die "Can't open file EXC/$outfile: $!\n";
	    
	    $_ = <EXC>;
	    my @trans = split;
	    printf INFO "%s\t%d\t%10.3e\n",$outfile,(($trans[0] > 0 && $osc[$lev[0]]->[$lev[2]] > 1e-3) ? 0 : 2),$osc[$lev[0]]->[$lev[2]];
	
	    print OUTEXC "\n\n";
	
	    for (1..$negrid) {
	        $_ = <EXC>;
	        chomp;
	        my @b = split;
	        if ($b[-1] > 0 ) {
	            printf OUTEXC "%10.3e  %10.3e  %10.3e\n",($b[0]+$lev[4])/$lev[4],$b[-1]*1e-20,$b[0]+$lev[4];
	        }
	    }
	    close OUTEXC;    
	    
	}
	
	close EXC;
	close INFO;
}
open EXCITINP, ">EXCIT.INP" or die "Can't open EXCIT.INP: $!\n";

foreach my $i (1..$total-1) {
    foreach my $j ($i+1..$total) {
        next if $om[$i]->[$j] == 0 and $osc[$i]->[$j] == 0;
        printf EXCITINP " 47  %4d %4d 1  %10.3e  0.000e+00  0.000e+00  0.000e+00  0.000e+00  0.000e+00  -%9.3e\n",
        $i,$j,$om[$i]->[$j],$osc[$i]->[$j];

    }
}

close EXCITINP;
print "end excitations\n";	

# ----- PHOTOIONIZATION ------

# $levs is the number of ALL CALCULATED levels for the given charge state so that
# $levs+1 is the next ground state

if (-e $recfile) {
	print "start photoionization\n";	
	open RREC, 'fac.rr' or die "Can't open file fac.rr: $!\n";
	
	my $recdir = "REC" ;
	mkdir $recdir unless -e -d $recdir;

	open INFOR, ">$recdir/info_ph.dat" or die "$recdir/info_ph.dat: $!\n";
	
	while (<RREC>) {
	
	    $negrid = $1 if /^NEGRID\s+=\s+(\d+)/;
	    my @lev = split;
#	    print "@lev\n";
	    next unless $#lev == 5;
	    
	    next if $total and $lev[0] > $total;
	    
	    my $outfile = sprintf "%4.4d%4.4d.%s",$lev[0]+1,$lev[2]-$levs+1,'ph';
	    print INFOR "$outfile\n";
	    
	    open OUTREC, ">$recdir/$outfile" or die "Can't open file $recdir/$outfile: $!\n";
	    
	    $_ = <RREC>;
	    for (1..$negrid) {
	        $_ = <RREC>;
	        chomp;
	        my @b = split;
	        if ($b[-2] > 0 ) {
	            printf OUTREC "%10.3e  %10.3e  %10.3e\n",($b[0]+$lev[4])/$lev[4],$b[-2]*1e-20,$b[0]+$lev[4];
	        }
	    }
	    close OUTREC;    
	}
	close INFOR;
	close RREC;
}
print "end photoionization\n";

# ---------- IMPACT IONIZATION ------------

if (-e $ionfile) {
	print "start impact ionization";

    print "Enter spectroscopic charge: ";
    $spch = <STDIN>;
    chomp($spch);
    open ION, $ionfile or die "Can't open $ionfile: $!\n";

    open OUTION, ">BCFP.INP";


    while (<ION>) {
        $negrid = $1 if /^NEGRID\s+=\s+(\d+)/;
    
        my @lev = split;
        next unless $#lev == 5;
    
	    next if $total and $lev[0] > $total;
	    
	    $_ = <ION>;
        chomp;
	    my @trans = split;
        next unless $#trans == 3;
	    printf OUTION "%5d %5d %5d %5d    ".(("\t%11.4e ")x4)."\n",$spch,$lev[0]+1,$spch+1,$lev[2]-$first_ion+1,$trans[-1],-$trans[0],@trans[1,2];
	}
	
	close ION;
	close OUTION;
	print "end impact ionization\n";
}

#exit;


