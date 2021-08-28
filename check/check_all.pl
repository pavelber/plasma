#!/usr/bin/perl

# EXCIT.INP

$ssh = shift;

if ($ssh eq '-d') {
	foreach my $dir (<*>) {
    	next unless -d $dir;
        chdir $dir;
        print "Directory: $dir\n";
        &doit;
        chdir '..';
    }
} else {
	&doit;
}

exit 0;

# --------------------

sub doit {

unlink <check_*>;


if (-e 'IN1.INP') {
	open INP, 'IN1.INP' or die "hoho\n";

    my $old;
    while (<INP>) {
    	my $numb = substr $_,11,5;
        my $current = $1 if /\b(\d+\.\d+)\b/;
        if ($current != 0) {
        	if ($current == $old) {
        		print "Same energy for $current $old at $numb\n";
            } elsif ($current < $old) {
            	print "Wrong energy order for $current $old at $numb\n";
            }
        }
        $old = $current;
    }
    close INP;
}

exit 0 if $ssh eq '-e';

if (-e 'EXCIT.INP') {

	$ind = 0;

	EXCLOOP:
	while (1) {
		$ind++;
	    print '.';
		open EXC,"check.exe|" or die "Can't start check: $!\n"; #TODO: check exit code!
	    my ($totlines,$badx);
	    while (<EXC>) {
	    	($totlines,$badx) = /(\d+)  lines with\s+(\d+)\s+bad lines/;
	    }
	    close EXC;
	    last EXCLOOP if $badx == 0;
	    system "perl fix_excit.pl -1", next EXCLOOP if $ind == 1; #TODO: check exit code!
	    if ($ind == 10 or $badx/$totlines < 0.001) {
	    	system "fix_excit.pl 1"; #TODO: check exit code!
	        last EXCLOOP;
	    }
	    system "perl fix_excit.pl"; #TODO: check exit code!
	}
	print "done EXC\n";
}

# BCFP.INP

if (-e 'BCFP.INP') {
	system "check_bcfp.exe; perl fix_bcfp.pl"; #TODO: check exit code!
	print "done BCFP\n";
}

# RREC.INP

if (-e 'RREC.INP') {
	$ind = 0;

	RRLOOP:
	while (1) {
		$ind++;
	    print '.';
		open RR,"check_rr.exe|" or die "Can't start check_rr: $!\n";  #TODO: check exit code!
	    my ($totlines,$badx);
	    while (<RR>) {
	    	($totlines,$badx) = /(\d+)  lines with\s+(\d+)\s+bad lines/;
	    }
	    close RR;
	    last RRLOOP if $badx == 0;
	    if ($ind == 50 or $badx/$totlines < 0.001) {
	    	system "perl fix_rr.pl";  #TODO: check exit code!
	        last RRLOOP;
	    }
	    system "perl fix_rr.pl 1";  #TODO: check exit code!

	}
print "done RREC\n";
}

}
