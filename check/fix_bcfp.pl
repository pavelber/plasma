#!/usr/bin/perl

# removes transitions with A < 0

if (shift) {
	print "Starting check_bcfp...\n";
	system "check_bcfp";
}

print "starting fix_bcfp.pl"

my %check;

if (-e 'check_bcfp') {
	exit 0 if -z 'check_bcfp';
    open CHECK, 'check_bcfp';
    while (<CHECK>) {
        my @b = split;
        next if $b[2] != $b[0]+1;
        my $key = $b[0].'.'.$b[1].'.'.$b[3];
        $check{$key} = 1;
        print "+\n";
    }
    close CHECK;


	open BC, 'BCFP.INP';
	open BB, ">bb";

	while (<BC>) {
	    (print BB), next if /^\s*$/;
	    $total++;
		my @a = split;
	    
	    my $key = $a[0].'.'.$a[1].'.'.$a[3];
	    
	    if ($check{$key}) {
	        $bad++;
	        next;
	    } else {
	        print BB;
	    }
	    
	    next;
	    
		if ($a[4] >= 0) {
			print BB;
		} else {
			print "$a[0]  $a[1]  $a[2]  $a[3]\n";
	        $bad++;
		}
	}
	
	close BC;
	close BB;
	
	rename 'BCFP.INP', 'BCFP.INP.old';
	system "mv -f bb BCFP.INP";
	system "gzip -f BCFP.INP.old";
	
	print "---\n",($bad || '0 ')." bad transitions out of $total\n---\n";
	
}

exit 0;
