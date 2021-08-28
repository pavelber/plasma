#!/usr/bin/perl

# calculates total ion rates from total.dat

$popdiv = shift;

open TOT, 'total.dat' or die "Can't open total.dat: $!\n";

while (<TOT>) {
	next unless /Conf/;
    
    $_ = <TOT>;
    my ($ch) = (/(\d\d)\//);
    my $pop = substr $_,43,12;
    $N{$ch} += $pop;
    $_ = <TOT>;
    while (<TOT>) {
    	last if /^\s*$/;
        s/^.+\@//;
        
        while (s/\b([a-zA-Z]{3}): (\S+)//) {
#        	${$1}{$ch} += ($popdiv ? $2/$pop : $2);
        	${$1}{$ch} += $2;
        }
    }
}

close TOT;

foreach my $pr ('Ion','RaR','3bR') {
	map {printf "$pr: $_ %12.5e\n",($popdiv ? ${$pr}{$_}/$N{$_} : ${$pr}{$_})} sort {$a<=>$b} keys %{$pr};
}
