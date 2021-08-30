#!/usr/bin/perl
use File::Spec::Functions 'catfile';
use Cwd;

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
}
else {
    &doit;
}

exit 0;

# --------------------

sub doit {
    my $dir = getcwd;
    my $status;
    #unlink < check_ *>;

    if (-e 'IN1.INP') {
        print "Fixing IN1.INP\n";
        open INP, 'IN1.INP' or die "hoho\n";

        my $old;
        while (<INP>) {
            my $numb = substr $_, 11, 5;
            my $current = $1 if /\b(\d+\.\d+)\b/;
            if ($current != 0) {
                if ($current == $old) {
                    print "Same energy for $current $old at $numb\n";
                }
                elsif ($current < $old) {
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
            print "EXCIT\n";
            my $check_path =  catfile($dir, 'check.exe');
            print "running ${check_path}\n";
            my $check_path_pipe =  "${check_path}|";
            open EXC, $check_path_pipe or die "Can't start check: $!\n"; #TODO: check exit code!
            my ($totlines, $badx);
            while (<EXC>) {
                ($totlines, $badx) = /(\d+)  lines with\s+(\d+)\s+bad lines/;
                print $totlines," ",$badx,"\n";
            }
            close EXC;
            last EXCLOOP if $badx == 0;
            print "EXCIT before system\n" ;
            system "perl fix_excit.pl -1", next EXCLOOP if $ind == 1; #TODO: check exit code!
            if ($ind == 10 or $badx / $totlines < 0.001) {
                print "EXCIT before system 1.5\n" ;
                system "fix_excit.pl 1"; #TODO: check exit code!
                last EXCLOOP;
            }
            print "EXCIT before system 2\n" ;
            system "perl fix_excit.pl"; #TODO: check exit code!
        }
        print "done EXC\n";
    }

    # BCFP.INP

    if (-e 'BCFP.INP') {
         print "\nBCFP before system check\n" ;
         my $check_path =  catfile($dir, 'check_bcfp.exe');
        print "running ${check_path}\n";

        $status = system $check_path;
        print $status ;
        print "\n";
        if (($status >>= 8) != 0) {
            die "Failed to run check_bcfp";
        }
          print "\nBCFP before system fix\n" ;
        $status = system "perl fix_bcfp.pl";
        print $status ;
        print "\n";
        if (($status >>= 8) != 0) {
            die "Failed to run perl fix_bcfp";
        }
        print "done BCFP\n";
    }

    # RREC.INP

    if (-e 'RREC.INP') {
        $ind = 0;

        RRLOOP:
        while (1) {
            $ind++;
            print "check_rr\n";
            my $check_path =  catfile($dir, 'check_rr.exe');
            print "running ${check_path}\n";
            my $check_path_pipe =  "${check_path}|";
            open RR, $check_path_pipe or die "Can't start check_rr: $!\n"; #TODO: check exit code!
            my ($totlines, $badx);
            while (<RR>) {
                ($totlines, $badx) = /(\d+)  lines with\s+(\d+)\s+bad lines/;
            }
            close RR;
            print "RREC BAD = $badx\n";
            last RRLOOP if $badx == 0;
            if ($ind == 50 or $badx / $totlines < 0.001) {
                system "perl fix_rr.pl"; #TODO: check exit code!
                last RRLOOP;
            }
            system "perl fix_rr.pl 1"; #TODO: check exit code!

        }
        print "done RREC\n";
    }

}
