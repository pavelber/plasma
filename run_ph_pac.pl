use File::Path;
use IO::CaptureOutput qw/capture_exec/;

$spch = $1;
$ph_exe = $2;

print "Working on REC...\n";
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

sub run_exec() {
    my $cmd = shift;
    print "Running $cmd\n";
    my ($stdout, $stderr, $success, $exit_code) = capture_exec($cmd);
    print "($stdout, $stderr, $success, $exit_code)\n";
    if ($exit_code != 0) {
        die "BAD EXIT CODE"
}
}