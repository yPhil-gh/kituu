#!/usr/bin/perl
# MyWget.pl (based off wget-queue.pl (Based off of wget-queue.sh) (C) 2004 C. Maloey) (c) 2012 C.M. Coatmeur

# Reads from a todo list and downloads each of the files in turn.
# Maintains a list of URLS already downloaded to avoid extra effort

use Digest::MD5 qw(md5_hex);

# Initialize (Put this in a config file?)
$home_dir = $ENV{HOME};
$user = $ENV{USER};
chdir "$home_dir/Downloads/";
$list_files = "$home_dir/Downloads/MyWget-to-do.txt";
$completed_files = "$home_dir/Downloads/MyWget-done.txt";
$log = "$home_dir/Downloads/MyWget.log";
$prog = "/usr/bin/wget";
$done = ();
# End of init. Shouldn't need to configure after this point

# Build completed files hash
if (-e $completed_files) {
	open DONE, $completed_files;
	while (<DONE>) {
		chomp;
		$done{$_} =1;
	}
	close DONE;
}

open DONE, ">>$completed_files" || warn "Can't write completed files\n";
open LIST, $list_files || die "No list of files to process. Exiting.\n";

while (<LIST>) {
	$line = $_;
	chomp $line;
	$digest = md5_hex($line);
	if (not($done{$digest})) { # Haven't seen this URL before
			print "Retreiving $line\n";
			`$prog -a $log -c "$line"`;
			if ($? != 0) {
				warn "Download failed. Please check the logs for more information\n";
			} else {
				print DONE "$digest\n";
				print "Download completed. Writing digest.\n";
			}
	} else {
		print "Already downloaded $line. Skipping.\n";
	}
}
close LIST;
close DONE;
