#!/usr/bin/perl
# # MyWget.pl (based off wget-queue.pl (Based off of wget-queue.sh) (C) 2004 C. Maloey) (c) 2012 C.M. Coatmeur

use HTML::LinkExtor;
use LWP::Simple;
use File::Basename;
use File::Path;
use URI::Escape;
# use strict;
use Term::ANSIColor;

my $sep = "sep";

sub sep {
    print color "reset";
    my $title = shift;
    print colored ("\n$title", 'bold'), "\n\n";
    print color "reset";
}

$num_args = $#ARGV + 1;
if ($num_args != 3) {
    &$sep(' USAGE');

    print "
  -Where <page> is a full url, and <ext> the extension of the file(s) you wish to get.
  -Don't forget the quotes if your new dir has spaces in its name.
  -If the intermediate directories don't exist they will be created as well.";

    &$sep(' EXAMPLE');

    print "
  $0 http://site.org/sfa.html&a=guerilla mp3 'Music/Pop/Super furry Animals/Guerilla (2009)'
";
    exit;
}

$basic_url = $ARGV[0];
$base_url = uri_unescape($basic_url);

$parser = HTML::LinkExtor->new(undef, $base_url);
$parser->parse(get($base_url))->eof;
@links = $parser->links;
foreach $linkarray (@links) {
  local(@element) = @$linkarray;
  local($elt_type) = shift @element;
  while (@element) {
    local($attr_name, $attr_value) = splice (@element, 0, 2);
    # print $attr_value;
    $seen{$attr_value}++;
  }
}

&$sep('### Found :');
# for (sort keys %seen) { print $_, "\n"}
for (sort keys %seen) {
    if($_ =~ m/(.*?)\.$ARGV[1]/ && $_ !~ m/m3u/) {
	$base = basename($_);
	$dir  = dirname($_);
	($base, $dir, $ext) = fileparse($_);
	print color "bold green";
	# print "\n", $_, " (", uri_unescape($base), ")\n";
	print uri_unescape($base), "\n";
	print color "reset";
    }
}


my $quit = 0;

until ($quit) {
    print colored ("\n### DL those files in $ARGV[2]/? (Y/n) ", 'bold');
    print color "reset";
    # print "\n### DL those files in $ARGV[2]/? (Y/n) ";
    chomp(my $input = <STDIN>);

    if ($input =~ /^[Y]?$/i) {
	$mydir = $ARGV[2];
	if (-r $mydir) {
	    chdir($mydir);
	    `ls -la`;
	    for (sort keys %seen) {
		if($_ =~ m/(.*?)\.$ARGV[1]/ && $_ !~ m/m3u/) {
		    $base = uri_unescape(basename($_));
		    $dir  = dirname($_);
		    ($base, $dir, $ext) = fileparse($_);
		    $my_real_file = uri_unescape($base);
		    print colored ("$my_real_file", 'bold green'), "\n";
		    print color "reset";
		    `curl -# -C - -o '$my_real_file' '$_'`;
		}
	    }
	    $quit = 1;
	} else {
	    print colored ("\n### $mydir/ does not exist, create it? (Y/n) ", 'bold');
	    print color "reset";
	    # print "\n### $mydir/ does not exist, create it? (Y/n) ";
	    chomp(my $input = <STDIN>);
	    if ($input =~ /^[Y]?$/i) {
		`mkdir -p '$mydir'`;
		chdir($mydir);
		for (sort keys %seen) {
		    if($_ =~ m/(.*?)\.$ARGV[1]/ && $_ !~ m/m3u/) {
		    $base = uri_unescape(basename($_));
		    $dir  = dirname($_);
		    ($base, $dir, $ext) = fileparse($_);
		    # `wget -c '$_' -P $mydir` ;
		    $my_real_file = uri_unescape($base);
		    print colored ("$my_real_file", 'bold green'), "\n";
		    print color "reset";
		    `curl -# -C - -o '$my_real_file' '$_'`;
		    }
		}
		$quit = 1;
	    } else {
		print "\n### Bye!\n";
		$quit = 1;
	    }
	}

    } elsif ($input =~ /^[N]$/i) {
        print "\n### Bye!\n";
        $quit = 1;
    } else {
        print "Please anwser to proceed.\n";
    }
}
