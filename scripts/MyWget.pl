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

    print "\t$0 {uri} {ext} {dir}

\t-If there are spaces in your new dir name, enclose it with quotes.
\t-If the intermediate directories don't exist they will be created as well.
\t-If your URI has spaces in it (feh) encode them (replace with %20) or enclose the URI in quotes.
";

    &$sep(' EXAMPLE');

    print "\t\$$0 http://site.org:81/scorn.html&a=colossus mp3 'Music/Dub/Colossus (1991)'
";
    exit;
}

$basic_url = $ARGV[0];
# $base_url = uri_unescape($basic_url);

$parser = HTML::LinkExtor->new(undef, $basic_url);
$parser->parse(get($basic_url))->eof;
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
    print colored ("\n### DL those files in ", 'bold');
    print color "reset";
    print colored ("$ARGV[2]/", 'bold blue');
    print colored ("? (Y/n) ", 'bold');
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

		    my $url = URI->new( $_ );
		    my $scheme = $url->scheme;
		    my $domain = $url->host . ":";
		    my $port = $url->port;
		    my $rest = $url->path;
		    $myfulluri = $scheme . "://" . $domain . $port . uri_escape($rest, "][");
		    print $myfulluri . "\n\n";

		    # $base = uri_unescape(basename($_));
		    # $dir  = dirname($_);
		    # ($base, $dir, $ext) = fileparse($_);
		    # $my_real_file = uri_unescape($base);
		    # print colored ("\n$my_real_file", 'bold green'), "\n";
		    # print color "reset";
		    # $real_uri = uri_unescape($_);
		    # # print "(", $dir, ")\n\n";
		    # `curl -# -C - -o '$my_real_file' '$real_uri'`;
		    # system(curl -# -C - -o '$my_real_file' '$real_uri');
		    # @args = ("curl -L ", "'$real_uri'", "'$my_real_file' ");
		    # @myurl = ($dir, uri_escape($real_uri, "]["), $ext);
		    # # exec(@args) || die $!;
		    # # print @args;
		    # print "\n\n";
		    # # print $dir;
		    # print $_;
		    # print "\n\n";
		    # # or die "system @args failed: $?"

		}
	    }
	    $quit = 1;
	} else {
	    print color "reset";
	    print colored ("\n### ", 'bold');
	    print colored ("$mydir/ ", 'bold blue');

	    print colored ("does not exist, create it? (Y/n) ", 'bold');
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
		    $my_real_uri = uri_unescape($_);
		    print colored ("$my_real_uri", 'bold green'), "\n";
		    print color "reset";
		    `curl -# -C - -o '$base' $my_real_uri`;
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

# http://studio.parisson.com:8888/Music2/Pablo%20Moses/Pave%20the%20Way%20%5BDub%5D%20Disc%201/01%20Proverbs%20Extractions.mp3

# http://studio.parisson.com:8888/Music0/25%20ans%20de%20radio%20Nova/1981/01%20-%20Jingle%20-%201981.mp3
