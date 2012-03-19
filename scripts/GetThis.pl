#!/usr/bin/perl
# # GetThis.pl (c) 2012 C.M. Coatmeur

use HTML::LinkExtor;
use LWP::Simple;
use File::Basename;
use File::Path;
use URI::Escape;
# use strict;
use Term::ANSIColor;
use List::Util 'max';

my $sep = "sep";

sub sep {
    print color "reset";
    my $title = shift;
    print colored ("\n$title", 'bold'), "\n\n";
    print color "reset";
}

sub canonicalize {
    my $uri = shift;

    my $url = URI->new( $uri );
    my $scheme = $url->scheme;
    my $domain = $url->host . ":";
    my $port = $url->port;
    my $rest = $url->path;
    $myfulluri = $scheme . "://" . $domain . $port . uri_escape($rest, "][|)(");
    return $myfulluri;
}

sub prettyname {
    my $name = shift;
    (my $base, my $dir, my $ext) = fileparse($_);
    my $prettyname = uri_unescape($base);
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
    @myarray = $seen{$attr_value}++;
    $plop{$attr_value}++;
  }
}


%seen = ();
while (defined ($key = each %plop)) {
    $seen{$key}++;
}
%uniq = keys %seen;

# @uniq = keys %seen;

for (sort keys %uniq) {
    if($_ =~ m/(.*?)\.$ARGV[1]/ && $_ !~ m/m3u/) {
	print "\n(" . $_ . ")\n\n";
    }
}

# use Data::Dumper;
# print Dumper(\@uniq);
print "size of hash:  " . %uniq . ".\n";
# print "We got " . (scalar @uniq) . " elements";

&$sep('### Found $size files');
# for (sort keys %seen) { print $_, "\n"}
for (sort keys %uniq) {
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

# my $i = 0;
# for (sort keys %seen) {
#     if($_ =~ m/(.*?)\.$ARGV[1]/ && $_ !~ m/m3u/) {
# 	$myhash{$_}++;
# 	# print $i++ . ">" . $_ . "\n\n";
#     }
# }

# # my %saw = ();
# # my @unique = grep { ! $saw{ $_ }++ } %myhash;

# # # my @unique = uniq(%seen);

# # print @unique;

# $size = keys %seen;
# $hashish = $size / 2;
# print ">" . $size;

my $quit = 0;

until ($quit) {
    print colored ("\n### DL those files in [", 'bold');
    print color "reset";
    print colored ("$ARGV[2]", 'bold blue');
    print colored ("]? (Y/n) ", 'bold');
    print color "reset";
    # print "\n### DL those files in $ARGV[2]/? (Y/n) ";
    chomp(my $input = <STDIN>);

    if ($input =~ /^[Y]?$/i) {
	$mydir = $ARGV[2];
	# if ($sentence =~ /~/);
	$string = $mydir;
	$string =~ s/~/zob/g;
	print "(" . $string . ")\n\n";
	if (-r $mydir) {
	    chdir($mydir);
	    `ls -la`;
	    for (sort keys %uniq) {
		if($_ =~ m/(.*?)\.$ARGV[1]/ && $_ !~ m/m3u/) {
		    $myuri = canonicalize($_);
		    $prettyname = prettyname($_);
		    print colored ("\n$prettyname", 'bold green'), "\n";
		    print color "reset";


		    # use IPC::Open3;
		    # use File::Spec;
		    # use Symbol qw(gensym);
		    # open(NULL, ">", File::Spec->devnull);
		    # my $pid = open3(gensym, \*PH, ">&NULL", "curl -# -C - -o $prettyname $myuri");
		    # while( <PH> ) { }
		    # waitpid($pid, 0);
		    `curl -# -C - -o "$prettyname" $myuri`;
		}
	    }
	    $quit = 1;
	} else {
	    print color "reset";
	    print colored ("\n### [", 'bold');
	    print colored ("$mydir", 'bold blue');

	    print colored ("] does not exist, create it? (Y/n) ", 'bold');
	    print color "reset";
	    # print "\n### $mydir/ does not exist, create it? (Y/n) ";
	    chomp(my $input = <STDIN>);
	    if ($input =~ /^[Y]?$/i) {
		`mkdir -p '$mydir'`;
		chdir($mydir);
		for (sort keys %uniq) {
		    if($_ =~ m/(.*?)\.$ARGV[1]/ && $_ !~ m/m3u/) {
		    $myuri = canonicalize($_);
		    $prettyname = prettyname($_);
		    print colored ("\n$prettyname", 'bold green'), "\n";
		    print color "reset";
		    `curl -# -C - -o '$prettyname' $myuri`;
		    print "beeeuuuarh \n\n";
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
# http://studio.parisson.com:8888/Music2/Pablo Moses/Pave the Way [Dub] Disc 01/
