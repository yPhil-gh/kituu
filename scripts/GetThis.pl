#!/usr/bin/perl
# # MyWget.pl (c) 2012 C.M. Coatmeur


use diagnostics;
use diagnostics -verbose;
enable  diagnostics;

use strict;

use HTML::LinkExtor;
use LWP::Simple;
use File::Basename;
use File::Path;
use URI::Escape;
use Term::ANSIColor;

my $quit = 0;
my $mydir = $ARGV[1];
my $h = "'$ENV{HOME}'";
$mydir =~ s/~/$h/ee;

sub canonicalize {
  my $uri = shift;

  my $url = URI->new( $uri );
  my $scheme = $url->scheme;
  my $domain = $url->host . ":";
  my $port = $url->port;
  my $rest = $url->path;
  my $myfulluri = $scheme . "://" . $domain . $port . uri_escape($rest, "'\\|(\\|)\\|\\[\\|\\]");
  return $myfulluri;
}

sub prettyname {
  my $name = shift;
  (my $base, my $dir, my $ext) = fileparse($_);
  my $prettyname = uri_unescape($base);
  print colored ("\n$prettyname", 'bold green'), "\n";
  print color "reset";
}

my $num_args = $#ARGV + 1;
if ($num_args != 3) {
  print "
USAGE :
\t$0 {uri} {dir} {ext}

\t-If there are spaces in your new dir name, enclose it with quotes.
\t-If the intermediate directories don't exist they will be created as well.
\t-If your URI has spaces in it (feh) encode them (replace with %20) or enclose the URI in quotes.

EXAMPLE

\t\$$0 http://site.org:81/scorn.html&a=colossus mp3 'Music/Dub/Colossus (1991)'
";
  exit;
}

my $basic_url = $ARGV[0];
my $linkarray;
my $parser = HTML::LinkExtor->new(undef, $basic_url);
$parser->parse(get($basic_url))->eof;
my @links = $parser->links;
my %seen;
foreach $linkarray (@links) {
  my @element = @$linkarray;
  my $elt_type = shift @element;
  while (@element) {
    (my $attr_name, my $attr_value) = splice (@element, 0, 2);
    $seen{$attr_value}++;
  }
}

print colored ("\n### Found :", 'bold'), "\n\n";
print color "reset";

print "\n(" . keys( %seen ) . ")\n";

for (sort keys %seen) {
  if ($_ =~ m/(.*?)\.$ARGV[2]$/) {
    (my $base, my $dir, my $ext) = fileparse($_);
    print color "bold green";
    print uri_unescape($base), "\n";
    print color "reset";
  }
}

until ($quit) {
  print colored ("\n### DL those files in [", 'bold');
  print color "reset";
  print colored ("$mydir", 'bold blue');
  print colored ("]? (Y/n) ", 'bold');
  print color "reset";
  chomp(my $input = <STDIN>);

  if ($input =~ /^[Y]?$/i) {
    if (-r $mydir) {
      chdir($mydir);
      `ls -la`;
      for (sort keys %seen) {
	if ( $_ =~ m/(.*?)\.$ARGV[2]$/ ) {
	  my $myuri = canonicalize($_);
	  my $prettyname = prettyname($_);
	  (my $base, my $dir, my $ext) = fileparse($_);
	  my $my_real_file = uri_unescape($base);
	  print color "reset";

	  `curl -# -C - -o "$my_real_file" $myuri`;
	}
      }
      $quit = 1;
    } else {
      print color "reset";
      print colored ("\n### [", 'bold');
      print colored ("$mydir", 'bold blue');

      print colored ("] does not exist, create it? (Y/n) ", 'bold');
      print color "reset";
      chomp(my $input = <STDIN>);
      if ($input =~ /^[Y]?$/i) {
	`mkdir -p '$mydir'`;
	chdir($mydir);
	for (sort keys %seen) {
	  if ( $_ =~ m/(.*?)\.$ARGV[2]$/ ) {
	    my $myuri = canonicalize($_);
	    my $prettyname = prettyname($_);

	    (my $base, my $dir, my $ext) = fileparse($_);
	    my $my_real_file = uri_unescape($base);
	    print color "reset";

	    `curl -# -C - -o "$my_real_file" $myuri`;
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
