#!/usr/bin/perl
# # MyWget.pl (based off wget-queue.pl (Based off of wget-queue.sh) (C) 2004 C. Maloey) (c) 2012 C.M. Coatmeur

# # Reads from a todo list and downloads each of the files in turn.
# # Maintains a list of URLS already downloaded to avoid extra effort

# use Digest::MD5 qw(md5_hex);
# use File::Path
# # Initialize (Put this in a config file?)
# $home_dir = $ENV{HOME};
# $user = $ENV{USER};
# chdir "$home_dir/Downloads/";
# $list_files = "$home_dir/Downloads/MyWget-to-do.txt";
# $completed_files = "$home_dir/Downloads/MyWget-done.txt";
# $log = "$home_dir/Downloads/MyWget.log";
# $prog = "/usr/bin/wget";
# $done = ();
# # End of init. Shouldn't need to configure after this point

# # Build completed files hash
# if (-e $completed_files) {
# 	open DONE, $completed_files;
# 	while (<DONE>) {
# 		chomp;
# 		$done{$_} =1;
# 	}
# 	close DONE;
# }

# open DONE, ">>$completed_files" || warn "Can't write completed files\n";
# open LIST, $list_files || die "No list of files to process. Exiting.\n";

# while (<LIST>) {
# 	$line = $_;
# 	chomp $line;
# 	$digest = md5_hex($line);
# 	if (not($done{$digest})) { # Haven't seen this URL before
# 			print "Retreiving $line\n";
# 			`$prog -a $log -c "$line"`;
# 			if ($? != 0) {
# 				warn "Download failed. Please check the logs for more information\n";
# 			} else {
# 				print DONE "$digest\n";
# 				print "Download completed. Writing digest.\n";
# 			}
# 	} else {
# 		print "Already downloaded $line. Skipping.\n";
# 	}
# }
# close LIST;
# close DONE;

# dirs #############
# my $quit = 0;

# until ($quit) {
#     print "Create a new dir for those files? (Y/n) ";
#     chomp(my $input = <STDIN>);

#     if ($input =~ /^[Y]?$/i) {      # Match Yy or blank
# 	print "Path of the new dir? > ";
# 	$mypath = <>;
# 	print "Dir is $mypath";
# 	mkpath $mypath || die $!;
#     } elsif ($input =~ /^[N]$/i) {  # Match Nn
#         print "No prize for you!\n";
#     } elsif ($input =~ /^[Q]$/i) {  # Match Qq
#         print "Ok, you're done.\n";
#         $quit = 1;
#     } else {
#         print "Please anwser to proceed.\n";
#     }
# }

# use LWP::UserAgent;
# use HTML::LinkExtor;
# use URI::URL;

# $url = "http://studio.parisson.com:8888/Music0/10cc/";  # for instance
# $ua = LWP::UserAgent->new;

# # Set up a callback that collect image links
# my @dllinx = ();
# sub callback {
#     my($tag, %attr) = @_;
# # if($mystring =~ m/mp3/) { print "Yes"; }
#     return if $tag =~ 'm/mp3/';  # we only look closer at <img ...>
#     # return if $mystring =~ m/mp3/;  # we only look closer at <img ...>
#     push(@dllinx, values %attr);
# }

# # Make the parser.  Unfortunately, we don't know the base yet
# # (it might be diffent from $url)
# $p = HTML::LinkExtor->new(\&callback);

# # Request document and parse it as it arrives
# $res = $ua->request(HTTP::Request->new(GET => $url),
# 		    sub {$p->parse($_[0])});

# # Expand all image URLs to absolute ones
# my $base = $res->base;
# @dllinx = map { $_ = url($_, $base)->abs; } @dllinx;

# # Print them out
# print join("\n", @dllinx), "\n";

# another one
use HTML::LinkExtor;
use LWP::Simple;
use File::Basename;
use File::Path;
use URI::Escape;

# quit unless we have the correct number of command-line args
$num_args = $#ARGV + 1;
if ($num_args != 3) {
  print "Usage: DL.pl page_url ext dir\n";
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

print "\n### Found :\n\n";
# for (sort keys %seen) { print $_, "\n"}
for (sort keys %seen) {
    if($_ =~ m/(.*?)\.$ARGV[1]/ && $_ !~ m/m3u/) {
	$base = basename($_);
	$dir  = dirname($_);
	($base, $dir, $ext) = fileparse($_);
	# print "\n", $_, " (", uri_unescape($base), ")\n";
	print uri_unescape($base), "\n";
    }
}


my $quit = 0;

until ($quit) {
    print "\n### DL those files in $ARGV[2]? (Y/n) ";
    chomp(my $input = <STDIN>);

    if ($input =~ /^[Y]?$/i) {      # Match Yy or blank
	# print "Path of the new dir? > ";
	# $mypath = <STDIN>;
	# print "Dir is $mypath";
	# mkpath $mypath || die $!;

	$mydir = $ARGV[2];

	if (-r $mydir) {
	    chdir($mydir);
	    `ls -la`;
	    for (sort keys %seen) {
		if($_ =~ m/(.*?)\.$ARGV[1]/ && $_ !~ m/m3u/) {
		    $base = uri_unescape(basename($_));
		    $dir  = dirname($_);
		    ($base, $dir, $ext) = fileparse($_);
		    # `wget -c '$_' -P $mydir` ;
		    $my_real_file = uri_unescape($base);
		    print $my_real_file, ": \n";
		    exec("curl -# -C - -o '$my_real_file' '$_' 2\>&");
		    `curl -# -C - -o '$my_real_file' '$_'`
		}
	    }
	    $quit = 1;
	} else {
	    print "\n### $mydir does not exist, create it? (Y/n) ";
	    chomp(my $input = <STDIN>);
	    if ($input =~ /^[Y]?$/i) {
		system "mkdir -p $mydir" || die $!;
		chdir($mydir);
		for (sort keys %seen) {
		    if($_ =~ m/(.*?)\.$ARGV[1]/ && $_ !~ m/m3u/) {
			print $_, ": \n";
			`curl -# -C -O '$_'`
		    }
		}
		$quit = 1;
	    } else {
		print "\### Bye!\n";
		$quit = 1;
	    }
	}

    } elsif ($input =~ /^[N]$/i) {
        print "\### Bye!\n";
        $quit = 1;
    } else {
        print "Please anwser to proceed.\n";
    }
}
