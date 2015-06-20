#!/usr/bin/env perl

# A simple clone of cowsay for when it's not in your repos

$width = 40;
$o = "oO";

use Text::Wrap;

if ($#ARGV + 1 == 1) {
    if ($ARGV[0] eq "fortune") {
        $moo = `/usr/games/fortune -a`;
    } elsif ($ARGV[0] eq "dlfp") {
        $moo = `curl http://sam.linuxfr.org/random.txt 2>/dev/null`;
    } else {
        $moo = $ARGV[0];
    }
} else {
    $moo = "I got nothing to moo";
}

$Text::Wrap::columns = $width;
$Text::Wrap::unexpand = 0;
$Text::Wrap::separator = "|$/";

chomp($moo);

my $linewidth = $width - 1;

my $top =  "\n " . '_' x $linewidth . "\n";
my $bottom =  "\n " . '-' x $linewidth . "\n";


my $cow ="       \\   ^__^
        \\  ($o)\\_______
           (__)\\       )\\/\\
                ||----w |
                ||     ||
";

my $text = wrap('/ ', '| ', $moo) . "\n";
$text =~ s/(^.+)\K\|/' ' x ($Text::Wrap::columns - length($1) -1) . ' |'/gem;

chop($text);

print $top . $text . $bottom . $cow;
