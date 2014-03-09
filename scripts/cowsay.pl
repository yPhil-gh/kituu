#!/usr/bin/env perl

$num_args = $#ARGV + 1;

if ($num_args != 1) {
    $WIDTH = 40;
} else {
    $WIDTH = $ARGV[0];
}

use Text::Wrap;
$Text::Wrap::columns = $WIDTH;
$Text::Wrap::unexpand = 0;
$Text::Wrap::separator = "|$/";

my $FORTUNE = `/usr/games/fortune -a`;

my $LINEWIDTH = $WIDTH - 1;

my $TOP =  " " . '_' x $LINEWIDTH . "\n";
my $BOTTOM =  " " . '-' x $LINEWIDTH . "\n";

my $o = "oO";

my $COW ="
       \\   ^__^
        \\  ($o)\\_______
           (__)\\       )\\/\\
                ||----w |
                ||     ||
";

my $text = wrap('/ ', '| ', $FORTUNE) . "\n";
$text =~ s/(^.+)\K\|/' ' x ($Text::Wrap::columns - length($1) -1) . ' |'/gem;

my $CHOP = chop($text);

print $TOP . $text . $BOTTOM . $COW;
