#!/usr/bin/env perl

use Text::Wrap;

$num_args = $#ARGV + 1;

if ($num_args != 1) {
    $WIDTH = 40;
} else {
    $WIDTH = $ARGV[0];
}

$Text::Wrap::columns = $WIDTH;
$Text::Wrap::unexpand = 0;
$Text::Wrap::separator = "|$/";

my $FORTUNE = `/usr/games/fortune -a`;

chomp($FORTUNE);

my $LINEWIDTH = $WIDTH - 1;

my $TOP =  "\n " . '_' x $LINEWIDTH . "\n";
my $BOTTOM =  "\n " . '-' x $LINEWIDTH . "\n";

my $o = "oO";

my $COW ="       \\   ^__^
        \\  ($o)\\_______
           (__)\\       )\\/\\
                ||----w |
                ||     ||
";

my $text = wrap('/ ', '| ', $FORTUNE) . "\n";
$text =~ s/(^.+)\K\|/' ' x ($Text::Wrap::columns - length($1) -1) . ' |'/gem;

chop($text);

print $TOP . $text . $BOTTOM . $COW;
