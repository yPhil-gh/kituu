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

my $FORTUNE = `fortune -a`;

my $LINEWIDTH = $WIDTH - 1;

my $TOP =  "/" . '‾' x $LINEWIDTH . "\\\n";
my $BOTTOM =  "\\" . '_' x $LINEWIDTH . "/\n";

my $EYES = "oo";

my $COW ="
       \\   ^__^
        \\  ($EYES)\\_______
           (__)\\       )\\/\\
                ||----w |
                ||     ||
";

print $TOP;

my $text = wrap('| ', '| ', $FORTUNE) . "\n";
$text =~ s/(^.+)\K\|/' ' x ($Text::Wrap::columns - length($1) -1) . ' |'/gem;

my $CHOP = chop($text);

print $text;

print $BOTTOM;

print $COW;
