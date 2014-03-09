#!/usr/bin/env perl

use Text::Wrap;
my $WIDTH = 39;
$Text::Wrap::columns = $WIDTH;
# $Text::Wrap::separator = "\t|\n";
# $Text::Wrap::separator2 = "\t|\n";
$Text::Wrap::separator = "|$/";
# $Text::Wrap::separator2 = "|";

my $FORTUNE = `fortune -a`;

my $LINEWIDTH = $WIDTH - 2;

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
$text =~ s/(^.+)\K\|/' ' x ($Text::Wrap::columns - length($1)) . '|'/gem;
print $text;

print $BOTTOM;

print $COW;
