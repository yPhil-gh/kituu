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

# system("fortune");


# print("Fortune : $FORTUNE");

# $FORTUNE =~ s/([^\n]{0,30})(?:\b\s*|\n)/$1\n/gi;

# print $FORTUNE

# print "-" x 30,"\n";

# $Text::Wrap::columns = 72;
# print wrap('', '', @text);


# WIDTH=30
# str="$(fortune)"
# str=`fortune -s`
# # str="$1"

# underscore=$(seq -s "_" `expr $WIDTH - 1` | sed 's/[0-9]//g')
# # echo $underscore
# echo $str

# echo "
 # _______________________________________
# /                                       \\"

# while [ ! -z "$str" ]
# do
#     echo "| ${str:0:$WIDTH} "
#     str="${str:$WIDTH}"
# done

# echo "\\                                       /"
# echo " ---------------------------------------"

# echo "        \\   ^__^
#          \\  (oo)\\_______
#             (__)\\       )\\/\\
#                 ||----w |
#                 ||     ||"
