# Perl Test
# By saul-bt
# PUBLIC DOMAIN

use strict;
use warnings;

## REFERENCES ##
my @colors = ("red", "green", "blue");

# '\' can be used to get a reference
my $colorsRef = \@colors;

my %superHash = (
    "colors" => $colorsRef,
    # Also you can create an anonymous
    # array with '[]' ({} for hashes)
    # that returns the reference
    "numbers" => [1, 2, 3]
);

# Now the hash stores something like
# this: ("colors", ARRAY(0x...),
#        "numbers", ARRAY(0x...))

# And you can access these arrays with:
print qq(@{$superHash{"colors"}}\n);

# To print an element:
print qq(${$superHash{"numbers"}}[0]\n);
print $superHash{"colors"} -> [0], "\n";

# Size of array:
print scalar @{$superHash{"colors"}};


## ARRAYS ##
%meh1 = (num => 0, val => 4);
%meh2 = (
    num => 1,
    val => 3
);

@mehs = (\%meh1, \%meh2);

print $mehs[0]{val};


## HANDLERS & HEREDOC ##
print "What's your name? ";
$name = <STDIN>;
chomp($name);

print <<WELCOME;

Hi $name, Where are you from?
WELCOME

$place = <STDIN>;
chomp($place);

print <<GOODBYE;

Oh, you are $name from $place...
I hear that $place is a beautiful place.
It's nice meet people like you $name.
I hope to see you soon :)

Bye $name.
GOODBYE

open (content, "<", "file.txt");

for $line (<content>) {
    print $line;
}

print "What are you looking for? ";
$numResults = 0;
$word = <STDIN>;
chomp($word);

for $line (<>) {
  if ($line =~ m/\b$word\b/i) {
    $numResults += 1;
    print "[$word FOUND]> $line\n";
    next;
  }
  print $line;
}

print "\n\n=== There are $numResults coincidences ===";


## SCRIPT ARGUMENTS ##
$nargs = $#ARGV + 1;

print "There are $nargs arguments:\n";

for $arg (@ARGV) {
    print "- $arg\n";
}

## REGEX STUFF ##
$string = "Perl is cool";

if ($string =~ m/[Pp]erl/) {
  print "Yeah";
}
elsif ($string =~ m(perl)i) {
  print "Sad";
}
else {
  print "MEH";
}

# From my dummy recreation of printf
sub checkTypes {
    my @percents = @{scalar(shift)};
    my @args = @{scalar(shift)};
    my $size = scalar(@percents);

    foreach my $n (0..$size - 1) {
        my $currArg = $args[$n];
        my $currFormat = substr($percents[$n],-1);
	
        $currFormat eq 's' && $currArg =~ m/^\D+$/ ||
        $currFormat =~ m/[dx]/ && $currArg =~ m/^\d+$/ ||
        $currFormat eq 'f' && $currArg =~ m/^\d+(?:\.\d+)?$/ or
        die "'$currArg' can't be formatted as '$currFormat'";
    }
}

## WEIRD STUFF (JAPH) ##
# VMS <3
not exp log srand xor s qq qx xor
s x x length uc ord and print chr
ord for qw q join use sub tied qx
xor eval xor print qq q q xor int
eval lc q m cos and print chr ord
for qw y abs ne open tied hex exp
ref y m xor scalar srand print qq
q q xor int eval lc qq y sqrt cos
and print chr ord for qw x printf
each return local x y or print qq
s s and eval q s undef or oct xor
time xor ref print chr int ord lc
foreach qw y hex alarm chdir kill
exec return y s gt sin sort split

@P=split//,".URRUU\c8R";@d=split//,"\nrekcah xinU / lreP rehtona tsuJ";sub p{
@p{"r$p","u$p"}=(P,P);pipe"r$p","u$p";++$p;($q*=2)+=$f=!fork;map{$P=$P[$f^ord
($p{$_})&6];$p{$_}=/ ^$P/ix?$P:close$_}keys%p}p;p;p;p;p;map{$p{$_}=~/^[P.]/&&
close$_}%p;wait until$?;map{/^r/&&<$_>}%p;$_=$d[$q];sleep rand(2)if/\S/;print

''=~('(?{'.('-)@.)@_*([]@!@/)(@)@-@),@(@@+@)'
^'][)@]`}`]()`@.@]@%[`}%[@`@!#@%[').',"})')
