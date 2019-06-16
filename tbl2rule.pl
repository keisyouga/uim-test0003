# generate scheme rule file from tablefile
#
# usage:
#   perl tbl2rule.pl < tablefile > scmfile
#
# aaa aa
# aaa ab
# bbb bb
# ccc cc
#  |
#  v
# ((("a" "a" "a")) ("aa" "ab"))
# ((("b" "b" "b")) ("bb"))
# ((("c" "c" "c")) ("cc"))

use strict;
use warnings;

binmode(STDIN, ':utf8');
binmode(STDOUT, ':utf8');

# hash of array
my %hoa = ();

while (<>) {
	my @F = split(' ');
	if (@F == 2) {
		push @{$hoa{$F[0]}}, $F[1];
	}
}

print "(\n";
for my $stroke (sort (keys %hoa)) {
	print "(((";
	print join(' ', map { quote($_) } split(//, $stroke));
	print ")) (";
	print join(' ', map { quote($_) } @{$hoa{$stroke}});
	print "))";
	print "\n";
}
print ")\n";

# str => "str"
# str"\ => "str\"\\"
sub quote {
	my $s = shift;

	# escape \
	$s =~ s/\\/\\\\/g;

	# escape "
	$s =~ s/"/\\\"/g;

	# quote
	$s =~ s/^.*$/"$&"/;

	return $s;
}
