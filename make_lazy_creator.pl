#!/usr/bin/perl

use strict;

my @names = ();

while (<>) {
    chomp;
    push @names, $_;
}

print "#include <iostream>\n";
print "extern \"C\" {\n";
foreach my $name (@names) {
    print "extern void $name (void);\n";
}
print "}\n";
print "void*\nlazy_creator (const std::string \&name)\n{\n";
foreach my $name (@names) {
    print "if (name == \"$name\") return (void*)$name;\n";
}
print "std::cout << \"could not resolve \" << \"`\" << name << \"'\" << std::endl;\n";
print "return NULL;\n";
print "}\n";
