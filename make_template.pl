#!/usr/bin/perl

use strict;
use English;

my %snippets = ();

foreach my $filename (@ARGV[0 .. $#ARGV - 1]) {
    open FILE, $filename or die;
    while (<FILE>) {
	if (/^\/\* TEMPLATE (\w+) \*\/$/) {
	    my $name = $1;
	    my $snippet = "";

	    while (1) {
		$_ = <FILE> or die;
		last if /^\/\* END \*\/$/;
		$snippet .= $_;
	    }

	    $snippets{$name} = $snippet;
	}
    }
    close FILE;
}

open FILE, $ARGV[-1] or die;
while (<FILE>) {
    my $line = $_;
    while ($line =~ /\$def_(\w+)/) {
	my $name = $1;
	exists $snippets{$name} or die;
	$line = $PREMATCH . $snippets{$name} . $POSTMATCH;
    }
    print $line;
}
close FILE;
