package oan::loudly;

use 5.010;
use strict;
use constant { FALSE => 0, TRUE => 1 };
use utf8;

use Moan qw//;

BEGIN {
	$oan::loudly::AUTHORITY = 'cpan:TOBYINK';
	$oan::loudly::VERSION   = '0.001';
}

sub import
{
	my $class = shift;
	Moan->loudly(@_);
}

1;
