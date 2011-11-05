package oan::quietly;

use 5.010;
use strict;
use constant { FALSE => 0, TRUE => 1 };
use utf8;

use Moan qw//;

BEGIN {
	$oan::quietly::AUTHORITY = 'cpan:TOBYINK';
	$oan::quietly::VERSION   = '0.001';
}

sub import
{
	my $class = shift;
	Moan->quietly(@_);
}

1;
