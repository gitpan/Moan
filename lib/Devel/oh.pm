package Devel::oh;

use 5.010;
use strict;
use constant { FALSE => 0, TRUE => 1 };
use utf8;

use Moan qw//;

BEGIN {
	$Devel::oh::AUTHORITY = 'cpan:TOBYINK';
	$Devel::oh::VERSION   = '0.001';
}

sub import
{
	my $class = shift;
	Moan->loudly(@_);
	Moan->fatally(@_);
}

sub DB::DB {};

1;