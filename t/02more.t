use Test::More tests => 2;
use Test::Warn;
use Moan qw(assert);

warning_like
	{
		assert(0, 'BUM');
	}
	qr/BUM/, 'assertions print warnings';

warning_like
	{
		Moan::loudly(':wibble');
		Moan::diag('BUM', ':wibble');
	}
	qr/BUM/, 'diag is visible when loud';
