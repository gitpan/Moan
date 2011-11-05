package Moan;

use 5.010;
use strict;
use constant { FALSE => 0, TRUE => 1 };
use utf8;

BEGIN {
	$Moan::AUTHORITY = 'cpan:TOBYINK';
	$Moan::VERSION   = '0.001';
}

use Devel::StackTrace;
use Scalar::Util qw[blessed looks_like_number];

use base qw[Exporter];

our (@EXPORT, @EXPORT_OK, %EXPORT_TAGS);
BEGIN {
	%EXPORT_TAGS = (
		testmore => [qw/ok is isnt like unlike can_ok isa_ok/],
		carpish  => [qw/assert affirm should shouldnt DEBUG/],
		standard => [qw/assert affirm diag/],
		);
	@EXPORT = @{ $EXPORT_TAGS{standard} };
	@EXPORT_OK = (
		qw/blessed_ok defined_ok does_ok TRUE FALSE/,
		@{ $EXPORT_TAGS{standard} },
		@{ $EXPORT_TAGS{testmore} },
		@{ $EXPORT_TAGS{carpish} },
		);
}

sub DEBUG { 1; }

sub import
{
	strict->import();
	feature->import(':5.10');
	$^H{Moan_ignore} = FALSE;
	return __PACKAGE__->export_to_level(1, @_);
}

sub unimport
{
	$^H{Moan_ignore} = TRUE;
}

BEGIN
{
	no strict 'refs';
	my %global;
	my %local;
	
	foreach my $feature (qw/quiet loud fatal/)
	{
		$global{$feature} = FALSE;
		$local{$feature}  = [];

		*{'_' . $feature . '_dump'} = sub
		{
			require Data::Dumper;
			local $Data::Dumper::Terse = FALSE;
			return Data::Dumper::Dumper({global => $global{$feature}, local => $local{$feature}});
		};

		*{'_' . $feature . '_is_enabled'} = sub
		{
			return TRUE if $global{$feature};
		
			my ($key) = @_;
			return TRUE if grep { $key ~~ $_ } @{$local{$feature}};
			return;
		};
		
		*{'_' . $feature . '_is_global'} = sub
		{
			return TRUE if $global{$feature};
		};
		
		*{'_' . $feature . '_enable'} = sub
		{
			my ($key) = @_;
			if (defined $key and !ref $key and $key =~ /^:[^:]+$/)
			{
				my @caller = caller(0);
				$key = $caller[0] . $key;
			}
			push @{$local{$feature}}, $key;
		};

		*{'_' . $feature . '_disable'} = sub
		{
			my ($key) = @_;
			if (defined $key and !ref $key and $key =~ /^:[^:]+$/)
			{
				my @caller = caller(0);
				$key = $caller[0] . $key;
			}
			@{$local{$feature}} = grep { "$key" eq "$_" } @{$local{$feature}};
		};

		*{'_' . $feature . '_enable_globally'} = sub
		{
			my $r = $global{$feature};
			$global{$feature} = TRUE;
			return $r;
		};

		*{'_' . $feature . '_dont_enable_globally'} = sub
		{
			my $r = $global{$feature};
			$global{$feature} = TRUE;
			return $r;
		};
		
		*{$feature . 'ly'} = sub
		{
			shift if $_[0] eq __PACKAGE__;
			
			my $en      = *{"_${feature}_enable"};
			my $dis     = *{"_${feature}_dis"};
			my $en_ALL  = *{"_${feature}_enable_globally"};
			my $en_nALL = *{"_${feature}_dont_enable_globally"};
			my @caller  = caller(0);
			my @keys    = @_ ? @_ : ($caller[0]);
			
			foreach my $key (@keys)
			{
				if (ref $key)
				{
					$en->($key);
				}
				
				if (defined $key and $key =~ /^:[^:]+$/)
				{
					$key = $caller[0] . $key;
				}
				
				if ($key eq 'ALL')
					{ $en_ALL->(); }
				elsif ($key eq '-ALL')
					{ $en_nALL->(); }
				elsif ($key =~ /^-(.+)$/)
					{ $en->($key); }
				else
					{ $en->($key); }
			}
		};
	}
}

my %diags;
sub diag ($;$)
{
	my ($message, $key) = @_;
	my @caller = caller(0);

	if (not defined $key)
	{
		$key = $caller[0];
	}
	elsif (defined $key and $key =~ /^:[^:]+$/)
	{
		$key = $caller[0] . $key;
	}
	
	chomp $message;
	push @{ $diags{$key} }, $message;
	warn "$message\n" if is_loud($key, \@caller);
}

sub _fail ($$$)
{
	my ($message, $key, $caller) = @_;
	chomp $message;
	
	if (not defined $key)
	{
		$key = $caller->[0];
	}
	elsif (defined $key and $key =~ /^:[^:]+$/)
	{
		$key = $caller->[0] . $key;
	}
	
	if (is_fatal($key, $caller))
	{
		if (is_loud($key, $caller))
		{
			$message = _garnish($message, $key, 1);
			die $message;
		}
		else
		{
			$message = sprintf("%s (fatal) at %s line %s (%s)\n", $message, $caller->[1], $caller->[2], $key);
			if (exists $diags{$key})
			{
				$message .= "\nPreceding diagnostics:\n";
				$message .= " - $_\n" foreach @{$diags{$key}};
			}
			die $message;
		}
	}
	else
	{
		if (is_loud($key, $caller))
		{
			$message = _garnish($message, $key, 0);
		}
		else
		{
			$message = sprintf("%s at %s line %s (%s)\n", $message, $caller->[1], $caller->[2], $key);
		}
		warn $message;
	}
}

sub _garnish
{
	my ($message, $key, $fatal) = @_;
	my $char = $fatal ? '!' : '=';
	
	$message =~ s/[\r\n\s]+$//;
	$message .= "\n";
	
	my $rv =
		($char x 4) . " " . $key . " " . ($char x (66 - length $key)) . "\n" .
		$message .
		"---- Trace " . ('-' x 61) . "\n" .
		Devel::StackTrace
			->new(ignore_package=>__PACKAGE__)
			->as_string .
		($char x 72) . "\n";
	$rv;
}

sub is_fatal (;$$)
{
	my ($key, $_call) = @_;
	my @caller = defined $_call ? @$_call : caller(0);
	
	if (not defined $key)
	{
		$key = $caller[0];
	}
	elsif (defined $key and $key =~ /^:[^:]+$/)
	{
		$key = $caller[0] . $key;
	}

	return _fatal_is_enabled($key);
}

sub is_quiet (;$$)
{
	my ($key, $_call) = @_;
	my @caller = defined $_call ? @$_call : caller(0);
	
	return TRUE if (defined $caller[10]{Moan_ignore} and $caller[10]{Moan_ignore});
	
	if (not defined $key)
	{
		$key = $caller[0];
	}
	elsif (defined $key and $key =~ /^:[^:]+$/)
	{
		$key = $caller[0] . $key;
	}

	return _quiet_is_enabled($key);
}

sub is_loud (;$$)
{
	my ($key, $_call) = @_;
	my @caller = defined $_call ? @$_call : caller(0);
	
	if (not defined $key)
	{
		$key = $caller[0];
	}
	elsif (defined $key and $key =~ /^:[^:]+$/)
	{
		$key = $caller[0] . $key;
	}

	return _loud_is_enabled($key);
}

sub ok ($;$$)
{
	my ($truthy, $message, $key) = @_;
	my $caller = [caller(0)];
	return TRUE if is_quiet($key, $caller);
	
	unless ($truthy)
	{
		_fail(($message // 'not ok'), $key, $caller);
		return FALSE;
	}
	
	return TRUE;
}

sub is ($$;$$)
{
	my ($given, $expected, $message, $key) = @_;
	my $caller = [caller(0)];
	return TRUE if is_quiet($key, $caller);
	my $pass   = TRUE;
	my $diag;
	
	if (looks_like_number($expected) and $given != $expected)
	{
		$diag = <<DIAG;
    given: $given
 expected: $expected
DIAG
		$pass = FALSE;
	}

	elsif (!looks_like_number($expected) and $given ne $expected)
	{
		$diag = <<DIAG;
    given: '$given'
 expected: '$expected'
DIAG
		$pass = FALSE;
	}
	
	return $pass if $pass;
	
	if (defined $message)
	{
		chomp $message;
		_fail "$message\n$diag          ", $key, $caller;
	}
	else
	{
		_fail "$diag          ", $key, $caller;
	}
	return FALSE;
}

sub isnt ($$;$$)
{
	my ($given, $expected, $message, $key) = @_;
	my $caller = [caller(0)];
	return TRUE if is_quiet($key, $caller);
	my $pass   = TRUE;
	my $diag;
	
	if (looks_like_number($expected) and $given == $expected)
	{
		$diag = <<DIAG;
    given: $given
DIAG
		$pass = FALSE;
	}

	elsif (!looks_like_number($expected) and $given eq $expected)
	{
		$diag = <<DIAG;
    given: '$given'
DIAG
		$pass = FALSE;
	}
	
	return $pass if $pass;
	
	if (defined $message)
	{
		chomp $message;
		_fail "$message\n$diag          ", $key, $caller;
	}
	else
	{
		_fail "$diag          ", $key, $caller;
	}
	return FALSE;
}

sub like ($$;$$)
{
	my ($given, $expected, $message, $key) = @_;
	my $caller = [caller(0)];
	return TRUE if is_quiet($key, $caller);
	my $pass   = TRUE;
	my $diag;
	
	if ($given !~ $expected)
	{
		$diag = <<DIAG;
    given: $given
 expected: $expected
DIAG
		$pass = FALSE;
	}

	return $pass if $pass;
	
	if (defined $message)
	{
		chomp $message;
		_fail "$message\n$diag          ", $key, $caller;
	}
	else
	{
		_fail "$diag          ", $key, $caller;
	}
	return FALSE;
}

sub unlike ($$;$$)
{
	my ($given, $expected, $message, $key) = @_;
	my $caller = [caller(0)];
	return TRUE if is_quiet($key, $caller);
	my $pass   = TRUE;
	my $diag;
	
	if ($given =~ $expected)
	{
		$diag = <<DIAG;
    given: $given
 expected: $expected
DIAG
		$pass = FALSE;
	}

	return $pass if $pass;
	
	if (defined $message)
	{
		chomp $message;
		_fail "$message\n$diag          ", $key, $caller;
	}
	else
	{
		_fail "$diag          ", $key, $caller;
	}
	return FALSE;
}

sub can_ok ($$;$$) 
{
	my ($value, $method, $message, $key) = @_;
	my $caller = [caller(0)];
	return TRUE if is_quiet($key, $caller);
	
	if (blessed $value)
	{
		unless ($value->can($method))
		{
			_fail(($message // "object cannot $method"), $key, $caller);
			return FALSE;
		}
	}
	elsif (defined $value)
	{
		unless ($value->can($method))
		{
			_fail(($message // "$value cannot $method"), $key, $caller);
			return FALSE;
		}
	}
	else
	{
		_fail(($message // "undef can't do anything"), $key, $caller);
		return FALSE;
	}
	
	return TRUE;
}

sub does_ok ($$;$$) 
{
	my ($value, $role, $message, $key) = @_;
	my $caller = [caller(0)];
	return TRUE if is_quiet($key, $caller);
	
	if (blessed $value)
	{
		unless ($value->DOES($role))
		{
			_fail(($message // "object doesn't do $role"), $key, $caller);
			return FALSE;
		}
	}
	elsif (defined $value)
	{
		unless ($value->DOES($role))
		{
			_fail(($message // "$value doesn't do $role"), $key, $caller);
			return FALSE;
		}
	}
	else
	{
		_fail(($message // "undef doesn't do anything"), $key, $caller);
		return FALSE;
	}
	
	return TRUE;
}

sub isa_ok ($$;$$)
{
	my ($value, $class, $message, $key) = @_;
	my $caller = [caller(0)];
	return TRUE if is_quiet($key, $caller);
	
	if (blessed $value)
	{
		unless ($value->isa($class))
		{
			_fail(($message // "object is not a $class"), $key, $caller);
			return FALSE;
		}
	}
	elsif (defined $value)
	{
		unless ($value->isa($class))
		{
			_fail(($message // "$value is not a $class"), $key, $caller);
			return FALSE;
		}
	}
	else
	{
		_fail(($message // "undef is not a $class"), $key, $caller);
		return FALSE;
	}
	
	return TRUE;
}

sub blessed_ok ($;$$) 
{
	my ($value, $message, $key) = @_;
	my $caller = [caller(0)];
	return TRUE if is_quiet($key, $caller);
	
	unless (blessed $value)
	{
		_fail(($message // 'value not blessed'), $key, $caller);
		return FALSE;
	}
	
	return TRUE;
}

sub defined_ok ($;$$)
{
	my ($value, $message, $key) = @_;
	my $caller = [caller(0)];
	return TRUE if is_quiet($key, $caller);
	
	unless (defined $value)
	{
		_fail(($message // 'value undefined'), $key, $caller);
		return FALSE;
	}
	
	return TRUE;
}

sub assert ($;$$)
{
	my ($truthy, $message, $key) = @_;
	my $caller = [caller(0)];
	return TRUE if is_quiet($key, $caller);
	
	unless ($truthy)
	{
		_fail(($message // 'assertion failed'), $key, $caller);
		return FALSE;
	}
	
	return TRUE;
}

sub affirm (&;$$)
{
	my ($truthy, $message, $key) = @_;
	my $caller = [caller(0)];
	return TRUE if is_quiet($key, $caller);
	
	unless ($truthy->())
	{
		if (!defined $message)
		{
			eval
			{
				 require B::Deparse;
				 $message = B::Deparse->new->coderef2text($truthy) . ' failed';
			};
		}
		_fail(($message // 'affirmation failed'), $key, $caller);
		return FALSE;
	}
	
	return TRUE;
}

sub should ($$;$$)
{
	my ($given, $expected, $message, $key) = @_;
	my $caller = [caller(0)];
	return TRUE if is_quiet($key, $caller);
	my $pass   = TRUE;
	my $diag;
	
	if (not $given ~~ $expected)
	{
		$diag = <<DIAG;
    given: $given
 expected: $expected
DIAG
		$pass = FALSE;
	}

	return $pass if $pass;
	
	if (defined $message)
	{
		chomp $message;
		_fail "$message\n$diag          ", $key, $caller;
	}
	else
	{
		_fail "$diag          ", $key, $caller;
	}
	return FALSE;
}

sub shouldnt 
{
	my ($given, $expected, $message, $key) = @_;
	my $caller = [caller(0)];
	return TRUE if is_quiet($key, $caller);
	my $pass   = TRUE;
	my $diag;
	
	if ($given ~~ $expected)
	{
		$diag = <<DIAG;
    given: $given
 expected: $expected
DIAG
		$pass = FALSE;
	}

	return $pass if $pass;
	
	if (defined $message)
	{
		chomp $message;
		_fail "$message\n$diag          ", $key, $caller;
	}
	else
	{
		_fail "$diag          ", $key, $caller;
	}
	return FALSE;
}

TRUE;

__END__

=head1 NAME

Moan - yet another "assert" module

=head1 SYNOPSIS

In C<< lib/MyApp/Database.pm >>:

 package MyApp::Database;
 use strict;
 use Moan;
 sub new {
   my ($class, $dbh) = @_;
   assert($dbh->isa('DBI::db'), 'Passed a database.');
   assert($dbh->ping, 'Database is alive.');
   bless [$dbh], $class;
 }
 sub dbh {
   $_[0][0];
 }
 1;

In C<< bin/myapp.pl >>:

 #!/usr/bin/perl
 use MyApp::Database;
 use Moan;
 assert(defined $ENV{MYAPP_DB}, 'MYAPP_DB set');
 my $db = MyApp::Database->new(DBI->connect($ENV{MYAPP_DB}));
 assert($db->isa('MyApp::Database'), 'Class OK');
 # do something

At the command-line:

 perl -Moan::more=MyApp::Database bin/myapp.pl

=head1 DESCRIPTION

Assertions are somewhere between inlined unit tests, checks against
malicious input and executable comments. Assertions should be written
to have no side-effects, and to always pass under normal circumstances.

This is not the only Perl assertion module, and probably not the best,
but it does what I need. This module requires Perl 5.10.

=head2 Assertion Functions

=over

=item C<< assert($expression, $message, $tag) >>

Checks whether C<$expression> evaluates to true. If not, the assertion fails
(see L</Assertion Consequences>).

The second parameter C<$message> is optional, and provides an error message to
associate with the assertion. If omitted or undef, a default non-descriptive
message is used.

The third parameter C<$tag> is a category for the assertion. This allows you
to classify assertions and disable them by tag. If ommitted or undef, the
default tag is the package name where the assertion occurred. The following
two assertions are equivalent:

 package Foo::Bar;
 assert(1);
 
 package main;
 assert(1, undef, 'Foo::Bar');

Tags starting with a single colon are appended to the package name. So the
following two assertions are equivalent:

 package Foo::Bar;
 assert(1, undef, ':quux');
 
 package main;
 assert(1, undef, 'Foo::Bar:quux');

This means you have very little excuse for not namespacing your tags. For
small packages, just leave the tag undefined and you'll get your assertion
tagged with your package. For larger packages, use tags matching the
expression C<< /^:(\w+)$/ >>.

Tags are in fact arbitrary strings, but you can save yourself headaches if
you stick to the conventions above.

=item C<< affirm {CODE_BLOCK} $message, $tag >>

Checks that the return value of the code is true. Good for more complex
tests. (This is also more efficient than C<assert> in cases where you've
disabled assertion reporting, as C<affirm> can avoid running the code block
entirely.

=item C<< diag $message, $tag >>

Strictly speaking, this does not actually make an assertion, but generates
a diagnostic message (which is not usually shown).

The L</Assertion Consequences> section explains how to enable diagnostic
messages on a per-tag basis.

=item C<< no Moan >>

=begin private

=item C<< Moan->unimport >>

=end private

Disables moaning within a lexical block.

=back

=head3 Carp::Assert

These functions are provided for rough compatibility with L<Carp::Assert>.
You should be able to more or less drop in C<Moan> as a replacement:

 use Moan ':carpish';

=over

=item C<< should($given, $expected, $message, $tag) >>

Equivalent to:

 assert($given ~~ $expected, $message, $tag);

This is I<slightly> different from C<Carp::Assert::should> which uses
C<eq> instead of the smart match.

=item C<< shouldnt($given, $expected, $message, $tag) >>

Equivalent to:

 assert(!($given ~~ $expected), $message, $tag);

=item C<< DEBUG >>

Right now this just returns 1.

=back

=head3 Test::More

These functions are provided for rough compatibility with
L<Test::More>. (Though note that C<Moan> is not designed for
writing module tests. It doesn't use TAP.) They are not
exported by default.

 use Moan ':testmore';

=over

=item C<< ok($expression, $message, $tag) >>

Largely the same as C<assert>, provided for people addicted to
L<Test::More>.

=item C<< is($given, $expected, $message, $tag) >>

Equivalent to:

 ok($given eq $expected, $message, $tag);

or 

 ok($given == $expected, $message, $tag);

... depending on whether C<$expected> looks like a number.

=item C<< isnt($given, $expected, $message, $tag) >>

Equivalent to:

 ok($given ne $expected, $message, $tag);

or 

 ok($given != $expected, $message, $tag);

... depending on whether C<$expected> looks like a number.

=item C<< like($given, $expected, $message, $tag) >>

Equivalent to:

 ok($given =~ $expected, $message, $tag);

=item C<< unlike($given, $expected, $message, $tag) >>

Equivalent to:

 ok($given !~ $expected, $message, $tag);

=item C<< isa_ok($package_name, $class, $message, $tag) >>

=item C<< isa_ok($object, $class, $message, $tag) >>

Roughly equivalent to:

 ok($package_name->isa($class), $message, $tag);
 ok($object->isa($class), $message, $tag);

... but without failing horribly if $object is, say, an unblessed reference.

=item C<< can_ok($package_name, $method, $message, $tag) >>

=item C<< can_ok($object, $method, $message, $tag) >>

Roughly equivalent to:

 ok($package_name->can($method), $message, $tag);
 ok($object->can($method), $message, $tag);

... but without failing horribly if $object is, say, an unblessed reference.

=back

=head3 Also Useful

Here are some other functions that are occasionally useful.

  use Moan qw/:standard :testmore does_ok defined_ok blessed_ok/;

=over

=item C<< does_ok($package_name, $role, $message, $tag) >>

=item C<< does_ok($object, $role, $message, $tag) >>

Roughly equivalent to:

 ok($package_name->DOES($role), $message, $tag);
 ok($object->DOES($role), $message, $tag);

... but without failing horribly if $object is, say, an unblessed reference.

=item C<< defined_ok($value, $message, $tag) >>

Pretty much identical to:

 ok(defined $value, $message, $tag);

=item C<< blessed_ok($object, $message, $tag) >>

 ok(Scalar::Util::blessed($object), $message, $tag);

=back

=head2 Export

 %EXPORT_TAGS = (
     testmore => [qw/ok is isnt like unlike can_ok isa_ok/],
     carpish  => [qw/assert affirm should shouldnt DEBUG/],
     standard => [qw/assert affirm diag/],
     );
 @EXPORT = @{ $EXPORT_TAGS{standard} };
 @EXPORT_OK = (
     qw/blessed_ok defined_ok does_ok TRUE FALSE/,
     @{ $EXPORT_TAGS{standard} },
     @{ $EXPORT_TAGS{testmore} },
     @{ $EXPORT_TAGS{carpish} },
     );

Yeah, you can export the constants TRUE and FALSE, just because they're
so often handy to have.

=head2 Assertion Consequences

The normal consequence of an assertion is that a warning will be emitted, if
the assertion does not hold (i.e. is false). Diagnostic messages (C<diag>)
will not be printed.

C<Moan> can be kicked into a more verbose mode, where diagnostic messages are
printed, and assertions that don't hold are accompanied by stacktracers. Just
call C<< Moan->loudly >> 

 Moan->loudly(qw/MyApp::Database YourApp::Greaterface/);

Each argument is a tag which you want to make "louder". The C<< /^:\w+$/ >>
convention for tags that are local to your package will work, but to specify
tags in other packages, you need to fully-qualify the names.

For example, the tag ":connection" in package MyApp::Database could be made
louder using:

 Moan->loudly('MyApp::Database:connection');  # sic

Tags can be preceded with a minus to switch off the loudness feature.

Regular expressions also work:

 Moan->loudly(qr/^MyApp::Database:(\w+)$/);

There's currently no way to remove regular expressions from the list though,
so use with caution.

A special tag "ALL" makes all moaning louder. "-ALL" counteracts that.

But what if you want to decrease C<Moan>'s verbosity? As you might have
guessed, C<< Moan->quietly >> fulfils this role. It takes the same
parameters as C<< Moan->loudly >>.

If a tag has been quietened, then no warnings will be emitted for it whether
the assertion holds or not. In fact, in many cases C<Moan> can even avoid 
completely evaluating the assertion.

Curiously, a tag can be made both loud and quiet, as the two features are
quite separate. If a tag is both, then it will act quiet, except that
warnings for diagnostic messages will be emitted. This should be considered
an "at risk feature" (i.e. an arguable bug).

There is also a C<< Moan->fatally >> feature. This again takes the same
parameters as C<< Moan->loudly >>. It makes failed assertions throw an
exception (i.e. die).

When C<Moan> needs to throw an exception for an assertion, it checks to see
if the tag is loud, and if not, appends all previous diagnostics to the
exception.

At some point, the exceptions thrown will probably be switched from strings
to blessed objects in the next release.

(C<Moan::loudly>, C<Moan::quietly> and C<Moan::fatally> also work.)

The following functions are provided to check the behaviour of a tag:

=over

=item C<< Moan::is_loud($tag) >>

=item C<< Moan::is_quiet($tag) >>

=item C<< Moan::is_fatal($tag) >>

=back

=head2 Command-Line Options

=over

=item C<< perl -Moan::quietly=tag1,tag2,tag3 >> - quietly

=item C<< perl -Moan::loudly=tag1,tag2,tag3 >> - loudly

=item C<< perl -d:oh=tag1,tag2,tag3 >> - loudly and fatally

=back

=head1 BUGS

Please report any bugs to
L<http://rt.cpan.org/Dist/Display.html?Queue=Moan>.

=head1 SEE ALSO

C<Carp::Assert>, C<Test::More>.

=head1 AUTHOR

Toby Inkster E<lt>tobyink@cpan.orgE<gt>.

=head1 COPYRIGHT AND LICENCE

This software is copyright (c) 2011 by Toby Inkster.

This is free software; you can redistribute it and/or modify it under
the same terms as the Perl 5 programming language system itself.

=head1 DISCLAIMER OF WARRANTIES

THIS PACKAGE IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR IMPLIED
WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.

