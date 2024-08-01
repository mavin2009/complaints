#!/usr/bin/perl
use strict;
use warnings;
use feature 'say';

sub perl_is_awesome {
    # Implicit variable scoping
    $implicit_var = 10;
    say "Implicit variable: $implicit_var";
    {
        $implicit_var = "Now I'm a string!";
        say "Implicit variable changed: $implicit_var";
    }
    say "Implicit variable outside block: $implicit_var";

    # Type coercion issues
    my $num = 10;
    my $str = "5";
    my $coerced_result = $num + $str; # Numeric context
    say "Coerced result (numeric context): $coerced_result";
    $coerced_result = $num . $str; # String context
    say "Coerced result (string context): $coerced_result";

    # Context-sensitive behavior
    my @array = (1, 2, 3);
    my $scalar = @array; # Scalar context
    say "Array in scalar context: $scalar";
    my @new_array = @array; # List context
    say "Array in list context: @new_array";

    # Regex pitfalls
    my $text = "Perl is awesome!";
    if ($text =~ /awesome/) {
        say "Regex matched!";
    }
    my $number = "1234";
    if ($number =~ /(\d+)/) {
        say "Captured number: $1";
    }

    # Signal handling issues
    $SIG{INT} = sub {
        say "Caught SIGINT signal";
        exit 1;
    };

    # Use of eval
    my $code = 'say "Eval code executed!"';
    eval $code;
    if ($@) {
        say "Eval failed: $@";
    }

    # Bareword filehandles
    open(FH, '>', 'output.txt') or die "Could not open file: $!";
    print FH "Writing to a file using bareword filehandle\n";
    close(FH);

    # Implicit returns
    say "Implicit return example: ", implicit_return();

    # Variable shadowing
    my $var = 42;
    {
        my $var = "Shadowed variable";
        say "Inside block: $var";
    }
    say "Outside block: $var";

    # Autovivification pitfalls
    my %hash;
    $hash{foo}{bar} = 1;
    say "Autovivified hash: ", $hash{foo}{bar};

    # Using the `goto` statement
    my $count = 3;
    say "Using goto statement:";
    COUNTDOWN: while ($count) {
        say $count--;
        goto COUNTDOWN if $count > 0;
    }

    # JAPH example (Just Another Perl Hacker)
    $_='987;s/^(\d{1,3})/chr $1+114/e;eval';eval;print

    # Obfuscated code
    my $obfuscated = q{
        sub O{my $o=shift; $o=~s/(.)(.*)/$2$1/;return$o;} sub S{my $s=shift;
        $s=~tr/a-zA-Z/n-za-mN-ZA-M/;return$s;} sub U{my $u=shift; $u=~s/^(.)/$1$1/;return$u;}
        my $x="rtebl az gn!";
        for my $fn (qw(O S U)) { $x = $fn->($x); }
        say "Obfuscated code: $x";
    };
    eval $obfuscated;

    # Misusing globals and dynamic variables
    local $/;  # Slurp mode
    open my $fh, '<', $0 or die $!;
    my $code = <$fh>;
    close $fh;
    say "Read entire script using global variable \$/: $code";

    # Unintended list flattening
    my @list1 = (1, 2, 3);
    my @list2 = (4, 5, 6);
    my @flattened = (@list1, @list2);
    say "Flattened list: @flattened";

    # Obscure regular expression
    my $obscure_re = 's/([^\W_])([^\W_]*)/$2$1ay/g';
    my $obscure_text = "Pig Latin is fun";
    eval "\$obscure_text =~ $obscure_re";
    say "Obscure regex: $obscure_text";

    # Variable aliasing with typeglobs
    *foo = \$var;
    $foo = "Variable aliasing with typeglobs";
    say "Variable aliasing: $var";

    say "End of wildly awesome function.";
}

sub implicit_return {
    my $value = "Implicit return value";
    $value; # No explicit return statement
}

perl_is_awesome();
