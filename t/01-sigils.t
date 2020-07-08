use Test;

sub make-query-class(Any:U $return-class) {
    my class TEMP-QUERY-CLASS {
        has @.last-params;
        has $.return-class;
        method query(::?CLASS:D: $, *@!last-params) {
            $.return-class.new
        }
    }.new(:$return-class)
}

{
    use Badger <t/sql/01/00-count.sql>;
    my $runner = make-query-class(class TEST0 { method rows { 3 } });
    my $result = base-query($runner, 1, 2);
    is $result, 3;
    is $runner.last-params, (1, 2);
}

{
    use Badger <t/sql/01/01-scalar.sql>;
    my $runner = make-query-class(class TEST1 { method value { 1 } });
    my $result = base-query($runner);
    is $result, 1;
    is $runner.last-params, ();
}

{
    use Badger <t/sql/01/02-typed-scalar.sql>;
    my $new-called = False;
    class Result2 {
        has $.result;
        submethod BUILD(:$!result!) {
            $new-called = True;
        }
    }
    my $runner = make-query-class(class TEST2 { method hash { result => 1 } });
    my $result = base-query($runner);
    ok $result ~~ Result2;
    is $result.result, 1;
    ok $new-called, "It went through Result2";
    is $runner.last-params, ();
}

{
    use Badger <t/sql/01/03-hash.sql>;
    my $runner = make-query-class(class TEST3 { method hash { result => 1 } });
    my %result = base-query($runner);
    is-deeply %result, %(result => 1);
    is $runner.last-params, ();
}

{
    throws-like { EVAL "use Badger <t/sql/01/04-typed-hash-FAIL.sql>" },
        Exception, message => /'Hash return cannot have a type ascription'/;
}

{
    use Badger <t/sql/01/05-array.sql>;
    my $runner = make-query-class(class TEST5 { method hashes { {result => 1}, {result => 2} } });
    my @result = base-query($runner);
    is-deeply @result, [{result => 1}, {result => 2}];
    is $runner.last-params, ();
}

{
    use Badger <t/sql/01/06-typed-array.sql>;
    my $new-called = False;
    class Result6 {
        has $.result;
        submethod BUILD(:$!result!) {
            $new-called = True;
        }
    }
    my $runner = make-query-class(class TEST6 { method hashes { @({result => 1}, {result => 2}) } });
    my @result = base-query($runner);
    ok all(@result) ~~ Result6;
    is +@result, 2;
    ok $new-called, "It went through Result6";
    is @result[0].result, 1;
    is @result[1].result, 2;
    is $runner.last-params, ();
}

throws-like { EVAL "use Badger <t/sql/01/07-no-sigil-after-type-FAIL.sql>" },
        Exception, message => /'Expected sigil after return type ascription'/;

{
    use Badger <t/sql/01/08-returnless.sql>;
    my $runner = make-query-class(class TEST8 { method rows { 0 } });
    my $result = base-query($runner);
    is $result, 0;
}

{
    use Badger <t/sql/01/03-hash.sql>;
    my $runner = make-query-class(class TEST9 { method hash { Nil } });
    my %result = base-query($runner);
    is-deeply %result, %();
    is $runner.last-params, ();
}

{
    use Badger <t/sql/01/03-hash.sql>;
    my $runner = make-query-class(class TEST10 { method hash {
            if $++ {
                flunk
            } else {
                %(a => 1)
            }
        }
    });
    my %result = base-query($runner);
    is-deeply %result, %(a => 1);
    is $runner.last-params, ();
}

done-testing;
