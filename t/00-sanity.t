use Test;

sub make-query-class(Any:U $return-class) {
    my class TEMP-QUERY-CLASS {
        has $.last-query;
        has @.last-params;
        has $.return-class;
        method query(::?CLASS:D: $query, @params) {
            $!last-query = $query;
            @!last-params = @params;
            $.return-class.new
        }
    }.new(:$return-class)
}

{
    use CheckedSQL <t/sql/00-count.sql>;
    my $runner = make-query-class(class TEST0 { method rows { 3 } });
    my $result = base-query($runner, 1, 2);
    is $result, 3;
    is $runner.last-params, (1, 2);
    is $runner.last-query, 'SELECT $a + $b;',
            "Should be running from the file";
}

{
    use CheckedSQL <t/sql/01-scalar.sql>;
    my $runner = make-query-class(class TEST1 { method value { 1 } });
    my $result = base-query($runner);
    is $result, 1;
    is $runner.last-params, ();
    is $runner.last-query, 'SELECT 1;',
            "Should be running from the file";
}

{
    use CheckedSQL <t/sql/02-typed-scalar.sql>;
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
    use CheckedSQL <t/sql/03-hash.sql>;
    my $runner = make-query-class(class TEST3 { method hash { result => 1 } });
    my %result = base-query($runner);
    is-deeply %result, %(result => 1);
    is $runner.last-params, ();
}

{
    throws-like { EVAL "use CheckedSQL <t/sql/04-typed-hash-FAIL.sql>" },
        Exception, message => /'Hash return cannot have a type ascription'/;
}

{
    use CheckedSQL <t/sql/05-array.sql>;
    my $runner = make-query-class(class TEST5 { method hashes { {result => 1}, {result => 2} } });
    my @result = base-query($runner);
    is-deeply @result, [{result => 1}, {result => 2}];
    is $runner.last-params, ();
}

{
    use CheckedSQL <t/sql/06-typed-array.sql>;
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

done-testing;