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
    my $runner = make-query-class(class TEST1 { method rows { 3 } });
    my $result = base-query($runner, 1, 2);
    is $result, 3;
    is $runner.last-params, (1, 2);
    is $runner.last-query, 'SELECT $a + $b;',
            "Should be running from the file";
}

{
    use CheckedSQL <t/sql/01-scalar.sql>;
    my $runner = make-query-class(class TEST2 { method value { 1 } });
    my $result = base-query($runner);
    is $result, 1;
    is $runner.last-params, ();
    is $runner.last-query, 'SELECT 1;',
            "Should be running from the file";
}

#(--> +)  returns the count (thinking of +@, which is valid in a signature, and +@a as an expression
#(--> @) returns a list of hashes
#(--> Foo @) returns a list of Foo.new(|%current-line)
#not sure about $ vs % ?
#(--> %) returns the first line
#(--> Foo $) returns Foo.new(|%the-first-line)
#?

done-testing;