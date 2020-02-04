use Test;

my class TEMP-QUERY-CLASS {
    has $.last-query;
    method query(::?CLASS:D: $!last-query, *@) {
        class { method rows { 0 } }.new
    }
}

{
    use CheckedSQL <t/sql/03/01-multi-queries.sql>;
    my $runner = TEMP-QUERY-CLASS.new;
    query1($runner);
    is $runner.last-query, 'SELECT 1;';
    query2($runner);
    is $runner.last-query, 'SELECT 2;';
    query3($runner);
    is $runner.last-query, trim(q:to/END/);
-- note the whitespace before this
SELECT 3;
END
}

{
    use CheckedSQL <t/sql/03/02-same-param-in-diff-queries.sql>;
    my $runner = TEMP-QUERY-CLASS.new;
    query1($runner, 1);
    is $runner.last-query, 'SELECT $1;';
    query2($runner, 1);
    is $runner.last-query, 'SELECT $1;';
}

done-testing;
