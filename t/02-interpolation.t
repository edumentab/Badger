use Test;

{
    throws-like { EVAL "use CheckedSQL <t/sql/02/01-double-param-untyped-untyped-FAIL.sql>" },
            Exception, message => /'Duplicate parameter name: a'/;
}

{
    throws-like { EVAL "use CheckedSQL <t/sql/02/02-double-param-typed-untyped-FAIL.sql>" },
            Exception, message => /'Duplicate parameter name: a'/;
}

{
    throws-like { EVAL "use CheckedSQL <t/sql/02/03-double-param-untyped-typed-FAIL.sql>" },
            Exception, message => /'Duplicate parameter name: a'/;
}

{
    throws-like { EVAL "use CheckedSQL <t/sql/02/04-double-param-typed-typed-FAIL.sql>" },
            Exception, message => /'Duplicate parameter name: a'/;
}


my class TEMP-QUERY-CLASS {
    has $.last-query;
    method query(::?CLASS:D: $!last-query, @) {
        class { method rows { 0 } }.new
    }
}

{
    use CheckedSQL <t/sql/02/05-with-params.sql>;
    my $runner = TEMP-QUERY-CLASS.new;
    base-query($runner, 1, 2);
    is $runner.last-query, 'SELECT $1 + $2;';
}

{
    throws-like { EVAL "use CheckedSQL <t/sql/02/06-unknown-param-FAIL.sql>" },
            Exception, message => /'Unknown parameter: $c'/;
}

{
    throws-like { EVAL "use CheckedSQL <t/sql/02/07-unknown-param-suggest-FAIL.sql>" },
            Exception, message => /'Unknown parameter @a, do you mean $a or $A?'/;
}

done-testing;
