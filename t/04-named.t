use Test;

my class TEMP-QUERY-CLASS {
    has $.last-query;
    has @.last-pos;
    method query(::?CLASS:D: $!last-query, *@!last-pos) {
        class { method rows { 0 } }.new
    }
}

{
    use Badger <t/sql/04/00-named-typed.sql>;
    my $runner = TEMP-QUERY-CLASS.new;
    query-typed($runner);
    is $runner.last-query, 'SELECT ($1::int);';
    is-deeply $runner.last-pos, [Nil];
}

{
    use Badger <t/sql/04/01-named-untyped.sql>;
    my $runner = TEMP-QUERY-CLASS.new;
    query-untyped($runner);
    is $runner.last-query, 'SELECT $1 + $1;';
    is-deeply $runner.last-pos, [Nil];

    query-untyped($runner, a => 1);
    is-deeply $runner.last-pos, [1];
}

{
    use Badger <t/sql/04/02-extra-FAIL.sql>;
    my $runner = TEMP-QUERY-CLASS.new;
    throws-like { query($runner, d => 2, c => 3); },
      Exception, message => "Extra named parameters for SQL query query: c, d. Named parameters: a, b.";
}

{
    use Badger <t/sql/04/03-mixed.sql>;
    my $runner = TEMP-QUERY-CLASS.new;
    query($runner, 1, 2, a => 3, b => 4);
    is $runner.last-query, 'SELECT $1 * $3 + $2 * $4;';
    is-deeply $runner.last-pos, [1, 2, 3, 4];

    query($runner, 1, 2, b => 4, a => 3);
    is-deeply $runner.last-pos, [1, 2, 3, 4];
}

throws-like { EVAL "use Badger <t/sql/04/04-dupe-named-FAIL.sql>" },
        Exception, message => /'Duplicate parameter name'/;

throws-like { EVAL "use Badger <t/sql/04/05-dupe-mixed-FAIL.sql>" },
        Exception, message => /'Duplicate parameter name'/;

throws-like { EVAL "use Badger <t/sql/04/06-pos-after-named-FAIL.sql>" },
        Exception, message => /'Cannot have a positional parameter after a named one'/;

{
    use Badger <t/sql/04/07-mandatory.sql>;
    my $runner = TEMP-QUERY-CLASS.new;
    query($runner, a => 1, b => 2);
    is $runner.last-query, 'SELECT $1 + $2;';
    is-deeply $runner.last-pos, [1, 2];

    throws-like { query($runner); },
      Exception, message => "Missing required named parameters for SQL query query: a, b.";
}


done-testing;
