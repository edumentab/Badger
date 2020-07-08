Badger
======

Badger, not an ORM (a snake).

### What's Badger?

Badger is a SQL library that allows you to invoke SQL snippets as function as if it was Raku code.
This way you can keep writing your SQL queries by hand for performance and tweakability, and your tools still recognize the `.sql` files to help you work with them.

### What does a Badger SQL file look like?

A badger-compatible SQL file is just a normal SQL file, with signature headers. These signatures are intended to look like Raku signatures.
The most basic example:

```sql
-- sub my-query()
SELECT;
```

### How do I feed Badger my SQL?

You have to pass the .sql file(s) to the `use Badger` statement:

```perl6
use Badger <sql/my-query.sql>; # The file in the previous code block
```

This will generate this function Raku-side:

```perl6
sub my-query(Database $db --> Int) { ... }
```

Which you can call just like any other Raku subs,
by passing any object that has an interface similar to `DB::Pg` (for now at least) as the connection.

For parameters and return values, see below.

## Parameters

A Badger SQL sub can have arguments that you can use in the SQL body.
Interpolation works for sigilled variables:

```sql
-- sub query-with-params($a, $b)
SELECT $a + $b, @c
```

This will generate a prepared query with `$a` and `$b` replaced `$1`, `$2` (or with `?`s depending on the RDBMS).

### Parameter Sigils

The Raku allowed sigils are `$` and `@`. 

### Parameter typing

You can put type annotations on the parameters:

```sql
-- sub query-with-params(Int $x, Int @xs)
SELECT $x = ANY(@xs)
```

If a parameter is typed, Badger will try to help you by inserting coercions in the generated SQL.
This is what the executed SQL looks like:

```sql
SELECT ($1::int) = ANY(($2::int[]))
```

### Named Parameters

Parameters can be named, just like in Raku:

```sql
-- sub query-nameds(Int :$a, :$b)
SELECT $a + $b
```

Just like in Raku, you can't have a positional parameter after a named one.

If a parameter is missing, 

### Mandatory Named Parameters 

Also just like in Raku, named parameters can be marked mandatory:

```sql
-- sub query-nameds(:$mandatory!)
SELECT $a * 2
```

## Return Sigils

### `+` (default)

The default one -- in you don't specify a return sigil, you get this.
Returns the number of affected rows (as an `Int`).

```sql
-- sub count-unnests(--> +)
-- ... or ...
-- sub count-unnests()
UPDATE products
   SET price = 999
   WHERE price IS NULL
```

### `$`

Returns a single value. `Nil` is returned otherwise:

```sql
-- sub get-username(Str $token --> $)
SELECT username
FROM users
WHERE token = $token
```

### Typed `$`

Calls `.new` on the given type with all the data returned from the SQL query:

```sql
-- sub get-user(Int $id --> User)
SELECT 1 AS id, 'steve' AS username
```

```perl6
class User {
  has Int $.id;
  has Str $.username;
}
...
my User $user = get-user(db, 1);
# Result: `User.new(id => 1, :username<steve>);`
```

### `%`

Returns a hash.

```sql
-- sub get-hash(--> %)
SELECT 'comment' as type, 'Hello world!' as txt
```

```perl6
my %h = get-hash($db);
# Result: `%(type => "comment", txt => "Hello world!")`
```

If the database doesn't return anything, Badger gives you an empty hash back.

### `@`

Returns an array of hashes.

```sql
-- sub get-hashes(--> @)
SELECT 'comment' as type, txt
FROM unnest(array['Hello', 'world!']) txt
```

```perl6
my @hashes = get-hashes($db);
# Result: `%(type => "comment", txt => "Hello"), %(type => "comment", txt => "world!")`
```

### Typed `@`

Calls `.new` on the given type on each row of the data returned from the SQL query:

```sql
-- sub get-data(--> Datum @)
SELECT row_number() OVER () as id
     , unnest(ARRAY['a','b']) as value
```

```perl6
class Datum {
  has Int $.id;
  has Str $.value;                                                                                                                                                    
}
...
my Datum @data = get-data($db);
# Result: `Datum.new(id => 1, :value<a>), Datum.new(id => 2, :value<b>)`
```