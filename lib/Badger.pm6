no precompilation;

module AST {

}

role AST::Typed {
    has Str $.type-name;

    method type {
        $!type-name ?? ::($!type-name) !! Any
    }
}

class AST::Param does AST::Typed {
    has Str $.sigil;
    has Str $.name;
    has Bool $.named;
    has Bool $.mandatory;

    submethod BUILD(:$!sigil, :$!name, :$!named, :$!type-name, :$quantifier) {
      # It's mandatory if it's a positional parameter OR has a bang
      $!mandatory = !$!named || $quantifier eq '!';
    }

    method Str {
        $.sigil ~ $.name
    }
}

class AST::Return is repr('Uninstantiable') {

}
class AST::Return::SingleHash is AST::Return {

}
class AST::Return::MultiHash is AST::Return {

}
class AST::Return::Scalar is AST::Return {

}
class AST::Return::Count is AST::Return {

}
class AST::Return::Typed is AST::Return does AST::Typed {

}
class AST::Return::MultiTyped is AST::Return does AST::Typed {

}

class AST::Sig {
    has AST::Param:D @.by-name is required;
    has AST::Param:D @.by-pos is required;
    has AST::Param:D @.param is required;
    has AST::Return:D $.return is required;

    submethod BUILD(:$!return, :@param) {
        # Sort by-name so that we can deal with incoming parameters more easily later on
        @!by-name = @param.grep(*.named).sort(*.name);
        @!by-pos = @param.grep(!*.named);

        @!param = |@!by-pos, |@!by-name;
    }
}

class AST::Module {
    has Str $.name;
    has Str $.content;
    has AST::Sig:D $.sig is required handles <param return by-name by-pos>;
}

class X::ParseFail is Exception {
    has Str $.reason is required;
    has Cursor $.cursor is required;

    method message() {
        "SQL parse failed: $!reason at line $.line near '$.near'"
    }

    method line() {
        $!cursor.orig.substr(0, $!cursor.pos).split(/\n/).elems
    }

    method near() {
        $!cursor.orig.substr($!cursor.pos, 40)
    }
}

#use Grammar::Tracer;
grammar FileGrammar {
    token TOP { <module>+ }

    token module {
        :my @*param-names;
        :my $*param-has-named = False;
        <header>
        <content>
    }

    token header {
        '--' <.ws>
        'sub' <.ws>
        <name>
        [ '(' ~ ')' <sig> \n
        || <.panic: "No signature found for routine $<name>"> ]
    }

    rule sig {
        <param>* % [ ',' ]
        ['-->' <return>]?
    }

    token named {
        [ ':' { $*param-has-named = True; }
        || <?{ $*param-has-named }> <.panic: "Cannot have a positional parameter after a named one"> ]?
    }

    token param {
        [ $<type>=<.qualified-name> \s+ ]?
        <named> $<sigil>=<[ $ @ % ]> <name> $<quantifier>=<[ ! ]>?
        [ <?{ ~$<name> (elem) @*param-names }> <.panic: "Duplicate parameter name: $<name>">
        || { @*param-names.push: ~$<name> } ]
    }

    proto token return { * }

    multi token return:count { '+' '@'? }

    multi token return:sigil { (<[ $ @ % ]>) }

    multi token return:typed-sigil {
        $<type>=<.qualified-name> <|b> \s*
        [  $<sigil>=<[ $ @ ]>
        || '%' <.panic: "Hash return cannot have a type ascription">
        || <.panic: "Expected sigil after return type ascription">
        ]
    }

    token content {
        [ ^^ <!before '--' <.ws> 'sub' <.ws>> \N* $$ ]+%% \n
    }

    token qualified-name { <name>+ % '::' }

    token name { <[- \w]>+ }

    token ws { \h* }

    method panic($reason) {
        die X::ParseFail.new(:$reason, :cursor(self));
    }
}

class FileActions {
    method TOP($/) {
        make $<module>>>.made
    }

    method module($/) {
        make AST::Module.new(
            :name(~$<header><name>),
            :sig($<header><sig>.made),
            :content(~$<content>),
        )
    }

    method sig($/) {
        make AST::Sig.new(
            :param($<param>>>.made),
            :return($<return> ?? $<return>.made !! AST::Return::Count.new) # default to `+`
        )
    }

    method return:count ($/) {
        make AST::Return::Count.new
    }
    method return:sigil ($/) {
        given ~$0 {
            when '@' { make AST::Return::MultiHash.new }
            when '%' { make AST::Return::SingleHash.new }
            when '$' { make AST::Return::Scalar.new }
            default { die "Unrecognized sigil" }
        }
    }
    method return:typed-sigil ($/) {
        given ~$<sigil> {
            when '@' { make AST::Return::MultiTyped.new(type-name => ~$<type>) }
            when '$' { make AST::Return::Typed.new(type-name => ~$<type>) }
            default { die "Unrecognized sigil" }
        }
    }

    method named ($/) {
        make $/ eq ":"
    }

    method param ($/) {
        make AST::Param.new(
            sigil => ~$<sigil>,
            name => ~$<name>,
            type-name => $<type> ?? ~$<type> !! Nil,
            named => $<named>.made,
            quantifier => $<quantifier>
        )
    }
}

subset File of Str where *.IO.e;

class PopulateClass {
    has Code $.fn;
    method populate($obj) {
        $!fn($obj)
    }
}

multi sub build-return-class($, AST::Return::Count) {
    PopulateClass.new(fn => *.rows)
}

multi sub build-return-class($, AST::Return::SingleHash) {
    PopulateClass.new(fn => *.hash)
}

multi sub build-return-class($, AST::Return::MultiHash) {
    PopulateClass.new(fn => *.hashes)
}
multi sub build-return-class($, AST::Return::Scalar) {
    PopulateClass.new(fn => *.value)
}

multi sub build-return-class($, AST::Return::Typed $typed) {
    PopulateClass.new(fn => { $typed.type.new(|.hash) })
}

multi sub build-return-class($, AST::Return::MultiTyped $typed) {
    PopulateClass.new(fn => {
        my $type = $typed.type;
        .hashes.map({
            $type.new(|$_)
        })
    })
}

multi sub build-return-class($name, AST::Return:D $return) {
    die "NYI, AST::Return::Create and AST::Return::MultiCreate";
    my $return-class = Metamodel::ClassHOW.new_type(:name("ReturnType-$name"));
    for $return.attributes -> $attr {
        $return-class.^add_attribute(Attribute.new(
            :name($attr.sigil ~ '!' ~ $attr.name),
            :type($attr.type),
            :has_accessor(1),
            :package($return-class),
            :required
        ));
    }
    # TODO "Int $.affected-rows is required;"?
    $return-class.^add_method("populate", method ($obj) {
        # $.affected-rows = $obj.rows;
        $obj.hashes.map({ $return-class.new(|$_) })
    });
    $return-class.^compose;
}

#| Automatically adds an annotation for the type in case we know it in advance
sub type-to-ascription(AST::Param $param) {
    my $sql-type = do given $param.type {
        when Int { "int" }
        when Rat { "float" }
        when Str { "text" }
        default { return Nil }
    }

    given $param.sigil {
        when "@" { "array[$sql-type]" }
        default { $sql-type }
    }
}

role SignatureOverload[$sig] {
  # Enable this to see the optimizer fail
  #method signature {
  #  $sig
  #}
}

#| Replaces `$a`, `$b`, ... with `$1` or `$2` in the SQL query.
#| Uses C<type-to-ascription> to optionally ascribe them in the SQL.
sub interpolate-sql($module) {
    my @arg-names = $module.param.map({ .sigil ~ .name });
    $module.content.trim.subst(/<[$@%]> (<[- \w]>+)/, {
        with @arg-names.first(~$/, :k) -> $i {
            with type-to-ascription($module.param[$i]) {
                '($' ~ 1 + $i ~ "::$_)"
            } else {
                '$' ~ 1 + $i
            }
        } else {
            if $module.param.grep({ $0.fc eq .name.fc }) -> @vars {
                die "Unknown parameter $/, do you mean $(@vars.join: " or ")?";
            } else {
                die "Unknown parameter: $/";
            }
        }
    }, :g)
}

sub gen-sql-sub(AST::Module:D $module) {
    my $name = $module.name;
    my $return-class = build-return-class($name, $module.return);

    my $params := Array.new(Parameter.new(:name('$connection'), :mandatory, :type(Any)));
    for $module.param -> $param {
        $params.push: Parameter.new(:name($param.name), :mandatory($param.mandatory), :type($param.type), :named($param.named));
    }
    my @named-names = $module.by-name.map(*.name);

    my $sql = interpolate-sql($module);

    my $sig = Signature.new(:returns($return-class), :count(1.Num), :params($params.List));

    return "&$name" => (sub ($connection, *@params, *%named-params) {
        unless @params == $module.by-pos {
            die "SQL query $name takes $module.by-pos.elems() positional SQL arguments, got @params.elems().";
        }
        if %named-params.keys (-) $module.by-name.map(*.name) -> $extra {
            die "Extra named parameters for SQL query $name: $($extra.keys.sort.join: ", "). Named parameters: $($module.by-name.map(*.name).join: ", ").";
        }
        if $module.by-name.grep(*.mandatory).map(*.name) (-) %named-params.keys -> $missing {
            die "Missing required named parameters for SQL query $name: $($missing.keys.sort.join: ", ").";
        }
        my %by-name-params = %(@named-names X=> Nil), %named-params;

        my $query = $connection.query($sql, |@params, %by-name-params.sort(*.key).map(*.value));
        # NOTE DB::Pg returns an Int for non-SELECT queries. We should probably abstract all this in some adapter class.
        # TODO Maybe make sure that we have a AST::Return::Count in that case?
        return $query ~~ Int ?? $query !! $return-class.populate($query);
    } does SignatureOverload.^parameterize($sig)),
      "ReturnType-$name" => $return-class;
}

sub EXPORT(File $file) {
    my $content = $file.IO.slurp;
    my $ast = FileGrammar.parse($content, :actions(FileActions.new));
    with $ast {
        my @h = $ast.made.map(&gen-sql-sub).flat;
        return @h.hash;
    } else {
        return Map.new;
    }
}
