module AST {

}

role AST::Typed {
    has Str $.type-name;

    method type {
        $!type-name ?? ::($!type-name) !! Any
    }
}

class AST::Param does AST::Typed {
    has Str $.name;
}

class AST::Return is repr('Uninstantiable') { }
class AST::Return::SingleHash is AST::Return { }
class AST::Return::MultiHash is AST::Return { }
class AST::Return::Scalar is AST::Return { }
class AST::Return::Count is AST::Return { }
class AST::Return::Typed is AST::Return does AST::Typed { }
class AST::Return::MultiTyped is AST::Return does AST::Typed { }

class AST::Sig {
    has AST::Param:D @.param is required;
    has AST::Return:D $.return is required;
}

class AST::Module {
    has Str $.name;
    has Str $.content;
    has AST::Sig:D $.sig is required handles <param return>;
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

    proto token param { * }

    multi token param:untyped {
        $<sigil>=<[$@%]> <name>
        { @*param-names.push: ~$<name> }
    }

    multi token param:typed {
        $<type>=<.qualified-name> \s+
        $<sigil>=<[$@%]> <name>
        { @*param-names.push: ~$<name> }
    }

    proto token return { * }

    multi token return:count { '+' '@'? }

    multi token return:sigil { (<[ $ @ % ]>) }

    multi token return:typed-sigil {
        $<type>=<.qualified-name> \s+
        [  $<sigil>=<[ $ @ ]>
        || '%' <.panic: "Hash return cannot have a type ascription">
        || <.panic: "Expected sigil after return type ascription">
        ]
    }

    token content {
        [ ^^ <!before '--'> .+ $$ ]+%% \n
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
        make $<module>>>.made;
    }

    method module($/) {
        make AST::Module.new(
            :name(~$<header><name>),
            :sig($<header><sig>.made),
            :content(~$<content>),
        );
    }

    method sig($/) {
        make AST::Sig.new(
            :param($<param>>>.made),
            :return($<return>.made)
        );
    }

    method return:count ($/) {
        make AST::Return::Count.new
    }
    method return:sigil ($/) {
        given ~$0 {
            when '@' { make AST::Return::MultiHash.new }
            when '%' { make AST::Return::SingleHash.new }
            when '$' { make AST::Return::Scalar.new }
            default { die }
        }
    }
    method return:typed-sigil ($/) {
        given ~$<sigil> {
            when '@' { make AST::Return::MultiTyped(type-name => ~$<type>) }
            when '$' { make AST::Return::Typed(type-name => ~$<type>) }
            default { die }
        }
    }

    method param:untyped ($/) {
        make AST::Param.new(
            sigil => ~$<sigil>,
            name => ~$<name>,
        )
    }
    method param:typed ($/) {
        make AST::Param.new(
            sigil => ~$<sigil>,
            name => ~$<name>,
            type-name => ~$<type>,
        )
    }
}

subset File of Str where *.IO.e;

my sub make-populate-class(Code $f) {
    class POPULATE-CLASS {
        has Code $.f;
        method populate($obj) {
            $.f($obj)
        }
    }.new(:$f)
}

multi sub build-return-class($, AST::Return::Count) {
    make-populate-class(*.rows)
}

multi sub build-return-class($, AST::Return::SingleHash) {
    make-populate-class(*.hash)
}

multi sub build-return-class($, AST::Return::MultiHash) {
    make-populate-class(*.hashes)
}

multi sub build-return-class($, AST::Return::Scalar) {
    make-populate-class(*.value)
}

multi sub build-return-class($, AST::Return::Typed $typed) {
    make-populate-class({ $typed.type.new(.hash) })
}

multi sub build-return-class($, AST::Return::MultiTyped $typed) {
    my $type = $typed.type;
    make-populate-class({ .hashes.map({ $type.new($_) }) })
}

multi sub build-return-class($name, AST::Return:D $return) {
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
        $obj.hashes # TODO
    });
    $return-class.^compose;
}

sub gen-sql-sub(AST::Module:D $module) {
    #say $module;
    my $name = $module.name;
    my $return-class = build-return-class($name, $module.return);

    my $params := Array.new(Parameter.new(:name('$connection'), :mandatory, :type(Any)));
    for $module.param -> $param {
        $params.push: Parameter.new(:name($param.name), :mandatory, :type($param.type));
    }
    say Signature.new(:returns($return-class), :count(1.Num + $module.param.Num), :params($params.List));

    return "&$name" => (sub ($connection, *@params) {
        die "SQL query $name takes $module.param.elems() SQL arguments, got @params.elems()." unless @params == $module.param;
        $return-class.populate($connection.query($module.content, @params));
    } but role {
        method signature {
            return Signature.new(:returns($return-class), :count(1.Num), :params($params.List));
        }
    }), "ReturnType-$name" => $return-class;
}

sub EXPORT(File $file) {
    my %queries;
    try {
        my $content = $file.IO.slurp;
        say $content;
        my $ast = FileGrammar.parse($content, :actions(FileActions.new));
        # TODO remove all this debugging code
        with $ast {
            my @h = $ast.made.map(&gen-sql-sub).flat;
            dd @h;
            return @h.hash;
        } else {
            say "No AST";
            return %();
        }
        CATCH {
            default { .say; }
        }
    }
}