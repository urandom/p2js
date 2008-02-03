#! /bin/false
# vim: set autoindent shiftwidth=4 tabstop=8:

package IWL::P2JS;

use strict;

use B::Deparse;
use PPI::Document;
use Text::Balanced 'extract_bracketed';
use Scalar::Util qw(blessed);
use base qw(Exporter);

use IWL::Config '%IWLConfig';

use vars qw($VERSION @EXPORT $this $arguments);

# export $this for JavaScript
@EXPORT = qw($this $arguments);

my $number  = qr/^-?\d+(?:\.\d+)?(?:e[-+]\d+)?$/;
my $escapes = qr/[\x00-\x1f\\"]/;
my %special = ("\b" => '\b', "\t" => '\t', "\n" => '\n', "\f" => '\f', "\r" => '\r', "\\" => '\\\\', '"' => '\"');

$VERSION = '0.01';

=head1 NAME

IWL::P2JS - a basic Perl to JavaScript converter

=head1 DESCRIPTION

IWL::P2JS is a class, which provides methods for converting perl subroutines into javascript code. This is a VERY experimental module, whose goal is to provide a way for developers to use Perl code in IWL signal handlers.

=head1 PLUGINS

IWL::P2JS has basic plugin support. To load a plugin, it must be added to the I<P2JS_PLUGINS> L<IWL::Config> variable. I<P2JS_PLUGINS> is a comma (I<,>) separated list of classes, which will be loaded, and their constructors will be invoked. If the variable doesn't exist, it is set to the following: L<IWL::P2JS::Prototype>,L<IWL::P2JS::IWL>

=head1 CONSTRUCTOR

IWL::P2JS->new

=cut

sub new {
    my ($proto) = @_;
    my $class = ref($proto) || $proto;
    my $self = bless {}, $class;

    $self->{__deparser} = B::Deparse->new("-sC", "-q");

    $IWLConfig{P2JS_PLUGINS} = 'IWL::P2JS::Prototype,IWL::P2JS::IWL' unless $IWLConfig{P2JS_PLUGINS};
    do { eval "require $_"; $_->new($self) unless $@ } foreach split ',', $IWLConfig{P2JS_PLUGINS};

    return $self;
}

=head1 METHODS

=over 4

=item B<convert> (B<SUBREF>)

Tries to convert a subroutine reference into JavaScript code

Parameters: B<SUBREF> - a subroutine reference

Returns the converted JavaScript code

=cut

sub convert {
    my ($self, $subref) = @_;
    return '' if !$subref;
    my @perl = split /\n/, $self->{__deparser}->coderef2text($subref);
    shift @perl, pop @perl;
    foreach (@perl) {
        $_ =~ s/\s+/ /g;
        $_ =~ s/^\s+//;
    }

    $self->{__pad} = $self->__walker($subref);
    return $self->__parser(join "", @perl);
}

=item B<signalConnect> (B<SIGNAL>, B<CALLBACK>)

Connects a Perl subroutine to a callback. When the callback is fired, the subroutine will be invoked.

Parameters: B<SIGNAL> - a signal. The following signals are currently supported:

=over 8

=item B<Token::Word>

The signal is fired, whenever a L<PPI::Token::Word> is about to be converted

=back

B<CALLBACK> - a Perl subroutine. It will receive a L<PPI::Element> as its first argument

=cut

sub signalConnect {
    my ($self, $signal, $callback) = @_;

    push @{$self->{__signals}{$signal}}, $callback;
    return $self;
}

# Internal
#
sub __parser {
    my ($self, $perl) = @_;
    return '' unless $perl;
    my $document = $self->{__currentDocument} = PPI::Document->new(\$perl);
    $document->prune('Token::Comment');
    $document->prune('Token::Pod');
    $document->prune('Statement::Package');
    $document->prune('Statement::Include');
    $document->prune('Statement::Null');
    $document->find(sub {
        my $token = $_[1];
        if ($token->isa('PPI::Token::Operator')) {
            $token->set_content('<')  if $token->content eq 'lt';
            $token->set_content('>')  if $token->content eq 'gt';
            $token->set_content('<=') if $token->content eq 'le';
            $token->set_content('>=') if $token->content eq 'ge';
            $token->set_content('==') if $token->content eq 'eq';
            $token->set_content('!=') if $token->content eq 'ne';
            $token->set_content('+')  if $token->content eq '.';
            $token->set_content('&&') if $token->content eq 'and';
            $token->set_content('||') if $token->content eq 'or';
            $token->set_content('!')  if $token->content eq 'not';
        } elsif ($token->isa('PPI::Token::Word')) {
            $token->set_content('var')      if $token->content =~ /my|our|local/;
            $token->set_content('for')      if $token->content eq 'foreach';
            $token->set_content('else if')  if $token->content eq 'elsif';
            $token->set_content('function') if $token->content eq 'sub';
        } elsif ($token->isa('PPI::Token::Quote::Literal')) {
            my $quote = PPI::Token::Quote::Single->new("'");
            my $string = $token->string;
            $string =~ s/(?<!\\)'/\\'/g;
            $quote->set_content("'" . $string . "'");
            $token->insert_before($quote);
            $token->delete;
        }

        return '';
    });

    $self->__parseStatement($_) foreach $document->schildren;

    my $js = $document->content;
    $js =~ s/[ ]+/ /g;
    return $js;
}

sub __parseStatement {
    my ($self, $statement) = @_;
    my ($ref, $js) = (ref $statement, '');

    return $js unless $statement->isa('PPI::Statement');

    $self->__parseSimpleStatement($statement)
      if $ref eq 'PPI::Statement' || $ref eq 'PPI::Statement::Variable';
    $self->__parseCompoundStatement($statement)
      if $ref eq 'PPI::Statement::Compound';

    return;
}

sub __parseSimpleStatement {
    my ($self, $statement) = @_;
    my $i = -1;
    foreach my $child ($statement->schildren) {
        ++$i;
        next if !$child->parent;
        if ($child->isa('PPI::Structure::List')) {
            $self->__parseStructureList($child);
        } elsif ($child->isa('PPI::Structure::Constructor')) {
            my $token = PPI::Token->new($self->__getConstructor($child, 1));
            $child->insert_before($token) and $child->delete;
        } elsif ($child->isa('PPI::Token::Word')) {
            $self->__parseWord($child);
        } elsif ($child->isa('PPI::Structure::Block')) {
            # Subroutine block
            $self->__parseStatement($_) foreach $child->schildren;
        } elsif ($child->isa('PPI::Token::Quote')
                 && $child->snext_sibling->isa('PPI::Token::Operator')
                 && $child->snext_sibling->content eq '->') {
            $child->set_content($self->__getExpressionValue($child, $child->string));
        } elsif ($child->isa('PPI::Token')) {
            if ($child->isa('PPI::Token::Operator')) {
                $child->set_content('.') if $child->content eq '->';
            } elsif ($child->isa('PPI::Token::Structure') && $child->content eq ';') {
                $child->previous_sibling->remove while $child->previous_sibling->isa('PPI::Token::Whitespace');
            }
            $self->__parseTokenSymbol($child);
        }
    }
}

sub __parseCompoundStatement {
    my ($self, $statement) = @_;
    my ($i, $assignment, $operator, $sigil) = (-1, '');
    foreach my $child ($statement->schildren) {
        ++$i;
        next if !$child->parent;
        if ($child->isa('PPI::Token::Word') && {if => 1, unless => 1, while => 1, until => 1, foreach => 1, for => 1}->{$child->content}) {
            if ($child->content eq 'unless' || $child->content eq 'until') {
                my $list = $child->snext_sibling;
                $child->set_content('if') if $child->content eq 'unless';
                $child->set_content('while') if $child->content eq 'until';
                $list->start->set_content('(!(');
                $list->finish->set_content('))');
            }
        } elsif ($child->isa('PPI::Structure::Condition')) {
            my $symbols = $child->find('Token::Symbol');
            if ($symbols) {
                foreach (@$symbols) {
                    $sigil = $_->symbol_type;
                    $self->__parseTokenSymbol($_);
                }
            }
        } elsif ($child->isa('PPI::Token::Magic')
              && $child->sprevious_sibling->isa('PPI::Token::Word')
              && $child->sprevious_sibling->content eq 'for') {
            $child->delete;
        } elsif ($child->isa('PPI::Structure::ForLoop')) {
            $self->__parseForLoop($child);
        } elsif ($child->isa('PPI::Structure::Block')) {
            if ($child->schildren == 1) {
                my $st = $child->schild(0);
                my $operators = $st->find(sub {
                    return $_[1]->isa('PPI::Token::Operator') && $_[1]->content eq ','
                });
                my $last = $st->schild(-1);
                # Most likely a hashref
                if ($operators && @$operators % 2 && (!$last->isa('PPI::Token::Structure') || $last->content ne ';')) {
                    my $clone = $child->clone;
                    bless $clone, 'PPI::Structure::Constructor';
                    my $result = PPI::Token->new($self->__getConstructor($clone));
                    $child->insert_before($result) && $child->delete;
                    next;
                }
            }
            $self->__parseStatement($_) foreach $child->schildren;
        }
    }
}

sub __parseTokenSymbol {
    my ($self, $token) = @_;
    return unless $token->isa('PPI::Token::Symbol');
    my $sigil = $token->symbol_type;
    my $content = $token->content;
    my $name = substr $content, 1;
    my $assignment;
    my $snext = $token->snext_sibling;
    unless ($snext && (($snext->isa('PPI::Token::Operator') && $snext->content eq '->') || $snext->isa('PPI::Structure::Subscript'))) {
        $snext = $token->parent->isa('PPI::Statement::Expression') && ($sigil eq '%' || $sigil eq '@') ? $token->parent->parent : $token;
        while ($snext = $snext->snext_sibling) {
            last if $snext->isa('PPI::Token::Symbol') && !$snext->sprevious_sibling->isa('PPI::Token::Operator');
            if ($snext->isa('PPI::Token::Operator') && $snext->content eq '=') {
                $assignment = 1;
                last;
            }
        }
    }
    if ($assignment) {
        $self->{__currentDocument}{__variables}{$name} = $sigil;
        $token->set_content($name);
    } else {
        my $pad_value = !$self->{__currentDocument}{__variables}{$name} && $self->{__pad}{$token->content}
          ? do {
                my $value = $self->{__pad}{$token->content}{value};
                $token->{__value} = 1;
                if (ref $value eq 'REF' && blessed $$value) {
                    $self->__getExpressionValue($token, $$value);
                } else {
                    $self->__toJS($value)
                }
            }
          : $token->snext_sibling &&
            ($token->snext_sibling->isa('PPI::Structure::Subscript')
                || ($token->snext_sibling->isa('PPI::Token::Operator') && $token->snext_sibling->content eq '->'))
            ? $self->__getObjectExpression($token, $name)
            : $name;
        $token->set_content($pad_value);
    }
    return $self;
}

sub __parseWord {
    my ($self, $child) = @_;
    return unless $child && $child->isa('PPI::Token::Word');
    if ({if => 1, unless => 1, while => 1, until => 1, foreach => 1, for => 1}->{$child->content}) {
        # Statement modifiers
        my $brace = PPI::Token->new;
        my @elements = ($child, $brace);
        my $next = $child;
        $child->previous_sibling->delete if $child->previous_sibling->isa('PPI::Token::Whitespace');
        while ($next = $next->next_sibling) {
            last if $next->isa('PPI::Token::Structure') && $next->content eq ';';
            push @elements, $next;
        }
        my $first = $child->parent->first_element;
        my $modifier = $child->content eq 'unless' || $child->content eq 'until' ? '!(' : '';
        my $forloop = $elements[$#elements]->isa('PPI::Structure::ForLoop') ? $elements[$#elements] : undef;
        $self->__parseForLoop($forloop);
        $child->set_content('if') if $child->content eq 'unless';
        $child->set_content('while') if $child->content eq 'until';
        $brace->set_content(' (' . $modifier) unless $forloop;
        $brace = PPI::Token->new;
        $modifier = ')' if $modifier;
        $brace->set_content($forloop ? ' ' : $modifier . ') ');
        push @elements, $brace;
        $first->insert_before($_->remove) foreach @elements;
    } elsif ($child->content ne 'var' && $child->snext_sibling && $child->snext_sibling->isa('PPI::Structure::List')) {
        $self->__emitSignal('Token::Word', $child);
        # Functions
        my @composition = split /::/, $child->content;
        my $function    = pop @composition;
        my $package     = (join '::', @composition) || 'main';
        my $coderef     = $self->__getPackageAvailability($package, $function);
        if ($coderef) {
            $child->set_content($self->__getFunctionValue($child, $coderef));
        } else {
            $child->set_content(join '.', @composition, $function);
        }
    } elsif ($child->sprevious_sibling
          && $child->sprevious_sibling->isa('PPI::Token::Operator')
          && $child->sprevious_sibling->content eq '.'
          && !$child->snext_sibling->isa('PPI::Structure::List')
    ) {
        $self->__emitSignal('Token::Word', $child);
        # Method without arguments
        $child->insert_after(PPI::Token->new('()'));
    } elsif ($child->content eq 'function') {
        # Subroutines
        my $snext = $child->snext_sibling;
        if ($snext->isa('PPI::Structure::Block')) {
            my $first = $snext->schild(0);
            if ($first->isa('PPI::Statement::Variable')
                  && $first->schild(1)->isa('PPI::Structure::List')
                  && $first->schild(2)->isa('PPI::Token::Operator')
                  && $first->schild(2)->content eq '='
                  && $first->schild(3)->isa('PPI::Token::Magic')
                  && $first->schild(3)->content eq '@_'
            ) {
                my $list = $first->schild(1);
                $child->insert_after($list->remove);
                $first->delete;
                $self->__parseStructureList($child->snext_sibling);
            } else {
                $child->insert_after(PPI::Token->new('()'));
            }
            $self->__parseStatement($_) foreach $snext->children;
        }
    }
}

sub __parseForLoop {
    my ($self, $child) = @_;
    return unless $child;
    my $statement = $child->parent;
    if ($child->schildren == 1) {
        my $s = $child->schild(0);
        my $operator = $s->find_first('Token::Operator');
        my $content = PPI::Token->new;
        $operator = $operator ? $operator->content : '';
        $self->__parseStatement($s);
        my $expr = $s->content;

        if ($operator eq '..') {
            # Range
            my @elements = split ' .. ', $expr;
            $content->set_content(
                'var _ = ' . $elements[0] . '; _ < ' . ($elements[1] + 1) . '; ++_'
            );
            $_->delete foreach $s->children;
            $s->add_element($content);
        } elsif (!$operator && $s->first_element->isa('PPI::Token::Word') && $s->first_element->content eq 'keys') {
            $s->first_element->set_content('var _ in');
        } else {
            # Array
            my $array = PPI::Token->new;
            my $st = PPI::Statement->new;

            # For statement-modifier for-loop
            $expr = '[' . $expr . ']' if (extract_bracketed($expr, '[]'))[1];

            $array->set_content('var _$ = ' . $expr . ';');
            $st->add_element($array);
            $statement->insert_before($st);
            $content->set_content(
                'var i = 0, _ = _$[0]; i < _$.length; _ = _$[++i]'
            );
            $_->delete foreach $s->children;
            $s->add_element($content);
            $st = PPI::Statement->new;
            $array = PPI::Token->new;
            $array->set_content('delete _$;');
            $st->add_element($array);
            $statement->insert_after($st);
        }
    } else {
        $self->__parseStatement($_) foreach $child->schildren;
    }
}

sub __parseStructureList {
    my ($self, $child) = @_;
    my $sprev    = $child->sprevious_sibling;
    my @children = $child->children ? $child->schild(0)->schildren : ();

    do {
        $self->__parseTokenSymbol($_);
        $self->__parseWord($_);
    } foreach @children;
    
    if ($sprev->isa('PPI::Token::Word') && $sprev->content eq 'var') {
        my $whitespace = PPI::Token::Whitespace->new;
        $whitespace->set_content(' ');
        if (@children == 1) {
            $child->insert_before($whitespace);
            $child->insert_before($children[0]->remove);
            $child->delete;
        } elsif (@children > 1) {
            my $coma = PPI::Token::Operator->new(', ');
            my @tokens = map {$_->clone} grep {$_->isa('PPI::Token::Symbol')} @children;
            my ($token, @rhs);
            if ($child->snext_sibling->isa('PPI::Token::Operator') && $child->snext_sibling->snext_sibling->isa('PPI::Structure::List')) {
                $token = $child->snext_sibling->remove;
                @rhs = grep {!$_->isa('PPI::Token::Operator')} $child->snext_sibling->remove->schild(0)->schildren;

                $self->__parseTokenSymbol($_) foreach @rhs;
            }

            while(@tokens) {
                $child->insert_before($whitespace->clone);
                $child->insert_before(shift @tokens);
                if (@rhs) {
                    $child->insert_before($whitespace->clone);
                    $child->insert_before($token->clone);
                    $child->insert_before($whitespace->clone);
                    $child->insert_before(shift @rhs);
                }
                $child->insert_before($coma->clone) if @tokens;
            }

            $child->delete;
        }
    } else {
        if ($sprev->isa('PPI::Token::Operator') && $sprev->content eq '=') {
            my $symbol = $sprev->sprevious_sibling;
            if ($symbol->isa('PPI::Token::Symbol')) {
                if ($self->{__currentDocument}{__variables}{$symbol->content} eq '@') {
                    $child->{start}->set_content('[');
                    $child->{finish}->set_content(']');
                } elsif ($self->{__currentDocument}{__variables}{$symbol->content} eq '%') {
                    $child->{start}->set_content('{');
                    $child->{finish}->set_content('}');
                    my $operators = $child->find(sub {$_[1]->isa('PPI::Token::Operator') && $_[1] eq ','});
                    if ($operators) {
                        for (my $j = 0; $j < @$operators; $j += 2) {
                            $operators->[$j]->set_content(':');
                        }
                    }
                }
            }
        }
    }
}

# Checks whether the element's previous sibling is a method/function
sub __previousIsMethod {
    my ($self, $element) = @_;
    return unless $element->sprevious_sibling->isa('PPI::Token::Word')
      && $element->sprevious_sibling->content ne 'var';
    return 1;
}

# Get the object expression ($example->method()[->method2()...], $$example{foo}{bar}, or Example->method())
sub __getExpressionValue {
    my ($self, $element, $value) = @_;
    my ($start, $ret, $sprev, $string) = ($element->snext_sibling, $value, $element->sprevious_sibling, 0);

    $sprev->delete if $sprev && $sprev->isa('PPI::Token::Cast') && $sprev->content eq '$';
    while (1) {
        $sprev = $start->sprevious_sibling;
        $sprev->delete unless $sprev == $element;
        if ($start->isa('PPI::Token::Operator') && $start->content eq '->') {
            $start = $start->snext_sibling and next;
        } elsif ($start->isa('PPI::Token::Word') && $sprev->isa('PPI::Token::Operator')) {
            my $method = $start->content;
            my $coderef = ref $ret ? $ret : $self->__getPackageAvailability($ret, $method);
            if ($coderef) {
                my $list = $start->snext_sibling;
                my @args;
                if (ref $list && $list->isa('PPI::Structure::List')) {
                    map {$self->__parseTokenSymbol($_)} @{$list->find('Token::Symbol') || []};
                    @args = eval $list->content;
                    $list->delete;
                }
                $ret = $ret->$method(@args);
            } else {
                my @args = $self->__getArguments($start->snext_sibling, 1);
                $ret = join '.', split '::', $ret;
                $ret = ($method eq 'new' ? ('new ' . $ret) : ($ret . '.' . $method))
                  . '(' . (join ', ', map {
                    ref $_ ? $self->__toJS($_) : $_
                  } @args) . ')';
                $string = 1;
            }
        } elsif ($start->isa('PPI::Structure::Subscript') && $start->start->content eq '{') {
            my $property = $start->schild(0);
            $property = $property->schild(0) if $property->isa('PPI::Statement');
            $ret = $ret->{$property->can('string') ? $property->string : $property->content};
        } elsif ($start->isa('PPI::Structure::Subscript') && $start->start->content eq '[') {
            my $property = $start->schild(0)->content;
            $ret = $ret->[$property];
        } else {
            last;
        }
        $start->snext_sibling ? $start = $start->snext_sibling : ($start->delete and last);
    }
    return $string ? $ret : $self->__toJS($ret);
}

# Convert perl object interaction into javascript object interaction
sub __getObjectExpression {
    my ($self, $element, $value) = @_;
    my ($start, $ret, $sprev, $operator) = ($element->snext_sibling, $value, $element->sprevious_sibling, 0);

    $sprev->delete if $sprev && $sprev->isa('PPI::Token::Cast') && $sprev->content eq '$';
    while (1) {
        $sprev = $start->sprevious_sibling;
        $sprev->delete unless $sprev == $element;
        if ($start->isa('PPI::Token::Operator') && $start->content eq '->') {
            $start = $start->snext_sibling and next;
        } elsif ($start->isa('PPI::Token::Word') && $sprev->isa('PPI::Token::Operator')) {
            my $method = $start->content;
            my @args = $self->__getArguments($start->snext_sibling, 1);
            $ret = join '.', split '::', $ret;
            $ret = ($method eq 'new' ? ('new ' . $ret) : ($ret . '.' . $method))
              . '(' . (join ', ', @args) . ')';
        } elsif ($start->isa('PPI::Structure::Subscript') && $start->start->content eq '{') {
            my $property = $start->schild(0);
            $property = $property->schild(0) if $property->isa('PPI::Statement');
            $self->__parseTokenSymbol($property);
            # XXX Perhaps it would be better to handle PPI::Token::Quotes with the ['foo'] notation, instead of .foo
            if (($property->isa('PPI::Token::Symbol') && $property->{__value})
                  || $property->isa('PPI::Token::Quote')) {
                $ret = $ret . '.' . ($property->can('string') ? $property->string : $property->content);
            } else {
                $ret = $ret . '[' . $property->content . ']';
            }
        } elsif ($start->isa('PPI::Structure::Subscript') && $start->start->content eq '[') {
            my $property = $start->schild(0)->content;
            $ret = $ret . '[' . $property . ']';
        } else {
            last;
        }
        $start->snext_sibling ? $start = $start->snext_sibling : ($start->delete and last);
    }
    return $ret;
}

# Returns a perl function value (foo() or Foo::bar())
sub __getFunctionValue {
    my ($self, $element, $coderef) = @_;
    my $list = $element->snext_sibling;
    my @args;
    if (ref $list && $list->isa('PPI::Structure::List')) {
        map {$self->__parseTokenSymbol($_)} @{$list->find('Token::Symbol') || []};
        @args = eval $list->content;
        $list->delete;
    }
    return $self->__toJS($coderef->(@args));
}

# Returns a list of arguments, which are to be passed to a function/method
sub __getArguments {
    my ($self, $list, $keep_strings) = @_;
    return () unless $list->isa('PPI::Structure::List');
    my ($element, @args) = $list->children ? $list->schild(0)->schild(0) : ();
    $list->delete and return () unless $element;

    do {{
        next if $element->isa('PPI::Token::Operator');
        if ($element->isa('PPI::Token::Symbol')) {
            $self->__parseTokenSymbol($element);
            push @args, $element->content;
        } elsif ($element->isa('PPI::Token::Quote')) {
            push @args, $keep_strings ? $element->content : $element->string;
        } elsif ($element->isa('PPI::Structure::Constructor') || $element->isa('PPI::Structure::Block')) {
            push @args, $self->__getConstructor($element);
        } elsif ($element->isa('PPI::Token::Cast') && $element->content  eq '$') {
            next;
        } else {
            push @args, $element->content;
        }
    }} while ($element = $element->snext_sibling);

    $list->delete;
    return @args;
}

# Returns the contents of an anonymous hash/array
sub __getConstructor {
    my ($self, $constructor, $preserve) = @_;
    return unless defined $constructor && ($constructor->isa('PPI::Structure::Constructor') || $constructor->isa('PPI::Structure::Block'));
    my ($element, @args) = $constructor->children ? $constructor->schild(0)->schild(0) : ();
    my ($hash, $add, $even) = ($constructor->start->content eq '{', 0, 0);
    ($preserve || $constructor->delete) and return $hash ? {} : [] unless $element;

    do {{
        if ($element->isa('PPI::Token::Operator') && $element->content eq ',') {
            $add = 0;
            next;
        }
        if ($element->isa('PPI::Token::Symbol')) {
            $self->__parseTokenSymbol($element);
            $add ? $args[$#args]->add(' ' . $element->content) : push @args, _JS_LITERAL->new($element->content);
        } elsif ($element->isa('PPI::Token::Quote')) {
            $add ? $args[$#args]->add(' "' . $element->string . '"') : push @args, _JS_LITERAL->new('"' . $element->string . '"');
        } elsif ($element->isa('PPI::Structure::Constructor')) {
            $add
              ? $args[$#args]->add(' ' . $self->__getConstructor($element, 1))
              : push @args, _JS_LITERAL->new($self->__getConstructor($element, 1));
        } elsif ($element->isa('PPI::Token::Word')) {
            my $snext = $element->snext_sibling;
            my $ret = '';
            $self->__parseWord($element);
            if ($snext->isa('PPI::Structure::List')) {
                $self->__parseStructureList($snext);
                $ret = $snext->content;
                $snext->delete;
            }
            $add ? $args[$#args]->add(' ' . $element->content . $ret) : push @args, _JS_LITERAL->new($element->content . $ret);
        } elsif ($element->isa('PPI::Token::Cast') && $element->content  eq '$') {
            next;
        } else {
            $add ? $args[$#args]->add(' ' . $element->content) : push @args, _JS_LITERAL->new($element->content);
        }
        $add = 1;
    }} while ($element = $element->snext_sibling);

    $constructor->delete unless $preserve;
    @args = map {($even = !$even) ? $self->__toJS($_) : $_} @args;
    return $self->__toJS($hash ? ${\{@args}} : \@args);
}

# Returns the package glob reference if the package is available (if it has any methods)
sub __getPackageAvailability {
    my ($self, $package, $function) = @_;
    my $mainref = \%::;
    $mainref = $mainref->{$_ . '::'} or last foreach split /::/, $package;
    return unless $mainref;

    if ($function) {
        my $coderef = exists $mainref->{$function}
          ? *{$mainref->{$function}}{CODE}
          : undef;
        return $coderef if $coderef;
    } else {
        *{$mainref->{$_}}{CODE} and return $mainref foreach keys %$mainref;
    }
    return;
}

# Emits a signal to all registered handlers
sub __emitSignal {
    my ($self, $signal, $element) = @_;

    $_->($element) foreach @{$self->{__signals}{$signal}};
}

# Gets all 'outside' local lexical variables for the subref
sub __walker {
    my $self    = shift;
    my $cv      = B::svref_2object(shift);
    my $depth   = $cv->DEPTH ? $cv->DEPTH : 1;
    my $padlist = $cv->PADLIST;
    my %outside = map {$_ => 1} grep {defined $_} @{$cv->OUTSIDE->PADLIST->ARRAYelt(0)->object_2svref};
    my $names   = $padlist->ARRAYelt(0)->object_2svref;
    my $values  = [map {$_->object_2svref} $padlist->ARRAYelt($depth)->ARRAY];
    my $list    = {};
    for (my $i = 0; $i < @$names; ++$i) {
        next unless defined $names->[$i];
        next unless $outside{$names->[$i]};
        $list->{$names->[$i]} = {
            value => $values->[$i],
            type => ref $padlist->ARRAYelt($depth)->ARRAYelt($i),
        };
    }
    return $list;
}

# Local toJS
sub __toJS {
    my ($self, $data) = @_;
    my $ref  = ref $data;

    return 'null' unless defined $data;

    unless ($ref) {
        return $data if $data =~ $number
          || $self->{__currentDocument}{__variables}{$data};
        $data =~ s/^"//;
        $data =~ s/"$//;
        $data =~ s/($escapes)/
          my $ret = $special{$1} || '\\u00' . unpack('H2', $1);
          $ret;
        /eg;
        return qq{"$data"};
    }
    if ($ref eq 'ARRAY') {
        my @results;
        my @array = @$data;
        foreach my $value (@array) {
            $value = $self->__toJS($value);
            push @results, $value if defined $value;
        }
        return '[' . (join ", ", @results) . ']';
    }
    if ($ref eq 'HASH') {
        my @results;
        my %hash = %$data;
        foreach my $key (keys %hash) {
            my $value = $self->__toJS($hash{$key});
            $key = qq{"$key"} unless substr($key, 0, 1) eq '"' && substr($key, -1) eq '"';
            push @results, ($key . ': ' . $value) if defined $value;
        }
        return '{' . (join ", ", @results) . '}';
    }
    if ($ref eq 'SCALAR' || $ref eq 'REF') {
        my $copy = $$data;
        return $self->__toJS($copy);
    }
    if ($ref eq '_JS_LITERAL') {
        return $data->get;
    }
    return;
}

=head1 _JS_LITERAL

_JS_LITERAL is an internal class, which is used to I<mark> literal JavaScript strings

=cut

package _JS_LITERAL;

sub new {
    my ($proto, $content) = @_;
    my $class = ref $proto || $proto;
    return bless { content => $content || '' }, $class;
}

sub add {
    my ($self, $content) = @_;
    $self->{content} .= $content;
    return $self;
}

sub get {
    my $self = shift;
    return $self->{content};
}

1;

=head1 AUTHOR

  Viktor Kojouharov

=head1 Website

L<http://code.google.com/p/iwl>

=head1 LICENCE AND COPYRIGHT

Copyright (c) 2006-2007  Viktor Kojouharov. All rights reserved.

This module is free software; you can redistribute it and/or
modify it under the same terms as Perl itself. See perldoc perlartistic.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

=cut
