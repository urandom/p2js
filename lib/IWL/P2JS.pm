#! /bin/false
# vim: set autoindent shiftwidth=4 tabstop=8:

package IWL::P2JS;

use strict;

use B::Deparse;
use IWL::JSON;
use PPI::Document;
use Scalar::Util qw(blessed);

use vars qw($VERSION);

my $ignore_json = 0;

$VERSION = '0.01';

=head1 NAME

IWL::P2JS - a basic Perl to JavaScript converter

=head1 DESCRIPTION

IWL::P2JS is a class, which provides methods for converting perl subroutines into javascript code. This is a VERY experimental module, whose goal is to provide a way for developers to use Perl code in IWL signal handlers.

=head1 CONSTRUCTOR

IWL::P2JS->new

=cut

sub new {
    my ($proto) = @_;
    my $class = ref($proto) || $proto;
    my $self = bless {}, $class;

    $self->{__deparser} = B::Deparse->new("-sC", "-q");

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
    my ($i, $assignment, $operator, $sigil) = (-1, '');
    foreach my $child ($statement->schildren) {
        ++$i;
        next if !$child->parent;
        if ($child->isa('PPI::Structure::List')) {
            my $symbols = $child->find('Token::Symbol');
            if ($symbols) {
                foreach (@$symbols) {
                    $sigil = $_->symbol_type;
                    $self->__parseToken($_);
                }
            }
            if ($assignment || $self->__previousIsMethod($child)) {
                if ($sigil eq '@') {
                    $child->{start}->set_content('[');
                    $child->{finish}->set_content(']');
                } elsif ($sigil eq '%') {
                    $child->{start}->set_content('{');
                    $child->{finish}->set_content('}');
                    my $operators = $child->find(sub {$_[1]->isa('PPI::Token::Operator') && $_[1] eq ','});
                    if ($operators) {
                        for (my $j = 0; $j < @$operators; $j += 2) {
                            $operators->[$j]->set_content(':');
                        }
                    }
                }
            } else {
                $child->{start}->set_content(' ');
                $child->{finish}->set_content(' ');
            }
        } elsif ($child->isa('PPI::Structure::Constructor')) {
            my $token = PPI::Token->new(toJSON($self->__getConstructor($child, 1)));
            $child->insert_before($token) and $child->delete;
        } elsif ($child->isa('PPI::Token::Word') && {if => 1, unless => 1, while => 1, until => 1, foreach => 1, for => 1}->{$child->content}) {
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
        } elsif ($child->isa('PPI::Token::Word') && $child->content ne 'var' && $child->snext_sibling->isa('PPI::Structure::List')) {
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
        } elsif ($child->isa('PPI::Token::Word')
              && $child->sprevious_sibling
              && $child->sprevious_sibling->isa('PPI::Token::Operator')
              && $child->sprevious_sibling->content eq '.'
              && !$child->snext_sibling->isa('PPI::Structure::List')
        ) {
            $self->__emitSignal('Token::Word', $child);
            # Method without arguments
            $child->insert_after(PPI::Token->new('()'));
        } elsif ($child->isa('PPI::Token::Quote')
                 && $child->snext_sibling->isa('PPI::Token::Operator')
                 && $child->snext_sibling->content eq '->') {
            $child->set_content($self->__getExpressionValue($child, $child->string));
        } elsif ($child->isa('PPI::Token')) {
            if ($child->isa('PPI::Token::Operator')) {
                $operator = 1;
                $assignment = $child->content eq '=';
                $child->set_content('.') if $child->content eq '->';
            } elsif ($child->isa('PPI::Token::Symbol')) {
                $sigil = $child->symbol_type;
            }
            $self->__parseToken($child);
        } elsif ($child->isa('PPI::Structure::Subscript')) {
            my $prev = $child->sprevious_sibling;
            next unless $prev;
            if ($prev->isa('PPI::Token::Symbol')
                  && $self->{__currentDocument}{__variables}{$prev} eq '%') {
                $child->start->set_content('[');
                $child->finish->set_content(']');
                $self->__parseToken($_) foreach @{$child->find('Token')};
            }
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
                    $self->__parseToken($_);
                }
            }
        } elsif ($child->isa('PPI::Token::Magic')
              && $child->sprevious_sibling->isa('PPI::Token::Word')
              && $child->sprevious_sibling->content eq 'for') {
            $child->delete;
        } elsif ($child->isa('PPI::Structure::ForLoop')) {
            $self->__parseForLoop($child);
        } elsif ($child->isa('PPI::Structure::Block')) {
            $self->__parseStatement($_) foreach $child->schildren;
        }
    }
}

sub __parseToken {
    my ($self, $token) = @_;
    if ($token->isa('PPI::Token::Symbol')) {
        my $sigil = $token->symbol_type;
        my $content = $token->content;
        my $name = substr $content, 1;
        my $assignment;
        my $snext = $token;
        $snext = $token->parent->parent if $token->parent->isa('PPI::Statement::Expression') && $sigil eq '%';
        while ($snext = $snext->snext_sibling) {
            last if $snext->isa('PPI::Token::Symbol') && !$snext->sprevious_sibling->isa('PPI::Token::Operator');
            if ($snext->isa('PPI::Token::Operator') && $snext->content eq '=') {
                $assignment = 1;
                last;
            }
        }
        if ($assignment) {
            $self->{__currentDocument}{__variables}{$name} = $sigil;
            $token->set_content($name);
        } else {
            my $pad_value = $self->{__pad}{$token->content}
              ? do {
                    my $value = $self->{__pad}{$token->content}{value};
                    if (ref $value eq 'REF' && blessed $$value) {
                        $self->__getExpressionValue($token, $$value);
                    } else {
                        toJSON($value)
                    }
                }
              : $name;
            $token->set_content($pad_value);
        }
    }
    return $self;
}

sub __parseForLoop {
    my ($self, $child) = @_;
    return unless $child;
    my $statement = $child->parent;
    if ($child->schildren == 1) {
        my $s = ($child->children)[0];
        my $operator = $s->find_first('Token::Operator');
        my $elements = $s->find(
            sub {
                return 1 if $_[1]->isa('PPI::Token::Number');
                $self->__parseToken($_[1]) and return 1 if ($_[1]->isa('PPI::Token::Symbol'));
            }
        );
        my $content = PPI::Token->new;

        if ($operator && $operator->content eq '..') {
            # Range
            $content->set_content(
                'var _ = ' . $elements->[0]->content . '; _ < ' . ($elements->[1]->content + 1) . '; ++_'
            );
            $_->delete foreach $s->children;
            $s->add_element($content);
        } elsif (!$operator && $s->first_element->isa('PPI::Token::Word') && $s->first_element->content eq 'keys') {
            $s->first_element->set_content('var _ in');
        } elsif (($operator && $operator->content eq ',') || $elements->[0]->isa('PPI::Token::Symbol')) {
            # Array
            my $array = PPI::Token->new;
            my $st = PPI::Statement->new;
            $array->set_content(
                $operator
                  ? 'var _$ = [' . join(',', map {$_->content} @$elements) . '];'
                  : 'var _$ = ' . $elements->[0]->content . ';'
            );
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
                my @args = $self->__getArguments($start->snext_sibling);
                $ret = $ret->$method(@args);
            } else {
                my @args = $self->__getArguments($start->snext_sibling, 1);
                $ret = join '.', split '::', $ret;
                $ret = ($method eq 'new' ? ('new ' . $ret) : ($ret . '.' . $method))
                  . '(' . (join ', ', map {
                    ref $_ ? toJSON($_) : $_
                  } @args) . ')';
                $string = 1;
            }
        } elsif ($start->isa('PPI::Structure::Subscript') && $start->start->content eq '{') {
            my $property = ($start->children)[0]->content;
            $property =~ s/^'// and $property =~ s/'$//;
            $ret = $ret->{$property};
        } elsif ($start->isa('PPI::Structure::Subscript') && $start->start->content eq '[') {
            my $property = ($start->children)[0]->content;
            $ret = $ret->[$property];
        } else {
            last;
        }
        $start = $start->snext_sibling;
    }
    return $string ? $ret : toJSON($ret);
}

# Returns a perl function value (foo() or Foo::bar())
sub __getFunctionValue {
    my ($self, $element, $coderef) = @_;
    my $list = $element->snext_sibling;

    return toJSON($coderef->($self->__getArguments($list)));
}

# Returns a list of arguments, which are to be passed to a function/method
sub __getArguments {
    my ($self, $list, $keep_strings) = @_;
    return () unless $list->isa('PPI::Structure::List');
    my ($element, @args) = $list->children ? ((($list->children)[0])->children)[0] : ();
    $list->delete and return () unless $element;

    $ignore_json = 1;
    do {{
        next if $element->isa('PPI::Token::Operator');
        if ($element->isa('PPI::Token::Symbol')) {
            $self->__parseToken($element);
            push @args, $element->content;
        } elsif ($element->isa('PPI::Token::Quote')) {
            push @args, $keep_strings ? $element->content : $element->string;
        } elsif ($element->isa('PPI::Structure::Constructor')) {
            push @args, $self->__getConstructor($element);
        } else {
            push @args, $element->content;
        }
    }} while ($element = $element->snext_sibling);
    $ignore_json = 0;

    $list->delete;
    return @args;
}

# Returns the contents of an anonymous hash/array
sub __getConstructor {
    my ($self, $constructor, $skip) = @_;
    return unless $constructor->isa('PPI::Structure::Constructor');
    my ($element, @args) = $constructor->children ? ((($constructor->children)[0])->children)[0] : ();
    my ($hash, $odd) = ($constructor->start->content eq '{', 1);
    ($skip || $constructor->delete) and return $hash ? {} : [] unless $element;

    $ignore_json = 1;
    do {{
        next if $element->isa('PPI::Token::Operator');
        if ($element->isa('PPI::Token::Symbol')) {
            $self->__parseToken($element);
            push @args, $element->content;
        } elsif ($element->isa('PPI::Token::Quote')) {
            push @args, $element->string;
        } elsif ($element->isa('PPI::Structure::Constructor')) {
            push @args, $self->__getConstructor($element, 1);
        } else {
            push @args, $element->content;
        }
    }} while ($element = $element->snext_sibling);
    $ignore_json = 0;

    $constructor->delete unless $skip;
    return $hash ? ${\{@args}} : \@args;
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

# Local toJSON
sub toJSON {
    my $data = shift;
    return IWL::JSON::toJSON($data) unless $ignore_json;
    my $ref = ref $data;
    if ($ref eq 'SCALAR' || $ref eq 'REF') {
        $data = $$data;
    }
    return $data;
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
