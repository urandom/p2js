#! /bin/false
# vim: set autoindent shiftwidth=4 tabstop=8:

package IWL::P2JS::IWL;

use strict;

use IWL::JSON 'toJSON';

=head1 NAME

IWL::P2JS::IWL - a plugin for P2JS to handle IWL specific tasks

=head1 DESCRIPTION

IWL::P2JS::IWL is a plugin, whose task is to overload specific L<IWL> functions, so that they use L<IWL::P2JS>.

=head1 CONSTRUCTOR

IWL::P2JS::IWL->new(B<IWL::P2JS>)

where B<IWL::P2JS> is an instantiated object from the L<IWL::P2JS> class

=cut

sub new {
    my ($proto, $converter) = @_;
    my $class = ref($proto) || $proto;
    my $self = bless {p2js => $converter}, $class;

    $self->__connect;
    return $self->__inspectInc;
}

# Internal
#
my @monitors = qw(IWL/Object IWL/Script IWL/Widget);
my %overridden;
my %translations = (
    firstChild      => 'down',
    lastChild       => 1,
    getChildren     => 'childElements',
    nextSibling     => 'next',
    prevSibling     => 'previous',
    getParent       => 'up',
    appendChild     => 1,
    setAttribute    => 'writeAttribute',
    setAttributes   => 'writeAttribute',
    getAttribute    => 'readAttribute',
    hasAttribute    => 1,
    deleteAttribute => 'removeAttribute',

    getStyle        => 'getStyle',
    appendClass     => 'addClassName',
    prependClass    => 'addClassName',
    hasClass        => 'hasClassName',
    removeClass     => 'removeClassName',

    isDisabled      => 'isNotEnabled',
);

sub __inspectInc {
    my $self = shift;
    do { $self->__override($_ . '.pm') if $INC{$_ . '.pm'} } foreach @monitors;

    BEGIN {
        *CORE::GLOBAL::require = sub {
            CORE::require($_[0]);
            $self->__override($_[0]) if $self && grep {$_[0] eq $_ . '.pm'} @monitors;
            return 1;
        } unless $overridden{require};
    }
    $overridden{require} = [1];

    return $self;
}

sub __override {
    my ($self, $filename) = @_;
    my $method = '__iwl' . (join '', split '/', substr $filename, 4, -3) . 'Init';

    $self->$method;
    return $self;
}

sub __isOverridden {
    my ($self, $ref, $method) = @_;
    foreach my $class (grep {$ref->isa($_)} keys %overridden) {
        return 1 if grep { $_ eq $method } @{$overridden{$class}};
    }
}

sub __connect {
    my $self = shift;
    $self->{p2js}->signalConnect('blessed_expression', sub {
        my ($element, $value) = @_;
        return $element unless $value->isa('IWL::Widget');
        my $snext = $element->snext_sibling;
        my $method = $snext && $snext->isa('PPI::Token::Operator') && $snext->content eq '->'
          ? $snext->snext_sibling->content
          : undef;
        my $id = $value->getId;
        $element->set_content('null') and return unless $id;
        if ($method && !exists $translations{$method} && !$self->__isOverridden($value, $method)) {
            my $coderef = $self->{p2js}->_getPackageAvailability($value, $method);
            return $element if $coderef;
        }
        $element->{__iwlElement} = $self->{_inhibit} = 1;
        $element->set_content("\$('$id')");
        return;
    });

    $self->{p2js}->signalConnect('token_word', sub {
        my $element = shift;
        my $iterator = $element;
        my $ok = 0;
        do {{
            last if $iterator->isa('PPI::Token::Operator') && $iterator->content ne '.';
            $ok = 1 if $iterator->{__iwlElement};
        }} while $iterator = $iterator->sprevious_sibling;
        return $element unless $ok;

        my $content = $element->content;
        my $method = $translations{$content};
        if ($method) {
            $method = $content if $method eq '1';
            $element->set_content($method);
        }
        return $element;
    });

    return $self;
}

sub __callerP2JS {
    my $self = shift;

    my $stack = 0;
    while (my @stack = caller($stack++)) {
        return 1 if $stack[0] eq 'IWL::P2JS';
    }
    return;
}

no warnings qw(redefine);
# IWL::Object
#
sub __iwlObjectInit {
    my $self = shift;
    return if $overridden{"IWL::Object"};

    my $nextChild = *IWL::Object::nextChild{CODE};
    *IWL::Object::nextChild = sub {
        my ($self_, $reference) = @_;
        if ($self->__callerP2JS) {
            my $id = $reference->getAttribute('id');
            return _JS_LITERAL->new("down('#$id').next()") if $id;
        }

        goto $nextChild;
    };

    my $prevChild = *IWL::Object::prevChild{CODE};
    *IWL::Object::prevChild = sub {
        my ($self_, $reference) = @_;
        if ($self->__callerP2JS) {
            my $id = $reference->getAttribute('id');
            return _JS_LITERAL->new("down('#$id').previous()") if $id;
        }

        goto $prevChild;
    };

    my $prependChild = *IWL::Object::prependChild{CODE};
    *IWL::Object::prependChild = sub {
        my ($self_, $reference) = @_;
        if ($self->__callerP2JS) {
            my $parent = $self_->getAttribute('id');
            my $id = $reference->getAttribute('id');
            return _JS_LITERAL->new("insertBefore(\$('$id'), \$('$parent').firstChild)") if $id;
        }

        goto $prependChild;
    };

    my $setChild = *IWL::Object::setChild{CODE};
    *IWL::Object::setChild = sub {
        my ($self_, $reference) = @_;
        if ($self->__callerP2JS) {
            my $id = $reference->getAttribute('id');
            return _JS_LITERAL->new("update().appendChild(\$('$id'))") if $id;
        }

        goto $setChild;
    };

    $overridden{"IWL::Object"} = [qw(nextChild prevChild prependChild setChild)];
}

# IWL::Script
#

sub __iwlScriptInit {
    my $self = shift;
    return if $overridden{"IWL::Script"};

    no strict 'refs';
    foreach my $method (qw(set append prepend)) {
        my $original = *{"IWL::Script::${method}Script"}{CODE};

        *{"IWL::Script::${method}Script"} = sub {
            my ($self_, $param) = @_;
            @_ = ($self_, ref $param eq 'CODE' ? $self->{p2js}->convert($param) : $param);

            goto $original;
        };
    }

    $overridden{"IWL::Script"} = [qw(setScript appendScript prependScript)];
}

# IWL::Widget
#
sub __iwlWidgetInit {
    my $self = shift;
    return if $overridden{"IWL::Widget"};

    no strict 'refs';
    foreach my $method (qw(Connect Disconnect)) {
        my $original = *{"IWL::Widget::signal${method}"}{CODE};

        *{"IWL::Widget::signal${method}"} = sub {
            my ($self_, $signal, $callback) = @_;
            my $globalScope = $self->{p2js}{_options}{globalScope};
            $self->{p2js}{_options}{globalScope} = 1;
            @_ = ($self_, $signal, ref $callback eq 'CODE' ? $self->{p2js}->convert($callback) : $callback);
            $self->{p2js}{_options}{globalScope} = $globalScope;

            goto $original;
        };
    }

    my $setStyle = *IWL::Widget::setStyle{CODE};
    *IWL::Widget::setStyle = sub {
        my ($self_, %style) = @_;
        return _JS_LITERAL->new('setStyle(' . toJSON(\%style) . ')') if $self->__callerP2JS;

        goto $setStyle;
    };

    my $deleteStyle = *IWL::Widget::deleteStyle{CODE};
    *IWL::Widget::deleteStyle = sub {
        my ($self_, $style) = @_;
        return _JS_LITERAL->new(qq|setStyle({"$style": ""})|) if $self->__callerP2JS;

        goto $deleteStyle;
    };

    $overridden{"IWL::Widget"} = [qw(signalConnect signalDisconnect setStyle deleteStyle)];
}

1;

=head1 LICENCE AND COPYRIGHT

Copyright (c) 2006-2007  Viktor Kojouharov. All rights reserved.

This module is free software; you can redistribute it and/or
modify it under the same terms as Perl itself. See perldoc perlartistic.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

=cut
