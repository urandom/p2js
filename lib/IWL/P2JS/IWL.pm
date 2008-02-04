#! /bin/false
# vim: set autoindent shiftwidth=4 tabstop=8:

package IWL::P2JS::IWL;

use strict;

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

    return $self->__inspectInc;
}

# Internal
#
my %monitors = qw(IWL/Script.pm 1);

sub __inspectInc {
    my $self = shift;
    do { $self->__connect($_) if $INC{$_} } foreach keys %monitors;

    return $self;
}

sub __connect {
    my ($self, $filename) = @_;

    if ($filename eq 'IWL/Script.pm') {
        $self->__iwlScriptInit;
    }
    *CORE::GLOBAL::require = sub {
        CORE::require(@_);
        $self->__connect($_[0]) if $monitors{$_[0]};
    };

    return $self;
}

# IWL::Script
#

sub __iwlScriptInit {
    my $self = shift;

    my $setScript = *IWL::Script::setScript{CODE};

    no warnings qw(redefine);
    *IWL::Script::setScript = sub {
        my ($self_, $param) = @_;
        @_ = ($self_, ref $param eq 'CODE' ? $self->{p2js}->convert($param) : $param);

        goto $setScript;
    }
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
