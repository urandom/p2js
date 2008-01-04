#! /bin/false
# vim: set autoindent shiftwidth=4 tabstop=8:

package IWL::P2JS::Prototype;

use strict;

=head1 NAME

IWL::P2JS::Prototype - a plugin for P2JS to handle Prototype's specific tasks

=head1 DESCRIPTION

IWL::P2JS::Prototype is a class, which handles various specifics of the JavaScript Prototype library.  Currently, this class provides a mapping for the I<$> utility methods, described in L<http://prototypejs.org/api/utility>. The mapping replaces the functions, whose names correspond to the utility functions, but with the I<$> replaced by an I<S>. Example:

    $('foo') -> S('foo')

=head1 CONSTRUCTOR

IWL::P2JS::Prototype->new(B<IWL::P2JS>)

where B<IWL::P2JS> is an instantiated object from the L<IWL::P2JS> class

=cut

sub new {
    my ($proto, $p2js) = @_;
    my $class = ref($proto) || $proto;
    my $self = bless {p2js => $p2js}, $class;

    return $self->__connect;
}

# Internal
#
my $token_word = sub {
    my $element = shift;
    
    return $element->set_content('$') if $element->content eq 'S';
    return $element->set_content('$$') if $element->content eq 'SS';
    return $element->set_content('$A') if $element->content eq 'SA';
    return $element->set_content('$F') if $element->content eq 'SF';
    return $element->set_content('$H') if $element->content eq 'SH';
    return $element->set_content('$R') if $element->content eq 'SR';
};

sub __connect {
    my $self = shift;
    $self->{p2js}->signalConnect('Token::Word', $token_word);

    return $self;
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
