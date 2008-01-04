#!/bin/false

package Foo;

use strict;

sub new {bless {prop1 => 42, prop2 => Bar->new}, shift}
sub printJS {
    return "Hello JS.";
}
sub overloaded {
    return 1 if (caller(1))[0] eq 'IWL::P2JS';
    return 0;
}
sub this {
    return shift;
}
sub printArgs {
    shift if ref $_[0] eq __PACKAGE__ || $_[0] eq __PACKAGE__;
    return join ', ', @_;
}

package Bar;

sub new {bless [am => 'bar'], shift}

package Foo::Bar;

sub new {bless{}, shift};

1;
