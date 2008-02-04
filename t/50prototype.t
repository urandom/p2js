use Test::More tests => 8;
use IWL::P2JS;
use strict;

BEGIN { use_ok('IWL::P2JS::Prototype'); push @INC, "./t"; }
use Foo;

my $p = IWL::P2JS->new(globalScope => 1);

sub test_general {
    is($p->convert(sub {S('foo')->down}), q|$('foo').down();|);
    is($p->convert(sub {SS('#foo')->down}), q|$$('#foo').down();|);
}

sub test_real {
    no strict 'vars';
    is($p->convert(sub {
        my $content = S('content')->down();
        $params->{codeFor} = $content->{id} if $content;
    }), q|var content = $('content').down();if ( content) params.codeFor = content.id;|);
    is($p->convert(sub {{text => SF('image_entry_text') || 0}}), q{{"text": $F('image_entry_text') || 0}});
    my $message = "five";
    is($p->convert(sub {S($this)->isPulsating ? $this->setPulsate(0) : $this->setPulsate(1)}), '$(this).isPulsating() ? this.setPulsate(0) : this.setPulsate(1);');
    my $container = Foo->new;
    is($p->convert(sub {my $container = S('main_container')->positionAtCenter;$container->setStyle({visibility => 'visible', display => 'none'});$container->appear->shake;}),
        q|var container = $('main_container').positionAtCenter();container.setStyle({"visibility": "visible", "display": "none"});container.appear().shake();|);
    is($p->convert(sub {document::signalConnect('dom:ready' => sub { S('main_container')->hide })}), q|document.signalConnect('dom:ready', function() {$('main_container').hide();});|);
}

test_general;
test_real;
