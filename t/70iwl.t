use Test::More tests => 12;
use IWL::P2JS;
use strict;

BEGIN { use_ok('IWL::P2JS::IWL'); }

my $p = IWL::P2JS->new(globalScope => 1);

sub test_script {
    require IWL::Script;
    my $script = IWL::Script->new;
    is($script->setScript(sub {return 1})->getScript, q|return 1;|);
    is($script->setScript("return 1;")->getScript, q|return 1;|);
    is($script->appendScript(sub {return 2})->getScript, q|return 1; return 2;|);
    is($script->appendScript("return 3;")->getScript, q|return 1; return 2; return 3;|);
    is($script->prependScript(sub {return 4})->getScript, q|return 4; return 1; return 2; return 3;|);
    is($script->prependScript("return 5;")->getScript, q|return 5; return 4; return 1; return 2; return 3;|);
}

sub test_widget {
    use IWL::Widget;
    my $widget = IWL::Widget->new;
    is($widget->signalConnect(click => sub {return 0})->getContent, qq|< onclick="return 0;"></>\n|);
    is($widget->signalConnect(click => "return false;")->getContent, qq|< onclick="return 0;return false;"></>\n|);
    $widget->signalConnect(click => sub { return 2});
    $widget->signalConnect(click => sub { return 3});
    is($widget->signalDisconnect(click => sub { return 3 })->getContent, qq|< onclick="return 0;return false;return 2;"></>\n|);
    is($widget->signalDisconnect(click => 'return false;')->getContent, qq|< onclick="return 0;return 2;"></>\n|);
    is($widget->signalDisconnect('click')->getContent, qq|<></>\n|);
}

test_script;
test_widget;
