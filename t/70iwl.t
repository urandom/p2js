use Test::More tests => 22;
use IWL::P2JS;
use strict;

BEGIN { use_ok('IWL::P2JS::IWL'); }

my $p = IWL::P2JS->new;

sub test_object {
    require IWL::Widget;
    my $parent = IWL::Widget->new(id => 'parent');
    my $child1 = IWL::Widget->new(id => 'child1');
    my $child2 = IWL::Widget->new(id => 'child2');
    $parent->appendChild($child1, $child2);

    is($p->convert(sub {$parent->nextChild($child1);}), q|(function () {$('parent').down('#child1').next();})()|);
    is($p->convert(sub {$parent->prevChild($child2);}), q|(function () {$('parent').down('#child2').previous();})()|);
    is($parent->prevChild($child2), $child1);
    is($p->convert(sub {$parent->prependChild($child2, $child1);}), q|(function () {$('parent').insertBefore($('child2'), $('parent').firstChild);})()|);
    is($p->convert(sub {$parent->setChild($child2);}), q|(function () {$('parent').update().appendChild($('child2'));})()|);
}

sub test_script {
    require IWL::Script;
    my $script = IWL::Script->new;
    is($script->setScript(sub {return 1})->getScript, q|(function () {return 1;})();|);
    is($script->setScript("return 1;")->getScript, q|return 1;|);
    is($script->appendScript(sub {return 2})->getScript, q|return 1; (function () {return 2;})();|);
    is($script->appendScript("return 3;")->getScript, q|return 1; (function () {return 2;})(); return 3;|);
    is($script->prependScript(sub {return 4})->getScript, q|(function () {return 4;})(); return 1; (function () {return 2;})(); return 3;|);
    is($script->prependScript("return 5;")->getScript, q|return 5; (function () {return 4;})(); return 1; (function () {return 2;})(); return 3;|);
}

sub test_widget {
    my $widget = IWL::Widget->new;
    is($widget->signalConnect(click => sub {return 0})->getContent, qq|< onclick="return 0;"></>\n|);
    is($widget->signalConnect(click => "return false;")->getContent, qq|< onclick="return 0;return false;"></>\n|);
    $widget->signalConnect(click => sub { return 2});
    $widget->signalConnect(click => sub { return 3});
    is($widget->signalDisconnect(click => sub { return 3 })->getContent, qq|< onclick="return 0;return false;return 2;"></>\n|);
    is($widget->signalDisconnect(click => 'return false;')->getContent, qq|< onclick="return 0;return 2;"></>\n|);
    is($widget->signalDisconnect('click')->getContent, qq|<></>\n|);
}

sub test_variable {
    use IWL::Container;
    my $c1 = IWL::Container->new(id => 'c1');
    my $c2 = IWL::Container->new;
    like($c2->signalConnect(click => sub {$c2->remove})->getContent, qr|<div.*?onclick="null.remove\(\);".*?></div>|);
    like($c1->signalConnect(click => sub {$c1->remove})->getContent, qr|<div.*?onclick="\$\(&#39;c1&#39;\).remove\(\);".*?></div>|);
    $c1->signalDisconnect;
    like($c1->signalConnect(click => sub {$c1->appendClass('foo')})->getContent, qr|<div.*?onclick="\$\(&#39;c1&#39;\).addClassName\(&#39;foo&#39;\);".*?></div>|);
    $c1->signalDisconnect;
    like($c1->signalConnect(click => sub {$c1->appendClass('foo')->appendChild($c2)->firstChild})->getContent, qr|<div.*?onclick="\$\(&#39;c1&#39;\).addClassName\(&#39;foo&#39;\).appendChild\(null\).down\(\);".*?></div>|);
    $c1->signalDisconnect;
    like($c1->signalConnect(click => sub {$c1->setStyle(display => 'none')->deleteStyle('visibility')->remove})->getContent,
        qr|<div.*?onclick="\$\(&#39;c1&#39;\).setStyle\(\{&quot;display&quot;: &quot;none&quot;\}\)\.setStyle\(\{&quot;visibility&quot;: &quot;&quot;\}\)\.remove\(\);".*?></div>|);
}

test_object;
test_script;
test_widget;
test_variable;
