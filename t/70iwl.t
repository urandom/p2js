use Test::More tests => 7;
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

test_script;
