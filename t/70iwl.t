use Test::More tests => 1;
use IWL::P2JS;
use strict;

my $p = IWL::P2JS->new(globalScope => 1);

sub test_overrides {
    require IWL::Script;
    like(IWL::Script->new->setScript(sub {return 1})->getContent, qr|<script .*?>return 1;</script>|);
}

test_overrides;
