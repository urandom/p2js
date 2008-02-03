use inc::Module::Install;

name			'IWL-P2JS';
license         'perl';
all_from		'lib/IWL/P2JS.pm';

requires		'PPI' => 1.200;
requires		'Text::Balanced' => 2.0.0;

auto_install;

WriteAll;
