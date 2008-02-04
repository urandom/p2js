use inc::Module::Install;

name			'IWL-P2JS';
license         'perl';
all_from		'lib/IWL/P2JS.pm';

requires		'PPI' => 1.200;

auto_install;

WriteAll;
