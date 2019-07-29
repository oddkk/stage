#!/usr/bin/perl
use strict;
use warnings;

use File::Path 'rmtree';
use File::Path 'mkpath';
use File::Find;
use File::Basename;
use Term::ANSIColor;
use Config;

my $src_dir = 'src';
my $test_dir = 'test';
my $build_dir = 'test/build';

if (-e $build_dir) {
	rmtree([$build_dir]);
}

my $test_cases = 0;
my $successful_test_cases = 0;

my $color_arg = '';

$color_arg = '-fdiagnostics-color=always';

my @signal_names = split(' ', $Config{sig_name});

sub return_code_success {
	my $code = shift(@_);
	my $err = $code >> 8;
	my $sig = $code & 127;
	my $sig_name = $signal_names[$sig];

	print "signal $sig_name ($sig) " if ($sig != 0);

	return $code == 0;
}

my $project_build_dir = "$build_dir/stage";

sub compile_project {
	print "Compiling project... ";
	my $project_compile_out;

	$project_compile_out = qx{
	bison $src_dir/config_parser.y -t --report=all -o $src_dir/config_parser.y.re2c 2>&1
	};
	if (!return_code_success($?)) {
		print colored(['red'], 'COMPILE FAIL') . "\n$project_compile_out\n";
		exit;
	}

	$project_compile_out = qx{
	re2c $src_dir/config_parser.y.re2c -o $src_dir/config_parser.y.c 2>&1
	};
	if (!return_code_success($?)) {
		print colored(['red'], 'COMPILE FAIL') . "\n$project_compile_out\n";
		exit;
	}

	$project_compile_out = '';
	my $compile_err = 0;

	my @files;

	File::Find::find({
		'wanted'   => sub { push @files, $_ if ($_ =~ /\.c/); },
		'no_chdir' => 1,
	}, $src_dir);
	foreach my $file (@files) {
		my $dirname = dirname($file);
		my $out_dir = "$project_build_dir/$dirname";
		mkpath($out_dir);
		$project_compile_out .= qx{
		clang $color_arg -c -g -Wall -DSTAGE_TEST=1 -pedantic $file -o $project_build_dir/$file.o 2>&1
		};
		$compile_err = 1 if (!return_code_success($?));
	}
	if ($compile_err) {
		print colored(['red'], 'COMPILE FAIL') . "\n$project_compile_out\n";
		exit;
	}
	print colored(['green'], 'DONE') . "\n";
	print "$project_compile_out\n" if ($project_compile_out);

	return map { "$project_build_dir/$_.o" } @files;
}

mkdir $build_dir;
mkdir $project_build_dir;

my $project_objects = join(' ', compile_project);

opendir(my $dir, $test_dir) or die $!;

while (my $file = readdir($dir)) {
	if ($file =~ /^test_([a-z_]+)\.c/) {
		my $test_name = $1;

		$test_cases += 1;

		print "Running test '$test_name'... ";

		my $out_file = "$build_dir/$test_name";
		my $compiler_out = qx{
clang $color_arg -g -lm -lpthread -lssl -lcrypto -Wall -pedantic -I$src_dir $project_objects $test_dir/$file -o $out_file 2>&1
};
		if (!return_code_success($?)) {
			print colored(['red'], 'COMPILE FAIL') . "\n$compiler_out\n";
			next;
		}

		my $test_out = qx{$out_file 2>&1};
		if (!return_code_success($?)) {
			print colored(['red'], 'FAIL') . "\n";
			print "$compiler_out\n" if ($compiler_out);
			print "$test_out\n";
			next;
		}

		print colored(['green'], 'PASS') . "\n";

		print "$compiler_out\n" if ($compiler_out);

		$successful_test_cases += 1;
	}
}

closedir($dir);

if ($test_cases > 0) {
	my $percent = $successful_test_cases / $test_cases * 100;
	print "$successful_test_cases / $test_cases tests passed ($percent%).\n";
} else {
	print "No test cases.\n"
}

rmtree([$build_dir]);
exit 0;
