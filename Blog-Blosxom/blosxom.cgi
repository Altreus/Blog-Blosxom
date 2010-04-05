#!/usr/bin/perl

use strict;
use warnings;
use Blog::Blosxom;
use FindBin;
use File::Spec;

# The simplest blog is just a name and a data dir.
# A description helps a bit.
my $blog = Blog::Blosxom->new(
    datadir => File::Spec->catdir($FindBin::Bin, "blog"),
    blog_title => "My Blog",
    blog_description => "A blog for blogging with",
);

my $path = "";
my $flavour = "html";

print $blog->run($path, $flavour);
