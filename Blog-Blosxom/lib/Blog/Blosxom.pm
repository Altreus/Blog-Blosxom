package Blog::Blosxom;

use warnings;
use strict;

use CGI qw(:standard :netscape);
use FindBin;
use FileHandle;
use File::Find;
use File::Spec;
use File::stat;
use Time::localtime;

=head1 NAME

Blog::Blosxom - A module version of the apparently inactive blosxom.cgi

=head1 VERSION

Version 0.01

=head1 DESCRIPTION

Blosxom is a blog engine found at www.blosxom.com. It is hideously simple and hideously
written.

This module aims to replace the blosxom CGI script with a module that can incidentally
be run as a CGI script without too much alteration. It should be backwardly compatible
with the original blosxom script, meaning that your plugins should work correctly. That
is to say that the same variables should be available at the same times, and the 
functions you register will be called at the same or equivalent parts of the processing.

Hopefully some improvements will be made on the original script. Although it boasts a
low line count, this is at the unfortunate expense of legibility, replacing it with the
line noise Perl is so famous for. This module spreads out the original script's
processing and puts it into neat containers where it can be seen and fixed.

=cut

our $VERSION = '0.01';

=head1 SYNOPSIS

    use Blog::Blosxom;

    my $blog = Blog::Blosxom->new(%params);
    $blog->run();
    ## OR
    Blog::Blosxom->run(%params)

=head1 METHODS

=head2 new(%params)

Create a new blog. Parameters are provided in a hash and are:

=over

=item blog_title

This will be available in your templates as $blog_title, and by default appears in the
page title and at the top of the page body.

This parameter is required.

=item blog_description

This will be available in your templates as $blog_description. This does not appear in
the default templates.

=item blog_language

This is used for the RSS feed, as well as any other flavours you specify that have a
language parameter.

=item datadir

This is where blosxom will look for the blog's entries. A relative path will be relative
to the script that is using this module.

This parameter is required.

=item url

This will override the base URL for the blog, which is automatic if you do not provide.

=item depth

This is how far down subdirectories of datadir to look for more blog entries. 0 is the
default, which means to look down indefinitely. 1, therefore, means to look only in the
datadir itself, up to n, which will look n-1 subdirectories down.

=item num_entries

This is the number of entries to display on the home page. By default this is 40.

=item file_extension

By default, Blosxom will treat .txt files as blog entries. Change this to use a
different file extension. Do not provide the dot that separates the filename and the
file extension.

=item default_flavour

The flavour simply determines which set of templates to use to draw the blog. This 
defines which flavour to use by default. Vanilla blosxom has HTML and RSS flavours.

=item show_future_entries

Original Blosxom has this, which is a boolean value that determines whether or not to
show entries dated in the future. However, since Blosxom uses the filesystem's metadata
to determine the date of the entry, and a future modification date is an error in ext
filesystems, this is probably not going to do anything useful. However, it will be
available in your plugins and templates if you need it.

=item plugin_dir

Tells blosxom where to look for plugins. This is empty by default, which means it won't
look for plugins. Relative paths will be taken relative to the script that uses this
module.

=item plugin_state_dir

Some plugins wish to store state. This is where the state data will be stored. It will
need to be writable. Defaults to plugin_dir/state if you specify a plugin_dir.

=item static_dir

Blosxom can publish your files statically, which means you run the script and it creates
HTML files (for example) for each of your entries, instead of loading them dynamically.
This defines where those files should go.

=item static_password

You have to provide a password if you want to use static rendering, as a security
measure or something.

=item static_flavours

An arrayref of the flavours that Blosxom should generate statically. By default this is
html and rss.

=item static_entries

Set this to a true value to turn on static generation of individual entries. Generally
there is no point because your entries are static files already, but you may be using a
plugin to alter them before rendering.

=back

=cut

sub new {
    my ($class, %params) = @_;

    for (qw(blog_title datadir)) {
        die "Required parameter $_ not provided to Blog::Blosxom->new"
            unless exists $params{$_} && $params{$_};
    }

    my %defaults = (
        blog_description    => "",
        blog_language       => "en",
        url                 => "",
        depth               => 0,
        num_entries         => 40,
        file_extension      => "txt",
        default_flavour     => "html",
        show_future_entries => 0,
        plugin_dir          => "",
        plugin_state_dir    => "",
        static_dir          => "",
        static_password     => "",
        static_flavours     => [qw(html rss)],
        static_entries      => 0,
    );

    %params = (%defaults, %params);

    $params{plugin_state_dir} ||= $params{plugin_dir} if $params{plugin_dir};

    # Absolutify relative paths
    for my $key (qw(plugin_dir datadir static_dir plugin_state_dir)) {
        my $dir = $params{$key};

        unless (File::Spec->file_name_is_absolute( $dir )) {
            $dir = File::Spec->catdir($FindBin::Bin, $dir);
        }

        $params{$key} = $dir;
    }

    my $self = bless \%params, $class;
    $self->_load_plugins;
    $self->_load_templates;

    return $self;
}

=head2 template($path, $component, $flavour)

Returns a chunk of markup for the requested component in the requested flavour for the 
requested path. The path in this case will be the one entered in the URL, but this is 
applied to the datadir in order to find the actual file anyway.

=cut

sub template {
    my ($path, $comp, $flavour) = @_;

    my $template;

    unless ($template = $self->_check_plugins('template', @_)) {
        my $fn = File::Spec->catfile($self->{datadir}, $path, "$comp.$flavour");

        while (1) {
            # Return the contents of this template if the file exists
            if (-e $fn) {
                open my $fh, "<", $fn;
                return join '', <$fh>;
            }

            # Stop looking when there is no $path to go between datadir and the
            # template file. For portability, we can't check whether it is a "/"
            last if !$path;

            # Look one dir higher and go again.
            @dir = File::Spec->splitdir($path);
            pop @dir;
            $path = File::Spec->catdir(@dir);
            my $fn = File::Spec->catfile($self->{datadir}, $path, "$comp.$flavour");
        }
    }

    $template ||= $self->{template}{$flavour}{$comp} || $self->{template}{error}{$comp};

}

sub entries_for_path {
    my ($self, $path) = @_;

}

=head2 static_mode($password, $on)

Sets static mode. Pass in the password to turn it on. Turns it off if it is already on.

=cut

sub static_mode {
    my ($self, $password, $on) = @_;

    die "No static dir defined" unless $self->{static_dir};
    die "No static publishing password defined" unless $self->{static_password};

    # Set it to toggle if we don't specify.
    $on = !$self->{static_mode} if !defined $on;

    if ($self->{static_mode} && !$on) {
        $self->{static_mode} = 0;
        $blosxom::static_or_dynamic = 'dynamic';
        return;
    }
    
    if ($on && $password eq $self->{static_password}) {
        $self->{static_mode} = 1;
        $blosxom::static_or_dynamic = 'static';
    }
}

## PRIVATE FUNCTIONS
#  Purposely not in the POD

## _load_plugins
#  Trawl the plugins directory and look for plugins. Put them in the object hash.

sub _load_plugins {
    my $self = shift;

    my $dir = $self->{plugins_dir};
    return unless $dir;

    opendir my $plugins, $dir;

    # blosxom docs say modules ending in _ will not be loaded.
    for my $plugin (grep { /^\w+$/ && !/_$/ && -f File::Spec->join($dir, $_) }
                    sort readdir $plugins) {
        # blosxom docs say you can order modules by prefixing numbers.
        $plugin =~ s/^\d+//;
        require "$plugin_dir/$plugin";
        if ($plugin->start()) {
            $self->{active_plugins}->{$plugin_name} = 1;

            $self->{plugins_ordered} ||= [];
            push @{$self->{plugins_ordered}}, $plugin_name;
        }
    }

    closedir $plugins;
}

## _load_templates
#  Read the default templates from DATA. Later the plugins get an opportunity to
#  override what happens when the real templates are read in, so we don't do that here.

sub _load_templates {
    my $self = shift;

    while (<DATA>) {
      last if /^(__END__)?$/;
      my($flavour, $comp, $txt) = /^(\S+)\s(\S+)\s(.*)$/;
      $txt =~ s/\\n/\n/mg;
      $self->{template}{$flavour}{$comp} = $txt;
    }

}

## _check_plugins
#  Look for plugins that can do the first arg, and pass them the rest of the args.
#  Return the first plugin that returns a value.

sub _check_plugins {
    my ($self, $method, @underscore) = @_;

    return unless $self->{plugins_ordered};

    for my $plugin (@{$self->{plugins_ordered}}) {
        return $plugin->$method($self, @underscore)
        if $plugin->can($method);
    }
}

=head1 AUTHOR

Altreus, C<< <altreus at perl.org> >>

=head1 TODO

Currently the backward-compatibility of plugins is not implemented. However, if any
instance of $blosxom::variable is replaced with $blosxom->{variable} it should work.
That is because the blog object is passed through to the functions as the first argument
after the $self that is the plugin object.

=head1 BUGS

Please report any bugs or feature requests to C<bug-blog-blosxom at rt.cpan.org>, or through
the web interface at L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Blog-Blosxom>.  I will be notified, and then you'll
automatically be notified of progress on your bug as I make changes.




=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Blog::Blosxom


You can also look for information at:

=over 4

=item * RT: CPAN's request tracker

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=Blog-Blosxom>

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/Blog-Blosxom>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/Blog-Blosxom>

=item * Search CPAN

L<http://search.cpan.org/dist/Blog-Blosxom/>

=back


=head1 ACKNOWLEDGEMENTS


=head1 LICENSE AND COPYRIGHT

Copyright 2010 Altreus.

This program is free software; you can redistribute it and/or modify it
under the terms of either: the GNU General Public License as published
by the Free Software Foundation; or the Artistic License.

See http://dev.perl.org/licenses/ for more information.


=cut

1; 

__DATA__
html content_type text/html
html head <html><head><link rel="alternate" type="type="application/rss+xml" title="RSS" href="$url/index.rss" /><title>$blog_title $path_info_da $path_info_mo $path_info_yr</title></head><body><h1>$blog_title</h1><p>$path_info_da $path_info_mo $path_info_yr</p>
html story <h2><a name="$fn">$title</a></h2><p>$body</p><p>posted at: $ti | path: <a href="$url$path">$path</a> | <a href="$url/$yr/$mo_num/$da#$fn">permanent link to this entry</a></p>\n
html date <h3>$dw, $da $mo $yr</h3>\n
html foot <p><a href="http://www.blosxom.com/"><img src="http://www.blosxom.com/images/pb_blosxom.gif" border="0" /></a></p></body></html>
rss content_type text/xml
rss head <?xml version="1.0"?>\n<!-- name="generator" content="blosxom/$version" -->\n<!DOCTYPE rss PUBLIC "-//Netscape Communications//DTD RSS 0.91//EN" "http://my.netscape.com/publish/formats/rss-0.91.dtd">\n\n<rss version="0.91">\n  <channel>\n    <title>$blog_title $path_info_da $path_info_mo $path_info_yr</title>\n    <link>$url</link>\n    <description>$blog_description</description>\n    <language>$blog_language</language>\n
rss story   <item>\n    <title>$title</title>\n    <link>$url/$yr/$mo_num/$da#$fn</link>\n    <description>$body</description>\n  </item>\n
rss date \n
rss foot   </channel>\n</rss>
error content_type text/html
error head <html><body><p><font color="red">Error: I'm afraid this is the first I've heard of a "$flavour" flavoured Blosxom.  Try dropping the "/+$flavour" bit from the end of the URL.</font>\n\n
error story <p><b>$title</b><br />$body <a href="$url/$yr/$mo_num/$da#fn.$default_flavour">#</a></p>\n
error date <h3>$dw, $da $mo $yr</h3>\n
error foot </body></html>
__END__

