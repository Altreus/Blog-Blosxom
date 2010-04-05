package Blog::Blosxom;

use warnings;
use strict;

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

    die $params{datadir} . " does not exist!" unless -d $params{datadir};

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

=head2 run ($path, $flavour)

It is now the responsibility of the user to provide the correct path and 
flavour. That is because there are several ways that you can gain this
information, and it is not up to this engine to decide what they are. That is,
this information comes from the request URL and, possibly, the parameter string,
POST, cookies, what-have-you.

Therefore:

=over

=item

The path is the entire path up to the filename. The filename shall not include
a file extension. The filename is optional, and if omitted, the directory given
will be searched for all entries and an index page generated.

=item

The flavour can be gathered in any manner you desire. The original Blosxom
script would use either a parameter string, C<?flav=html>, or simply by using
the flavour as the file extension for the requested path.

=back

No flavour provided will result in the default being used, obviously. No path
being provided will result in the root path being used, since these are 
equivalent.

=cut

sub run {
    my ($self, $path, $flavour) = @_;

    $path ||= "";
    $flavour ||= $self->{default_flavour};

    my @entries;

    $self->{single_entry} = 1 if (-f $path . "." . $self->{file_extension});

    $self->{path_info} = $path;
    $self->{flavour} = $flavour;

    # Build an index page for the path
    @entries = $self->entries_for_path($path);
    @entries = $self->filter(@entries);
    @entries = $self->sort(@entries);
    
    # A special template. The user is going to need to know this, but not print it.
    $self->{content_type} = $self->template($path, "content_type", $flavour);

    my @templates;
    push @templates, $self->interpolate($self->template($path, "head", $flavour));

    my $date;
    for my $entry (@entries) {
        # TODO: Here is an opportunity to style the entries in the style
        # of the subdir they came from.
        if ($date != ($date = $entry->[1]->{date})) {
            push @templates, $self->interpolate($self->template($path, "date", $flavour), {date=>$date} );
        }

        my $entry_data = $self->entry_data($entry);
        push @templates, $self->interpolate($self->template($path, "story", $flavour), $entry_data);
    }

    push @templates, $self->interpolate($self->template($path, "foot", $flavour));
    # A skip plugin will stop processing just before anything is output.
    # Not sure why.
    return if $self->_check_plugins('skip');

    return join "\n", @templates;
}

=head2 template($path, $component, $flavour)

Returns a chunk of markup for the requested component in the requested flavour for the 
requested path. The path in this case will be the one entered in the URL, but this is 
applied to the datadir in order to find the actual file anyway.

=cut

sub template {
    my ($self, $path, $comp, $flavour) = @_;

    my $template;

    unless ($template = $self->_check_plugins('template', @_)) {
        my $fn = File::Spec->catfile($self->{datadir}, $path, "$comp.$flavour");

        while (1) {
            # Return the contents of this template if the file exists. If it is empty,
            # we have defaults set up.
            if (-e $fn) {
                open my $fh, "<", $fn;
                $template = join '', <$fh>;
            }

            # Stop looking when there is no $path to go between datadir and the
            # template file. For portability, we can't check whether it is a "/"
            last if !$path;

            # Look one dir higher and go again.
            my @dir = File::Spec->splitdir($path);
            pop @dir;
            $path = File::Spec->catdir(@dir);
            my $fn = File::Spec->catfile($self->{datadir}, $path, "$comp.$flavour");
        }
    }

    $template ||= $self->{template}{$flavour}{$comp} || $self->{template}{error}{$comp};
}

=head2 entries_for_path

Given a path, find the entries that should be returned. This may be overridden
by a plugin defining the function "entries", or this "entries_for_path" function.
They are synonymous. See L<PLUGINS> for information on overriding this method.

This implements two behaviours. If the path requested is a real path then it is
searched for all blog entries, honouring the depth parameter that limits how far
below the data_dir we should look for blog entries.

If it is not then it is expected to be a date, being in 1, 2 or 3 parts, in one
true date ISO format. This version will return all entries filtered by this date
specification. See also L<date_of_post>, which determines the date on which the
post was made and can be overridden in plugins.

=cut

sub entries_for_path {
    my ($self, $path) = @_;
    
    my @entries;

    return @entries if @entries = $self->_check_plugins('entries', @_);

    my $abs_path = File::Spec->catdir( $self->{datadir}, $path );

    # If this is an entry, return it.
    return $path if (-f $abs_path . "." . $self->{file_extension});

    if (-d $abs_path) {
        # We use File::Find on a real path
        my $find = sub {
            return unless -f;

            my $rel_path = File::Spec->abs2rel( $File::Find::dir, $self->{datadir} );
            my $curdepth = File::Spec->splitpath($rel_path);

            my $fex = $self->{file_extension};

            # not specifying a file extension is a bit silly.
            if (!$fex || /\.$fex/) {
                no warnings "once"; # File::Find::name causes a warning.

                my $rel_file = File::Spec->catfile( $rel_path, $_ );
                my $date = $self->date_of_post($File::Find::name);
                my $file_info = { date => $date };

                push @entries, [$rel_file, $file_info ];
            }

            $File::Find::prune = ($self->{depth} && $curdepth > $self->{depth});
        };

        File::Find::find( $find, $abs_path );
    }
    else {
        # We use date stuff on a fake path.
        # TODO: see whether we can split the path into a date section and a real section.
        my @ymd = File::Spec->splitpath( $path );
        my @all_entries = $self->entries_for_path( "" );

        for( @all_entries ) {
            if (                   $ymd[0] == $_[1]->{date}->[0]
            && (!exists $ymd[1] or $ymd[1] == $_[1]->{date}->[1])
            && (!exists $ymd[2] or $ymd[2] == $_[1]->{date}->[2])) {
                push @entries, $_;
            }
        }
    }

    return @entries;
}

=head2 date_of_post ($fn)

Return a unix timestamp defining the date of the post. The filename provided to
the method is an absolute filename.

=cut

sub date_of_post {
    my ($self, $fn) = @_;

    my $dop;
    return $dop if $dop = $self->_check_plugins("date_of_post", @_);

    return stat($fn)->mtime;
}

=head2 filter (@entries)

This function returns only the desired entries from the array passed in. By
default it just returns the array back, so is just a place to check for plugins.

This can be overridden by plugins in order to alter the way the module filters
the files. See L<PLUGINS> for more details.

=cut

sub filter {
    my ($self, @entries) = @_;

    my @remaining_files = $self->_check_plugins("filter", @_);

    return @remaining_files || @entries;
}

=head2 sort (@entries) 

Sort @entries and return the new list.

Default behaviour is to sort by date.

=cut

sub sort {
    my ($self, @entries) = @_;

    my @sorted_entries;
    return @sorted_entries if @sorted_entries = $self->_check_plugins("sort", @_);

    @sorted_entries = sort { $a->[1]->{date} <=> $b->[1]->{date} } @entries;
    return @sorted_entries;
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

=head2 interpolate($template, $extra_data) 

Each template is interpolated, which means that variables are swapped out if
they exist. Each template may have template-specific variables; e.g. the story
template has a title and a body. Those are provided in the extra data, which is
a hashref with the variable name to be replaced (without the $) as the key, and
the corresponding value as the value.

This method can be overridden by a plugin.

=cut

sub interpolate {
    my ($self, $template, $extra_data) = @_;
 
    my $done;
    return $done if $done = $self->_check_plugins("interpolate", @_);

    for my $var (keys %$extra_data){
        $template =~ s/\$$var\b/$extra_data->{$var}/g;
    }

    # The blosxom docs say these are global vars, so let's mimic that.
    for my $var (qw(blog_title blog_description blog_language url path_info flavour)) {
        $template =~ s/\$$var\b/$self->{$var}/g;
    }

    # I couldn't think of a better way. I don't think there are any blosxom::
    # namespace vars that need to be exposed to templates, but other plugins
    # may make some.
    $template =~ s/(\$\w+(?:::\w+)?)/"defined $1 ? $1 : ''"/gee;
    return $template;
}

=head2 entry_data ($entry) 

Provided with the entry data, which is an arrayref with the entry filename,
relative to datadir, in the first slot and a hashref in the second. The hashref
will have at least a date entry, being a UNIX timestamp for the entry. See
the section on plugin entries.

Returns a hashref containing the following keys:

=over

=item title

Post title

=item body

The body of the post

=item yr

=item mo

=item mo_num

=item da

=item dw

=item hr

=item min

Timestamp of entry. mo = month name; dw = day name

=item path

The folder in which the post lives, relative to the blog's base URL.

=item fn

The filename of the post, sans extension.

=back

These should be returned such that it is true that

  $path . "/" . $fn . "." . $flavour eq $request_url

i.e. these components together are what was originally asked for. (Note that
flavour is a variable available to templates but not returned by this method.)

=cut

sub entry_data {
    my ($self, $entry) = @_;

    my $entry_data = {};

    my $fn = $entry->[0];

    {
        open my $fh, "<", File::Spec->catfile($self->{datadir}, $fn);
        my $title = <$fh>; chomp $title;
        $entry_data->{title} = $title;

        $entry_data->{body} = join "", <$fh>;
        close $fh;
    }
    
    {
        my @path = (File::Spec->splitpath($fn));
        $fn = pop @path;
        $fn =~ s/\.$self->{file_extension}$//;
        $entry_data->{fn} = $fn;
        $entry_data->{path} = File::Spec->catpath(@path);
    }

    {
        my $i = 1;
        my %month2num = map {$_, $i++} qw(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec);
        my $c_time = ctime($entry->[1]->{date});
        my($dw,$mo,$da,$hr,$min,$yr) = ( $c_time =~ /(\w{3}) +(\w{3}) +(\d{1,2}) +(\d{2}):(\d{2}):\d{2} +(\d{4})$/ );
        $da = sprintf("%02d", $da);
        my $mo_num = $month2num{$mo};
        $mo_num = sprintf("%02d", $mo_num);

        @{$entry_data}{qw(dw mo da yr mo_num hr min)} = ($dw, $mo, $da, $yr, $mo_num, $hr, $min);
    }

    return $entry_data;
}

## PRIVATE FUNCTIONS
#  Purposely not in the POD

## _load_plugins
#  Trawl the plugins directory and look for plugins. Put them in the object hash.

sub _load_plugins {
    my $self = shift;

    my $dir = $self->{plugins_dir};
    return unless $dir;

    opendir my($plugins), $dir;

    # blosxom docs say modules ending in _ will not be loaded.
    for my $plugin (grep { /^\w+$/ && !/_$/ && -f File::Spec->join($dir, $_) }
                    sort readdir $plugins) {
        # blosxom docs say you can order modules by prefixing numbers.
        $plugin =~ s/^\d+//;

        # This will blow up if your package name is not the same as your file name.
        require "$dir/$plugin";
        if ($plugin->start()) {
            $self->{active_plugins}->{$plugin} = 1;

            $self->{plugins_ordered} ||= [];
            push @{$self->{plugins_ordered}}, $plugin;
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
#  Return the first value returned by a plugin.

sub _check_plugins {
    my ($self, $method, @underscore) = @_;

    return unless $self->{plugins_ordered};
    return if $self->{no_plugins};

    for my $plugin (@{$self->{plugins_ordered}}) {
        local $self->{no_plugins} = 1;
        my @return;
        @return = $plugin->$method($self, @underscore) if $plugin->can($method);

        return @return if @return;
    }
}

=head1 PLUGINS

Writing plugins for this new version of Blosxom is easy. If you know exactly
what you want from your plugin, you can simply subclass this one and override
the methods below. Alternatively, you can create files in some directory, and
then configure your Blosxom object to use that directory as your plugins
directory.

In order to use a plugin directory, the package name in the plugin file must
be identical to the filename itself. That is because the blosxom engine uses
the filename to know which package to give to C<require>. The only thing that
deviates from this rule is that you can prepend the filename with any number of
digits, and these will be used to load the plugins in order. The order is that
returned by the sort function, so it is recommended all your numbers have the
same number of digits.

In order to disable a plugin, simply alter its C<start> subroutine to return a
false value instead of a true one.

=head2 Starting a plugin

As mentioned, it is necessary that your plugin's filename is the same as the
package defined in the plugin. Please also include a bit of a comment about
what your plugin does, plus author information. The community would appreciate
it if you would use an open licence for your copyrighting, since the community
is built on this attitude. However, the licence you use is, of course, up to 
you.

The smallest possible plugin (comments aside) is shown below, and its filename
would be C<myplugin>.

  ## Blosxom plugin myplugin
  ## Makes blosxom not suck
  ## Author: Altreus
  ## Licence: X11/MIT

  package myplugin;

  sub start{1}

  1;

=head2 Hooks

Every single hook in the plugin will be passed the I<Blosxom> object as the
first argument, effectively running the function as a method on the object
itself. This $self will therefore not be mentioned in the following 
explanations; only any extra parameters will be described.

In all cases, the first plugin that defines a hook is the one that gets to do
it. For this reason you may find that you want to use the method above to
decide the order in which the plugins are loaded.

Also in all cases, except where a true/false value is expected, simply not
returning anything is the way to go about deciding you don't want to alter the
default behaviour. For example, if you wanted to take the date of a post from
the filename, then in the cases where the filename does not define a date, you
can simply C<return;> and processing will continue as if it had not defined 
this functionality in the first place.

Also also in all cases, you can get the return value of the default method by
simply calling $self->method. This is helpful if you want to slightly but not
wildly alter the output, such as adding extra information to the same set of
data. Obviously this is not true of C<start> and C<skip>, since these are not
methods on the class in the first place.

=head3 start

The C<start> subroutine is required in your module and will return either a
true or a false value to decide whether the plugin should be used.

You can use the values on the Blosxom object if you need to make a decision.

  sub start {
      return shift->{static_mode}; # Only active in static mode
  }

=head3 template ($path, $comp, $flavour)

This returns the template to use in the given path for the given component for
the given flavour. The requested filename will not be part of the path.

The default template procedure is to check the given path and all parent
directories of that path, up to the blog root, for the file $comp.$flavour,
and to use the first one found.

Since it is pretty easy to find this file based on just the filename, you'd
think this method has something a bit more difficult to do. In fact this 
function returns the I<content> of that file, ready for interpolation.

This function implements the functionality of both the C<template> hook in the
original blosxom script, as well as the hooks for the individual templates
themselves. That means that if your original plugin defined a new template for,
e.g., the date.html in certain situations, this is where you should now return
that magic HTML.

=head3 entries_for_path ($path)

This returns an array of items. Each item is itself an arrayref, whose first
entry is the filename and whose second entry is a hashref. The hashref is
required to contain the 'date' key, which specifies the date of the file. That
is, the function returns:

    @(
        [
            filename,
            {
                date => [ year, month, day ],
                ...
            }
        ],
        ...
    )

Obviously this is not real Perl syntax, but it is similar, and should get the
point across. The arrayref may contain other things in the third and subsequent
positions, and the hashref may contain other keys, but this much is required.

The filename returned is the path and filename of the entry file relative to
the datadir. The input $path is not necessarily relative to anything, because
it will be the path of the HTTP request. Thus, please note, it may contain the
year, month and day of the requested post(s) and not a path to any real file or
directory at all.

It is worth noting that you can override how Blosxom decides the date of the
file by implementing the date_of_post method instead of this one.

=head3 date_of_post ($post)

The post provided will be the filename and path to the post, relative to the
root of the blog directory, C<< $self->{datadir} >>. You should return an
arrayref where [0] is the 4-digit year, [1] is the 2-digit month, and [2] is
the 2-digit day. This is not checked for validity but will probably cause 
something to blow up somewhere if it is not a real date.

=head3 filter (@entries)

This function does nothing by default and is a hook by which you can 
scrupulously filter out posts one way or another. You are given the output of
the C<entries_for_path> method above, and you should return an array in exactly
the same format, except having removed any entries you do not want to show up on
the generated page.

=head3 sort (@entries)

You can override the default sorting mechanism, which is by date by default,
by implementing this method. It is advisable to empty your date template if
you do this, because the date template is inserted every time the date changes
as processing goes down the list.

A future release may see the date template replaced by a divider template, which
would be configurable and merely default to the date.

=head3 skip

The skip function is a feature from the original blosxom. Setting it to return
a true value will cause the Blosxom object to stop just before anything is
actually output. That is, it will find all the entries and pull in the templates
but not do anything with them if any active plugin makes this function return 
true. This is useful if, e.g., your plugin issues a redirect header.

=head3 interpolate ($template, $extra_data)

This is where you can override the default way in which the variables are
interpolated into the template.

The extra data will be a hashref of var => val, var being the variable name to
interpolate, without its $.

=head3 entry_data ($entry)

This is provided with an arrayref as returned by entries_for_path, and should
return a hashref with the keys as described above, in the method's own
documentation. Briefly, they are 

 title body yr mo mo_name da dw hr min path fn

You may also provide any extra keys that your own templates may want. See above
about how to call the default function and then alter the return value instead
of completely reimplementing it.

=head1 AUTHOR

Altreus, C<< <altreus at perl.org> >>

=head1 TODO

Most existing plugins won't work because I've changed the way it works to the
extent that the same functions don't necessarily exist. However, existing
plugins should be fairly easy to tailor to the new plugin system. I didn't
think this was too important a feature because a good number of the plugins at
the blosxom plugin repository are 404s anyway.

The plugin system is like the original blosxom's, where the first plugin to
define a function is the boss of that function. Some functions may benefit
from the combined effort of all plugins, such as filtering. That is something
to think about in the future.

Static rendering is not yet implemented. Frankly I don't think I can be 
bothered.

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

Thanks to the original author of blosxom.cgi for writing it and giving me code
to do much of the stuff it did.

http://blosxom.com

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
html head <html><head><link rel="alternate" type="application/rss+xml" title="RSS" href="$url/index.rss" /><title>$blog_title $path_info_da $path_info_mo $path_info_yr</title></head><body><h1>$blog_title</h1><p>$path_info_da $path_info_mo $path_info_yr</p>
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
