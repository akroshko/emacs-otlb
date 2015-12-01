otlb: org-table Logbook
=======================

This package uses Emacs `org-mode` tables as an activity logbook,
which incorporates data that has been downloaded from selected GPS
devices (currently a Garmin 310 fitness watch and a Samsung Galaxy III
phone).

Features include the ability to automatically track weather conditions
and mileage on footwear, as well as being able to edit the activity
data as `org-mode` tables while still retaining the original copy.
This package could be extended to other devices, as well as allowing
the recorded data and information entered to be used for custom
purposes by Emacs-lisp scripting.

INFO: In the future I hope to include some sample data, but I decided
I wanted to release the package as-is as soon as possible.  Once I can
gather an adequate set of sample-data that does not include any
privacy-violating information I will include it.

Requirements and Installation
=============================

This package requires a standard Emacs installation, of course. It has
mostly been tested with the current Emacs package on *Debian Linux
(Jessie) 8.2*. The packages required, given by a convenient install
command are:

    sudo apt-get install git python-lxml xmlstarlet

which are required to support the included *bash* and *Python*
scripts.

The associated library from
[http://github.com/akroshko/emacs-stdlib](http://github.com/akroshko/emacs-stdlib)
is the only additional Emacs requirement, which can be installed using
the command:

    git clone https://github.com/akroshko/emacs-stdlib.git ../emacs-stdlib

in the directory for this package, which satisfies the default in the
sample configuration file.

The `git` version control system is also an optional requirement,
which helps with installation of the Python tools for working with GPS
devices and data.  The *Python* packages required can be installed
directory from https://github.com using the commands:

    pip install git+https://github.com/Tigge/antfs-cli.git
    pip install git+https://github.com/Tigge/FIT-to-TCX.git
    pip install git+https://github.com/Tigge/openant.git
    pip install git+https://github.com/dtcooper/python-fitparse.git

WARNING: I haven't verified these commands on a fresh install.

It has been found anything but the latest versions of the above
*Python* packages do not work well.

The GPS devices used so far are a Garmin 310XT and the Samsung Galaxy
SIII smartphone with the
[My Tracks](https://play.google.com/store/apps/details?id=com.google.android.maps.mytracks)
application.

##Configuration

To use the provided *bash* functions to download and process data from
the devices, source the
[otlb-sample-config.sh](http://github.com/akroshko/emacs-otlb/otlb-sample-config.sh)
and
[otlb-sample-init.sh](http://github.com/akroshko/emacs-otlb/otlb-sample-init.sh)
files with:

    source otlb-sample-config.sh otlb-sample-init.sh

The environment variables that need to be set are:

    export OTLBLOGS="<<otlb log location>>"                        # the location to store the logfiles
    export ANTID="<<ant device id>>"                               # the ID number of the ant device
    export ANTCONFIG="$OTLBLOGS"/antfs-cli/"$ANTID"                # the location to store the antfs-cli files
    export ANTDEVICENAME="garmin-310-$ANTID"                       # value for the Garmin 310
    export OTLBPYTHONPATH=/usr/local/lib/python2.7/site-packages/  # value on my machine

The directory given by `$ANTCONFIG` must be symlinked to
`~/.config/antfs-cli` using the command:

    ln -s ~/.config/antfs-cli $ANTCONFIG

I keep `$OTLBLOGS` directory in my [Dropbox](https://www.dropbox.com)
and have had no issues with using this package on both my desktop and
laptop.  The appropriate value for the `$ANTID` variable can be found
by the name of the new sub-directory in `$ANTCONFIG` corresponding to
a 10 digit number after `antfs-cli` is run for the first time.  The
`$ANTDEVICENAME` variable simply corresponds to a convenient unique
name for the particular device.

Downloading data
================

The `fetch-garmin-310` script just displays a help message if run
without arguments.  To download data from a Garmin 310 watch, run it
as:

    fetch-garmin-310 --download

The downloaded `.fit` files downloaded from the Garmin 310 are stored
in the `$OTLBLOGS/org-logs/antfs-cli/` directory.  In order to process
the data run it as:

    fetch-garmin-310 --process

The function `fetch-garmin-310` from
[otlb-functions.sh](http://github.com/akroshko/emacs-otlb/otlb-functions.sh)
converts the aforementioned `.fit` files into intermediate `.tcx`
files in `$OTLBLOGS/garmin-310-$ANTID-intermediate/`, which are then
amenable to further processing as XML files.  Using the date/time
information contained within these intermediate files, they are
renamed into a standard notation in the directory
`$OTLBLOGS/garmin-310-$ANTID` for use from the Emacs part of this
package.  The standard notation for the files, which comes from an old
program used for the Garmin 305, is given by `<<YYYYMMDD>>T<<HHMMSS>>`
and known as the *otlb id*; corresponding to the starting time for
each activity and used to uniquely identify it.

My Garmin 310 watch requires deleting the authorization key every day
or two and this is done with:

    fetch-garmin-310 --reset

Using otlb in Emacs
===================

After following the basic installation, run Emacs with `emacs -q
--load otlb-sample-init.el` to use the sample configuration.

The `otlb-gps-find-pedestrian-location` (`s-j l`) key sequence is a
shortcut to visit the file `$OTLBLOGS/pedestrian-log.org` in the
current buffer.  While visiting this file in Emacs, the key sequence
`otlb-gps-insert` (`s-l i`) will allow selection of a file from the
primary device (the Garmin 310XT in this case), whose data has not
already been inserted into `$OTLBLOGS/pedestrian-log.org`, by the
*otlb id*.  The default selection is always the most recent `.tcx`
file in `$OTLBLOGS/garmin-310-$ANTID` that meets this criteria.

During the insertion process, the footwear can be selected from the
file `$OTLBLOGS/footwear-current.org`, generally based on a unique ID
and a coloured tag (on the footwear itself to distinguish identical
footwear).  Retired footwear can be moved into the `org-mode` table in
`$OTLBLOGS/footwear-retired.org`, after which it can no longer be
selected, thus allowing the colour of the tag to be used for new
footwear.

TODO: image of tag

If not specified at the time of insertion, the footwear for the
current `org-mode` table in `$OTLBLOGS/pedestrian-log.org` can be
inserted with the key sequence `command otlb-gps-footwear` (`s-l w`).

The `org-mode` tag indicating the sport can be toggled using the key
sequence `otlb-gps-toggle` (`s-l t`).

Weather conditions are added to the last entry by using the key
sequence `otlb-gps-insert-conditions` (`s-l c`).  This currently looks
up the temperature and wind based on Environment Canada data (by
scraping the website using the script
[scrape_weather_ec.py](http://github.com/akroshko/emacs-otlb/scrape_weather_ec.py)),
although it could easily be modified for other websites.

Using the key sequence `otlb-gps-insert` (`C-u s-l i`) with a prefix
will allow selection of an uninserted file from the secondary device
(the MyTracks application on a Samsung Galaxy SIII in this case),
after which the procedure is the same as for the primary device.

TODO: Not working yet.

Unrecorded activities can be inserted using the key sequence
`otlb-gps-insert-unrecorded` (`s-l u`), however, the data values must
be estimated and filled in manually.

Miscellaneous activities and notes not associated with mileage covered
can be inserted using the key sequence `otlb-gps-insert-miscellaneous`
(`s-l m`).

TODO: Not working yet.

Due to having multiple drawers that should normally be closed in the
file
[org-logs/pedestrian-log.org](http://github.com/akroshko/emacs-otlb/org-logs/pedestrian-log.org);
in order to keep the drawers closed in most cases, the key sequence
`otlb-gps-cycle` (`s-l l`) opens only headings as a substitute for
(`C-u C-u C-u <Tab>`) and the key sequence `otlb-gps-cycle` (`s-L L`),
corresponding to three prefixes, closes everything like (`C-u C-u C-u
<Shift-Tab>`).

Entries are added to the top of `$OTLBLOGS/pedestrian-log.org`.  To
ensure the logbook is in chronological order with most recent at the
top, the key sequence `otlb-gps-sort` (`s-l s`) reverse sorts the
`org-mode` table entries by date and time.

A running total of the number of kilometres run in previous week back
from each day, going back for a total of 4 weeks, can be plotted using
the key sequence `otlb-gps-plot` (`s-l p`).  Weekly mileage totals
(Sunday-to-Saturday) back 26 weeks can be plotted using the key
sequence `otlb-gps-plot` (`C-u s-l p`).

TODO: describe where raw data can be found

The key sequence `otlb-gps-recalculate-all` (`s-l *`) automatically
recalculates all tables, however, it shouldn't have to be used that
often.

TODO: Talk about sample data here.

### Screenshots

TODO: no screenshots yet

Planned Development
===================

- make sure data from past logs is incorporated where appropriate

- incorporate data better from handheld GPSs

- possibly read `.fit` files directly

- examine how to effectively edit and use incomplete data

- better handling of how good/bad data is incorporated

- better and quicker plotting on maps (a preview window would be nice)

- allow attachments for particular entry (photos, datafiles, etc.)

- how to incorporate other kinds of training?

- better way of selecting interesting data, rather than elisp
  expressions

- export/integrate with other software, e.g., SQL, R, SciPy, etc.

- better handling to get appropriate summary of retired footwear

## Bugs and other defects

- the code (especially some of the XML and data selection code) is
very hacky right now

- read XML better and clean up the associated file, i.e., use
something like XPath rather than `xml-parse-file` from elisp

- need to deal consistently with corner cases, e.g., activities
crossing midnight Sunday or similar situations

- need better de-duplication of downloaded data

- handle imperial and metric in an appropriate way

- examine algorithms, use something better than bubble sort and have a
  quicker select of logbook entries

- is my wide usage of `list`, `elt`, and other functions hurting
  anything?

- can I reduce some of the hardcoding while not unnecessarily
  increasing code complexity?
