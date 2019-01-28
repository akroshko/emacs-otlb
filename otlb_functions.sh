#!/bin/bash
# otlb_functions.sh is a set of bash functions to download data off of
# the Garmin 310 fitness watch and the Samsung Galaxy SIII phone using
# the MyTracks applicaton along with an appropriate file transfer
# application.
#
# Old code is also included for the Garmin 305.
#
# Copyright (C) 2015-2018, Andrew Kroshko, all rights reserved.
#
# Author: Andrew Kroshko
# Maintainer: Andrew Kroshko <akroshko.public+devel@gmail.com>
# Created: Fri Mar 27, 2015
# Version: 20190108
# URL: https://github.com/akroshko/emacs-otlb
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or (at
# your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see http://www.gnu.org/licenses/.

################################################################################
# This script has mostly been tested with Debian Linux (Jessie), with
# the packages 'xmlstarlet' and 'python-lxml' being required.
# Additional packages not included in the Debian package manager
# include the packages 'fitparse' and 'antfs-cli' installable using
# the 'pip' Python package management tool.
#
# See the included README.md file for more information.

USAGE="Usage: fetch-garmin-310 [--help] [--reset] [--download] [--process] [--no-process]
  --help       Display this messange and exit.
  --reset      Reset the authorization, required because after some time the
               download stops working if not reset.
  --download   Download files from watch to machine.
  --process    Process the files into form ready for import in otlb.
  --no-process Do not process the files into form ready for import in otlb, even if download successful.
"

export GARMIN310CACHE="$HOME/tmp/cache-garmin-310.txt"
export FITDIRECTORY="$ANTCONFIG/activities"
export OSMCARTODIRECTORY="$HOME/cic-var/openstreetmap-data/openstreetmap-carto"

# TODO: this needs to be a more general function
get-otlb-source () {
    # https://stackoverflow.com/questions/59895/can-a-bash-script-tell-what-directory-its-stored-in
    SOURCE="${BASH_SOURCE[0]}"
    while [[ -h "$SOURCE" ]]; do # resolve $SOURCE until the file is no longer a symlink
        DIRNAMESOURCE=$(dirname "$SOURCE")
        # TODO: is this redundant
        local DIR=$(cd -P "$DIRNAMESOURCE" && pwd)
        local SOURCE=$(readlink "$SOURCE")
        [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE" # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
    done
    local DIR=$(cd -P "$( dirname "$SOURCE" )" && pwd)
    echo $DIR
}

# TODO: several years takes about 9 minutes, I cron every three hours,
#       this still may be too costly, maybe have a full cron every day
#       and update based on missing
cache-garmin-310-full () {
    local OTLBSOURCE=$(get-otlb-source)
    local THEIDS=$(python "$OTLBSOURCE"/read_files.py "$FITDIRECTORY" --fit-id)
    mkdir -p "$HOME/tmp"
    echo "$THEIDS" > "$GARMIN310CACHE"
}

# TODO: this whole cache thing should be in python
# TODO: think this is non-function right now
cache-garmin-310-add () {
    # add missing files to cache
    if [[ -e "$GARMIN310CACHE" ]];then
        local OTLBSOURCE=$(get-otlb-source)
        # find things that are missing or have change since last one and repocess
        # check for files in $FITDIRECTORY that are not in cacheids
        local FITFILES=($(readlink -f "$FITDIRECTORY")/*.fit)
        local ORPHANS=()
        local OLDIFS=$IFS
        IFS=$'\n'
        for ((i=0; i<${#FITFILES[@]}; i++)); do
            # do something to each element of array
            # check if each fitfile is in cache, make this as easy as possible
            # TODO: should already be normalized
            FITFILE="${FITFILES[$i]}"
            # TODO: fix up this grep so I check for something valid
            if grep -- "${FITFILES[$i]}" "$GARMIN310CACHE" &>/dev/null; then
                true
                # msg "Found ${FITFILES[$i]}!!!"
            else
                ORPHANS+=("${FITFILES[$i]}")
                warn "Not found for ${FITFILES[$i]}!!!"
            fi
        done
        for ((i=0; i<${#ORPHANS[@]}; i++)); do
            local DATESTAMP=$(python "$OTLBSOURCE"/read_files.py --fit-id "${ORPHANS[$i]}")
            echo "$DATESTAMP" >> "$GARMIN310CACHE"
        done
        IFS=$OLDIFS
    else
        yell "Garmin 310 cache not present, rebuild whole thing!"
    fi
}

fetch-garmin-310-reset-download () {
    fetch-garmin-310 --reset
    sleep 1
    fetch-garmin-310 --download
}

# FITDIRECTORY="$ANTCONFIG"/activities;OTLBSOURCE="$(get-otlb-source)";time python "$OTLBSOURCE"/read_files.py "$FITDIRECTORY" --fit-id
# TODO: cache if not exist... or if stale or corrupt...
fetch-garmin-310 () {
    time {
        if [[ $@ == *"--dry-run"* ]]; then
            local DRYRUN="T"
        else
            local DRYRUN=""
        fi
        # Fetch data from a GARMIN-310, could be easily modified to fetch
        # data from other devices compatible with the 'antfs-cli' package.
        # set up the specific directories based on the confiuration
        # TODO: outdated name
        local TCXDIRECTORY="$OTLBLOGS"/"$DEVICENAME"
        local TCXDIRECTORYOLDER="$OTLBLOGSOLDER"/"$DEVICENAME"
        # there's probably a better way of dealing with arguments
        if [[ -z "$1" || "$1" == "--help" ]]; then
            echo "$USAGE"
            return 0
        fi
        h2
        # XXXX this seems to be necessary about once a day, but hangs if I do it every time
        if [[ $@ == *"--reset"* ]]; then
            [[ -e "$ANTCONFIG"/authfile ]] && rm "$ANTCONFIG"/authfile
            return 0
        fi
        if [[ $@ == *"--download"* ]]; then
            # currently used with my Garmin 310
            antfs-cli
            local DOWNLOAD_SUCCESSFUL=$?
        fi
        # check if downloaded files are already in intermediate directory
        if [[ ! $@ == *"--no-process"* ]] && [[ $@ == *"--process"* || "$DOWNLOAD_SUCCESSFUL" == 0 ]]; then
            if [[ "$DOWNLOAD_SUCCESSFUL" == 0 ]]; then
                echo "Download successful!!!"
            fi
            local OTLBSOURCE=$(get-otlb-source)
            pushd . >/dev/null
            cd "$OTLBLOGS"
            h2
            echo "Scanning .fit directory..."
            time {
                # TODO: get the cached files
                local THECACHEDFITS=$(<"$GARMIN310CACHE")
                local THECACHEDFILES=""
                # build up a list of which .fit files have cached ids to match
                local OLDIFS=$IFS
                IFS=$'\n'
                for THEFIT in $THECACHEDFITS; do
                    # TODO: command and append at each loop stupidly inefficient, but at only 3 seconds....
                    local CACHEDFILE=$(cut -d' ' -f1 <<< "$THEFIT")
                    THECACHEDFILES="$THECACHEDFILES $CACHEDFILE"
                done
                IFS=$OLDIFS
                # echo "$THECACHEDFITS"
                echo "Processed cached ids! The new .fit files aren't close to ready yet!"
            }
            local THEMISSINGFITS=""
            h2
            echo "Finding the full ids to process..."
            time {
                for THEFITFILE in $FITDIRECTORY/*.fit;do
                    THEFITFILENORMALIZED=$(readlink -f $THEFITFILE)
                    if [[ ! "$THECACHEDFILES" =~ $THEFITFILENORMALIZED ]]; then
                        # read file....
                        echo "Read: $THEFITFILE"
                        local THEMISSINGID=$(python "$OTLBSOURCE"/read_files.py --fit-id "$THEFITFILE")
                        # echo "$THENEWID"
                        THEMISSINGFITS="${THEMISSINGFITS}"$'\n'"${THEMISSINGID}"
                    fi
                done
                echo "Found full ids to process! The new .fit files still aren't ready yet!"
            }
            h2
            echo "Copying the fit files..."
            time {
                # TODO: can only use subshell here because I am not
                (IFS=$'\n'
                 for THEMISSINGFIT in $THEMISSINGFITS; do
                     local THEORIGINALFIT=$(cut -d' ' -f1 <<< "$THEMISSINGFIT")
                     local THEID=$(cut -d' ' -f2 <<< "$THEMISSINGFIT")
                     if [[ ! -e "${TCXDIRECTORY}/${THEID}.tcx" && ! -e "${TCXDIRECTORY}/${THEID}.fit" && ! -e "${TCXDIRECTORYOLDER}/${THEID}.tcx" && ! -e "${TCXDIRECTORYOLDER}/${THEID}.fit" ]]; then
                         echo "Missing $THEID! Copying!"
                         if [[ ! -e "${THEORIGINALFIT}" ]]; then
                             echo "Cannot copy ${THEORIGINALFIT}! Not found!"
                         else
                             if [[ -z $DRYRUN ]]; then
                                 cp "${THEORIGINALFIT}" "${TCXDIRECTORY}/${THEID}.fit"
                             else
                                 echo "Dry run: cp ${THEORIGINALFIT} ${TCXDIRECTORY}/${THEID}.fit"
                             fi
                         fi
                     fi
                 done)
                echo "The new .fit files are finally processed and ready!"
            }
            h2
            echo "Updating cache!"
            cache-garmin-310-add
            h2
            if [[ -z $DRYRUN ]]; then
                echo "Creating osm maps"
                cd "${TCXDIRECTORY}"
                create-osm-maps
            else
                echo "Dry run! Not creating maps!"
            fi
            popd >/dev/null
        fi
    }
}

convert-aux-devices () {
    # convert files deposited in the aux incoming directory
    # TODO: document and add help here
    for f in $OTLBAUX_INCOMING/*; do
        if [[ ${f##*.} == "tcx" ]]; then
            if [[ ! -e "$OTLBAUX"/$(basename $f) ]]; then
                # TODO: in case of failure
                local THEID=$(get-id-tcx $f) || continue
                cp "$f" "$OTLBAUX"/"$THEID".tcx
            fi
        elif [[ ${f##*.} == "gpx" ]]; then
            if [[ ! -e "$OTLBAUX"/$(basename $f) ]]; then
                # TODO: in case of failure
                local THEID=$(get-id-gpx $f) || continue
                cp "$f" "$OTLBAUX"/"$THEID".gpx
            fi
        fi
    done
}

get-id-fit () {
    local OTLBSOURCE=$(get-otlb-source)
    local XMLID=$(python "$OTLBSOURCE"/read_files.py --fit-id "$1")
    echo $XMLID
}

# TODO: deal with corrupt files
get-id-tcx () {
    # get the id for a tcx file
    # TODO: should I get rid of the xmlstarlet and just use Python for everything
    local OTLBSOURCE=$(get-otlb-source)
    # get a timestamp from first track
    local XMLID=$(xmlstarlet sel -t -v "//*[local-name() = 'Id']" -n "$1")
    if [[ -z "$XMLID" ]]; then
        return 1
    fi
    local XMLID=${XMLID//-/}
    local XMLID=${XMLID//:/}
    echo $(python "$OTLBSOURCE"/read_files.py "$XMLID" --utc)
    # rename for import
    # TODO: convert time zone properly, just add 6 hours for now
}

get-id-gpx () {
    # get the id for a gpx file
    # TODO: should I get rid of the xmlstarlet and just use Python for everything
    local OTLBSOURCE=$(get-otlb-source)
    # get a timestamp from first track
    local XMLID=$(xmlstarlet sel -t -v "//*[local-name() = 'metadata']/*[local-name() = 'time']" -n "$1")
    local XMLID=${XMLID//-/}
    local XMLID=${XMLID//:/}
    echo $(python "$OTLBSOURCE"/read_files.py "$XMLID" --utc)
    # rename for import
}

create-osm-maps () {
    # meant to run in current directory
    for f in ${PWD}/*; do
        # yes, avoid existing ones
        [[ "$f" =~ -1280.png ]] && continue
        if [[ ! -e ${f%%.*}.png ]]; then
            echo "Creating map for $f"
            create-osm-map "$f"
        fi
        # create a smaller image for previewing
        # TODO: have way to redo this
        if [[ ! -e ${f%%.*}-1280.png ]]; then
            echo "Creating x1280 map for $f"
            convert -geometry 1280x\> ${f%%.*}.png ${f%%.*}-1280.png
        fi
    done
}

create-osm-map () {
    local OTLBSOURCE=$(get-otlb-source)
    local ROUTEFILE="/tmp/otlb-gps-temp.gpx"
    # TODO: create a tmp working directory
    # TODO: needs a lost of work to avoid ~/osm silliness
    # TMPDIR=$(mktemp -d)
    # get the id
    if [[ ${1##*.} == "fit" ]]; then
        # TODO: not working yet
        local THEID=$(get-id-fit "$1" | cut -d' ' -f2)
    elif [[ ${1##*.} == "gpx" ]]; then
        local THEID=$(get-id-gpx "$1")
    else
        yell "No ID can be diserned!"
        return 1
    fi
    # TODO: safety
    [[ -e "$ROUTEFILE" ]] && rm "$ROUTEFILE"
    # convert to gpx if not already gpx, otherwise just copy to tmp directory
    # TODO: handle /temp files better
    if [[ ${1##*.} == "fit" ]]; then
        gpsbabel -i garmin_fit -f "$1" -o gpx -F "$ROUTEFILE"
    elif [[ ${1##*.} == "tcx" ]]; then
        gpsbabel -i tcx -f "$1" -o gpx -F "$ROUTEFILE"
    elif [[ ${1##*.} == "gpx" ]]; then
        cp "$1" "$ROUTEFILE"
    else
        yell "No appropriate file found for osm map!"
        return 1
    fi
    # parse the raw osm file, add the appropriate route file, and add to the directory
    # put the output file where it belongs
    # TODO: eventually just merge xml's automatically
    cp "$OTLBSOURCE/osm-route.xml" "${OSMCARTODIRECTORY}"/osm-route.xml
    # --add-layers route-line,route-points
    echo nik4.py --fit route-line --padding 100 -z 17 "${OSMCARTODIRECTORY}/osm-route.xml" "$(dirname $1)/${THEID}.png" --vars routefile="$ROUTEFILE"
    nik4.py --fit route-line --padding 100 -z 17 "${OSMCARTODIRECTORY}/osm-route.xml" "$(dirname $1)/${THEID}.png" --vars routefile="$ROUTEFILE"
}


################################################################################
## Old code that may prove useful Not documented and requires the
## package 'garmin-forerunner-tools'.

# these placeholders mess up my Emacs
# OWNERGROUP="<<owner>>:<<group>>"
fetch-garmin-305 () {
    # Only here for historical purposes, my Garmin 305 broke after 4
    # good years of service. Therefore, this command is disabled!
    # Requires the package from
    # https://code.google.com/p/garmintools/.
    return 1
    pushd . >/dev/null
    cd "$OTLBLOGS"
    # this is me, because the command required root
    sudo chown -R "$OWNERGROUP" garmin-305
    cd garmin-305
    sudo garmin_save_runs
    # XXXX oh boy.... assuming garmin names have NO whitespace
    for f in $(find . -iname "*.gmn"); do
        [[ -e "$f" ]] || continue
        [[ ! -f ${f%.gmn}.tcx ]] && gmn2tcx "$f" > ${f%.gmn}.tcx
    done
    popd >/dev/null
}

# old code, mytracks is gone, here as a useful reference only.
# TODO: better naming of SAMSUNG_DEVICE
ANDROID_INTERMEDIATEDIRECTORY="$OTLBLOGS"/"$SAMSUNG_DEVICENAME"-intermediate
ANDROID_DIRECTORY="$OTLBLOGS"/"$SAMSUNG_DEVICENAME"

convert-android-mytracks () {
    # just convert the IDs for now, these are loaded directly in as
    # .tcx files
    #
    # XXXX: I use this as a backup device, therefore not all tracks
    # are copied automatically: export in TCX
    #
    # TODO: copy and select from appropriate directory (or maybe over
    # SSH?), have some way to summarize
    for f in $SAMSUNG_INTERMEDIATEDIRECTORY/*; do
        if [[ ${f##*.} == "tcx" ]]; then
            # this only supports a very specific file format
            local XMLIDFILE=$(basename "$f")
            local XMLIDFILE="${XMLIDFILE// /T}"
            local XMLIDFILE="${XMLIDFILE//:/}"
            local XMLIDFILE="${XMLIDFILE//_/}"
            local XMLIDFILE="${XMLIDFILE//-/}"
            local XMLIDFILE="${XMLIDFILE//./00.}"
            # TODO: possibly flag and convert if different sizes/hashes?
            if [[ ! -e "${SAMSUNG_DIRECTORY}/${XMLIDFILE}" ]]; then
                msg "Converting $f to ${SAMSUNG_DIRECTORY}/${XMLIDFILE}!"
                cp "$f" "${SAMSUNG_DIRECTORY}/${XMLIDFILE}"
            fi
        fi
    done
}
