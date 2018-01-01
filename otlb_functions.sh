#!/bin/bash
# otlb_functions.sh is a set of bash functions to download data off of
# the Garmin 310 fitness watch and the Samsung Galaxy SIII phone using
# the MyTracks applicaton along with an appropriate file transfer
# application.
#
# Old code is also included for the Garmin 305.
#
# Copyright (C) 2015-2016, Andrew Kroshko, all rights reserved.
#
# Author: Andrew Kroshko
# Maintainer: Andrew Kroshko <akroshko.public+devel@gmail.com>
# Created: Fri Mar 27, 2015
# Version: 20170928
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

USAGE="Usage: fetch-garmin-310 [--reset] [--download] [--process]
  --reset     Reset the authorization, required because after some time the
              download stops working if not reset.
  --download  Download files from watch to machine.
  --process   Process the files into form ready for import in otlb."

get-otlb-source () {
    # https://stackoverflow.com/questions/59895/can-a-bash-script-tell-what-directory-its-stored-in
    SOURCE="${BASH_SOURCE[0]}"
    while [ -h "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
        local DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
        local SOURCE="$(readlink "$SOURCE")"
        [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE" # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
    done
    local DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
    echo $DIR
}

# TODO: several years takes about 9 minutes, I cron every three hours,
#       this still may be too costly, maybe have a full cron every day
#       and update based on missing
cron-cache-garmin-310 () {
    FITDIRECTORY="$ANTCONFIG"/activities
    OTLBSOURCE="$(get-otlb-source)"
    THEIDS=$(python "$OTLBSOURCE"/read_files.py "$FITDIRECTORY" --fit-id)
    mkdir -p "$HOME"/tmp
    echo "$THEIDS" > "$HOME"/tmp/cache-garmin-310.txt
}

fetch-garmin-310-reset-download () {
    fetch-garmin-310 --reset
    sleep 1
    fetch-garmin-310 --download
}

# testing
# FITDIRECTORY="$ANTCONFIG"/activities;OTLBSOURCE="$(get-otlb-source)";time python "$OTLBSOURCE"/read_files.py "$FITDIRECTORY" --fit-id
# TODO: cache if not exist... or if stale or corrupt...
fetch-garmin-310 () {
    h2
    time {
        if [[ $@ == *"--dry-run"* ]]; then
            local DRYRUN="T"
        else
            local DRYRUN=""
        fi
        # Fetch data from a GARMIN-310, could be easily modified to fetch
        # data from other devices compatible with the 'antfs-cli' package.
        # set up the specific directories based on the confiuration
        # TODO: no intermediate directory anymore
        # local INTERMEDIATEDIRECTORY="$OTLBLOGS"/"$DEVICENAME"-intermediate
        local FITDIRECTORY="$ANTCONFIG"/activities
        # TODO: outdated name
        local TCXDIRECTORY="$OTLBLOGS"/"$DEVICENAME"
        local TCXDIRECTORYOLDER="$OTLBLOGSOLDER"/"$DEVICENAME"
        # there's probably a better way of dealing with arguments
        if [[ -z "$1" ]]; then
            echo "$USAGE"
            return 0;
        fi
        # XXXX this is good to do if antfs-cli is hanging, can probably be
        # deleted at some point
        if [[ $@ == *"--reset"* ]]; then
            rm "$ANTCONFIG"/authfile
            return 0
        fi
        if [[ $@ == *"--download"* ]]; then
            # currently used with my Garmin 310
            antfs-cli
            local DOWNLOAD_SUCCESSFUL=$?
        fi
        # check if downloaded files are already in intermediate directory
        if [[ $@ == *"--process"* || "$DOWNLOAD_SUCCESSFUL" == 0 ]]; then
            if [[ "$DOWNLOAD_SUCCESSFUL" == 0 ]]; then
                echo "Download successful!!!"
            fi
            local OTLBSOURCE="$(get-otlb-source)"
            pushd . >/dev/null
            cd "$OTLBLOGS"
            h2
            echo "Scanning .fit directory..."
            time {

                # TODO: load files....
                # TODO: get the file list and read files that are not in the cache
                # TODO: add THEIDS that are not in the cache
                # TODO: get the cached files
                local THECACHEDIDS=$(<"$HOME"/tmp/cache-garmin-310.txt)
                local THECACHEDFILES=""
                local OLDIFS=$IFS
                IFS=$'\n'
                for THEID in $THECACHEDIDS; do
                    # TODO: command and append stupidly inefficient, but at only 3 seconds....
                    XMLFILE=$(echo "$THEID" | cut -d' ' -f1)
                    THECACHEDFILES="$THECACHEDFILES $XMLFILE"
                done
                IFS=$OLDIFS
                # echo "$THECACHEDIDS"
                echo "Processed cached ids! The new .fit files aren't close to ready yet!"
            }
            # TODO: have a cache?
            # THEIDS=$(python "$OTLBSOURCE"/read_files.py "$FITDIRECTORY" --fit-id)
            # TODO: potential issue with doubled slashes...
            h2
            echo "Finding the full ids to process..."
            time {
                for THEFILE in $FITDIRECTORY/*.fit;do
                    if [[ ! "$THECACHEDFILES" =~ $THEFILE ]]; then
                        # read file....
                        echo "Read: $THEFILE"
                        local THENEWID=$(python "$OTLBSOURCE"/read_files.py "$THEFILE" --fit-id)
                        # echo "$THENEWID"
                        local THECACHEDIDS="${THECACHEDIDS}"$'\n'"${THENEWID}"
                    fi
                done
            local THEIDS="${THECACHEDIDS}"
            echo "Found full ids to process! The new .fit files still aren't ready yet!"
            }
            # echo "${THEIDS}"
            h2
            echo "Copying the fit files..."
            time {
                (IFS=$'\n'
                 for THEID in $THEIDS; do
                     local XMLFILE=$(echo "$THEID" | cut -d' ' -f1)
                     local XMLID=$(echo "$THEID"   | cut -d' ' -f2)
                     if [[ ! -e "${TCXDIRECTORY}/${XMLID}.tcx" && ! -e "${TCXDIRECTORY}/${XMLID}.fit" && ! -e "${TCXDIRECTORYOLDER}/${XMLID}.tcx" && ! -e "${TCXDIRECTORYOLDER}/${XMLID}.fit" ]]; then
                         echo "Missing $XMLID! Copying!"
                         if [[ ! -e "${XMLFILE}" ]]; then
                             echo "Cannot copy ${XMLFILE}! Not found!"
                         else
                             if [[ -z $DRYRUN ]]; then
                                 cp "${XMLFILE}" "${TCXDIRECTORY}/${XMLID}.fit"
                             else
                                 echo "Dry run: cp ${XMLFILE} ${TCXDIRECTORY}/${XMLID}.fit"
                             fi
                         fi
                     fi
                 done)
                echo "The new .fit files are finally processed and ready!"
            }
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
                local THEID="$(get-id-tcx $f)" || continue
                cp "$f" "$OTLBAUX"/"$THEID".tcx
            fi
        elif [[ ${f##*.} == "gpx" ]]; then
            if [[ ! -e "$OTLBAUX"/$(basename $f) ]]; then
                # TODO: in case of failure
                local THEID="$(get-id-gpx $f)" || continue
                cp "$f" "$OTLBAUX"/"$THEID".gpx
            fi
        fi
    done
}

get-id-fit () {
    local OTLBSOURCE="$(get-otlb-source)"
    local XMLID=$(python "$OTLBSOURCE"/read_files.py "$1" --fit-id)
    echo $XMLID
}

# TODO: deal with corrupt files
get-id-tcx () {
    # get the id for a tcx file
    # TODO: should I get rid of the xmlstarlet and just use Python for everything
    local OTLBSOURCE="$(get-otlb-source)"
    # get a timestamp from first track
    local XMLID=`xmlstarlet sel -t -v "//*[local-name() = 'Id']" -n "$1"`
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
    local OTLBSOURCE="$(get-otlb-source)"
    # get a timestamp from first track
    local XMLID=`xmlstarlet sel -t -v "//*[local-name() = 'metadata']/*[local-name() = 'time']" -n "$1"`
    local XMLID=${XMLID//-/}
    local XMLID=${XMLID//:/}
    echo $(python "$OTLBSOURCE"/read_files.py "$XMLID" --utc)
    # rename for import
}

create-osm-maps () {
    # meant to run in current directory
    for f in ${PWD}/*; do
        # yes, avoid existing ones
        if [[ "${f}" =~ -1280.png ]]; then
            continue
        fi
        if [[ ! -e ${f%%.*}.png ]]; then
            echo "Creating map for $f"
            create-osm-map "$f"
        fi
        # create a smaller image for previewing
        if [[ ! -e ${f%%.*}-1280.png ]]; then
            echo "Creating x1280 map for $f"
            convert -geometry 1280x\> ${f%%.*}.png ${f%%.*}-1280.png
        fi
    done
}

create-osm-map () {
    local OTLBSOURCE="$(get-otlb-source)"
    # TODO: create a tmp working directory
    # TODO: needs a lost of work to avoid ~/osm silliness
    # TMPDIR=$(mktemp -d)
    # get the id
    if [[ ${1##*.} == "fit" ]]; then
        # TODO: not working yet
        local THEID="$(get-id-fit $1 | cut -d' ' -f2)"
    elif [[ ${1##*.} == "gpx" ]]; then
        local THEID="$(get-id-gpx $1)"
    else
        return 1
    fi
    if [[ -e "/tmp/otlb-gps-temp.gpx" ]]; then
        rm "/tmp/otlb-gps-temp.gpx"
    fi
    # convert to gpx if not already gpx, otherwise just copy to tmp directory
    if [[ ${1##*.} == "fit" ]]; then
        gpsbabel -i garmin_fit -f "${1}" -o gpx -F "/tmp/otlb-gps-temp.gpx"
    elif [[ ${1##*.} == "tcx" ]]; then
        gpsbabel -i tcx -f "${1}" -o gpx -F "/tmp/otlb-gps-temp.gpx"
    else
        cp "${1}" "/tmp/otlb-gps-temp.gpx"
    fi
    # parse the raw osm file, add the appropriate route file, and add to the directory
    # put the output file where it belongs
    # TODO: eventually just merge xml's
    cp "$OTLBSOURCE/osm-route.xml" ~/osm/openstreetmap-carto/osm-route.xml
    nik4.py --fit route-line --add-layers route-line,route-points --padding 100 -z 17 ~/osm/openstreetmap-carto/osm-route.xml "$(dirname $1)/${THEID}.png"
}


################################################################################
## Old code that may prove useful Not documented and requires the
## package 'garmin-forerunner-tools'.

OWNERGROUP="<<owner>>:<<group>>"
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
        if [[ ! -f ${f%.gmn}.tcx ]]; then
            gmn2tcx "$f" > ${f%.gmn}.tcx
        fi
    done
    popd >/dev/null
}

# old code, mytracks is gone, here as a useful reference only.
# TODO: better naming of SAMSUNG_DEVICE
SAMSUNG_INTERMEDIATEDIRECTORY="$OTLBLOGS"/"$SAMSUNG_DEVICENAME"-intermediate
SAMSUNG_DIRECTORY="$OTLBLOGS"/"$SAMSUNG_DEVICENAME"

convert-samsung-mytracks () {
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
            local XMLIDFILE=$(basename "${f}")
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
