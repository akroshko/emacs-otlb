#!/bin/bash
# otlb_functions.sh is a set of bash functions to download data off of
# the Garmin 310 fitness watch and the Samsung Galaxy SIII phone using
# the MyTracks applicaton along with an appropriate file transfer
# application.
#
# Old code is also included for the Garmin 305.
#
# Copyright (C) 2015-2016 Andrew Kroshko, all rights reserved.
#
# Author: Andrew Kroshko
# Maintainer: Andrew Kroshko <akroshko.public+devel@gmail.com>
# Created: Fri Mar 27, 2015
# Version: 20151201
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
# the 'pip' Python package management tool and the package
# 'FIT-TO-TCX' from https://github.com/Tigge/FIT-to-TCX that must be
# installed manually.
#
# See the included README.md file for more information.

USAGE="Usage: fetch-garmin-310 [--reset] [--download] [--process]
  --reset     Reset the authorization, required because after some time the
              download stops working if not reset.
  --download  Download files from watch to machine.
  --process   Process the files into form ready for import in otlb."

fetch-garmin-310 () {
    # Fetch data from a GARMIN-310, could be easily modified to fetch
    # data from other devices compatible with the 'antfs-cli' package.

    # set up the specific directories based on the confiuration
    local INTERMEDIATEDIRECTORY="$OTLBLOGS"/"$DEVICENAME"-intermediate
    local FITDIRECTORY="$ANTCONFIG"/activities
    local TCXDIRECTORY="$OTLBLOGS"/"$DEVICENAME"
    # there's probably a better way of dealing with arguments
    if [[ -z "$1" ]]; then
        echo "$USAGE"
        return 0;
    fi
    # XXXX this is good to do if antfs-cli is hanging, can probably be
    # deleted at some point
    if [[ "$1" == "--reset" ]]; then
        rm "$ANTCONFIG"/authfile
        return 0
    fi
    if [[ "$1" == "--download" ]]; then
        # currently used with my Garmin 310
        antfs-cli
        return 0
    fi
    # loop over downloaded files to see if they are already in intermediate directory
    if [[ "$1" == "--process" ]]; then
        pushd . >> /dev/null
        cd "$OTLBLOGS"
        for f in $FITDIRECTORY/*; do
            # TODO: delete corrupted files to avoid nonsense
            local GPSNAME=$(basename -s .fit "$f")
            if [[ ! -e "${INTERMEDIATEDIRECTORY}/${GPSNAME}.tcx" ]]; then
                msg "Converting $f to intermediate file ${INTERMEDIATEDIRECTORY}/${GPSNAME}.tcx!"
                # TODO: how to check and delete corrupted .fit files
                PYTHONPATH="$OTLBPYTHONPATH" fittotcx "$f" > "${INTERMEDIATEDIRECTORY}/${GPSNAME}".tcx
                # TODO: better way? fittotcx returns 1 when there is an error
                if [[ $? != 0 ]]; then
                    msg "Corruption in $f, moving to trash!"
                    home-trash "$f"
                fi
                # XXXX: gpsbabel does not convert laps properly, but this is left here for fun
                # gpsbabel -i garmin_fit -f "$f" -o gtrnctr,course=0,sport=Running -F "${INTERMEDIATEDIRECTORY}/${GPSNAME}".tcx
            fi
        done
        # now go over to see if they are in permanent directory
        for f in $INTERMEDIATEDIRECTORY/*; do
            # delete if size is zero to avoid nonsense
            # TODO: better check and delete corrupted intermediate files
            if [[ $(ls -nl "$f" | awk '{print $5}') == '0' ]]; then
                # TODO: more universal command for this
                home-trash "$f"
                continue
            fi
            # get the GPS ID from each file from each file i.e., parse the
            # TODO: XML handling could probably be a LOT better!
            local XMLID=`xmlstarlet sel -t -v "//*[local-name() = 'Id']" -n "$f"`
            XMLID=${XMLID//-/}
            XMLID=${XMLID//:/}
            # convert using appropriate time zone data using Emacs script
            # TODO: would prefer if this used --batch and launch-emacsclient noframe is probably not on everyones system
            #       probably need to make this a configurable variable
            # TODO: make an external function here
            XMLID=$(launch-emacsclient noframe --eval "(otlb-gps-adjust-id-timezone \"$XMLID\")" | sed -n 2p)
            XMLID=${XMLID//\"/}
            # if file does not exist, rename and copy
            if [[ ! -e ${TCXDIRECTORY}/${XMLID}.tcx ]]; then
                msg "Copying $f to ${TCXDIRECTORY}/${XMLID}.tcx!"
                cp "$f" ${TCXDIRECTORY}/${XMLID}.tcx
            fi
        done
        popd >> /dev/null
    fi
}

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
            XMLIDFILE="${XMLIDFILE// /T}"
            XMLIDFILE="${XMLIDFILE//:/}"
            XMLIDFILE="${XMLIDFILE//_/}"
            XMLIDFILE="${XMLIDFILE//-/}"
            XMLIDFILE="${XMLIDFILE//./00.}"
            # TODO: possibly flag and convert if different sizes/hashes?
            if [[ ! -e "${SAMSUNG_DIRECTORY}/${XMLIDFILE}" ]]; then
                msg "Converting $f to ${SAMSUNG_DIRECTORY}/${XMLIDFILE}!"
                cp "$f" "${SAMSUNG_DIRECTORY}/${XMLIDFILE}"
            fi
        fi
    done
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
    pushd . >> /dev/null
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
    popd >> /dev/null
}
