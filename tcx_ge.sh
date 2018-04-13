#!/bin/bash
# View the given TCX file in Google Earth.
#
# Copyright (C) 2015-2018, Andrew Kroshko, all rights reserved.
#
# Author: Andrew Kroshko
# Maintainer: Andrew Kroshko <akroshko.public+devel@gmail.com>
# Created: Fri Mar 27, 2015
# Version: 20180412
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

main () {
    # convert to tmp
    local BASENAME=$(basename "$1")
    local TMPFILE=$(mktemp --suffix=.kml)
    if [[ "$1" == *.tcx ]]; then
        gpsbabel -i gtrnctr -f "$1" -o kml -F "${TMPFILE}"
    elif [[ "$1" == *.gpx ]]; then
        gpsbabel -i gpx -f "$1" -o kml -F "${TMPFILE}"
    elif [[ "$1" == *.fit ]]; then
        gpsbabel -i garmin_fit -f "$1" -o kml -F "${TMPFILE}"
    fi
    google-earth "${TMPFILE}"
}
main "$@"
