#!/bin/bash
# View the given TCX file in Google Earth.
#
# Copyright (C) 2015 Andrew Kroshko, all rights reserved.
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

main () {
    # convert to tmp
    BASENAME=$(basename "$1")
    TMPFILE=$(mktemp --suffix=.kml)
    gpsbabel -i gtrnctr -f "$1" -o kml -F "${TMPFILE}"
    googleearth "${TMPFILE}"
}
main "$@"
