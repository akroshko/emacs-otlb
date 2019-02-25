#!/bin/bash
# otlb-sample-config.sh sets bash variables to reasonable defaults or
# placeholders for testing purposes.
#
# Old code is included for the Garmin 305.
#
# Copyright (C) 2015-2019, Andrew Kroshko, all rights reserved.
#
# Author: Andrew Kroshko
# Maintainer: Andrew Kroshko <akroshko.public+devel@gmail.com>
# Created: Fri Mar 27, 2015
# Version: 20180516
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

# the path where the downloaded data and log files are stored
OTLBLOGS=`dirname $0`/org-logs
########################################
# primary device, generally the Garmin 310
# device ID
ANTID="<<your device ID here>>"
# the config directory for antfs-cli, this is typically
# ~/.config/antfs-cli but I symlink it elsewhere
ANTCONFIG=~/.config/antfs-cli/"$ANTID"
# device name, this must be the same as the elisp variable
# otlb-gps-device-primary, but with dashes instead of spaces
DEVICENAME="<<model>>-$ANTID"
########################################
# secondary device, the Samsung Galaxy SIII
# SAMSUNG_DEVICENAME="<<model>>-<<application name>>"
