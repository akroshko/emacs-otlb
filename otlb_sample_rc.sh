#!/bin/bash
# otlb-sample.sh sets up emacs-otlb to run with the default
# installation and the sample data provided.
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
# the 'pip' Python package management tool and the package
# 'FIT-TO-TCX' from https://github.com/Tigge/FIT-to-TCX that must be
# installed manually.
#
# See the included README.md file for more information.

########################################################################
# load the bash functions
source `dirname $0`/otlb_sample_config.sh `dirname $0`/otlb_functions.sh
