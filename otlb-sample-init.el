;;; otlb-sample-init.el --- A sample init file I used for testing
;;; this library in isolation from my main Emacs installation.
;;
;; Copyright (C) 2015-2019, Andrew Kroshko, all rights reserved.
;;
;; Author: Andrew Kroshko
;; Maintainer: Andrew Kroshko <akroshko.public+devel@gmail.com@gmail.com>
;; Created: Fri Apr 10, 2015
;; Version: 20190228
;; URL: https://github.com/akroshko/emacs-otlb
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Commentary:
;;
;; Use the package associated with this file with the command "emacs
;; -q --load otlb-sample-init.el".  See the included README.md file
;; for more information on this package.
;;
;; Features that might be required by this library:
;;
;; Standard Emacs features, to be documented specificly later.  Also
;; requires features from https://github.com/akroshko/cic-emacs-common,
;; when installed appropriately using (require 'cic-emacs-common) is
;; sufficient.
;;
;; Has supporting bash and Python scripts also have their own
;; requirements, see the supporting README.md for more information.
;;
;;; Code:

;; XXXX: these should produce errors when unable to load, or else why use
;; this file?
(require 'cl)
(require 'org-table)

(add-to-list 'load-path ".")
(add-to-list 'load-path "../cic-emacs-common/")
(load "../cic-emacs-common/cic-emacs-config.el")
(requiring-package (cic-emacs-common-aliases))
(requiring-package (cic-emacs-patterns))
(requiring-package (cic-emacs-functions))
(requiring-package (cic-emacs-macros))
(requiring-package (cic-emacs-org-mode))
(requiring-package (cic-emacs-passwords))
(requiring-package (cic-emacs-strings))
(requiring-package (cic-emacs-keys))
(requiring-package (tblel))
;; XXXX: uncomment to use my keys
;; (cic-emacs-keys-mode t)
;; (cic-emacs-keys-non-term-mode t)
(requiring-package (otlb-sample-config))
(requiring-package (otlb-gps))
