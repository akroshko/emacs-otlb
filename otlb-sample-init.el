;;; otlb-sample-init.el --- A sample init file I used for testing
;;; this library in isolation from my main Emacs installation.
;;
;; Copyright (C) 2015-2016, Andrew Kroshko, all rights reserved.
;;
;; Author: Andrew Kroshko
;; Maintainer: Andrew Kroshko <akroshko.public+devel@gmail.com@gmail.com>
;; Created: Fri Apr 10, 2015
;; Version: 20160525
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
;; requires features from https://github.com/akroshko/emacs-stdlib,
;; when installed appropriately using (require 'emacs-stdlib) is
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
(add-to-list 'load-path "../emacs-stdlib/")
(load "../emacs-stdlib/emacs-config.el")
(requiring-package (emacs-stdlib-aliases))
(requiring-package (emacs-stdlib-constants))
(requiring-package (emacs-stdlib-functions))
(requiring-package (emacs-stdlib-commands))
(requiring-package (emacs-stdlib-keys))
(requiring-package (tblel))
;; XXXX: uncomment to use my keys
;; (emacs-stdlib-keys-mode t)
;; (emacs-stdlib-keys-non-term-mode t)
(requiring-package (emacs-stdlib-super-hyper-keys))
;; XXXX: uncomment to use my super and hyper keys
;; (emacs-stdlib-super-keys-mode t)
;; (emacs-stdlib-hyper-keys-all-mode t)
(requiring-package (otlb-sample-config))
(requiring-package (otlb-gps))
