;; Copyright (C) 2015-2018, Andrew Kroshko, all rights reserved.
;;
;; Author: Andrew Kroshko
;; Maintainer: Andrew Kroshko <akroshko.public+devel@gmail.com>
;; Created: Sun Apr  5, 2015
;; Version: 20180516
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use GPS data in emacs org-tables

(defconst otlb-gps-root-package
  (file-name-directory load-file-name)
  "The root directory where the otlb-package is.")

(defconst otlb-gps-root-binary
  (file-name-directory load-file-name)
  "The root directory where the binary data is handled.")

(defconst otlb-gps-root-text
  (file-name-directory load-file-name)
  "The root directory where the text logbooks are handled.")

(defconst otlb-gps-devices
  "<<model number>> <<serial number>>"
  "List of devices.  When referencing directories the spaces are
  replaced by dashes.  Suggested formats are \"<<model number>>
  <<serial number>>\" or \"<<model>> <<application name>>\"")

(defconst otlb-gps-footwear-current
  (concat otlb-gps-root-text "/footwear-current.org")
  "The location of current footwear.")

;; TODO: update this sample config
(defconst otlb-gps-locations
  (mapcar (lambda (e) (concat otlb-gps-root-binary "/" (replace-regexp-in-string " " "-" e))) otlb-gps-devices)
  "The location where data from the devices are stored.")

(defvar otlb-gps-time-zone
  -6.0
  "A number giving the change from Zulu time.")

(defconst otlb-gps-conditions-script-command
  (concat "python " otlb-gps-root-package "/scrape_weather_ec.py")
  "The script to scrape weather, the default takes information
  from Environment Canada for Saskatoon, Saskatchewan.  Will need
  to be modified for other locations.")

(defconst otlb-gps-read-fit-command
  (concat "python " otlb-gps-root-package "/read_files.py --fit"))

(defconst otlb-gps-read-tcx-command
  (concat "python " otlb-gps-root-package "/read_files.py --tcx"))

(defconst otlb-gps-map-command
  (concat otlb-gps-root-package "/tcx_ge.sh")
  "The command to convert a tcx file named by ID and display in a
  map.  Defaults to a KML file in Google Earth.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XXXX my normal installation does not need this here
;;      eventually everything will be moved around

(defun otdb-table-number (value)
  "Read from string VALUE and convert to a number."
  ;; match numbers, strip whitespace
  (when value
    ;; deal with fractions
    (when (string-match "[0-9.]" value)
      (if (string-match "/" value)
          (let ((space-split (split-string value " "))
                (slash-part nil)
                (slash-split nil)
                first
                second)
            ;; find the part with the slash
            (dolist (part space-split)
              (when (or (not slash-part) (string-match "/" part))
                (when first
                  (setq second t))
                (unless second
                  (setq first t))
                (setq slash-part part)))
            (setq slash-split (split-string slash-part "/"))
            (if second
                (+ (string-to-float (elt space-split 0)) (/ (string-to-float (elt slash-split 0))
                                                            (string-to-float (elt slash-split 1))))
              (/ (string-to-float (elt slash-split 0))
                 (string-to-float (elt slash-split 1)))))
        (string-to-float value)))))

(provide 'otlb-sample-config)
