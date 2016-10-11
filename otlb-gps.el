;;; otlb-gps.el --- An activity logbook based on org-mode tables,
;;; which is centered around GPS data that has been downloaded from a
;;; Garmin 310XT fitness watch or Samsung Galaxy SIII phone.
;;
;; Copyright (C) 2015-2016, Andrew Kroshko, all rights reserved.
;;
;; Author: Andrew Kroshko
;; Maintainer: Andrew Kroshko <akroshko.public+devel@gmail.com>
;; Created: Sun Apr  5, 2015
;; Version: 20160525
;; URL: https://github.com/akroshko/emacs-otlb
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
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
;; Use the package associated with this file with "emacs -q --load
;; otlb-sample-init.el".  See the included README.md file for more
;; information on this package.
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

(require 'org-table)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constants

(defconst otlb-gps-week-days
  (list
   "Sunday"
   "Monday"
   "Tuesday"
   "Wednesday"
   "Thursday"
   "Friday"
   "Saturday")
  "A list of days of the week for the GPS log.")

(defconst otlb-gps-months
  (list
   "January"
   "February"
   "March"
   "April"
   "May"
   "June"
   "July"
   "August"
   "September"
   "October"
   "November"
   "December")
  "A list of months for the GPS log.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set up buffer and keys

(defun otlb-gps-interactive-check ()
  "Check if an interactive function is being run in an
appropriate place."
  ;; TODO: convert into a proper macro
  (if (otlb-buffer-p)
      t
    nil))

(defun otlb-gps-interactive ()
  "For now this refreshes and ensures that commands are called
in appropriate place."
  (unless (string= (buffer-file-name) otlb-gps-pedestrian-location)
    (error "Appropriate buffer required for otlb commands!!!")))

(defun otlb-gps-interactive-device (&optional device)
  "For now this refreshes and ensures that commands are called
in appropriate place."
  (unless (string= (buffer-file-name) otlb-gps-pedestrian-location)
    (error "Appropriate buffer required for otlb commands!!!"))
  (if device
      (otlb-gps-refresh-secondary)
    (otlb-gps-refresh-primary)))

;; only global key required!
(global-set-key (kbd "s-j l") 'otlb-gps-find-pedestrian-location)

(defvar otlb-gps-mode-map
  nil
  "Keymap for otlb-gps.")


(defun otlb-gps-mode-map ()
  "Return a standard mode map for otlb-gps."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s-p *")   'otlb-gps-recalculate-all)
    (define-key map (kbd "s-p c")   'otlb-gps-insert-conditions)
    (define-key map (kbd "s-p f")   'otlb-gps-fetch)
    (define-key map (kbd "s-p M-g") 'otlb-gps-graph-distance)
    (define-key map (kbd "s-p g")   'otlb-gps-graph-time)
    (define-key map (kbd "s-p i")   'otlb-gps-insert)
    (define-key map (kbd "s-p M-i") 'otlb-gps-insert-auxiliary)
    ;; (define-key map (kbd "s-p l")   'otlb-gps-cycle)
    ;; (define-key map (kbd "s-p s-l") 'otlb-gps-cycle)
    ;; TODO: do I really want this capitalization?
    (define-key map (kbd "s-p L")   'otlb-gps-cycle-shift)
    (define-key map (kbd "s-p s-L") 'otlb-gps-cycle-shift)
    ;; TODO: make sure this increments workouts
    (define-key map (kbd "H-d")     'otlb-gps-map-preview)
    (define-key map (kbd "s-o o") 'otlb-gps-map-open)
    (define-key map (kbd "s-o s-o") 'otlb-gps-map-open)
    (define-key map (kbd "s-p m")   'otlb-gps-open-cached-osm)
    (define-key map (kbd "s-p M-m") 'otlb-gps-open-google-earth)
    (define-key map (kbd "s-p n")   'otlb-gps-insert-note)
    ;; TODO: o=other, change names
    (define-key map (kbd "s-p o")   'otlb-gps-insert-miscellaneous)
    (define-key map (kbd "s-p M-o") 'otlb-gps-insert-miscellaneous-ask)
    ;; TODO: fix these
    ;; (define-key map (kbd "s-p M-p") 'otlb-gps-plot-running-weekly-totals)
    ;; (define-key map (kbd "s-p p")   'otlb-gps-plot-per-week-totals)
    (define-key map (kbd "s-p p")   'otlb-gps-cycle)
    (define-key map (kbd "s-p s-p") 'otlb-gps-cycle)
    (define-key map (kbd "s-p q")   'otlb-gps-toggle-quality)
    (define-key map (kbd "s-p s")   'otlb-gps-sort)
    (define-key map (kbd "s-p t")   'otlb-gps-toggle)
    (define-key map (kbd "s-p u")   'otlb-gps-insert-unrecorded)
    (define-key map (kbd "s-p w")   'otlb-gps-footwear)
    (define-key map (kbd "H-t")     'otlb-gps-toggle)
    ;; menus
    (define-key map [menu-bar otlb-gps] (cons "otlb-gps" (make-sparse-keymap "otlb-gps")))
    (define-key map [menu-bar otlb-gps google-earth]             '("Open with Google Earth" . otlb-gps-open-google-earth))
    (define-key map [menu-bar otlb-gps osm]                      '("Open cached OSM"        . otlb-gps-open-cached-osm))
    (define-key map [menu-bar otlb-gps plot-running-weekly]      '("Plot running weekly totals" . otlb-gps-plot-running-weekly-totals))
    (define-key map [menu-bar otlb-gps plot-per-weekly]          '("Plot per-week totals" . otlb-gps-plot-per-week-totals))
    (define-key map [menu-bar otlb-gps graph-distance]           '("Graph by distance" . otlb-gps-graph-distance))
    (define-key map [menu-bar otlb-gps graph-time]               '("Graph by time" . otlb-gps-graph-time))
    (define-key map [menu-bar otlb-gps separator2]               '("--"))
    (define-key map [menu-bar otlb-gps toggle-quality]           '("Toggle quality" . otlb-gps-toggle-quality))
    (define-key map [menu-bar otlb-gps toggle-type]              '("Toggle type" . otlb-gps-toggle))
    (define-key map [menu-bar otlb-gps insert-footwear]          '("Insert footwear" . otlb-gps-footwear))
    (define-key map [menu-bar otlb-gps insert-conditions]        '("Insert conditions" . otlb-gps-insert-conditions))
    (define-key map [menu-bar otlb-gps insert-note]              '("Insert note" . otlb-gps-insert-note))
    (define-key map [menu-bar otlb-gps insert-miscellaneous-ask] '("Insert miscellaneous type" . otlb-gps-insert-miscellaneous-ask))
    (define-key map [menu-bar otlb-gps insert-miscellaneous]     '("Insert miscellaneous" . otlb-gps-insert-miscellaneous))
    (define-key map [menu-bar otlb-gps insert-unrecorded]        '("Insert unrecorded" . otlb-gps-insert-unrecorded))
    (define-key map [menu-bar otlb-gps insert]                   '("Insert activity" . otlb-gps-insert))
    (define-key map [menu-bar otlb-gps separator1]               '("--"))
    (define-key map [menu-bar otlb-gps cycle-shift]              '("Cycle shift" . otlb-gps-cycle-shift))
    (define-key map [menu-bar otlb-gps cycle]                    '("Cycle" . otlb-gps-cycle))
    (define-key map [menu-bar otlb-gps recalulate]               '("Recalculate all" . otlb-gps-recalculate-all))
    map))
(setq otlb-gps-mode-map (otlb-gps-mode-map))

(define-minor-mode otlb-gps-mode
  :global nil
  :lighter " otlb"
  :keymap otlb-gps-mode-map)

(add-hook 'otlb-gps-mode-hook 'otlb-gps-mode-init)
(defun otlb-gps-mode-init ()
  (when (and otlb-gps-mode (functionp 'hl-line-mode))
    (hl-line-mode 1)))

(defun otlb-buffer-p ()
  "Check if this is an otlb buffer."
  (and (eq major-mode 'org-mode)
       (save-excursion (goto-char (point-min))
                       ;; assume two spaces in front of TBLEL
                       (re-search-forward "^  #\\+TBLEL: otlb-gps-calc" nil t))
       t))

(defun otlb-setup-hook ()
  "Setup when otlb-gps-mode when activating org-mode."
  (when (otlb-buffer-p)
    (otlb-gps-mode 1)
    (org-toggle-headline-mode 0)))

(add-hook 'org-mode-hook 'otlb-setup-hook)

(defun otlb-gps-minibuffer-setup-hook ()
  "Make sure otlb-gps-mode is not interferring with minibuffer."
  (otlb-gps-mode 0))

(add-hook 'minibuffer-setup-hook 'otlb-gps-minibuffer-setup-hook)

(defun otlb-gps-find-pedestrian-location ()
  "Find the pedestrian log location."
  (interactive)
  (find-file otlb-gps-pedestrian-location))

(defun otlb-gps-cycle ()
  "Cycle headings and tables open specific to otlb-gps."
  (interactive) (otlb-gps-interactive)
  (org-cycle '(64)) (org-cycle-hide-drawers 'all))

(defun otlb-gps-cycle-shift ()
  "Cycle headings and tables closed specific to otlb-gps."
  (interactive) (otlb-gps-interactive)
  (org-shifttab '(64)) (org-cycle-hide-drawers 'all))

(defun otlb-gps-footwear (&optional arg)
  "Command to handle tracking footwear mileage."
  (interactive "P") (otlb-gps-interactive)
  (if arg
      (progn
        (otlb-gps-insert-shoe-totals)
        (find-file otlb-gps-footwear-current))
    (otlb-gps-worn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reading data and inserting commands

(defun otlb-gps-file-ids (location)
  "Walk GPS files to get all IDs, .tcx files only for now.  GPS
files are in the format <<YYYYMMDD>>T<<HHMMSS>>, originally based
off of a tool for downloading off of a Garmin 305."
  (let ((otlb-gps-path location))
    (setq otlb-gps-file-ids nil)
    (cic:walk-path otlb-gps-path (lambda (d f) (otlb-gps-walker d f otlb-gps-file-ids)))
    (setq otlb-gps-file-ids (sort otlb-gps-file-ids (lambda (i j) (not (string< (car i) (car j))))))))

(defun otlb-gps-walker (d f found-tcx)
  "Recursive helper function for walking .tcx files."
  (let ((full-path (concat (file-name-as-directory d) f)))
    ;; extension
    (cond ((file-directory-p full-path)
           (cic:walk-path full-path (lambda (d f) (otlb-gps-walker d f otlb-gps-file-ids))))
          ((or (string= (file-name-extension f) "tcx") (string= (file-name-extension f) "gpx") (string= (file-name-extension f) "fit"))
           (add-to-list 'otlb-gps-file-ids
                        (list (file-name-sans-extension f) full-path))))))

(defun otlb-gps-log-ids ()
  "Get all GPS IDs from the configured logbook."
  (setq otlb-gps-log-ids nil)
  (with-current-file-min otlb-gps-pedestrian-location
    ;; walk all tables in the file
    ;; TODO: next thing to replace, bottleneck
    (let ((latest-id (otlb-gps-get-latest-id))
          (last-id (otlb-gps-get-last-id)))
      (do-otlb-gps-entries latest-id last-id  otlb-gps-log-ids (otlb-gps-get-id-from-heading))))
  (setq otlb-gps-log-ids (sort otlb-gps-log-ids (lambda (i j) (not (string< i j))))))

(defun otlb-gps-missing-ids ()
  "Get list of IDs that are in the filesystem but not in the
logbook."
  ;; now hopefully we have a list of keys and stuff
  ;; loop over
  (setq otlb-gps-missing-ids nil)
  (dolist (file-id (mapcar 'car otlb-gps-file-ids))
    (unless (member file-id otlb-gps-log-ids)
      (add-to-list 'otlb-gps-missing-ids file-id)))
  (setq otlb-gps-missing-ids (nreverse otlb-gps-missing-ids)))

(defun otlb-gps-refresh-primary ()
  "Refresh information from the filesystem and logbook related to
the primary device, generally used before an interactive command."
  (otlb-gps-file-ids (car otlb-gps-locations))
  (otlb-gps-log-ids))

(defun otlb-gps-refresh-secondary ()
  "Refresh information from the filesystem and logbook related to
the secondary device, generally used before an interactive
command."
  (otlb-gps-file-ids (cadr otlb-gps-locations))
  (otlb-gps-log-ids))

(defvar otlb-gps-id-history
  nil
  "The history of entered IDs, the same between all devices for
  now.")

(defun otlb-gps-insert-auxiliary (&optional id)
  (interactive)
  (otlb-gps-insert t id))

;; TODO: manually select device if prefix arg
(defun otlb-gps-insert (&optional device id)
  "Insert a gps ID by selecting from missing ones."
  (interactive)
  ;; sort first
  (goto-char (point-min))
  (otlb-gps-sort)
  (otlb-gps-interactive-device device)
  ;; insert newest if not given
  (unless id
    ;; have one alternate device for now, but will eventually choose
    (otlb-gps-missing-ids)
    ;; (setq id (completing-read "Workout ID: " otlb-gps-missing-ids nil nil (car otlb-gps-missing-ids) 'otlb-gps-id-history))
    ;; XXXX: works better with my current ido setup
    (setq id (completing-read "Workout ID: " otlb-gps-missing-ids nil nil nil 'otlb-gps-id-history)))
  ;; is the id a missing one
  (find-file otlb-gps-pedestrian-location)
  (goto-char (point-min))
  (when (member id otlb-gps-missing-ids)
    ;; create a new heading and table with raw data in the proper place
    (let* ( ;; TODO: different locations
           (device-location (if device
                                (cadr otlb-gps-locations)
                              (car otlb-gps-locations)))
           (fit-alist (cond ((file-exists-p (concat device-location "/" id ".fit"))
                             (with-temp-buffer
                               (insert (shell-command-to-string (concat otlb-gps-read-fit-command " " device-location "/" id ".fit --fit")))
                               (goto-char (point-min))
                               ;; (mpp (buffer-substring (point-min) (point-max)))
                               (json-read)))
                            ((file-exists-p (concat device-location "/" id ".tcx"))
                             (with-temp-buffer
                               (insert (shell-command-to-string (concat otlb-gps-read-tcx-command " " device-location "/" id ".tcx --tcx")))
                               (goto-char (point-min))
                               (json-read)))
                            ((file-exists-p (concat device-location "/" id ".gpx"))
                             (with-temp-buffer
                               (insert (shell-command-to-string (concat otlb-gps-read-gpx-command " " device-location "/" id ".gpx --gpx")))
                               (goto-char (point-min))
                               (json-read)))))
           (fit-laps (cdr (assoc 'laps fit-alist)))
           (lap-distances (cdr (assoc 'distance fit-laps)))
           (lap-times (cdr (assoc 'timer-time fit-laps)))
           (lap-paces (cdr (assoc 'pace fit-laps)))
           (lap-heart-rates (cdr (assoc 'average-heart-rate fit-laps)))
           (lap-maximum-heart-rates (cdr (assoc 'maximum-heart-rate fit-laps)))
           (lap-tuple (zip lap-times
                           lap-distances
                           lap-paces
                           lap-heart-rates
                           lap-maximum-heart-rates))
           (total-distance (cdr (assoc 'distance fit-alist)))
           (total-time (cdr (assoc 'timer-time fit-alist)))
           (total-pace (cdr (assoc 'pace fit-alist)))
           (start-latitude (cdr (assoc 'start-latitude fit-alist)))
           (start-longitude (cdr (assoc 'start-longitude fit-alist)))
           (end-latitude (cdr (assoc 'end-latitude fit-alist)))
           (end-longitude (cdr (assoc 'end-longitude fit-alist)))
           (start-time (cdr (assoc 'start-time fit-alist)))
           (end-time (cdr (assoc 'end-time fit-alist)))
           ;; (time-zone (cdr (assoc 'time-zone fit-alist)))
           ;; TODO: fix this
           (time-zone -6.0 ;; (cadr (assoc 'time-zone read-alist))
                      )
           (the-shoes (let ((shoes (otlb-gps-select-shoes)))
                        (if (eq shoes 'cancel)
                            ""
                          shoes)))
           (table ""))
      (when (eq the-shoes 'cancel)
        (setq the-shoes ""))
      (setq table (concat table
                          "  |---+---+---+---+---+---|\n"
                          "  | Duration | Distance | Pace | Average HR | Max HR | Shoes | Notes |\n"
                          "  |---+---+---+---+---+---|\n"))
      (dolist (lap lap-tuple)
        (setq table (concat table "  | "
                            (otlb-gps-duration-to-string (car lap)) " | "
                            (otlb-gps-distance-to-string (cadr lap)) " | "
                            (otlb-gps-pace-to-string (caddr lap)) " | "
                            (otlb-gps-hr-number-to-string (cadddr lap)) " | "
                            (otlb-gps-hr-number-to-string (cadddr (cdr lap))) " |\n")))
      (setq table (concat table
                          "  |---+---+---+---+---+----|\n"
                          "  | "
                          (otlb-gps-duration-to-string total-time) " | "
                          (otlb-gps-distance-to-string total-distance) " | "
                          (otlb-gps-pace-to-string total-pace)
                          "  | | | " the-shoes " |   |\n"
                          "  |---+---+---+---+---+---|\n"
                          "  #+TBLEL: otlb-gps-calc\n"
                          "  #+BEGIN_COMMENT\n"
                          "  #+END_COMMENT\n"))
      ;; use temporary org-mode buffer and align table
      (setq table (with-temp-buffer
                    (insert table)
                    (forward-line -4)
                    (org-table-align)
                    (buffer-substring (point-min) (point-max))))
      ;; create appropriate heading, based on date
      ;; will have to figure out if datetree is better...
      (move-end-of-line 1)
      (insert "\n")
      (org-insert-heading)
      (insert (concat (otlb-gps-id-to-full-date id) " :running:"))
      ;; add a property drawer
      (move-end-of-line 1)
      (insert "\n")
      ;; create the drawer for the original table
      (insert (concat  "  :PROPERTIES:\n"
                       "  :id: " id "\n"
                       "  :device: " (if device (cadr otlb-gps-devices) (car otlb-gps-devices)) "\n"
                       "  :start-latitude: " start-latitude "\n"
                       "  :start-longitude: " start-longitude "\n"
                       "  :end-latitude: " end-latitude "\n"
                       "  :end-longitude: " end-longitude "\n"
                       "  :start-time: " (otlb-gps-adjust-id-timezone (otlb-gps-time-strip start-time)) "\n"
                       "  :end-time: " (otlb-gps-adjust-id-timezone (otlb-gps-time-strip end-time)) "\n"
                       "  :time-zone: " (number-to-string time-zone) "\n"
                       "  :END:\n"))
      (search-backward ":PROPERTIES:")
      (org-cycle)
      ;; move to next line without ???
      (next-line)
      (move-beginning-of-line 1)
      ;; add the drawer for the original
      (insert (concat "  :RAW:\n"
                      table
                      "  :END:\n"))
      (search-backward ":RAW:")
      (org-cycle)
      ;; move to next line without ???
      (next-line)
      (move-beginning-of-line 1)
      ;; put the table
      (insert table)
      (org-back-to-heading)
      (search-forward ":END:")
      (search-forward ":END:")
      (forward-line 1)
      ;; TODO replace with nicer functions used elsewhere
      (cic:org-table-last-row)
      (forward-char 4)
      (org-table-next-field)
      (org-table-next-field)
      (org-table-next-field)
      (forward-word)
      (forward-char 2)))
  (goto-char (point-min))
  (otlb-gps-sort)
  (flush-lines "^\\s-*$"))

(defun otlb-gps-insert-unrecorded (&optional id)
  "Insert an unrecorded track.  Select start and end locations as
well as filling in known information."
  ;; TODO, still goto kill space after insert, maybe collapse space between next heading?
  ;; TODO: align table
  (interactive)
  (goto-char (point-min))
  ;; start with selecting date
  (let* ((the-start-time (org-read-date nil 'totime nil "Start time: "))
         (the-start-id (otlb-gps-encoded-time-to-id the-start-time))
         (start-location (read-string "Start/end location: "))
         ;; if empty, start the same as end
         ;; (end-location (read-string "End location: "))
         (start-latitude "")
         (start-longitude "")
         (end-latitude "")
         (end-longitude "")
         (the-distance (read-string "Distance covered (km): "))
         (the-pace (read-string "Estimated pace (min/km): "))
         (the-shoes (let ((shoes (otlb-gps-select-shoes)))
                      (if (eq shoes 'cancel)
                          ""
                        shoes)))
         ;; duration in seconds
         (the-duration (* 60 (string-to-float the-distance) (string-to-float the-pace)))
         (the-end-time "")
         (the-end-id "")
         (table (concat
                 "  |---+---+---+---+---+---|\n"
                 "  | Duration | Distance | Pace | Average HR | Max HR | Shoes | Notes |\n"
                 "  |---+---+---+---+---+---|\n"
                 "  | " (otlb-gps-duration-to-string the-duration) " | " the-distance " km | " the-pace " min/km |  |  | | |\n"
                 "  |---+---+---+---+---+---|\n"
                 "  | " (otlb-gps-duration-to-string the-duration) " | " the-distance " km | " the-pace " min/km |  |  | " the-shoes  " | Start/end Location: " start-location " |\n"
                 "  |---+---+---+---+---+---|\n "
                 " #+TBLEL: otlb-gps-calc\n"
                 "  #+BEGIN_COMMENT\n"
                 "  #+END_COMMENT")))
    ;; convert the start time and end time to ID
    (move-end-of-line 1)
    (insert "\n")
    (org-insert-heading)
    (insert (concat (otlb-gps-id-to-full-date the-start-id) " :running:unrecorded:"))
    (move-end-of-line 1)
    (insert "\n")
    ;; insert properties
    ;; TODO fix up time zone
    ;; TODO get locations from map
    ;; TODO calculate end-time correctly
    (insert (concat  "  :PROPERTIES:\n"
                     "  :id: " the-start-id "\n"
                     "  :start-latitude: " start-latitude "\n"
                     "  :start-longitude: " start-longitude "\n"
                     "  :end-latitude: " end-latitude "\n"
                     "  :end-longitude: " end-longitude "\n"
                     "  :start-time: " the-start-id "\n"
                     "  :end-time: " the-end-id "\n"
                     "  :time-zone: " "-6.0" "\n"
                     "  :END:\n"))
    ;; insert the table, incomplete
    (search-backward ":PROPERTIES:")
    (org-cycle)
    (next-line)
    (insert "  :RAW:\n")
    (insert table)
    (insert "\n")
    (insert "  :END:\n")
    (search-backward ":RAW:")
    (org-cycle)
    (next-line)
    (move-beginning-of-line 1)
    (insert table)
    ;; TODO collapse space here
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utility commands

(defun otlb-gps-insert-conditions (&optional arg)
  (interactive "P") (otlb-gps-interactive)
  (if arg
      (otlb-gps-get-conditions t)
    (let ((gps-string (concat " " (otlb-gps-get-conditions))))
      ;; need to forward to the real table
      (otlb-gps-table-last-row)
      (org-table-goto-column 7)
      (org-table-end-of-field 1)
      (insert gps-string))))

(defun otlb-gps-fetch ()
  (interactive) (otlb-gps-interactive)
  ;; TODO: configure command a little better
  ;; TODO: outdated because of no options too
  (start-process "terminal" nil "nohup" "rxvt-unicode" "-e" "bash" "-i" "-c" "(fetch-garmin-310);while read -r -t 0;do read -r; done;read -p 'Press enter to continue...'"))

;; sort the entries by reverse date/time, bubble sort is nice when log
;; is already almost sorted
;; TODO: n^2 sorting algorithm will probably have to be improved
(defun otlb-gps-sort ()
  "Sort the GPS logbook enteries."
  (interactive)
  (let ((bubble-count 0))
    ;; use bubble sort!
    (setq otlb-gps-sort-done-this-step t)
    (save-excursion
      (while otlb-gps-sort-done-this-step
        (message (concat "Bubble: " (number-to-string bubble-count)))
        (setq otlb-gps-sort-done-this-step nil)
        ;; goto first heading
        (goto-char (point-min))
        (outline-next-heading)
        (save-excursion
          ;; TODO: change to quiet or make my own message?
          ;; TODO: next thing to replace, bottleneck
          (org-map-entries 'otlb-gps-bubble nil))
        (setq bubble-count (+ 1 bubble-count))))
    (message "Sort done!")))

(defun otlb-gps-bubble ()
  "Helper function to bubble sort the GPS logbook enteries."
  (let (first-id second-id)
    ;; TODO: make this more efficient?
    (org-back-to-heading)
    (setq first-id (otlb-gps-get-id-from-heading))
    (save-excursion
      (condition-case nil
          (progn
            (otlb-gps-next-entry)
            (setq second-id (otlb-gps-get-id-from-heading))
            t)
        (error nil)))
    ;; map a swap if next newer, until buffer stops changing
    ;; need a comparison function for garmin ids
    (if (and second-id (string< first-id second-id))
        (save-excursion
          (progn
            (otlb-gps-next-entry)
            (org-move-subtree-up)
            (setq otlb-gps-sort-done-this-step t)
            t))
      nil)))

(defun otlb-gps-recalculate-all ()
  "Recalculate all GPS logbook enteries."
  (interactive)
  (otlb-gps-interactive)
  (with-current-file otlb-gps-pedestrian-location
    (org-table-map-tables 'org-table-recalculate)))

(defun otlb-gps-plot-per-week-totals (&optional start-id previous-weeks)
  "Plot starting at START-ID for PREVIOUS-WEEKS weeks.
TODO: need a better description of this"
  (interactive)
  (unless start-id
    (setq start-id (otlb-gps-get-id)))
  (unless previous-weeks
    ;; go back about 6 months?
    (setq previous-weeks 26))
  (let ((weekly-totals (otlb-gps-per-weekly-totals start-id previous-weeks))
        (dat-file (make-temp-file "otlb-gps" nil ".dat"))
        (script-file (make-temp-file "otlb-gps" nil ".gnuplot"))
        (last-week-end nil)
        (first-week-end nil))
    (save-window-excursion
      (with-current-file-min dat-file
        (erase-buffer)
        (dolist (thing weekly-totals)
          (let ((total-thing (cadr (assoc 'total thing)))
                (run-thing (cadr (assoc 'running thing)))
                (walk-thing (cadr (assoc 'walking thing)))
                (week-end (cadr (assoc 'week-end thing))))
            (unless last-week-end
              (setq last-week-end week-end))
            (setq first-week-end week-end)
            (unless total-thing
              (setq total-thing (list (list "") "0.0km" "0h:00")))
            (unless run-thing
              (setq run-thing (list (list "") "0.0km" "0h:00")))
            (unless walk-thing
              (setq walk-thing (list (list "") "0.0km" "0h:00")))
            ;; TODO: need way to get appropriate week
            (insert (concat week-end " | " (number-to-string (otdb-table-number (elt total-thing 1))) " | " (elt total-thing 2) " | "
                                           (number-to-string (otdb-table-number (elt   run-thing 1))) " | " (elt   run-thing 2) " | "
                                           (number-to-string (otdb-table-number (elt  walk-thing 1))) " | " (elt  walk-thing 2) "\n"))))
        (save-buffer)
        (kill-buffer))
      (message (concat "otlb-gps: wrote plot data to " dat-file))
      (with-current-file-min script-file
        (erase-buffer)
        (insert "clear\n")
        (insert "reset\n")
        (insert (concat "set title 'Mileage per-week (Sunday midnight to Sunday midnight) from " (otlb-gps-id-to-date (otlb-gps-id-sunday-midnight start-id)) " back " (number-to-string previous-weeks) " weeks'\n"))
        (insert "set timefmt \"%Y%m%dT%H%M%S\"\n")
        ;; axes
        (insert "set xdata time\n")
        (insert (concat "set xrange " "[\"" (otlb-gps-n-week-id 1 (otlb-gps-id-to-encode-time first-week-end)) "\":\"" (otlb-gps-n-week-id -1 (otlb-gps-id-to-encode-time last-week-end)) "\"]" "reverse\n"))
        (insert "set xtics out\n")
        (insert "set xtics format \"%m/%d\"\n")
        (insert (concat "set xtics \"" (otlb-gps-n-week-id 1 (otlb-gps-id-to-encode-time first-week-end))  "\",604800,\"" (otlb-gps-n-week-id -1 (otlb-gps-id-to-encode-time last-week-end)) "\"\n"))
        (insert "unset mxtics\n")
        (insert "set ylabel \"(km)\"\n")
        (insert "set mytics\n")
        ;; ???
        (insert "set datafile separator \"|\"\n")
        (insert "set style data boxes\n")
        (insert "set style histogram cluster gap 0\n")
        ;; number controls color?
        (insert "set style fill solid 0.2\n")
        (insert "set boxwidth 0.25 relative\n")
        (insert "set grid\n")
        (insert (concat "plot '" dat-file "' using (timecolumn(1)-60*60*24*7*0.15):2 title 'total', "
                                        " '' using (timecolumn(1)-60*60*24*7*0.45):4 title 'running', "
                                        " '' using (timecolumn(1)-60*60*24*7*0.75):6 title 'walking'\n"))
        (save-buffer)
        (gnuplot-send-buffer-to-gnuplot)
        (kill-buffer))
      (bury-buffer (get-buffer "*gnuplot*")))))

;; (otlb-gps-running-weekly-totals (otlb-gps-get-id) 1 4)
(defun otlb-gps-plot-running-weekly-totals (&optional start-id n-weekly previous-weeks)
  "Plot starting at START-ID for N-WEEKLY for PREVIOUS-WEEKS
weeks.
TODO: need a better description of this
TODO: create buffer for looking at raw data?
"
  (interactive)
  (unless start-id
    (setq start-id (otlb-gps-get-id)))
  (unless n-weekly
    (setq n-weekly 1))
  ;; possibility of going back to same calendar day
  (unless previous-weeks
    (setq previous-weeks 4))
  (let ((weekly-totals (otlb-gps-running-weekly-totals start-id n-weekly previous-weeks))
        (dat-file (make-temp-file "otlb-gps" nil ".dat"))
        (script-file (make-temp-file "otlb-gps" nil ".gnuplot"))
        (last-week-end nil)
        (first-week-end nil))
    ;; plot!!
    (save-window-excursion
      (with-current-file-min dat-file
        (erase-buffer)
        (dolist (thing weekly-totals)
          (let ((total-thing (cadr (assoc 'total thing)))
                (run-thing (cadr (assoc 'running thing)))
                (walk-thing (cadr (assoc 'walking thing)))
                (week-end (cadr (assoc 'week-end thing))))
            (unless last-week-end
              (setq last-week-end week-end))
            (setq first-week-end week-end)
            (unless total-thing
              (setq total-thing (list (list "") "0.0km" "0h:00")))
            (unless run-thing
              (setq run-thing (list (list "") "0.0km" "0h:00")))
            (unless walk-thing
              (setq walk-thing (list (list "") "0.0km" "0h:00")))
            (insert (concat week-end " | " (number-to-string (otdb-table-number (elt total-thing 1))) " | " (elt total-thing 2) " | "
                                           (number-to-string (otdb-table-number (elt   run-thing 1))) " | " (elt   run-thing 2) " | "
                                           (number-to-string (otdb-table-number (elt  walk-thing 1))) " | " (elt  walk-thing 2) "\n"))))
        (save-buffer)
        (kill-buffer))
      (with-current-file-min script-file
        (erase-buffer)
        (insert "clear\n")
        (insert "reset\n")
        (insert (concat "set title 'Running sum of mileage for previous " (number-to-string n-weekly) " week from each day starting at " start-id ", back " (number-to-string previous-weeks) " weeks'\n"))
        (insert "set timefmt \"%Y%m%dT%H%M%S\"\n")
        (insert "set xrange [*:*] reverse\n")
        (insert "set xdata time\n")
        (insert (concat "set xrange " "[\"" (otlb-gps-n-day-id 1 (otlb-gps-id-to-encode-time first-week-end)) "\":\"" (otlb-gps-n-day-id -1 (otlb-gps-id-to-encode-time last-week-end)) "\"]" "reverse\n"))
        (insert "set xtics out\n")
        (insert "set xtics format \"%m/%d\"\n")
        (insert (concat "set xtics \"" (otlb-gps-n-day-id 1 (otlb-gps-id-to-encode-time first-week-end))  "\",86400,\"" (otlb-gps-n-day-id -1 (otlb-gps-id-to-encode-time last-week-end)) "\"\n"))
        (insert "unset mxtics\n")
        (insert "set ylabel \"(km)\"\n")
        (insert "set mytics\n")
        ;; ???
        (insert "set datafile separator \"|\"\n")
        (insert "set style data boxes\n")
        (insert "set style histogram cluster gap 0\n")
        ;; number controls color?
        (insert "set style fill solid 0.2\n")
        (insert "set boxwidth 0.25 relative\n")
        (insert "set grid\n")
        (insert (concat "plot '" dat-file "' using (timecolumn(1)-60*60*24*0.15):2 title 'total', "
                                        " '' using (timecolumn(1)-60*60*24*0.45):4 title 'running', "
                                        " '' using (timecolumn(1)-60*60*24*0.75):6 title 'walking'\n"))
        (save-buffer)
        (gnuplot-send-buffer-to-gnuplot)
        (kill-buffer))
      (bury-buffer (get-buffer "*gnuplot*")))))

(defun otlb-gps-elp-instrument ()
  "Standard profiling setup that really helps find bottlenecks"
  (interactive)
  (require 'elp)
  (elp-restore-all)
  (elp-reset-all)
  (elp-instrument-package "otlb")
  (elp-instrument-package "cic:")
  (elp-instrument-package "org-table")
  (elp-instrument-package "org")
  ;; some commonly used functions
  (elp-instrument-function 'format)
  (elp-reset-all))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions

(defun otlb-gps-worn ()
  "Command to insert a set of shoes into a current activity."
  (interactive)
  (let* ((selected (otlb-gps-select-shoes))
         table-length)
    (unless (eq selected 'cancel)
      (if (org-at-table-p)
          (progn
            (org-back-to-heading)
            ;; TODO: repetitive
            (cic:org-find-table)
            (setq table-length (length (cic:org-table-to-lisp-no-separators)))
            (org-table-put table-length 6 selected)
            (cic:org-find-table 2)
            (setq table-length (length (cic:org-table-to-lisp-no-separators)))
            (org-table-put table-length 6 selected))
        (insert selected-string)))))

(defun otlb-gps-select-shoes ()
  "Interactively select the shoes worn based on current
footwear."
  (with-current-file otlb-gps-footwear-current
    (goto-char (point-min))
    ;; advance to table
    (cic:org-find-table)
    (let* ((table-lisp (cic:org-table-to-lisp-no-separators))
           (pairs (delq nil (mapcar (lambda (e)
                                      (when (string= (cic:strip-full (elt e 9)) "")
                                        (list (elt e 7)
                                              (elt e 8)
                                              (elt e 0)
                                              (elt e 1)
                                              (elt e 3))))
                                    (cdr table-lisp)))))
      ;; select the results
      (cic:select-list-item (mapcar (lambda (e) (mapconcat 'identity e " "))
                                    pairs)))))


(defun otlb-gps-insert-miscellaneous-ask ()
  (interactive)
  (let ((selected (cic:select-list-item (list "strength" "stretch" "strength/stretch"))))
    (cond ((equal selected "strength")
           (otlb-gps-insert-miscellaneous nil "strength:"))
          ((equal selected "stretch")
           (otlb-gps-insert-miscellaneous nil "stretch:"))
          ((equal selected "strength/stretch")
           (otlb-gps-insert-miscellaneous nil "strength:stretch:")))))

(defun otlb-gps-insert-miscellaneous (&optional id tag-string)
  "Insert a miscellaneous entry with ID or select if nil.  Add
TAG-STRING tags after :miscellaneous: tag."
  (interactive)
  (goto-char (point-min))
  (let* ((the-start-time (org-read-date nil 'totime nil "Start time: "))
         (the-start-id (otlb-gps-encoded-time-to-id the-start-time))
         (table (concat
                 "  |---+---+---+---+---|\n"
                 "  | Activity  | Number | Intensity | Rest      | Notes        |\n"
                 "  |---+---+---+---+---+\n"
                 "  |   |   |   |   |   |\n"
                 "  |---+---+---+---+---+\n"
                 "  #+BEGIN_COMMENT\n"
                 "  #+END_COMMENT")))
    (move-end-of-line 1)
    (insert "\n")
    (org-insert-heading)
    (if tag-string
        (insert (concat (otlb-gps-id-to-full-date the-start-id) " :miscellaneous:" tag-string " "))
      (insert (concat (otlb-gps-id-to-full-date the-start-id) " :miscellaneous: ")))
    (move-end-of-line 1)
    (insert "\n")
    (insert (concat  "  :PROPERTIES:\n"
                     "  :id: " the-start-id "\n"
                     "  :time-zone: " "-6.0" "\n"
                     "  :END:\n"))
    (search-backward ":PROPERTIES:")
    (org-cycle)
    (next-line)
    (insert table)
    (forward-line -3)
    (move-beginning-of-line 1)
    (forward-char 4)
    (org-table-align)))

(defun otlb-gps-insert-note (&optional id)
  "Insert a note entry with ID or select if nil."
  (interactive)
  (goto-char (point-min))
  (let* ((the-start-time (org-read-date nil 'totime nil "Start time: "))
         (the-start-id (otlb-gps-encoded-time-to-id the-start-time)))
    (move-end-of-line 1)
    (insert "\n")
    (org-insert-heading)
    (insert (concat (otlb-gps-id-to-full-date the-start-id) " :note: "))
    (move-end-of-line 1)
    (insert "\n")
    (insert (concat  "  :PROPERTIES:\n"
                     "  :id: " the-start-id "\n"
                     "  :time-zone: " "-6.0" "\n"
                     "  :END:\n"))
    (search-backward ":PROPERTIES:")
    (org-cycle)
    (next-line)
    (move-beginning-of-line 1)
    (insert "  ")))

(defun otlb-gps-hr-number-to-string (number)
  "Convert a HR number to an appropriate string."
  (let ((hr-string (ignore-errors (number-to-string number))))
    (if hr-string
        hr-string
      "")))

(defun otlb-gps-id-to-full-date (id)
  "Convert a GPS ID to a written date."
  (let ((id-year (substring id 0 4))
        (id-month (substring id 4 6))
        (id-day (substring id 6 8))
        (id-hour (substring id 9 11))
        (id-minute (substring id 11 13))
        (id-second (substring id 13 15)))
    (let* ((time-string)
           (day-of-week (calendar-day-of-week (list (string-to-number id-month)
                                                    (string-to-number id-day)
                                                    (string-to-number id-year)))))
      (concat (elt otlb-gps-week-days day-of-week) " "
              (elt otlb-gps-months (- (string-to-number id-month) 1)) " "
              id-day
              ", "
              id-year
              " "
              id-hour
              ":"
              id-minute
              ":"
              id-second))))

(defun otlb-gps-id-to-date (id)
  "Convert a GPS ID into a dash-seperated date."
  (let ((id-year (substring id 0 4))
        (id-month (substring id 4 6))
        (id-day (substring id 6 8)))
    (concat id-year "-" id-month "-" id-day)))

(defun otlb-gps-id-to-time (id)
  "Convert a GPS ID into a colon-seperated time representation."
  (let ((id-hour (substring id 9 11))
        (id-minute (substring id 11 13))
        (id-second (substring id 13 15)))
    (concat id-hour ":" id-minute ":" id-second)))

(defun otlb-gps-duration-to-string (duration)
  "Get a nice representation of a duration."
  (let* ((minutes (floor duration 60))
         (seconds (- duration (* minutes 60)))
         (hours (floor minutes 60)))
    (if (equal hours 0)
        (format "%2d:%05.2f" minutes seconds)
      (progn
        (setq minutes (mod minutes 60))
        (format "%dh%02d:%05.2f" hours minutes seconds)))))

(defun otlb-gps-distance-to-string (distance)
  "Get a nice string representation of DISTANCE in kilometers."
  ;; XXXX: to make any sums work nil distance converts to zero
  (if distance
      (format "%.3f km" (/ distance 1000.0))
    "0.000 km"))

(defun otlb-gps-pace-to-string (pace)
  "Get a nice representation of a running PACE in minutes per
kilometer."
  (if (or (equal pace (string-to-float "1.0e+INF"))
          (equal pace (string-to-float "1.0e-INF"))
          (isnan pace))
      ""
    (let* ((minutes (floor pace))
           (seconds (* 60 (- pace minutes))))
      (format "%d:%02.0f min/km" minutes seconds))
    ))

(defun otlb-gps-string-to-duration (str)
  "Convert the standard time string STR into a duration in
seconds."
  (let* ((str-strip-full (strip-full str))
         str-split
         hours
         minute-split
         minutes
         seconds)
    (setq str-split (split-string str-strip-full "h"))
    (if (> (length str-split) 1)
        (progn
          (setq hours (string-to-float (car str-split)))
          (setq str-strip-full (cadr str-split)))
      (progn
        (setq hours 0)
        (setq str-strip-full (car str-split))))
    (setq str-split (split-string str-strip-full ":")) ;
    (setq minutes (string-to-float (car str-split)))
    (setq seconds (string-to-float (cadr str-split)))
    (+ (* hours 3600) (* minutes 60) seconds)))

(defun otlb-gps-string-to-distance (str)
  "Convert the standard distance string STR into distance in
meters."
  (* (string-to-float str) 1000.0))

(defun otlb-gps-string-to-pace (str)
  "Convert the standard pace string STR into mins/km in decimal."
  (let* ((str-strip-full (strip-full str))
         str-split
         minutes
         seconds)
    (setq str-split (split-string str-strip-full ":"))
    (setq minutes (string-to-float (car str-split)))
    (setq seconds (string-to-float (cadr str-split)))
    (+ minutes (/ seconds 60.0))))

(defun otlb-gps-string-sum-durations (durations)
  "Sum DURATIONS and convert into a string. "
  (otlb-gps-duration-to-string (apply '+ (mapcar 'otlb-gps-string-to-duration durations))))

(defun otlb-gps-string-sum-distances (distances)
  "Sum DISTANCES and convert into a string."
  (otlb-gps-distance-to-string (apply '+ (mapcar 'otlb-gps-string-to-distance distances))))

(defun otlb-gps-string-average-paces (paces)
  "Average PACES and convert into a string."
  (otlb-gps-pace-to-string (/ (apply '+ (mapcar 'otlb-gps-string-to-pace paces)) (length paces))))

(defun otlb-gps-string-pace (duration distance)
  "Convert a DURATION and DISTANCE to a string giving pace."
  (otlb-gps-pace-to-string (/
                            (/ (otlb-gps-string-to-duration duration) 60.0)
                            (/ (otlb-gps-string-to-distance distance) 1000.0))))

(defun otlb-gps-toggle ()
  "Toggle the type of activity for the current logbook entry."
  (interactive)
  (when (otlb-gps-interactive-check)
    (save-excursion
      (org-back-to-heading)
      ;; go back to headline
      (let* ((otlb-gps-taglist (list "backpacking"
                                     "running"
                                     "walking"
                                     "invalid"))
             (current-tags (org-get-tags))
             (current-tag (car-only (cl-intersection otlb-gps-taglist current-tags :test 'equal)))
             (count 0)
             current-index
             next-index
             new-tags)
        ;; get current index and the next index
        (mapcar
         (lambda (e)
           (when (string= e current-tag)
             (setq current-index count))
           (setq count (1+ count)))
         otlb-gps-taglist)
        (setq next-index (mod (1+ current-index) (length otlb-gps-taglist)))
        ;; replace instances of current-tag with one at next-index
        (setq new-tags (cl-subst (elt otlb-gps-taglist next-index) current-tag current-tags :test 'equal))
        ;; delete tags and replace them
        (when (search-forward-regexp ":[[:alnum:]:]*:$" nil t)
          (replace-match (concat ":" (mapconcat 'identity new-tags ":") ":")))))
    t))

(defun otlb-gps-toggle-quality ()
  "Toggle the tags quality of the data for the current logbook
entry."
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (let* ((otlb-gps-taglist (list
                              '()
                              '("incomplete")
                              '("manual" "complete")
                              '("manual" "incomplete")))
           (current-tags (org-get-tags))
           (count (length otlb-gps-taglist))
           found-index
           next-index
           already-found
           new-taglist)
      ;; figure out which one this matches if any, probably work
      ;; backwards (only works for this case?)
      (dolist (tag-groups (reverse otlb-gps-taglist))
        (unless already-found
          (setq count (- count 1))
          (when (= (length (cl-intersection tag-groups current-tags :test 'equal))
                   (length tag-groups))
            (setq found-index count)
            (setq already-found t))))
      ;; must delete elements individually for now
      (mapcar (lambda (e)
                (setq current-tags (delete e current-tags)))
              (elt otlb-gps-taglist found-index))
      (setq next-index (mod (1+ found-index) (length otlb-gps-taglist)))
      (setq new-taglist (append current-tags (elt otlb-gps-taglist next-index)))
      (when (search-forward-regexp ":[[:alnum:]:]*:$"  nil t)
        (replace-match (concat ":" (mapconcat 'identity new-taglist ":") ":"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gps file operations

(defun otlb-gps-per-weekly-totals (start-id previous-weeks)
  "Get running weekly totals for each week starting at a week
starting Sunday/Monday midnight and containing START-ID going
back PREVIOUS-WEEKS weeks."
  (let* ((days-back (otlb-gps-id-range-weekly
                     (otlb-gps-id-sunday-midnight start-id)
                     previous-weeks))
         (daily-totals (mapcar (lambda (d)
                                 (otlb-gps-week-totals d 1))
                               days-back))
         (weekly-totals (mapcar 'otlb-gps-total-totals daily-totals)))
    (dotimes (count (length weekly-totals))
      (nconc (elt weekly-totals count)
             (list (list 'week-end (elt days-back count)))))
    weekly-totals))

(defun otlb-gps-running-weekly-totals (start-id n-weekly previous-weeks)
  "Get N-WEEKLY weekly totals for each week going back
PREVIOUS-WEEKS weeks from START-ID."
  ;; loop over all days going back previous-weeks, and get (n)-weekly totals
  (let* ((days-back (otlb-gps-id-range-daily
                     ;; TODO: start at midnight tonight?
                     (otlb-gps-id-midnight start-id)
                     (* 7 previous-weeks)))
         (daily-totals (mapcar (lambda (d)
                                 (otlb-gps-week-totals d n-weekly))
                               days-back))
         (weekly-totals (mapcar 'otlb-gps-total-totals daily-totals)))
    (dotimes (count (length weekly-totals))
      (nconc (elt weekly-totals count)
             (list (list 'week-end (elt days-back count)))))
    weekly-totals))

(defun otlb-gps-week-totals (start-id weeks-back)
  "Get the total mileage going back WEEKS-BACK weeks from
START-ID."
  (otlb-gps-totals start-id (otlb-gps-backward-time start-id (otlb-gps-weeks-to-encode-time weeks-back))))

(defun otlb-gps-total-totals (totals)
  "Helper function to add up the total mileage."
  ;; TODO: change to reflect tags
  (reduce 'otlb-gps-sum-totals totals :initial-value (list
                                                      (list 'total (list nil "0.000km" "0:00")))))

(defun otlb-gps-sum-totals (total1 total2)
  "Sum distance and duration totals from TOTAL1 and TOTAL2.."
  ;; assume this comes out as ID, distance, time
  ;; TODO: modify to sum an alist
  (cond (t
         ;;(eq (type-of (car total1)) 'cons)
         (let (total-list
               (all-keys (delete-dups (append (mapcar 'car total1) (mapcar 'car total2)))))
           ;; we have an alist
           ;; TODO: pass through if not ???
           (dolist (key all-keys)
             (cond ((and
                     (assq key total1)
                     (not (assq key total2)))
                    ;; if key is only in lst1
                    (setq total-list (append
                                      total-list
                                      (list (assoc key total1)))))
                   ((and
                     (not (assq key total1))
                     (assq key total2))
                    ;; if key is only in lst2
                    (setq total-list (append
                                      total-list
                                      (list (assoc key total2)))))
                   (t
                    (let* ((lst1 (cadr (assq key total1)))
                           (lst2 (cadr (assq key total2))))
                      (setq total-list (append total-list
                                               (list
                                                (list key
                                                      (list
                                                       (append (nth 0 lst2) (nth 0 lst1))
                                                       (otlb-gps-string-sum-distances (list (nth 1 lst2) (nth 1 lst1)))
                                                       (otlb-gps-string-sum-durations (list (nth 2 lst2) (nth 2 lst1))))))))))))
           total-list))))

(defun otlb-gps-totals (start-id end-id)
  "Get totals between START-ID and END-ID."
  (let (gathered)
    ;; how to accumulate data from table, use global?
    (setq otlb-gps-gathered nil)
    ;; copy table and delete old enteries than end
    ;; XXXX there may be issues with lists that are repeatedly reversed
    (setq gathered nil)
    (do-otlb-gps-entries start-id end-id gathered
                         (otlb-gps-entry-get))
    gathered))

(defun otlb-gps-entry-get ()
  "Get ID and total time and distance from a logbook entry."
  (otlb-gps-find-actual-table)
  (let ((tag (otlb-gps-get-tag-from-heading))
        (id (otlb-gps-get-id-from-heading))
        time
        distance)
    (otlb-gps-table-last-row)
    (setq time (strip-full-no-properties (org-table-get nil 1)))
    (setq distance (strip-full-no-properties (org-table-get nil 2)))
    (if (or (eq tag 'running) (eq tag 'walking))
        (list (list 'total (list (list id) distance time))
              (list tag (list (list id) distance time)))
      (list (list tag (list (list id) distance time))))))

(defun otlb-gps-id-capture ()
  "Read in a GPS ID based on an entered date and time."
  (interactive)
  (let* ((capture-date (org-read-date))
         (capture-time (read-string "Enter 24 hour time HHMM: "))
         (id (concat (replace-regexp-in-string "-" "" capture-date) "T" capture-time "00")))
    (setq otlb-gps-store-id id)
    (otlb-gps-id-to-full-date id)))

(defun otlb-gps-pedestrian-location ()
  "Go to a particular location in the GPS logbook."
  (interactive)
  ;; go to the file
  (find-file otlb-gps-pedestrian-location)
  (goto-char (point-min))
  ;; go to the second line
  (next-line)
  (move-beginning-of-line 1))

(defun otlb-gps-get-conditions (&optional just-url)
  "Get the conditions for the current logbook entry, generally
assumed to be conditions on the next calendar hour after the
start time."
  ;; get current and target times and convert to ID
  (let ((now-id (otlb-gps-now-id))
        (target-id (otlb-gps-get-id))
        weather-string)
    ;; compare to current id, is target longer than 24 hours ago?
    ;; call the python program to get the appropriate condition string
    (if (>= (float-time (time-subtract
                         (otlb-gps-id-to-encode-time now-id)
                         (otlb-gps-id-to-encode-time target-id)))
            86400)
        (if just-url
            (setq weather-string (shell-command-to-string (concat otlb-gps-conditions-script-command " " (substring target-id 0 8) " " (substring target-id 9 13) " --url")))
          (setq weather-string (shell-command-to-string (concat otlb-gps-conditions-script-command " " (substring target-id 0 8) " " (substring target-id 9 13)))))
      (if just-url
          (setq weather-string (shell-command-to-string (concat otlb-gps-conditions-script-command " --current" (substring target-id 9 13) " --url")))
        (setq weather-string (shell-command-to-string (concat otlb-gps-conditions-script-command " --current " (substring target-id 9 13))))))
    ;; insert into the conditions and add degree symbol
    (if just-url
        (let ((browse-url-generic-program "conkeror"))
          (browse-url-generic weather-string))
      (strip-full (replace-regexp-in-string "C" "°C" weather-string)))))

;; TODO: one of the places that needs testing to make sure I'm counting properly
(defun otlb-gps-shoe-totals ()
  "Total the amount of mileage on each pair of shoes."
  (let ((latest (otlb-gps-get-latest-id))
        (last (otlb-gps-get-last-id))
        (gathered)
        (table-length)
        ids
        sums)
    ;; go through document and count all shoe ID's
    (with-current-file otlb-gps-pedestrian-location
      (do-otlb-gps-entries latest last gathered
                           (progn
                             (otlb-gps-find-actual-table-last-row)
                             (list (org-table-get nil 6) (org-table-get nil 2)))))
    (setq ids (delq nil (mapcar (lambda (e)
                                  (let ((shoe-id (ignore-errors (substring-no-properties (strip-full (cadr (split-string (car e)))))))
                                        (kms (ignore-errors (cadr e))))
                                    (when (and (cic:is-not-empty-string-nil shoe-id) kms)
                                      (list shoe-id kms))))
                                gathered)))
    (dolist (id ids)
      (let ((current-sum (assoc (car id) sums)))
        (if current-sum
            (progn
              (setcdr current-sum (cons
                                   (+ (cadr current-sum)
                                      (string-to-number (cadr id)))
                                   nil)))
          (progn
            (setq sums (append sums (list
                                     (list
                                      (car id)
                                      (string-to-number (cadr id))))))))))
    sums))

(defun otlb-gps-insert-shoe-totals ()
  "Insert the shoe totals into the file containing them."
  ;; TODO: fix
  (interactive)
  (let ((shoe-totals (otlb-gps-shoe-totals)))
    (with-current-file-min otlb-gps-footwear-current
      (dolist (shoe shoe-totals)
        ;; find the unique shoe ID
        (goto-char (point-min))
        (search-forward (car shoe))
        (org-table-put nil 11 (concat (format "%.2f km" (cadr shoe))))
        (org-table-align)))))

;; (with-current-buffer-create "*imagetest*" (insert-image (create-image (concat (car otlb-gps-locations) "/" (otlb-gps-get-id) ".png"))))
(defun otlb-gps-open-cached-osm ()
  "Open the current logbook entry using saved OSM map image."
  (interactive)
  ;; get the current ID
  (let* ((id (otlb-gps-get-id))
         ;; TODO: allow on secondary devices too
         (output-file (and (file-exists-p (concat (car otlb-gps-locations) "/" id ".png")) (concat (car otlb-gps-locations) "/" id ".png"))))
    ;; open image with feh
    (start-process "feh otlb" nil "nohup" "feh-open-browse" output-file)))

;; TODO: all gps locations automatically
(defun otlb-gps-open-google-earth ()
  "Open the current logbook entry on Google Earth."
  (interactive)
  ;; get the current ID
  (let* ((id (otlb-gps-get-id))
         ;; TODO: allow on secondary devices too
         (output-file (or (and (file-exists-p (concat (car otlb-gps-locations) "/" id ".fit")) (concat (car otlb-gps-locations) "/" id ".fit"))
                          (and (file-exists-p (concat (car otlb-gps-locations) "/" id ".tcx")) (concat (car otlb-gps-locations) "/" id ".tcx"))
                          (and (file-exists-p (concat (car otlb-gps-locations) "/" id ".gpx")) (concat (car otlb-gps-locations) "/" id ".gpx")))))
    ;; run script to convert to gpx and open in Google Earth
    (start-process "google earth" nil "nohup" otlb-gps-map-command output-file)))

(defun otlb-gps-map-preview ()
  "Preview the current logbook entry in an emacs buffer."
  ;; TODO: potentially open to window size
  ;; TODO: make in another frame
  (interactive)
  ;; TODO: make sure otlb-gps-get-id doesn't get on non-headings
  (when (eq last-command 'otlb-gps-map-preview)
    (outline-next-heading))
  (let ((id (otlb-gps-get-id)))
    (with-current-buffer-create "*otlb map preview*"
      ;; TODO: do I need this here?
      (special-mode)
      (setq buffer-read-only nil)
      (erase-buffer)
      (display-buffer (with-current-buffer-create "*otlb map preview*"))
      (insert-image (create-image (concat (car otlb-gps-locations) "/" id "-1280.png")))
      (image-mode)
      (goto-char (point-min))
      (setq buffer-read-only t))))

(defun otlb-gps-map-open ()
  "Open the map of current logbook entry in feh."
  (interactive)
  (let ((id (otlb-gps-get-id)))
    (start-process "feh map" "*otlb feh maps*" "feh" (concat (car otlb-gps-locations) "/" id "-1280.png"))))

(defun otlb-gps-graph-distance ()
  "Build a speed/elevation graph with respect to distance."
  (interactive)
  (let* ((id (otlb-gps-get-id))
         (output-file (or (and (file-exists-p (concat (car otlb-gps-locations) "/" id ".fit")) (concat (car otlb-gps-locations) "/" id ".fit"))
                          (and (file-exists-p (concat (car otlb-gps-locations) "/" id ".tcx")) (concat (car otlb-gps-locations) "/" id ".tcx"))
                          (and (file-exists-p (concat (car otlb-gps-locations) "/" id ".gpx")) (concat (car otlb-gps-locations) "/" id ".gpx")))))
    ;; run script to graph it
    ;; TODO: want to record output here too
    (start-process "graph" "*graph output*" "nohup" "python" otlb-gps-graph-fit-command output-file "--graph-fit-distance")))

(defun otlb-gps-graph-time ()
  "Build a speed/elevation graph with respect to elapsed time."
  (interactive)
  (let* ((id (otlb-gps-get-id))
         (output-file (or (and (file-exists-p (concat (car otlb-gps-locations) "/" id ".fit")) (concat (car otlb-gps-locations) "/" id ".fit"))
                          (and (file-exists-p (concat (car otlb-gps-locations) "/" id ".tcx")) (concat (car otlb-gps-locations) "/" id ".tcx"))
                          (and (file-exists-p (concat (car otlb-gps-locations) "/" id ".gpx")) (concat (car otlb-gps-locations) "/" id ".gpx")))))
    ;; run script to graph it
    ;; TODO: want to record output here too
    (start-process "graph" "*graph output*" "nohup" "python" otlb-gps-graph-fit-command output-file "--graph-fit-time")))

(defun otlb-gps-calc (lisp-table)
  "Calculated an updated lisp table from the LISP-TABLE
corresponding to an otlb-gps log entry."
  (let ((duration 0)
        (distance 0)
        pace-column
        (new-lisp-table (list (car lisp-table))))
    (dolist (lisp-row (butlast (cdr lisp-table)))
      (setq duration (+ duration (otlb-gps-string-to-duration (elt lisp-row 0))))
      (setq distance (+ distance (otlb-gps-string-to-distance (elt lisp-row 1))))
      (setq pace-column (nconc pace-column (list (otlb-gps-string-pace (elt lisp-row 0) (elt lisp-row 1))))))
    (dolist (current-lisp-row (cdr (butlast lisp-table)))
      (setq new-lisp-table
            (nconc
             new-lisp-table
             (list (nconc
                    (subseq current-lisp-row 0 2)
                    (list (pop pace-column))
                    (nthcdr 3 current-lisp-row))))))
    (setq new-lisp-table (nconc
                          new-lisp-table
                          (list
                           (nconc
                            (list
                             (otlb-gps-duration-to-string duration)
                             (otlb-gps-distance-to-string distance)
                             ;; TODO: merge with otlb-gps-string-pace so this calculation is only in one place
                             (otlb-gps-pace-to-string (/
                                                       (/ duration 60.0)
                                                       (/ distance 1000.0))))
                            (nthcdr 3 (car (last lisp-table)))))))
    new-lisp-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helper functions

(defun otlb-gps-get-latest-id ()
  "Get the more recent ID in the logbook.  Assumes logbook is
sorted."
  (with-current-file-min otlb-gps-pedestrian-location
    (otlb-gps-get-id-from-heading)))

(defun otlb-gps-get-last-id ()
  "Get the least recent ID in the logbook.  Assumes logbook is
sorted."
  (with-current-file otlb-gps-pedestrian-location
    (goto-char (point-max))
    (org-back-to-heading)
    (otlb-gps-get-id-from-heading)))

(defun otlb-gps-duration ()
  "Read a duration in hours, minutes, and seconds."
  (interactive)
  (let ((capture-duration (read-string "Enter duration HHMMSS: ")))
    (if (string=  (substring capture-duration 0 2) "00")
        (progn
          (let ((duration (concat (substring capture-duration 2 4) ":" (substring capture-duration 4 6))))
            ;; get rid of leading zeros
            (if (substring duration 0 1)
                (substring duration 1)
              duration))
          )
      (let ((duration (concat (substring capture-duration 0 2) "h" (substring capture-duration 2 4) ":" (substring capture-duration 4 6))))
        ;; get rid of leading zeros
        (if (substring duration 0 1)
            (substring duration 1)
          duration)))))

(defun otlb-gps-weeks-to-encode-time (weeks)
  "Encode WEEKS weeks into encoded time."
  (seconds-to-time (* 604800 weeks)))

(defun otlb-gps-days-to-encode-time (days)
  "Encode DAYS days into encoded time."
  (seconds-to-time (* 86400 days)))

(defun otlb-gps-id-to-date-components (id)
  "Get the date components from a GPS ID"
  (let ((id-year (substring id 0 4))
        (id-month (substring id 4 6))
        (id-day (substring id 6 8))
        (id-hour (substring id 9 11))
        (id-minute (substring id 11 13))
        (id-second (substring id 13 15)))
    (list id-year id-month id-day)))

(defun otlb-gps-id-to-encode-time (id)
  "Encode a GPS ID into encoded time."
  (let ((id-year (substring id 0 4))
        (id-month (substring id 4 6))
        (id-day (substring id 6 8))
        (id-hour (substring id 9 11))
        (id-minute (substring id 11 13))
        (id-second (substring id 13 15)))
    (encode-time (string-to-number id-second)
                 (string-to-number id-minute)
                 (string-to-number id-hour)
                 (string-to-number id-day)
                 (string-to-number id-month)
                 (string-to-number id-year))))

(defun otlb-gps-zulu-to-encode-time (zulu)
  "Encode zulu time ZULU into encoded time."
  (let ((zulu-year (substring zulu 0 4))
        (zulu-month (substring zulu 5 7))
        (zulu-day (substring zulu 8 10))
        (zulu-hour (substring zulu 11 13))
        (zulu-minute (substring zulu 14 16))
        (zulu-second (substring zulu 17 19)))
    (encode-time (string-to-number zulu-second)
                 (string-to-number zulu-minute)
                 (string-to-number zulu-hour)
                 (string-to-number zulu-day)
                 (string-to-number zulu-month)
                 (string-to-number zulu-year))))

(defun otlb-gps-encoded-time-to-id (time)
  "Decode encoded TIME into a GPS ID."
  (let ((decoded (decode-time time)))
    (format "%04d%02d%02dT%02d%02d%02d"
            (nth 5 decoded)
            (nth 4 decoded)
            (nth 3 decoded)
            (nth 2 decoded)
            (nth 1 decoded)
            (nth 0 decoded))))

(defun otlb-gps-id-sunday-midnight (id)
  "Get the time of ID back Sunday/Monday midnight."
  (let* ((encoded-time (otlb-gps-id-to-encode-time id))
         (seconds (float-time encoded-time))
         (decoded-time (decode-time encoded-time))
         (seconds-forward (* 86400 (mod (- 7 (elt decoded-time 6)) 7)))
         (sunday-seconds (+ seconds seconds-forward))
         (sunday-time (seconds-to-time sunday-seconds))
         (sunday-time-decoded (decode-time sunday-time)))
    (setcar (nthcdr 0 sunday-time-decoded) 59)
    (setcar (nthcdr 1 sunday-time-decoded) 59)
    (setcar (nthcdr 2 sunday-time-decoded) 23)
    ;; might need to limit stuff here
    (otlb-gps-encoded-time-to-id (apply 'encode-time sunday-time-decoded))))

(defun otlb-gps-id-midnight (id)
  "Get the time of ID back to previous midnight."
  (let* ((encoded-time (otlb-gps-id-to-encode-time id))
         (decoded-time (decode-time encoded-time))
         ;; get previous day
         ;; TODO: maybe do not go back if workout started at 23:59:59
         (new-day (- (elt decoded-time 3) 1)))
    (setcar (nthcdr 0 decoded-time) 59)
    (setcar (nthcdr 1 decoded-time) 59)
    (setcar (nthcdr 2 decoded-time) 23)
    (setcar (nthcdr 3 decoded-time) new-day)
    ;; might need to limit stuff here
    (otlb-gps-encoded-time-to-id (apply 'encode-time decoded-time))))

(defun otlb-gps-time-strip (time-string)
  "Get rid of extranous characters from a TIME-STRING."
  (replace-regexp-in-string "-\\|:\\|Z" "" time-string))

(defun otlb-gps-adjust-id-timezone (id)
  "Adjust an ID based on the standard time-zone adjustment."
  ;; convert id to encoded time
  (otlb-gps-encoded-time-to-id (time-add (otlb-gps-id-to-encode-time id) (seconds-to-time (* 3600 otlb-gps-time-zone)))))

(defun otlb-gps-backward-time (start-id time-range)
  "Figure out how far TIME-RANGE is backward from START-ID, and
  return a new id corresponding to this."
  (let ((encoded-start-time (otlb-gps-id-to-encode-time start-id)))
    (otlb-gps-encoded-time-to-id (time-subtract encoded-start-time time-range))))

(defun otlb-gps-next-entry ()
  "Go to the next entry, replacement for but faster than
  org-forward-heading-same-level."
  ;; TODO: use known tags but without any complications that could kill performance
  (end-of-line)
  (search-forward-regexp "^* .*:.*:.*:.*:")
  (beginning-of-line))

(defun otlb-gps-id-capture-insert ()
  "Insert a stored GPS ID at point."
  (interactive)
  otlb-gps-store-id)

(defun otlb-gps-get-id ()
  "Get GPS ID from the current headline."
  ;; goto id entry
  (save-excursion
    (while (not (ignore-errors (assoc "id" (org-entry-properties))))
      ;; TODO: replace with below, will not stop at end of buffer
      (forward-line))
    (strip-full-no-properties (cdr (assoc "id" (org-entry-properties))))))

(defun otlb-gps-get-id-from-heading ()
  "Get GPS ID when directly at the heading.  Meant to be used
programaticly."
  ;; TODO: get the GPS ID, make sure we do not pass over next heading
  ;; TODO: this can be made faster
  (save-excursion
    ;; TODO: use bound to find next heading
    (when (search-forward ":id:" nil t)
      (strip-full (elt (split-string (cic:get-current-line) ":") 2)))))

(defun otlb-gps-get-tag-from-heading ()
  "Get GPS tag when directly at the heading.  Meant to be used
programaticly."
  ;; TODO: get the GPS ID, make sure we do not pass over next heading
  (save-excursion
    ;; TODO should not be here I don't thing
    (org-back-to-heading)
    ;; TODO: use bound to find next heading
    (let ((current-line (cic:get-current-line)))
      (cond ((string-match ":running:" current-line)
             'running)
            ((string-match ":walking:" current-line)
             'walking)
            ((string-match ":invalid:" current-line)
             'invalid)))))

(defun otlb-gps-distance ()
  "Read in a distance in kilometers."
  (interactive)
  (let ((capture-distance (read-string "Enter kilometers: ")))
    (concat capture-distance " km")))

(defun otlb-gps-find-actual-table ()
  "Interim name for a function that does the right thing and gets
  us on the right table while being more efficient han built-in
  functions."
  ;; get us back to a heading
  (org-back-to-heading)
  ;; find the right table
  (search-forward ":RAW:" nil t)
  (search-forward ":END:" nil t)
  (forward-line 1))

(defun otlb-gps-table-last-row ()
  ;; XXXX: this cookie is always present in an otlb table
  (search-forward "#+TBL")
  (forward-line -2)
  (forward-char 3))

(defun otlb-gps-find-actual-table-last-row ()
  "Interim name for a function that does the right thing and gets
  us on the right table while being more efficient han built-in
  functions."
  (otlb-gps-find-actual-table)
  (otlb-gps-table-last-row))

(defun otlb-gps-now-id (&optional date)
  "Get the GPS ID now (current time) or from the encoded date DATE."
  (interactive)
  (unless date
    (setq date (current-time)))
  (otlb-gps-encoded-time-to-id date))

(defun otlb-gps-id-range-weekly (&optional start-id weeks-back)
  "Get the GPS ids going WEEKS-BACK weeks from START-ID."
  (let (ids)
    (dotimes (week (1+ weeks-back))
      (setq ids (cons (otlb-gps-n-week-id week (otlb-gps-id-to-encode-time start-id)) ids)))
    (nreverse ids)))

(defun otlb-gps-id-range-daily (&optional start-id days-back)
  "Get the GPS ids going back DAYS-BACK from START-ID."
  (let (ids)
    (dotimes (day (1+ days-back))
      (setq ids (cons (otlb-gps-n-day-id day (otlb-gps-id-to-encode-time start-id)) ids)))
    (nreverse ids)))

(defun otlb-gps-n-day-id (&optional n-day date)
  "Get the GPS id that is N-DAY days back from DATE. If not
specified N-DAY is 1 and DATE is the current encoded time."
  (unless n-day
    (setq n-day 1))
  (unless date
    (setq date (current-time)))
  (let ((day-subtract (otlb-gps-days-to-encode-time n-day)))
    (otlb-gps-encoded-time-to-id (time-subtract date day-subtract))))

(defun otlb-gps-n-week-id (&optional n-week date)
"Get the GPS id that is N-WEEK weeks back from DATE. If not
specified N-WEEK is 1 and DATE is the current encoded time."
  (unless n-week
    (setq n-week 1))
  (unless date
    (setq date (current-time)))
  (let ((week-subtract (otlb-gps-weeks-to-encode-time n-week)))
    (otlb-gps-encoded-time-to-id (time-subtract date week-subtract))))

(defun otlb-gps-n-month-id (&optional n-month date)
"Get the GPS id that is N-MONTH months back from DATE. If not
specified N-MONTH is 1 and DATE is the current encoded time."
  (unless n-month
    (setq n-month 1))
  (unless date
    (setq date (decode-time (current-time))))
  (let* ((the-time (apply 'vector date))
         (new-month (- (elt the-time 4) n-month))
         (new-year (if (< new-month 1)
                       (- (elt the-time 5) 1)
                     (elt the-time 5))))
    (aset the-time 4 (mod new-month 12))
    (aset the-time 5 new-year)
    (otlb-gps-encoded-time-to-id (apply 'encode-time (mapcar 'identity the-time)))))

(defun otlb-gps-n-year-id (&optional n-year date)
  "Get the GPS id that is N-YEAR years back from DATE. If not
specified N-YEAR is 1 and DATE is the current encoded time."
  (interactive)
  (unless n-year
    (setq n-year 1))
  (unless date
    (setq date (decode-time (current-time))))
  (let* ((the-time (apply 'vector date))
         (new-year (- (elt the-time 5) n-year)))
    (aset the-time 5 new-year)
    (otlb-gps-encoded-time-to-id (apply 'encode-time (mapcar 'identity the-time)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macro definitions

(defmacro do-otlb-gps-entries (start-id end-id accum &rest body)
  "Run BODY on all (sorted) enteries between START-ID and
END-ID."
  `(save-excursion
     (goto-char (point-min))
     ;; TODO: we do not want to check all enteries
     ;; (otlb-gps-sort)
     (let ((keep-going t))
       (setq ,accum nil)
       (while keep-going
         ;; is another heading to find
         (setq keep-going (search-forward-regexp "^\* .*" nil t))
         (when keep-going
           ;; is it a GPS entry
           (let ((current-id (condition-case nil (otlb-gps-get-id-from-heading) (error nil))))
             (when current-id
               (when (or (and (string< current-id ,start-id)
                              (string< ,end-id current-id))
                         (string= ,end-id current-id)
                         (string= current-id ,start-id))
                 (let ((the-body ,@body))
                   (when the-body
                     (setq ,accum (cons the-body ,accum))))))))))))

(provide 'otlb-gps)
