;;; metoffice-mode.el --- shows metoffice weather information in the modeline

;; Copyright 2020- Twitchy Ears

;; Author: Twitchy Ears https://github.com/twitchy-ears/
;; URL: https://github.com/twitchy-ears/metoffice-mode
;; Version: 0.1
;; Package-Requires ((metoffice "<2017-09-20 20:49:05 jcgs>"))
;; Keywords: metoffice, weather

;; This file is not part of GNU Emacs.

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

;;; History
;;
;; 2020-08-28 - initial version

;;; Commentary:
;;
;; Get metoffice.el working first:
;; https://github.com/hillwithsmallfields/JCGS-emacs/blob/master/information-management/metoffice.el
;;
;; Then you should probably use the ever useful use-package
;;
;; (use-package metoffice
;;   :init (setq metoffice-home-location "xxxxxx"
;;               metoffice-api-key "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")
;;   :config (metoffice-setup))
;; 
;; (use-package metoffice-mode
;;   :requires metoffice
;;   :config (metoffice-mode))
;;
;; And made sure that ~/.metoffice-config.el contains
;; (setq metoffice-api-key "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")
;;
;;
;;; Alternatively do it by hand, not tested but something like this?
;; (setq metoffice-home-location "xxxxxx"
;;      metoffice-api-key "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")
;; (metoffice-setup)
;; (require 'metoffice)
;; (require 'metoffice-mode)
;; (metoffice-mode)
;;
;; Honestly I'd just try use-package as that's actually tested properly.

(eval-when-compile
  (require 'metoffice)
  (with-no-warnings (require 'cl-lib)))

(defvar metoffice-mode-current-weather nil
  "Caches the current weather-type from the metoffice")

(defvar metoffice-mode-update-in-seconds 3600
  "Seconds between running metoffice-mode-update-current-weather")

(defvar metoffice-mode-update-timer nil
  "Holds the timer for refreshing the weather")

;; Not needed since we're now inserting into mode-line-misc-info
;;(defvar metoffice-mode-modeline-location 1
;;  "Position in the modeline, defaults to 1 because that seems to work")

(defvar metoffice-mode-now-and-next t
  "By default fetch now and next weather, set to nil to make the string contain just the weather occuring now")

(defun metoffice-mode-day-or-night ()
  "Returns if we are in a 'day' (06:00 -> 18:00) or 'night' (18:00 -> 06:00) time"
  (let ((hour (nth 2 (decode-time))))
    (if (and (>= hour 6) ; simple 12-hour split, in absence of sunrise/sunset data from ephemeris etc
             (<= hour 18))
        'day
      'night)))

(defun metoffice-mode-inverse-day-or-night ()
  "Returns the inverse of metoffice-mode-day-or-night, so night if we're in day or day if we're in night"
  (let ((dn-now (metoffice-mode-day-or-night)))
    (if (eq dn-now 'day)
        'night)
    'day))

(defun metoffice-mode-get-current-weather-type (&optional siteid)
  "Fetches just the current weather-type as a string using metoffice.el, also stops the output of the message to the status line"
  (let* ((location (or siteid metoffice-home-location))
         (inhibit-message t))
    (metoffice-weather-aspect (metoffice-get-site-period-weather location 0) 'weather-type)))

(defun metoffice-mode-get-now-and-next-weather-type (&optional siteid)
  "Returns a string showing the current weather (and date + day/night) as well as the next block of times weather - so the current days night if this is a day period and the next days day if this is a night period"
  (let* ((inhibit-message t)
         (data (metoffice-get-site-weather siteid))
         (dn-now (metoffice-mode-day-or-night))
         (dn-next (metoffice-mode-inverse-day-or-night))
         (date-now (car (nth 0 data)))
         (date-next (if (eq dn-next 'day)
                        (car (nth 1 data))
                      date-now))
         (data-now (assoc dn-now (nth 0 data)))
         (data-next (if (eq dn-next 'day)
                        (assoc dn-next (nth 1 data))
                      (assoc dn-next (nth 0 data))))
         (wt-now (metoffice-weather-aspect (cdr data-now) 'weather-type))
         (wt-next (metoffice-weather-aspect (cdr data-next) 'weather-type)))
        
    (format "%s (%s %s)\n%s (%s %s)" wt-now date-now dn-now wt-next date-next dn-next)))

(defun metoffice-mode-update-current-weather (&optional siteid)
  "Updates the metoffice-mode-current-weather cache string, obeys metoffice-mode-now-and-next as a variable to determine which function it runs between metoffice-mode-now-and-next-weather-type or metoffice-mode-get-current weather-type

Run from a timer when the mode is enabled every metoffice-mode-update-in-seconds time"
  (setq metoffice-mode-current-weather (if metoffice-mode-now-and-next
                                           (metoffice-mode-get-now-and-next-weather-type siteid)
                                         (metoffice-mode-get-current-weather-type siteid))))

(defun metoffice-mode-cached-weather (&optional siteid)
  "Returns the current metoffice-mode-current-weather cache string or if its unset or empty sets it correctly"
  (if (and (boundp 'metoffice-mode-current-weather)
           (> (length metoffice-mode-current-weather) 0))
      metoffice-mode-current-weather
    (progn
      (metoffice-mode-update-current-weather siteid)
      metoffice-mode-current-weather)))

;; This section seems to work okay with the standard modeline just
;; sort of sitting on its own but it felt like a bodge - I've added
;; the horrible stuff to manipulate mode-line-misc-info because that
;; seems portable between standard modeline and doom-modeline and
;; feels more like the right thing to do ... although the method of
;; doing it is also kind of a bodge.
;;
;;;; Remove the mode string should it be there for some reason
;;(setq mode-line-position (assq-delete-all 'metoffice-mode mode-line-position))
;;
;;;; Add/Readd
;;(setq mode-line-position
;;      (append
;;       mode-line-position
;;       '((metoffice-mode
;;          (metoffice-mode-modeline-location
;;           (:eval (lambda ()
;;                    #("[W]" 0 3 (help-echo
;;                                 (metoffice-mode-cached-weather))))))
;;          nil))))

(defun metoffice-mode-output-modeline ()
  #("[W]" 0 3 (help-echo
               (metoffice-mode-cached-weather))))

(define-minor-mode metoffice-mode
  "When enabled will show a [W] in the modeline with a tooltip
  for current weather and refreshes it periodically"
  nil
  nil
  nil
  :global t
  (if metoffice-mode
      (progn
        (setq mode-line-misc-info (cons '(:eval (metoffice-mode-output-modeline)) mode-line-misc-info))
        (setq metoffice-mode-update-timer
              (run-with-timer 0 metoffice-mode-update-in-seconds
                              (lambda () (metoffice-mode-update-current-weather)))))
    (progn
      (setq mode-line-misc-info (cl-remove-if (lambda (k) (cl-search "(:eval (metoffice-mode-output-modeline))" (format "%s" k))) mode-line-misc-info))
      (cancel-timer metoffice-mode-update-timer)
      (setq metoffice-mode-update-timer nil))))

(provide 'metoffice-mode)


