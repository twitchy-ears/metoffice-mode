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

(defvar metoffice-mode-current-weather-short "[W]"
  "Caches the short details of the current weather, normally shown in the modeline")

(defvar metoffice-mode-current-weather-details ""
  "Caches the expanded details of the current weather, normally shown in a tooltip")

(defvar metoffice-mode-update-in-seconds 3600
  "Seconds between running metoffice-mode-update-current-weather")

(defvar metoffice-mode-update-timer nil
  "Holds the timer for refreshing the weather")

(defvar metoffice-mode-now-and-next t
  "By default fetch now and next weather, set to nil to make the string contain just the weather occuring now")

(defvar metoffice-mode-weather-type-to-symbol t
  "Controls if the weather type of now/next weather is displayed in the modeline as a unicode symbol from the metoffice-mode-weather-type-to-symbol-table")

(defvar metoffice-mode-weather-type-to-symbol-table
  '(("Clear night" . "☽")
    ("Sunny day" . "☀")
    ("Partly cloudy (night)" . "🌥")
    ("Partly cloudy (day)" . "🌤")
    ("Not used" . "☠")
    ("Mist" . "🌫")
    ("Fog" . "🌫")
    ("Cloudy" . "☁")
    ("Overcast" . "☁")
    ("Light rain shower (night)" . "🌦")
    ("Light rain shower (day)" . "🌧")
    ("Drizzle" . "🌦")
    ("Light rain" . "🌦")
    ("Heavy rain shower (night)" . "🌧☂")
    ("Heavy rain shower (day)" . "🌧☂")
    ("Heavy rain" . "🌧☂")
    ("Sleet shower (night)" . "🌧☃")
    ("Sleet shower (day)" . "🌧☃") 
    ("Sleet" . "🌧☃")
    ("Hail shower (night)" . "🌧☃")
    ("Hail shower (day)" . "🌧☃")
    ("Hail" . "🌧☃")
    ("Light snow shower (night)" . "☃")
    ("Light snow shower (day)" . "☃")
    ("Light snow" . "☃")
    ("Heavy snow shower (night)" . "☃")
    ("Heavy snow shower (day)" . "☃")
    ("Heavy snow" . "☃")
    ("Thunder shower (night)" . "🌩")
    ("Thunder shower (day)" . "🌩")
    ("Thunder" . "☈"))
  "Alist of weather types to output for when metoffice-mode-weather-type-to-symbol is t")

(defvar metoffice-mode-colourise-output t
  "Set to t if you want to colourise your output which will be blue/red based on the temperature you set in metoffice-mode-cold-temp-threshold")

(defvar metoffice-mode-cold-temp-threshold 19
  "If the temperature is this or below then the modeline will show blue, otherwise red.  Yes it should be like a neat gradient between the two but I am very lazy")

(defun metoffice-mode-day-or-night ()
  "Returns if we are in a 'day' (07:00 -> 19:00) or 'night' (19:00 -> 07:00) time"
  (let ((hour (nth 2 (decode-time))))
    (if (and (>= hour 6) ; simple 12-hour split, in absence of sunrise/sunset data from ephemeris etc
             (<= hour 18))
        'day
      'night)))

(defun metoffice-mode-inverse-day-or-night ()
  "Returns the inverse of metoffice-mode-day-or-night, so night if we're in day or day if we're in night"
  (let ((dn-now (metoffice-mode-day-or-night)))
    (if (eq dn-now 'day)
        'night
      'day)))

(defun metoffice-mode-weather-to-symbol (wt temp)
  "Takes a weather type and looks it up using the metoffice-mode-weather-type-to-symbol-table and returns whatever it finds"
  (let* ((text (cdr (assoc wt metoffice-mode-weather-type-to-symbol-table)))
         (bgcolour (if (> temp metoffice-mode-cold-temp-threshold)
                    'warm
                    'cold)))

    ;; Should probably be customisable
    (if metoffice-mode-colourise-output
        (if (eq bgcolour 'warm)
            (propertize text 'face '(:foreground "firebrick3"))
          (propertize text 'face '(:foreground "DeepSkyBlue4")))
;;            (propertize text 'face '(:background "firebrick3" :foreground "white"))
;;          (propertize text 'face '(:background "SkyBlue" :foreground "black")))
      text)))

;defun metoffice-mode-modeline-string-output (fmt weather-types)
; (condition-case nil
;     (format fmt weather-types)
;   (error (progn
;            (message "metoffice-mode: metoffice-mode-modeline-string-output(%s, %s): error with formatting" fmt weather-types)
;            ["W"]))))

(defun metoffice-mode-get-current-weather-type (&optional siteid)
  "Fetches just the current weather-type as a string using metoffice.el, also stops the output of the message to the status line"
  (let* ((location (or siteid metoffice-home-location))
         (inhibit-message t)
         (data (metoffice-get-site-period-weather location 0) 'weather-type)
         (wt (metoffice-weather-aspect data 'weather-type))
         (dn-now (metoffice-mode-day-or-night))

         (temp (if (eq dn-now 'day)
                   (metoffice-weather-aspect
                    data
                    'feels-like-day-maximum-temperature)
                 (metoffice-weather-aspect
                  data
                  'feels-like-night-minimum-temperature)))

         (mltext (if metoffice-mode-weather-type-to-symbol
                     (metoffice-mode-weather-to-symbol wt temp)
                   metoffice-mode-current-weather-short)))
    
    (cons mltext wt)))

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
         (wt-next (metoffice-weather-aspect (cdr data-next) 'weather-type))
         
         (temp-now (if (eq dn-now 'day)
                       (metoffice-weather-aspect
                        (cdr data-now)
                        'feels-like-day-maximum-temperature)
                     (metoffice-weather-aspect
                      (cdr data-now)
                      'feels-like-night-minimum-temperature)))
                      
         (temp-next (if (eq dn-next 'day)
                        (metoffice-weather-aspect
                         (cdr data-next)
                         'feels-like-day-maximum-temperature)
                      (metoffice-weather-aspect
                       (cdr data-next)
                       'feels-like-night-minimum-temperature))))


    ;; (message "wt-now '%s' wt-next '%s'" wt-now wt-next)
    
    (cons (if metoffice-mode-weather-type-to-symbol
              (format "[%s|%s]"
                      (metoffice-mode-weather-to-symbol wt-now temp-now)
                      (metoffice-mode-weather-to-symbol wt-next temp-next))
            metoffice-mode-current-weather-short)
          
          (format "%s, feels like %i°C (%s %s)\n%s feels like %i°C (%s %s)"
                  wt-now temp-now date-now dn-now
                  wt-next temp-next date-next dn-next))))

(defun metoffice-mode-update-current-weather (&optional siteid)
  "Updates the metoffice-mode-current-weather cache string, obeys metoffice-mode-now-and-next as a variable to determine which function it runs between metoffice-mode-now-and-next-weather-type or metoffice-mode-get-current weather-type

Run from a timer when the mode is enabled every metoffice-mode-update-in-seconds time"
  (let* ((data (if metoffice-mode-now-and-next
                   (metoffice-mode-get-now-and-next-weather-type siteid)
                 (metoffice-mode-get-current-weather-type siteid))))
    (progn
      ;; (message "Setting short '%s' details '%s'" (car data) (cdr data))
      (setq metoffice-mode-current-weather-short (car data))
      (setq metoffice-mode-current-weather-details (cdr data)))))

(defun metoffice-mode-cached-weather (&optional siteid)
  "Returns the current metoffice-mode-current-weather cache string or if its unset or empty sets it correctly"
  (if (and (boundp 'metoffice-mode-current-weather-details)
           (> (length metoffice-mode-current-weather-details) 0))
      metoffice-mode-current-weather-details
    (progn
      (metoffice-mode-update-current-weather siteid)
      metoffice-mode-current-weather-details)))

(defun metoffice-mode-weather-details (&optional siteid)
  (interactive)
  (message (metoffice-mode-cached-weather)))

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
  (propertize metoffice-mode-current-weather-short 'help-echo metoffice-mode-current-weather-details))
;;  #("[W]"
;;    0
;;    3
;;    (help-echo
;;     (metoffice-mode-cached-weather))))

(define-minor-mode metoffice-mode
  "When enabled will show a [W] in the modeline with a tooltip
  for current weather and refreshes it periodically"
  nil
  nil
  nil
  :global t
  (if metoffice-mode
      (progn
        (metoffice-mode-update-current-weather)
        (setq mode-line-misc-info (cons '(:eval (metoffice-mode-output-modeline)) mode-line-misc-info))
        (setq metoffice-mode-update-timer
              (run-with-timer metoffice-mode-update-in-seconds
                              metoffice-mode-update-in-seconds
                              (lambda () (metoffice-mode-update-current-weather)))))
    (progn
      (setq mode-line-misc-info (cl-remove-if (lambda (k) (cl-search "(:eval (metoffice-mode-output-modeline))" (format "%s" k))) mode-line-misc-info))
      (cancel-timer metoffice-mode-update-timer)
      (setq metoffice-mode-update-timer nil))))

(provide 'metoffice-mode)


(define-minor-mode metoffice-mode
  "When enabled will show a [W] in the modeline with a tooltip
  for current weather and refreshes it periodically"
  nil
  nil
  nil
  :global t
  (if metoffice-mode
      (progn
        (metoffice-mode-update-current-weather)
        (setq mode-line-misc-info (cons '(:eval (metoffice-mode-output-modeline)) mode-line-misc-info))
        (setq metoffice-mode-update-timer
              (run-with-timer metoffice-mode-update-in-seconds metoffice-mode-update-in-seconds
                              (lambda () (metoffice-mode-update-current-weather)))))
    (progn
      (setq mode-line-misc-info (cl-remove-if (lambda (k) (cl-search "(:eval (metoffice-mode-output-modeline))" (format "%s" k))) mode-line-misc-info))
      (cancel-timer metoffice-mode-update-timer)
      (setq metoffice-mode-update-timer nil))))

(provide 'metoffice-mode)


