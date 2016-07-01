;;; swedish-holidays.el --- Additional Swedish holidays -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Free Software Foundation, Inc.

;; Author: Gustaf Waldemarson <gustaf.waldemarson@gmail.com>
;; Keywords: convenience, calendar, holidays
;; Created: 2016-07-01
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package contains a number of additional holidays suitable for Swedish
;; calendars.

;;; Code:

(eval-when-compile
  (require 'calendar)
  (require 'holidays))


(defvar swedish-calendar-holidays
  '((holiday-fixed 1 1 "Nyårsdagen")
    (holiday-fixed 1 6 "Trettondedag jul")
    (holiday-fixed 1 13 "Tjogondag Knut")

    ;; Easter
    (holiday-easter-etc -47 "Fettisdagen")
    (holiday-easter-etc -46 "Askonsdagen")
    (holiday-easter-etc  -3 "Skärtorsdagen")
    (holiday-easter-etc  -2 "Långfredagen")
    (holiday-easter-etc  -1 "Påskafton")
    (holiday-easter-etc   0 "Påskdagen")
    (holiday-easter-etc  +1 "Annandag påsk")
    (holiday-easter-etc +39 "Kristi himmelfärdsdag")
    (holiday-easter-etc +49 "Pingstdagen")
    (holiday-easter-etc +50 "Annandag pingst")

    (holiday-fixed 4 30 "Valborgsmässoafton")
    (holiday-fixed 5 1 "Första maj")
    (holiday-fixed 6 6 "Sveriges Nationaldag")

    (holiday-sexp '(calendar-gregorian-from-absolute
                    (1- (calendar-dayname-on-or-before
                         6 (calendar-absolute-from-gregorian
                            (list 6 26 year)))))
                  "Midsommarafton")
    (holiday-sexp '(calendar-gregorian-from-absolute
                    (calendar-dayname-on-or-before
                     6 (calendar-absolute-from-gregorian
                        (list 6 26 year))))
                  "Midsommardagen")
    (holiday-sexp '(calendar-gregorian-from-absolute
                    (calendar-dayname-on-or-before
                     6 (calendar-absolute-from-gregorian
                        (list 11 6 year))))
                  "Alla helgons dag")

    (holiday-float 12 0 -4 "Första advent" 24)
    (holiday-float 12 0 -3 "Andra advent"  24)
    (holiday-float 12 0 -2 "Tredje advent" 24)
    (holiday-float 12 0 -1 "Fjärde advent" 24)
    (holiday-fixed 12 10 "Nobeldagen")
    (holiday-fixed 12 13 "Lucia")

    (holiday-fixed 12 24 "Julafton")
    (holiday-fixed 12 25 "Juldagen")
    (holiday-fixed 12 26 "Annandag jul")
    (holiday-fixed 12 31 "Nyårsafton"))
  "List of often celebrated holidays in Sweden.")


(provide 'swedish-holidays.el)

;;; swedish-holidays.el ends here
