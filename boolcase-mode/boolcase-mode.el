;; boolcase-mode.el

;; Author: Jeremie Juste
;; Created: 05-31-2022

;; This file is part of GNU Emacs.

;;; License:
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>

;;; Commentary:

;; This minor mode is a modified version of Daniel Gopar "https://www.youtube.com/watch?v=QaX3AaK3_Lk"
;; I just modified this slightly to work for R instead of python
;; The minor mode for ESS can be added in the following way (add-hook 'ess-r-mode  'boolcase-mode)
;; It has been a great ressource to create a first minor mode.

(defvar boolcase-mode-words '("true" "false")
  "Words to capitalize")

(defun boolcase-mode-check ()
  (if (= last-command-event) 101)
  (boolcase-mode-fix))

(defun boolcase-mode-fix ()
  (save-excursion
    (copy-region-as-kill (point) (progn (backward-sexp) (point)))
    (when (member (current-kill 0) boolcase-mode-words)
      (upcase-word 1))
    ;; Remove element rom kill ring
    (setq kill-ring (cdr kill-ring)))
  )

(define-minor-mode boolcase-mode
  "Automatically capitalize booleans"
  :lighter " BC"
  (if boolcase-mode
      (add-hook 'post-self-insert-hook
		'boolcase-mode-check nil t)
    (remove-hook 'post-self-insert-hook
		 'boolcase-mode-check t))
  )

(provide 'boolcase-mode)
