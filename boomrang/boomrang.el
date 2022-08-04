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

;; This program is a small utility function but has been of great
;; value to me Sometime auto-completion is not fast or easy enough. If
;; you want to just copy any work symbol or sentence on the screen
;; without having to move your cursor this will do a pretty good job.
;; This program heavily depends on avy, (building on the shoulders of
;; giants :-)). I learnt since then that avy is much more profound
;; than what meets the
;; eye. https://karthinks.com/software/avy-can-do-anything/

(defun djj/avy-goto-word-1 (char &optional arg beg end symbol)
  "Jump to the currently visible CHAR at a word start.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.
When SYMBOL is non-nil, jump to symbol start instead of word start."
  (interactive (list (read-char "char: " t)
                     current-prefix-arg))
  (avy-with avy-goto-word-1
    (let* ((str (string char))
           (regex (cond ((string= str ".")
                         "\\.")
                        ((and avy-word-punc-regexp
                              (string-match avy-word-punc-regexp str))
                         (regexp-quote str))
                        ((<= char 26)
                         str)
                        (t
                         (concat
                          (if symbol "\\_<" "\\b")
                          str)))))
      (avy-jump regex
                :beg beg
                :end end))))



(defun djj/copy-region-as-kill-wo-append (beg end &optional region)
  "Save the region as if killed, but don't kill it.
In Transient Mark mode, deactivate the mark.
If `interprogram-cut-function' is non-nil, also save the text for a window
system cut and paste.

The copied text is filtered by `filter-buffer-substring' before it is
saved in the kill ring, so the actual saved text might be different
from what was in the buffer.

When called from Lisp, save in the kill ring the stretch of text
between BEG and END, unless the optional argument REGION is
non-nil, in which case ignore BEG and END, and save the current
region instead.

This command's old key binding has been given to `kill-ring-save'."
  ;; Pass mark first, then point, because the order matters when
  ;; calling `kill-append'.
  (interactive (list (mark) (point)
		     (prefix-numeric-value current-prefix-arg)))
  (let ((str (if region
                 (funcall region-extract-function nil)
               (filter-buffer-substring beg end))))
  (kill-new str))
  (setq deactivate-mark t)
  nil)

(defun boomrang (p query-char )
 "Kill word and come back to point.
 Argument QUERY-CHAR The first character of the word to be copied."
    (interactive (list (prefix-numeric-value current-prefix-arg)
		       (read-char "Query Char:")))
    (let ((this-buffer (current-buffer)))
    (cond  ((eq p 1) (setq pattern "[[:alnum:]-\\./_\\$]+"))
	   ((eq p 4) (setq pattern ".+$"))
	   (t (setq pattern "[[.:alnum:]_-\\$]*")))
    (save-excursion
      (djj/avy-goto-word-1 query-char nil)
      (setq other-buffer (current-buffer))
      (djj/copy-region-as-kill-wo-append (point) (when (re-search-forward pattern
									  (point-max) t)
									  (point))))
    (yank)
    (if (not (eq other-buffer this-buffer))
      (switch-to-buffer-other-window this-buffer))))


(defun boomrang2 (p query-char )
 "Kill word and come back to point.
 Argument QUERY-CHAR The first character of the word to be copied."
    (interactive (list (prefix-numeric-value current-prefix-arg)
		       (read-char "Query Char:")))
    (let ((this-buffer (current-buffer)))
    (cond  ((eq p 4) (setq pattern "[[:alnum:]_-\\.]+"))	    	   
	   ((eq p 1) (setq pattern ".*\"+"))
	   (t (setq pattern "[[.:alnum:]_-\\$]*")))
    (save-excursion
      (djj/avy-goto-word-1 query-char nil)
      (setq other-buffer (current-buffer))
      (djj/copy-region-as-kill-wo-append (point) (when (re-search-forward pattern
									  (point-max) t)
									  (point))))
    (yank)
    (if (not (eq other-buffer this-buffer))
      (switch-to-buffer-other-window this-buffer))))


(provide 'boomrang)


