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


