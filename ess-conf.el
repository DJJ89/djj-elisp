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

;;; *  Commentary:

;;;  ess configuration


(require 'boolcase-mode)

(defun ess/format-dput (start end)
  "remove pointer from dput(data.table)"
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char end)
    (while (re-search-backward ".internal.*>," nil t 1)
      (replace-match "" nil nil))
    ))

;; favor horizontal split of window
(setq split-height-threshold 80)
(setq ess-auto-width-visible nil)

(use-package ess-site
  :init
  (require 'ess-site)
  (setq ess-r-company-backends
      '((company-R-library company-R-args company-R-objects company-files  company-id-backend company-dabbrev company-yasnippet :separate)))

  (setq ess-r-package-library-paths "/usr/local/lib/R/site-library")
  ;  (setq ess-eval-visibly "nowait")
  (setq ess-eval-visibly nil)
  (setq ess-ask-for-ess-directory nil)
  (setq ess-use-flymake nil)
  (setq ess-auto-width nil)
  (setq ess-history-directory "~/.R/")
  (setq inferior-R-args "--no-save")
  (setq ess-indent-with-fancy-comments nil)
  (setq ess-tab-always-indent t)
  (setq ess-style 'RStudio)
  (setq inferior-ess-replace-long+ 'strip)
  (setq inferior-ess-r-program "c:/Users/JeremieJuste/Documents/R/R-4.1.1/bin/x64/Rterm")
  (add-to-list 'exec-path "c:/Users/JeremieJuste/Documents/R/R-4.1.1/bin/")
  ;; (setq inferior-ess-r-program "c:/Users/JeremieJuste/Documents/R/R-4.2.0/bin/x64/Rterm")
  ;; (add-to-list 'exec-path "c:/Users/JeremieJuste/Documents/R/R-4.2.0/bin/")

  (add-hook 'ess-mode-hook (lambda ()
			     (yas-activate-extra-mode 'inferior-ess-r-mode)))
  (add-hook 'inferior-ess-r-mode-hook (lambda ()
					(yas-activate-extra-mode 'ess-r-mode)))

  :chords
  (:map ess-mode-map
	("»«" . c-quote)	
	(",." . ess-eval-buffer))
  :hook
  ((ess-r-mode . rainbow-delimiters-mode)
   (ess-r-mode . company-mode)
   (ess-r-mode . outshine-mode)
   (ess-r-mode . boolcase-mode)
   (ess-r-mode . ess-debug-minor-mode))

  :bind
  (:map ess-mode-map
	(("C-'" . corral-double-quotes-backward-quick))
	("C-z" . ess-describe-object-at-point))
  (:map inferior-ess-mode-map
	(("C-'" . corral-double-quotes-backward-quick))
	("C-z" . ess-describe-object-at-point))

  )




(defun corral-double-quotes-backward-quick ()
 (interactive)
 (save-excursion
   (corral-double-quotes-backward)
   )
 (forward-char ))


(defun corral-single-quotes-backward-quick ()
 (interactive)
 (save-excursion
   (corral-single-quotes-backward)
   )
 (forward-char ))

(setq ess-smart-S-assign-key nil)
(defun dumb-assign ()
  (interactive)
  (insert " <- "))
(key-chord-define ess-r-mode-map "-=" 'dumb-assign)



(key-chord-define inferior-ess-mode-map  "qe"  'djj-ess-describe-object-on-line)
(key-chord-define inferior-ess-mode-map  "aq"  'ess-describe-object-at-point)
(key-chord-define inferior-ess-mode-map "qw" 'yas-expand)


(defun r-summary-at-point ()
  "from https://emacs.stackexchange.com/questions/58554/emacs-ess-apply-function-to-token-under-point"
  (interactive)
  (let ((sym (ess-symbol-at-point)))
    (if sym
        (ess-send-string (get-buffer-process (ess-get-process-buffer)) 
                         (concat "summary(" (symbol-name sym) ")\n") t)
      (message "No valid R symbol at point"))))

(define-key ess-r-mode-map (kbd "C-c :") 'r-summary-at-point)

(defun djj-ess-narrow-to-defun ()
  (interactive)
  (ess-narrow-to-defun-or-para)
  )

(defun djj/interrupt-process ()
  "Interrupt current process see also ess-interupt"
  (interactive)
  (save-excursion
    (interrupt-process
     (ess-get-process-buffer) comint-ptyp)
    (inferior-ess-input-sender (get-buffer-process (ess-get-process-buffer)) "")
))


(defun djj/clean-console ()
  (interactive)
  (save-excursion
    (set-buffer (ess-get-process-buffer))
    (comint-clear-buffer)
    
    ))

(defun djj/enveloppe (query-string)
  "Wrap an expression in a function entered by QUERY-STRING"
  (interactive "M")
  (comint-bol )
  (add-hook 'minibuffer-setup-hook (lambda () (company-mode 1))) 
 (insert (concat query-string "("))
 (end-of-line)
 (insert ")")
 (backward-char))


(defun djj/select-function-expression ()
  "mark for instance unique( .* ), test_data(*) entirely"
  (interactive)
  (save-excursion
  (mark-sexp -2)
  ))


(defun ess/quote-expression (beg end)
  "double quote each element in region (a, b, c) => c('a','b','c')"
  (set-fill-column 500)
  (fill-paragraph)
  (interactive "r")
  (let ((end (+ 3 end)))
    (goto-char beg)
    (insert "c")
    (forward-char 1)   
    (insert "\""))
  (while (re-search-forward ", *$*" end t)
    (replace-match "\", \"")
    (setq end (+ 3 end)))
  (end-of-line)
  (backward-char)
  (insert "\"")
  (end-of-line)
  (set-fill-column 80)
  (fill-paragraph)
  )

(defun djj/r-ace-jump-describe-object-at-point (query-char)
  (interactive (list (read-char "Query Char:")))
  (save-excursion
    (progn
      (avy-goto-word-1 query-char )
      (ess-describe-object-at-point))))



(defhydra hydra-ess (:columns 4)
  "ess"
  ("e"  ess/toggle-error-on-warning "toggle")  
  ("h"  hl-line-mode "hl-line")
  ("r"  ess-parse-errors "get to first error")
  ("w"  ess/widen "widen")
  ("Q"  ess/quote-expression "quote-exp: (a, b, c)")  
  ("w"  djj-ess-ace-jump-describe-object-at-point "jump and describe")
  ("s" djj/select-function-expression "select function exp")
)

(define-key inferior-ess-mode-map (kbd "<f7>") 'hydra-ess/body)




(defhydra hydra-r-ess (:columns 3)
  "ess"
  ("r"  ess-parse-errors "get to first error")
  ("R"  ess-resynch "refresh completion cache")
  ("d"  ref-debug "debug")
  ("p"  goto-pbi  "goto pbi file")
  ("Q"  ess/quote-expression "quote-exp: (a, b, c)")
  ("e"  djj/eval-no-assign-r-fun "eval before assign")
  ("w"  djj-ess-ace-jump-describe-object-at-point "jump and describe")
  ("s" djj/select-function-expression "select function exp")
  ("g"  goto-output "goto output dir")
  ("t"  test "test" )  
)

(define-key ess-r-mode-map
   (kbd "<f7>") 'hydra-r-ess/body)

;; * company backend

(setq ess-r-company-backends
      '((company-R-library company-R-args company-R-objects company-files  company-id-backend company-dabbrev company-yasnippet :separate)))

(defun djj/create-process-named-test ()
  "Create an iess R process and name its buffer *test*"
  (progn (ess-start-process-specific nil "R")	    
	   (switch-to-buffer
	    (process-buffer (get-process (caar ess-process-name-list))))
	   (rename-buffer "*test*")))

(defun djj/run-test (arg)
  "Execute tiny test in a dedicated iESS buffer called *test*."
  (interactive "p")
  (let ((current (current-buffer))
	(path (djj/find-path (djj/add-test-prefix (buffer-name)))))
  (save-excursion
    (unless (get-buffer "*test*")	
      (djj/create-process-named-test))
    (switch-to-buffer "*test*")
    (end-of-buffer)
    (if (< arg 4)
      (insert (concat "library(tinytest); run_test_file(\"" path "\")"))
    (insert " library(tinytest);test_all(\"d:/repos/validatedata/\")"))
    (comint-send-input)
    (switch-to-buffer current))
  ))

(global-set-key (kbd "<C-f12>") 'djj/run-test)

(defun djj/add-test-prefix (arg)
  "Just add test prefix to string ARG if it does not exist."
  (if (string-match "^test" arg)
      arg
    (concat "test-" arg)))

(defun toggle-test-prefix ()
  (interactive)
  (let ((current-buffer (buffer-name)))
    (if (string-match "^test" current-buffer)
	(replace-regexp-in-string "test-" "" current-buffer)
      (concat "test-" current-buffer))))

(defun djj/find-path (arg)
  (let ((res))
    (if (string-match "^test" arg)
	(concat (projectile-acquire-root) "tests/tinytest/" arg)
      (concat (projectile-acquire-root)  arg)))
  )
(defun find-corresponding-other-window ()
  "Find test or script file"
  (interactive)
  (let* ((required-buffer (toggle-test-prefix)))
    (if (get-buffer required-buffer)
	(switch-to-buffer required-buffer)
      (find-file (djj/find-path required-buffer)))))

;; evaluation

(defun djj/eval-no-assign-r-fun ()
  "send a region to R console wihout the assign exp <-"
  (interactive)
  (save-excursion
      (let ((end  (progn (end-of-line)  (point))))  
      (re-search-backward "<- " (r/get-begin-of-line-point))
      (forward-char 3)      
      (ess-eval-region (point) end 4)))
  )


(defun ess/toggle-error-on-warning ()
  (interactive)
  (insert "options(warn=2)")  
  )
(defun ess/widen  ()
  (interactive)
  (insert "options(width=180)")  
  )

(defun ref-debug ()
  "Inserts debug(fn) above fn"
  (interactive)
  (set-mark (point))
  (let ((bg (point)))
    (search-forward "(")
    (backward-char 1)
    (copy-region-as-kill bg (point) )
    (line-move -1)
    (insert "debug(")
    (yank)
    (insert ")"))
  )


(provide 'ess-conf)

