;;; ob-visa.el --- Babel Functions for VISA

;; Author: Ferdinand Pieper <mail@pie.tf>
;; Keywords: literate programming, reproducible research

;; Copyright (c) 2022 Ferdinand Pieper

;; License: GPL v3, or any later version
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-Babel support for communicating with Virtual Instrument
;; Software Architecture (VISA) resources. Currently optimized to
;; run on Windows under WSL.

;;; Requirements:

;; - pyvisa-shell

;;; Code:

(require 'ob)

(defcustom org-babel-visa-command "pyvisa-shell.exe"
  "Name of the command to pyvisa-shell.")

(defvar org-babel-visa-delay 0.1)

(defun org-babel-visa-initiate-session (&optional session _params)
  "Initiate a pyvisa-shell sesion."
  (let* ((process-connection-type nil) ;; use communication per pipe
         (sessionname (if (or (not session) (string= session "none"))
                          "VISA" session))
         (session (make-comint sessionname org-babel-visa-command)))
    session))

;; This is a copy of org-babel-comint-with-output only with a small
;; delay added before output is expected. Should find a way to clean
;; this up sometime..
(defmacro org-babel-visa-comint-with-output (meta &rest body)
  "Evaluate BODY in BUFFER and return process output.
Will wait until EOE-INDICATOR appears in the output, then return
all process output.  If REMOVE-ECHO and FULL-BODY are present and
non-nil, then strip echo'd body from the returned output.  META
should be a list containing the following where the last two
elements are optional.

 (BUFFER EOE-INDICATOR REMOVE-ECHO FULL-BODY)

This macro ensures that the filter is removed in case of an error
or user `keyboard-quit' during execution of body."
  (declare (indent 1) (debug (sexp body)))
  (let ((buffer (nth 0 meta))
	(eoe-indicator (nth 1 meta))
	(remove-echo (nth 2 meta))
	(full-body (nth 3 meta)))
    `(org-babel-comint-in-buffer ,buffer
       (let* ((string-buffer "")
	      (comint-output-filter-functions
	       (cons (lambda (text) (setq string-buffer (concat string-buffer text)))
		     comint-output-filter-functions))
	      dangling-text)
	 ;; got located, and save dangling text
	 (goto-char (process-mark (get-buffer-process (current-buffer))))
	 (let ((start (point))
	       (end (point-max)))
	   (setq dangling-text (buffer-substring start end))
	   (delete-region start end))
	 ;; pass FULL-BODY to process
	 ,@body
	 ;; wait for end-of-evaluation indicator
         (sit-for org-babel-visa-delay)
	 (while (progn
		  (goto-char comint-last-input-end)
		  (not (save-excursion
			 (and (re-search-forward
			       (regexp-quote ,eoe-indicator) nil t)
			      (re-search-forward
			       comint-prompt-regexp nil t)))))
	   (accept-process-output (get-buffer-process (current-buffer))))
	 ;; replace cut dangling text
	 (goto-char (process-mark (get-buffer-process (current-buffer))))
	 (insert dangling-text)

	 ;; remove echo'd FULL-BODY from input
	 (when (and ,remove-echo ,full-body
		    (string-match
		     (replace-regexp-in-string
		      "\n" "[\r\n]+" (regexp-quote (or ,full-body "")))
		     string-buffer))
	   (setq string-buffer (substring string-buffer (match-end 0))))
	 (split-string string-buffer comint-prompt-regexp)))))

(defun org-babel-visa-replace-vars (body vars)
  "Expand BODY according to VARS.
Variabes in the BODY need to be prefixed with '$' and followed by
a space or newline."
  (let ((old-body ""))
    (mapc (lambda (pair)
            (setq body (replace-regexp-in-string
                        (format "\\$%s\\([ ]\\)\\|\\$%s$" (car pair)
                                (car pair))
                        (format "%s\\1" (cdr pair))
                        body)))
          vars)
    body))

(defun org-babel-expand-body:visa (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let ((vars (org-babel--get-vars params))
        (prologue (cdr (assq :prologue params)))
        (epilogue (cdr (assq :epilogue params))))
    (setq body (org-babel-visa-replace-vars body vars))
    ;; add prologue/epilogue
    (when prologue (setq body (concat prologue "\n" body)))
    (when epilogue (setq body (concat body "\n" epilogue)))
    body))

(defun org-babel-execute:visa (body params)
  "Execute a block of VISA code with Babel.
This function is called by `org-babel-execute-src-block'."
  (let ((result-params (cdr (assq :result-params params)))
        (result-type (cdr (assq :result-type params)))
        (session (org-babel-visa-initiate-session
                  (cdr (assq :session params))))
        (comint-prompt-regexp "^(visa\\|open) ")
        (eoe-string "org-babel-eoe")
        (replaced-body (org-babel-expand-body:visa body params)))
    (mapconcat
     #'identity
     (butlast
      (cdr
       (split-string
        (mapconcat
         #'org-trim
         (org-babel-visa-comint-with-output
             (session "(visa) *** Unknown syntax: org-babel-eoe" t)
           (mapc (lambda (line)
	           (insert (org-babel-chomp line)) (comint-send-input nil t))
	         (list replaced-body
                       eoe-string))
           ) "\n") "[\r\n]+")) 2)
     "\n")))


(provide 'ob-visa)
;;; ob-visa.el ends here
