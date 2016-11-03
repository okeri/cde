;; Cde - C development environment for emacs
;; Copyright (C) 2016 Oleg Keri

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>
(require 'company-template)
(require 'cl-lib)

(defgroup cde nil "cde mode" :group 'c)

;; TODO: find a valid color theme )
(defface cde-hideif-face '((t :background "grey10"))
  "Face for shadowing ifdef blocks."
  :group 'cde)

(defface cde-error-face '((t :foreground "red"))
  "Face for marking compilation errors"
  :group 'cde)

(defface cde-warning-face '((t (:italic t :foreground "color-100")))
  "Face for marking compilation warnings"
  :group 'cde)

(defface cde-note-face '((t (:italic t :foreground "color-112")))
  "Face for marking compilation notes"
  :group 'cde)


;; user variables
(defcustom cde-command "cde"
  "Additional arguments could be passed to cde,
for example:
  'cde -C/tmp/cde' changes cde cache dir.

other switches:
  -P - enables PCH cache
  -G<path> - set current gcc location (-Gn for disable gcc includes lookup)")

(defcustom cde-debug nil "toggle debug buffer")
(defcustom cde-check 0 "syntax check delay, MUST be or zero or
larger than company-idle-delay for comfort usage")
(defcustom cde-disp-delay 0.2 "delay for showing diagnostic info")

;; internal variables
(defvar cde--ring '())
(defvar cde--ref-window nil)
(defvar cde--process nil)
(defvar cde--idle-timer nil)
(defvar cde--check-timer nil)
(defvar cde--lock-guard nil)

(defvar-local cde--project nil)
(defvar-local cde--callback nil)
(defvar-local cde--diags nil)
(defvar-local cde--last-line nil)
(defvar-local cde--buffer-mapped nil)
(defvar-local cde--start nil)
(defvar-local cde--completion-list nil)

(defconst cde--process-buffer " *Cde*")
(defconst cde--process-name "cde-process")
(defconst cde--include-re "^\#*\\s *include\\s +[<\"]\\(.*\\)[>\"]")


;; public functions
(defun cde-update-project()
  (interactive)
  (when cde--project
    (cde--check-map)
    (cde--send-command (concat "U " cde--project "\n"))))

(defun cde-header-source()
  (interactive)
  (when cde--project
    (cde--send-command (concat "F " cde--project " "
			       buffer-file-name "\n"))))

(defun cde-symbol-def()
  (interactive)
  (when cde--project
    (cde--check-map)
    (let ((line (buffer-substring-no-properties
		 (line-beginning-position) (line-end-position))))
      (if (string-match cde--include-re line)
	  (cde--send-command (concat "F " cde--project " "
				     buffer-file-name " "
				     (match-string 1 line) "\n"))
	(cde--send-command (concat "D " cde--project " "
				   buffer-file-name " "
				   (cde--sympos-string) "\n"))))))

(defun cde-symbol-ref()
  (interactive)
  (when cde--project
    (cde--check-map)
    (cde--send-command (concat "R " cde--project " " buffer-file-name " "
			       (cde--sympos-string) "\n"))))

(defun cde-ref-jmp()
  (interactive)
  (let* ((curr (buffer-substring-no-properties
		(line-beginning-position)
		(line-end-position)))
	 (parent curr)
	 (line))
    (save-excursion
      (while (string-match "\t" parent)
	(forward-line -1)
	(setq parent (buffer-substring-no-properties
		      (line-beginning-position)
		      (line-end-position)))))
    (if (equal curr parent)
	(setq line 0)
      (setq line (string-to-int
		  (substring curr 0
			     (string-match "\t" curr)))))
    (find-file-other-window parent)
    (goto-char (point-min))
    (when (not (eq line 0))
      (forward-line (1- line)))))

(defun cde-symbol-back()
  (interactive)
  (let ((pos (car cde--ring)))
    (if pos (prog1 (find-file (car pos))
	      (goto-char (point-min))
	      (forward-char (nth 1 pos))
	      (setq cde--ring (cdr cde--ring)))
      (dframe-message "Jump history is empty"))))

(defun cde-compile()
  "Suggest to compile of project directory"
  (interactive)
  (when (or (not (boundp 'compile-history))
	    (= (length compile-history) 0))
    (setq compile-history '("make -k ")))
  (when cde--project
    (let ((curr (concat "make -k -C " cde--project " ")))
      (unless (catch 'found
		(dolist (v compile-history)
		  (when (string-prefix-p curr v)
		    (throw 'found t)))
		nil)
	(push curr compile-history))))
  (when (> (length compile-history) 0)
      (setq compile-command (car compile-history)))
  (execute-extended-command nil "compile"))

(defun cde-quit()
  (when cde--process
      (process-send-string cde--process "Q\n"))
  t)

(define-minor-mode cde-mode "cde"  nil  " cde" nil :group 'cde
  (if cde-mode (cde--init) (cde--deinit)))

(defun company-cde(command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-cde))
    (prefix (and cde-mode cde--project
		 (not (company-in-string-or-comment))
		 (company-grab-symbol)))
    (candidates (cons :async
		      'cde--candidates))
    (annotation (get-text-property 0 'anno arg))
    (meta (get-text-property 0 'meta arg))
    (post-completion (let ((anno (get-text-property 0 'anno arg)))
		       (when anno
			 (insert anno)
			 (company-template-c-like-templatify
			  (concat arg anno)))))
    (sorted t)))

;; private functions
(defun cde--sympos()
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (if bounds (car bounds) nil)))

(defun cde--sympos-string()
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (if bounds
	(int-to-string (car bounds)) nil)))

(defun cde--ref-setup(items)
  (let ((refbuf (get-buffer-create "references")))
    (when (not (window-live-p cde--ref-window))
      (delete-other-windows)

      (setq cde--ref-window (split-window-right))
      (set-window-buffer cde--ref-window refbuf)

      (select-window cde--ref-window))

    (with-current-buffer refbuf
      (setq-local buffer-read-only nil)
      (erase-buffer)
      (dolist (item items)
	(if (listp item)
	    (insert (propertize (format "%5d|" (car item)) 'face 'linum) "\t"
		    (nth 1 item) "\n")
	  (insert (propertize item 'face 'font-lock-type-face) "\n")))
      (setq-local buffer-read-only t)
      (goto-char (point-min))
      (local-set-key (kbd "RET") 'cde-ref-jmp))))

(defun cde--check-map()
  (unless cde--buffer-mapped
    (setq-local cde--buffer-mapped t)
    (cde--send-command (concat "M " buffer-file-name " "
			       (int-to-string (buffer-size))
			       "\n" (buffer-string)))))

(defun cde--unmap()
  (when cde--buffer-mapped
    (cde--send-command (concat "M " buffer-file-name "\n"))))

(defun cde--check-handler()
  (when cde-mode
    (when (timerp cde--check-timer)
      (cancel-timer cde--check-timer))
    (if (not cde--lock-guard)
	(prog1
	  (cde--check-map)
	  (setq cde--check-timer nil)
	  (cde--lock t)
	  (cde--send-command (concat "B " cde--project " "
				     buffer-file-name "\n")))
      (setq cde--check-timer
      	    (run-at-time cde-check nil #'cde--check-handler)))))

(defun cde--change (start end len)
  (setq-local cde--buffer-mapped nil)
  (when (and cde-mode  cde--project (> cde-check 0)
	     (not (company-in-string-or-comment)))
    (when (timerp cde--check-timer)
      (cancel-timer cde--check-timer))
    (setq cde--check-timer
	  (run-at-time cde-check nil #'cde--check-handler))))

(defun cde--deinit()
  (when (timerp cde--check-timer)
    (cancel-timer cde--check-timer))
  (when (timerp cde--idle-timer)
    (cancel-timer cde--idle-timer))
  (setq write-file-functions (delete 'cde--unmap write-file-functions))

  (remove-hook 'after-change-functions 'cde--change)
  (remove-hook 'company-completion-started-hook 'cde--lock)
  (remove-hook 'company-completion-cancelled-hook 'cde--unlock)
  (remove-hook 'company-completion-finished-hook 'cde--unlock)
  (cde-quit))

(add-hook 'kill-emacs-query-functions 'cde-quit)

(defun cde--init()
  (unless cde--process
    (let ((process-connection-type nil)
          (process-adaptive-read-buffering nil))
      (setq cde--process
	    (apply 'start-process (append (list cde--process-name
						cde--process-buffer)
					  (split-string cde-command))))

      (when cde-debug
	(get-buffer-create "cde-dbg")
	(buffer-disable-undo "cde-dbg"))

      (buffer-disable-undo cde--process-buffer)
      (set-process-query-on-exit-flag cde--process nil)
      (set-process-sentinel cde--process 'sent)
      (set-process-filter cde--process 'cde--handle-output))
    (setq cde--idle-timer
	  (run-with-idle-timer cde-disp-delay t #'cde--error-disp))
    (push 'cde--unmap write-file-functions)
    (add-hook 'after-change-functions 'cde--change)
    (add-hook 'company-completion-started-hook 'cde--lock)
    (add-hook 'company-completion-cancelled-hook 'cde--unlock)
    (add-hook 'company-completion-finished-hook 'cde--unlock))
  (cde--send-command (concat "A " buffer-file-name "\n")))

(defun cde--unlock(dummy)
  (setq cde--lock-guard nil))

(defun cde--lock(dummy)
  (setq cde--lock-guard t))

(defun cde--handle-output(process output)
  (let ((doeval nil) (cmds))
    (when cde-debug
      (with-current-buffer "cde-dbg"
	(insert output)
	(goto-char (point-max))))
    (with-current-buffer cde--process-buffer
      (insert output)
      (goto-char (point-max))
      (setq doeval (> (line-number-at-pos) 1)))
    (when doeval
      (with-current-buffer cde--process-buffer
	(setq cmds (car (read-from-string
			 (concat "(progn " (buffer-string) ")"))))
	(erase-buffer))
      (eval cmds))))

(defun cde--send-command(cmd)
  (when cde--process
    (process-send-string cde--process cmd)))

(defun cde--handle-completions(completions-pack)
  (setq cde--completion-list nil)
  (dolist (comp completions-pack)
    (setq-local cde--completion-list
		(nconc cde--completion-list
			(list (propertize (substring (nth 0 comp)
						     (nth 1 comp)
						     (nth 2 comp))
					  'anno (substring (nth 0 comp)
							   (nth 2 comp)
							   (nth 3 comp))
					  'meta (nth 0 comp))))))
  (funcall cde--callback cde--completion-list))

(defun cde--candidates(callback)
  (let ((pos (or (cde--sympos) (point)))
  	(prefix (company-grab-symbol)))
    (if (and (boundp 'cde--start) (eq pos cde--start))
  	(let ((completions '()))
  	  (dolist (completion cde--completion-list)
  	    (when (string-prefix-p prefix completion)
  	      (setq completions (nconc completions (list completion)))))
  	  (funcall callback completions))
      (progn
  	(setq-local cde--callback callback)
  	(setq-local cde--start pos)
  	(cde--check-map)
  	(cde--send-command (concat "C " cde--project " "
  				   buffer-file-name " "
  				   prefix " "
  				   (int-to-string (line-number-at-pos pos)) " "
  				   (int-to-string
  				    (save-excursion (goto-char pos)
  						    (current-column))) "\n"))))))

(defun cde--line-to-pt(line)
  (save-excursion
    (goto-char (point-min))
    (forward-line line)
    (point)))

(defun cde--line-bounds(line)
  (save-excursion
    (goto-char (point-min))
    (forward-line (- line 1))
    (list (point) (+ (line-end-position) 1))))

(defun cde--hideif(file ranges)
  (let ((buf (get-file-buffer file)))
    (when buf
      (with-current-buffer buf
	(dolist (r ranges)
	  (let ((start (cde--line-to-pt (nth 0 r)))
		(end (cde--line-to-pt (nth 1 r))))
	    (remove-overlays start end 'cde--hide-ifdef t)
	    (let ((o (make-overlay start end)))
	      (overlay-put o 'cde--hide-ifdef t)
	      (overlay-put o 'face 'cde-hideif-face))))))))

(defun cde--error-disp()
  (let* ((line (line-number-at-pos))
  	 (current (assq line cde--diags)))
    (unless cde--lock-guard
      (if current
	  (progn
	    (setq-local cde--last-line line)
	    (message (nth 2 current)))
	(when (eq line cde--last-line)
	  (setq-local cde--last-line nil)
	  (message ""))))))

;; we do not need cache results because reparse will be called on opening new
;; file

(defun cde--hl-line(line level)
  (let* ((bounds (cde--line-bounds line))
	 (start (nth 0 bounds))
	 (end (nth 1 bounds))
	 (o (make-overlay start end)))
    (overlay-put o 'cde--diag t)
    (cond ((= level 1)
	   (overlay-put o 'face 'cde-note-face))
	  ((= level 2)
	   (overlay-put o 'face 'cde-warning-face))
	  ((= level 3)
	   (overlay-put o 'face 'cde-error-face)))))

(defun cde--ack(file project)
  (with-current-buffer (get-file-buffer file)
    (setq-local cde--project project)
    (setq-local cde--buffer-mapped t)))

(defun cde--error-rep(&optional errors &optional regulars &optional links)
  (cde--unlock t)
  (when (> cde-check 0)
    (let ((project cde--project))
      (dolist (buf (buffer-list))
	(with-current-buffer buf
	  (when (and cde-mode (equal project cde--project))
	    (setq-local cde--diags nil)
	    (remove-overlays nil nil 'cde--diag t)))))
    (if errors
	(dolist (pos regulars)
	  (let* ((file (nth 0 pos))
		 (buf (get-file-buffer file)))
	    (dolist (diag (cdr pos))
	      (let* ((data (nth 1 diag))
		     (line (nth 0 diag))
		     (level (nth 0 data))
		     (index (nth 1 data))
		     (msg (aref errors index)))
		(when buf
		  (with-current-buffer buf
		    (cde--hl-line line level)
		    (push (cons line (list level msg)) cde--diags)))
		(aset errors index (concat file ":" (int-to-string line)
					   ": " msg)))))
	  (dolist (pos links)
	    (let ((buf (get-file-buffer (nth 0 pos))))
	      (when buf
		(with-current-buffer buf
		  (dolist (diag (cdr pos))
		    (let* ((data (nth 1 diag))
			   (line (nth 0 diag))
			   (level (nth 0 data)))
		      (cde--hl-line line level)
		      (push (cons line (list level (aref errors (nth 1 data))))
			    cde--diags))))))))))
    (cde--error-disp))

(provide 'cde)
