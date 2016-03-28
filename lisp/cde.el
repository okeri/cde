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
(require 'hideif) ;; for font variable

;; user variables
(defvar cde-args ""
  "Additional arguments could be passed to cde,
for example:
  'cde -C/tmp/cde' changes cde cache dir.

other switches:
  -P - enables PCH cache
  -G<path> - set current gcc location (-Gn for disable gcc includes lookup)")

(defvar cde-debug nil "toggle debug buffer")

;; internal variables
(defvar cde--ring '())
(defvar cde--ref-window nil)
(defvar cde--process nil)
(defvar-local cde--project nil)
(defvar-local cde--callback nil)
(defvar-local cde--buffer-changed nil)
(defconst cde--process-buffer " *Cde*")
(defconst cde--process-name "cde-process")
(defconst cde--include-re "^\#*\\s *include\\s +[<\"]\\(.*\\)[>\"]")


(add-hook 'kill-emacs-query-functions 'cde-try-quit)

;; public functions
(defun cde-update-project()
  (interactive)
  (when cde--project
    (cde--send-command (concat "U " cde--project "\n"))))


(defun cde-header-source()
  (interactive)
  (when cde--project
    (cde--send-command (concat "F " cde--project " "
			       buffer-file-name "\n"))))

(defun cde-symbol-def()
  (interactive)
  (when cde--project
    (let ((line (buffer-substring-no-properties
		 (line-beginning-position) (line-end-position))))
      (if (string-match cde--include-re line)
	  (cde--send-command (concat "F " cde--project " "
				       buffer-file-name " "
				       (match-string 1 line) "\n"))
	(cde--send-command (concat "D " cde--project " "
				   buffer-file-name " " (cde--sympos-string)
				   (when (buffer-modified-p)
				     (concat " " (cde--expr-to-inp
						  cde--buffer-changed)
					     " " (cde--buffer-size) "\n"
					     (buffer-string))) "\n"))))))


(defun cde-symbol-ref()
  (interactive)
  (when cde--project
    (cde--send-command (concat "R " cde--project " " buffer-file-name " "
			       (cde--sympos-string)
			       (when (buffer-modified-p)
				   (concat " " (cde--expr-to-inp
						cde--buffer-changed)
					   " " (cde--buffer-size) "\n"
					   (buffer-string))) "\n"))))

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
    (setq compile-history '("make -k "))
    (when cde--project
      (push (concat "make -k -C " cde--project " ") compile-history))
    (if (> (length compile-history) 0)
	(setq compile-command (car compile-history))))
  (execute-extended-command nil "compile"))


(defun cde-init()
  (unless cde--process
    (let ((process-connection-type nil)
          (process-adaptive-read-buffering nil))
      (setq cde--process
            (start-process cde--process-name cde--process-buffer
			   "cde" cde-args)
	    cde--hold t)
      (when cde-debug
	(get-buffer-create "cde-dbg")
	(buffer-disable-undo "cde-dbg"))

      (buffer-disable-undo cde--process-buffer)
      (set-process-query-on-exit-flag cde--process nil)
      (set-process-sentinel cde--process 'sent)
      (set-process-filter cde--process 'cde--handle-output)))
  (cde--send-command (concat "A " buffer-file-name "\n"))
  (add-hook 'after-change-functions
	    (lambda (start end len)
	      (setq cde--buffer-changed t)) nil t))

;;; temprotary
(defun sent(process event)
  (message "Process quit!!!")
  (setq cde--process nil))

(defun cde-try-quit()
  (if cde--process
      (prog2 (process-send-string cde--process "Q\n") nil) t))


;;;###autoload
(defun company-cde(command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-cde))
    (prefix (and (derived-mode-p 'c++-mode) (cons (cde--prefix) t)))
    (candidates (cons :async
		      'cde--candidates))
    (annotation (get-text-property 0 'anno arg))
    (meta (get-text-property 0 'meta arg))
    (post-completion (let ((anno (get-text-property 0 'anno arg)))
		       (when anno
			 (insert anno)
			 (company-template-c-like-templatify
			  (concat arg anno)))))
    (no-cache t)
    (sorted t)))


;; private functions
(defun cde--sympos()
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (if bounds (car bounds) nil)))


(defun cde--sympos-string()
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (if bounds
	(int-to-string (car bounds)) nil)))


(defun cde--buffer-size()
  (int-to-string (- (point-max) (point-min))))


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
      (local-set-key (kbd "RET") 'cde--ref-jmp))))


;; TODO: rewrite this function aims to convert build output to
;; compile_commands.json
(defun cde--parse-prj()
  (interactive)
  (let ((logfile (read-file-name "build log: ")))
    (when logfile
      (let ((includes '())
	    (defines '())
	    (incfile nil)
	    (words (with-temp-buffer
		     (insert-file-contents logfile)
		     (split-string (buffer-string)))))
	(dolist (exp words)
	  (cond (incfile
		 (push (concat "-include " exp) includes)
		 (setq incfile nil))
		((string-prefix-p "-include" exp)
		 (setq incfile t))
		((string-prefix-p "-I" exp)
		 (push exp includes))
		((or (string-prefix-p "-D" exp)
		     (string-prefix-p "-std" exp))
		 (let* ((epos (string-match-p "=" exp))
			(def (if epos (substring exp 0 epos) exp)))
		   (if (catch 'found
			 (dolist (v defines)
			   (if (string-prefix-p def v)
			       (throw 'found nil)))
			 t)
		       (push exp defines))))))
	(delete-dups includes)
	(save-excursion
	  (let ((buf (find-file-noselect cde--project-file)))
	    (set-buffer buf)
	    (erase-buffer)
	    (dolist (inc defines)
	      (princ (concat inc " ") buf))
	    (princ "\n" buf)
	    (dolist (inc includes)
	      (princ (concat inc "\n") buf))
	    (save-buffer)
	    (kill-buffer)))))))

(defun cde--line-to-pt(line)
  (save-excursion
    (goto-char (point-min))
    (forward-line line)
    (point)))

;; emacs build-in hideif compatible ?
;; while show-ifdefs works fine, hide-ifdefs failed because hide-ifdef-env
;; is empty and it's not local... the possible solution is export preprocessor
;; directives from c++ side and allow hideif to manage it.
(defun cde--hideif(ranges)
  (dolist (r ranges)
    (let ((start (cde--line-to-pt (nth 0 r)))
	  (end (cde--line-to-pt (nth 1 r))))
      (remove-overlays start end 'cde--hide-ifdef t)
      (let ((o (make-overlay start end)))
	(overlay-put o 'cde--hide-ifdef t)
	(overlay-put o 'face 'hide-ifdef-shadow)))))

;; TODO: when cde process throws multiple messages with newlines
;; this could work wrong
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
	(erase-buffer)
	)
      (eval cmds))))


(defun cde--send-command(cmd)
  (when cde--process
    (process-send-string cde--process cmd)))


(defun cde--candidates(callback)
  (setq-local cde--callback callback)
  (if cde--project
    (let ((pos (or (cde--sympos) (point))))
      (cde--send-command (concat "C " cde--project " "
				 buffer-file-name " "
				 (cde--prefix) " "
				 (int-to-string (line-number-at-pos pos)) " "
				 (int-to-string
				  (save-excursion (goto-char pos)
						  (current-column))) " "
				 (cde--buffer-size) "\n"
				 (buffer-string))))
    (funcall callback '())))


(defun cde--prefix()
  (if (not (company-in-string-or-comment))
      (or (let ((bounds (bounds-of-thing-at-point 'symbol)))
	      (if bounds (buffer-substring-no-properties
			  (car bounds) (cdr bounds)) "")) "")))

(defun cde--expr-to-inp(expr)
  (if expr "1" "0"))

(provide 'cde)
