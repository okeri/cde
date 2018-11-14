;; Cde - C development environment for emacs
;; Copyright (C) 2016-2018 Oleg Keri

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

(when (featurep 'cde-ref-ivy)
      (error "cde-ref is not compatible with cde-ref-ivy"))

(require 'cde)

(defvar cde--ref-window nil)

(defcustom cde-ref-display-function 'split-window-right
  "Function returns window for display references"
  :type 'symbol
  :group 'cde)
  
(defvar cde-ref-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'cde-ref-jmp) map)
  "Keymap for cde-ref mode")

(define-derived-mode cde-ref-mode nil "cde references"
  "Major mode for cde references navigation\\{cde-ref-mode-map}")

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
      (setq line (string-to-number
		  (substring curr 0
			     (string-match "\t" curr)))))
    (find-file-other-window parent)
    (goto-char (point-min))
    (when (not (eq line 0))
      (forward-line (1- line)))))

(defun cde--ref-setup(items)
  (let ((refbuf (get-buffer-create "references"))
	(current ""))
    (when (not (window-live-p cde--ref-window))
      (delete-other-windows)
      (setq cde--ref-window (funcall cde-ref-display-function))
      (set-window-buffer cde--ref-window refbuf)

      (select-window cde--ref-window))

    (with-current-buffer refbuf
      (setq-local buffer-read-only nil)
      (cde-ref-mode)
      (erase-buffer)
      (dolist (item items)
	(when (not (equal (nth 0 item) current))
	  (progn
	    (setq current (nth 0 item))
	    (insert (propertize current 'face 'font-lock-type-face) "\n")))
	(insert (propertize (format "%5d" (nth 1 item)) 'face 'font-lock-comment-face) "\t"
		(nth 2 item) "\n"))

      (setq-local buffer-read-only t)
      (goto-char (point-min)))))

(provide 'cde-ref)
