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

(require 'cl-lib)
(require 'company-template)

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
    (sorted t)
    (no-cache t)))

(add-hook 'cde-mode-hook
	  (lambda()
	    (add-hook 'company-completion-started-hook 'cde--lock)
	    (add-hook 'company-completion-cancelled-hook 'cde--unlock)
	    (add-hook 'company-completion-finished-hook 'cde--unlock)))

(add-hook 'cde-mode-exit-hook
	  (lambda()
	    (remove-hook 'company-completion-started-hook 'cde--lock)
	    (remove-hook 'company-completion-cancelled-hook 'cde--unlock)
	    (remove-hook 'company-completion-finished-hook 'cde--unlock)))


(provide 'company-cde)
