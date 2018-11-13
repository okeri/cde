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

(require 'cde)
(require 'counsel)

(defalias 'cde-symbol-ref-ivy #'cde-symbol-ref)

(defun cde-symbol-ref()
  "Find references"
  (interactive)
  (when cde--project
    (cde--check-map)
    (cde--send-command (concat "R " cde--project " " buffer-file-name " "
			       (cde--sympos-string) "\n"))))

(defun cde--ref-setup(items)
  (let (cands '())
    (dolist (item items)
      (setq cands (append cands (list (format "%s:%d:%s" (nth 0 item) (nth 1 item) (nth 2 item))))))
      
    (ivy-read "References" cands
	      :action #'counsel-git-grep-action
	      :caller 'cde--ref-setup)))

(ivy-set-display-transformer 'cde--ref-setup 'counsel-git-grep-transformer)

(provide 'cde-ivy)
