;;; blorg.el --- Static Site Generator for org-mode; -*- lexical-binding: t -*-
;;
;; Author: Lincoln Clarete <lincoln@clarete.li>
;;
;; Copyright (C) 2020  Lincoln Clarete
;;
;; This program is free software: you can redistribute it and/or modify
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Genenrate static websites off of Org Mode sources.
;;
;;; Code:

(require 'seq)
(add-to-list 'load-path "~/src/github.com/clarete/templatel/")
(require 'templatel)
(setq debug-on-error t)

(defun blorg-gen (&rest options)
  "Generate HTML setup with OPTIONS."
  (let* ((opt (seq-partition options 2))
         (base-dir (--blorg-get opt :base-dir default-directory))
         (input-pattern (--blorg-get opt :input-pattern "org$"))
         (output (--blorg-get opt :output "output"))
         (template (--blorg-get opt :template nil))
         (template-dir (--blorg-get opt :template-dir (file-name-directory template)))
         (env (templatel-env-new)))
    (dolist (tpl (blorg--find-source-files template-dir ".html$"))
      (templatel-env-add-template env tpl (templatel-new-from-file tpl)))
    (dolist (input-file (blorg--find-source-files base-dir input-pattern))
      (templatel-env-render env template '())
      (message "%s: %s" input-file output))))

(defun blorg--find-source-files (directory match)
  "Find MATCH files to be exported recursively in DIRECTORY."
  (let (output-files)
    (dolist (file (directory-files-and-attributes directory t))
      (cond
       ((string-match match (car file))
        (setq output-files (cons (car file) output-files)))
       ((eq t (car (cdr file)))
        (if (not (equal "." (substring (car file) -1)))
            (setq output-files
                  (append
                   (blorg--find-source-files (car file) match)
                   output-files))))))
    output-files))

(defun --blorg-get (lst sym default)
  "Pick SYM from LST or return DEFAULT."
  (let ((val (assoc sym lst)))
    (if val
        (cadr val)
      default)))

(provide 'blorg)
;;; blorg.el ends here
