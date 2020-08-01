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

(require 'ox-html)
(require 'seq)
(require 'templatel)

(defmacro --blorg-prepend (seq item)
  "Prepend ITEM to SEQ."
  `(setq ,seq (cons ,item ,seq)))

(defun --blorg-log-info (msg &rest vars)
  "Report MSG (formatted with VARS) to log level info."
  (message
   "%s INFO %s"
   (format-time-string "%Y-%m-%d %H:%M:%S")
   (apply 'format (cons msg vars))))

(defun blorg-cli (&rest options)
  "Generate HTML setup with OPTIONS.

This function is very similar to `blorg-gen', but has the
aditional feature of catching syntax and file-missing errors and
show them in a slightly nicer way."
  (condition-case exc
      (apply 'blorg-gen options)
    (templatel-error
     (message "Syntax Error: %s" (cdr exc)))
    (file-missing
     (message "%s: %s" (car (cddr exc)) (cadr (cddr exc))))))

(defun blorg-gen (&rest options)
  "Generate HTML setup with OPTIONS."
  (let* ((opt (seq-partition options 2))
         (base-dir (--blorg-get opt :base-dir default-directory))
         (input-pattern (--blorg-get opt :input-pattern "org$"))
         (output (--blorg-get opt :output "output/{{ slug }}/index.html"))
         (template (--blorg-get opt :template nil))
         (template-dir (--blorg-get opt :template-dir (file-name-directory template)))
         (env (templatel-env-new
               :importfn #'(lambda(en name)
                             (templatel-env-add-template
                              en name
                              (templatel-new-from-file (concat base-dir name)))))))

    ;; Add output template to the environment
    (templatel-env-add-template
     env
     (file-name-nondirectory template)
     (templatel-new-from-file template))
    ;; Find all input files and apply the template
    (--blorg-process-org-files `((env ,env)
                                 (base-dir ,base-dir)
                                 (input-pattern ,input-pattern)
                                 (template ,template)
                                 (output ,output)))))

(defun --blorg-process-org-files (blorg)
  "Fing input files and template them up with config in BLORG."
  (let ((env (--blorg-get blorg 'env))
        (base-dir (--blorg-get blorg 'base-dir))
        (template (--blorg-get blorg 'template))
        (input-pattern (--blorg-get blorg 'input-pattern))
        (output (--blorg-get blorg 'output)))
    (dolist (input-file (blorg--find-source-files base-dir input-pattern))
      (let* ((vars (--blorg-parse-org input-file))
             (template-name (file-name-nondirectory template))
             (rendered (templatel-env-render env template-name vars))
             (rendered-output (templatel-render-string output (cdr (assoc "post" vars))))
             (final-output (format "%s%s" base-dir rendered-output)))
        (--blorg-log-info "writing: %s" final-output)
        (mkdir (file-name-directory final-output) t)
        (write-region rendered nil rendered-output)))))

(defun --blorg-parse-org (input-file)
  "Read the generated HTML & metadata of the body of INPUT-FILE."
  (let (html keywords)
    (advice-add
     'org-html-template :override
     #'(lambda(contents _i) (setq html contents)))
    (advice-add
     'org-html-keyword :before
     #'(lambda(keyword _c _i)
         (--blorg-prepend
          keywords
          (cons
           (downcase (org-element-property :key keyword))
           (org-element-property :value keyword)))))
    (with-temp-buffer
      (insert-file-contents input-file)
      (org-html-export-as-html))
    (ad-unadvise 'org-html-template)
    (ad-unadvise 'org-html-keyword)

    (let ((slug (--blorg-get-cdr keywords "title" input-file)))
      (--blorg-prepend keywords (cons "slug" (--blorg-slugify slug))))
    (--blorg-prepend keywords (cons "html" html))
    `(("post" . ,keywords))))

(defun blorg--find-source-files (directory pattern)
  "Recursively find files that match with PATTERN within DIRECTORY."
  (let (output-files)
    (dolist (file (directory-files-and-attributes directory t))
      (cond
       ((string-match pattern (car file))
        (setq output-files (cons (car file) output-files)))
       ((eq t (car (cdr file)))
        (if (not (equal "." (substring (car file) -1)))
            (setq output-files
                  (append
                   (blorg--find-source-files (car file) pattern)
                   output-files))))))
    output-files))

(defun --blorg-slugify (s)
  "Make slug of S."
  (downcase
   (replace-regexp-in-string
    "\s" "-" (file-name-sans-extension (file-name-nondirectory s)))))

(defun --blorg-get (seq item &optional default)
  "Pick ITEM from SEQ or return DEFAULT from list of cons."
  (or (cadr (assoc item seq)) default))

(defun --blorg-get-cdr (seq item &optional default)
  "Pick ITEM from SEQ or return DEFAULT from list of cons."
  (or (cdr (assoc item seq)) default))

(provide 'blorg)
;;; blorg.el ends here
