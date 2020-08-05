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

(defvar blorg-module-dir (file-name-directory load-file-name))

(defvar blorg-version "0.1.0")

(defvar blorg-meta
  `(("meta" ("generator" . ,(format "blorg %s (https://github.com/clarete/blorg)" blorg-version)))))

(defmacro --blorg-prepend (seq item)
  "Prepend ITEM to SEQ."
  `(setq ,seq (cons ,item ,seq)))

(defun blorg-cli (&rest options)
  "Generate HTML documents off Org-Mode files using OPTIONS.

This function wraps `blorg-gen' and handle known errors and
report them in a nicer way."
  (condition-case exc
      (apply 'blorg-gen options)
    (templatel-error
     (message "Syntax Error: %s" (cdr exc)))
    (file-missing
     (message "%s: %s" (car (cddr exc)) (cadr (cddr exc))))))

(defun blorg-gen (&rest options)
  "Generate HTML documents off Org-Mode files using OPTIONS.

The OPTIONS parameter is a list of pairs of symbol value and
support the following pairs:

* `:base-dir': Base path for `:input-pattern' and `:output';

* `:input-pattern': Regular expression for selecting files within
   path `:base-dir'.  It defaults to \"org$\";

* `:input-exclude': Regular expression for excluding files from
   the input list.  Defaults to \"^$\";

* `:input-filter': Function for filtering out files after they
  were parsed.  This allows using data from within the Org-Mode
  file to decide if it should be included or not in the input
  list.

* `:input-aggregate': Function for grouping files into
  collections.  Templates are applied to collections, not to
  files from the input list.  The variables available for the
  template come from the return of this function.

* `:output': String with a template for generating the output
  file name.  The variables available are the variables of each
  item of a collection returned by `:input-aggregate'.

* `:template': Name of the template that should be used to render
  a collection of files.  Notice that this is the name of the
  template, not its path (neither relative or absolute).  The
  value provided here will be searched within 1. the directory
  *template* within `:base-dir' 2. the directory *templates*
  within blorg's source code.

* `:template-vars': Association list of extra variables to be
  passed down to the template.


This function will not handle errors gracefully.  Please refer to
`blorg-cli' if you don't want to handle any errors yourself."
  (let* ((opt (seq-partition options 2))
         ;; all parameters the entry point takes
         (base-dir (--blorg-get opt :base-dir default-directory))
         (input-pattern (--blorg-get opt :input-pattern "org$"))
         (input-exclude (--blorg-get opt :input-exclude "^$"))
         (input-filter (--blorg-get opt :input-filter))
         (input-aggregate (--blorg-get opt :input-aggregate #'blorg-input-aggregate-none))
         (output (--blorg-get opt :output "output/{{ slug }}.html"))
         (template (--blorg-get opt :template nil))
         (template-vars (--blorg-get opt :template-vars nil))
         (template-dirs
          (cons
           ;; Notice the templates directory close to `base-dir` has
           ;; higher precedence over the templates directory within
           ;; blorg's source code.
           (expand-file-name "templates" base-dir)
           (--blorg-template-base)))
         ;; template environment with import function attached.
         (env
          (templatel-env-new
           :importfn #'(lambda(en name)
                         (templatel-env-add-template
                          en name
                          (templatel-new-from-file
                           (--blorg-template-find template-dirs name))))))
         ;; all the variables passed down the pipe
         (blorg
          `((env ,env)
            (base-dir ,base-dir)
            (input-pattern ,input-pattern)
            (input-exclude ,input-exclude)
            (input-filter ,input-filter)
            (input-aggregate ,input-aggregate)
            (template ,template)
            (template-vars ,template-vars)
            (template-dirs ,template-dirs)
            (output ,output))))

    ;; Add output template to the environment
    (templatel-env-add-template
     env template
     (templatel-new-from-file
      (--blorg-template-find template-dirs template)))
    ;; Find all input files and apply the template
    (--blorg-run-pipeline blorg)))

(defun blorg-input-filter-drafts (post)
  "Exclude POST from input list if it is a draft.

We use the DRAFT file property to define if an Org-Mode file is a
draft or not."
  (ignore-errors (not (cdr (assoc "draft" post)))))

(defun blorg-input-aggregate-none (posts)
  "Aggregate each post within POSTS as a single collection.

This is the default aggregation function used by `blorg-gen' and
generate one collection per input file."
  (mapcar #'(lambda(p) `(("post" . ,p))) posts))

(defun blorg-input-aggregate-all (posts)
  "Aggregate all POSTS within a single collection.

This aggregation function generate a single collection for all
the input files.  It is useful for index pages, RSS pages, etc."
  `((("posts" . ,posts))))

(defun blorg-input-aggregate-by-category (posts)
  "Aggregate POSTS by category.

This function reads the FILETAGS file property and put the file
within each tag found there."
  (let (output
        (ht (make-hash-table :test 'equal)))
    (dolist (post posts)
      (dolist (tag (or (seq-filter
                        (lambda(x) (not (equal x "")))
                        (split-string
                         (or (cdr (assoc "filetags" post)) "") ":"))
                       '("none")))
        ;; Append post to the list under each tag
        (puthash (downcase tag)
                 (cons post (gethash (downcase tag) ht))
                 ht)))
    ;; Make a list of list off the hash we just built
    (maphash
     #'(lambda(k v)
         (--blorg-prepend
          output
          `(("category" . (("name" . ,k)
                           ("posts" . ,v))))))
     ht)
    output))

(defun --blorg-template-base ()
  "Base template directory.

The template system of blorg will search for a given template
name in a list of different environments, similar to the PATH
variable a shell.  This function returns the entry that usually
sits in the bottom of that list with the lowest priority.  It
contains the `template` directory bundled with the code of the
blorg module."
  (list (expand-file-name "templates" blorg-module-dir)))

(defun --blorg-template-find (directories name)
  "Find template NAME within DIRECTORIES.

This function implements a search for templates within the
provided list of directories.  The search happens from left to
right and returns on the first successful match.

This behavior, which is intentionally similar to the PATH
variable in a shell, allows the user to override just the
templates they're interested in but still take advantage of other
default templates."
  (if (null directories)
      ;; didn't find it. Signal an error upwards:
      (signal
       'file-missing
       (list "" "File not found" (format "Template `%s' doesn't exist" name)))
    ;; Let's see if we can find it in the next directory
    (let* ((path (expand-file-name name (car directories)))
           (attrs (file-attributes path)))
      (cond
       ;; doesn't exist; try next dir
       ((null attrs) (--blorg-template-find (cdr directories) name))
       ;; is a directory
       ((not (null (file-attribute-type attrs))) nil)
       ;; we found it
       ((null (file-attribute-type attrs))
        (--blorg-log-info "template %s found at %s" name path)
        path)))))

(defun --blorg-run-pipeline (blorg)
  "Fing input files and template them up with config in BLORG."
  (let* ((input-filter (--blorg-get blorg 'input-filter))
         ;; Find all files that match input pattern and don't match
         ;; exclude pattern
         (input-files
          (--blorg-find-source-files
           (--blorg-get blorg 'base-dir)
           (--blorg-get blorg 'input-pattern)
           (--blorg-get blorg 'input-exclude)))
         ;; Parse Org-mode files
         (parsed-files
          (mapcar #'--blorg-parse-org input-files))
         ;; Apply filters that depend on data read from parser
         (filtered-files
          (if (null input-filter) parsed-files
            (seq-filter input-filter parsed-files)))
         ;; Aggregate the input list into either a single group with
         ;; all the files or multiple groups
         (aggregated-data
          (funcall (--blorg-get blorg 'input-aggregate) filtered-files)))

    (dolist (data aggregated-data)
      (let* (;; Render the template
             (rendered
              (templatel-env-render
               (--blorg-get blorg 'env)
               (--blorg-get blorg 'template)
               (append (--blorg-get blorg 'template-vars) blorg-meta data)))
             ;; Render the relative output path
             (rendered-output
              (templatel-render-string
               (--blorg-get blorg 'output)
               (cdar data)))
             ;; Render the full path
             (final-output
              (expand-file-name rendered-output (--blorg-get blorg 'base-dir))))
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
    (--blorg-prepend keywords (cons "file" input-file))
    keywords))

(defun --blorg-find-source-files (directory pattern exclude)
  "Find files matching PATTERN but not EXCLUDE within DIRECTORY."
  (let (output-files)
    (dolist (file (directory-files-and-attributes directory t))
      (cond
       ((and (string-match pattern (car file))
             (not (string-match exclude (car file))))
        (setq output-files (cons (car file) output-files)))
       ((eq t (car (cdr file)))
        (if (not (equal "." (substring (car file) -1)))
            (setq output-files
                  (append
                   (--blorg-find-source-files (car file) pattern exclude)
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

(defun --blorg-log-info (msg &rest vars)
  "Report MSG (formatted with VARS) to log level info."
  (message
   "%s INFO %s"
   (format-time-string "%Y-%m-%d %H:%M:%S")
   (apply 'format (cons msg vars))))

(provide 'blorg)
;;; blorg.el ends here
