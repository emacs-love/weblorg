;;; blorg.el --- Static Site Generator for org-mode; -*- lexical-binding: t -*-
;;
;; Author: Lincoln Clarete <lincoln@clarete.li>
;; URL: https://clarete.li/blorg
;; Version: 0.1.0
;; Package-Requires: ((templatel "0.1.0") (emacs "26.1"))
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Genenrate static websites off of Org Mode sources.
;;
;;; Code:

(require 'org)
(require 'ox-html)
(require 'seq)
(require 'templatel)

(defvar blorg-module-dir (file-name-directory load-file-name)
  "Directory that points to the directory of blorgs source code.")

(defvar blorg-version "0.1.0"
  "The blorg's library version.")

(defvar blorg-meta
  `(("meta" ("generator" . ,(format "blorg %s (https://github.com/clarete/blorg)" blorg-version))))
  "Collection of variables that always get added to templates.")

(defconst blorg--default-url "http://localhost:8000/"
  "Default URL for a blorg.")

(defconst blorg--sites (make-hash-table :test 'equal)
  "Hashtable with site metadata indexed by their URL.")

(defmacro blorg--prepend (seq item)
  "Prepend ITEM to SEQ."
  `(setq ,seq (cons ,item ,seq)))

(defun blorg-site (&rest options)
  "Create a new blorg site.

OPTIONS can contain the following parameters:

 * ~:base-url~: Website's base URL.  Can be protocol followed by
   domain and optionally by path.  Notice that each site is
   registered within a global hash table `blorg--sites'.  If one
   tries to register two sites with the same ~:base-url~, an
   error will be raised."
  (let* ((opt (seq-partition options 2))
         (base-url (blorg--get opt :base-url nil))
         (theme (blorg--get opt :theme "default"))
         (site (blorg--site-get base-url)))
    (if (null site)
        ;; Shape of the blorg object is the following:
        ;;
        ;; 0. Hashtable where the routes are saved.  The key comes
        ;;    from the :name of the route, and the value is all the
        ;;    parameters of the route.
        (let ((new-site (make-hash-table :size 3)))
          (puthash :base-url base-url new-site)
          (puthash :theme theme new-site)
          (puthash :routes (make-hash-table :test 'equal) new-site)
          (puthash base-url new-site blorg--sites))
      ;; Already exists
      site)))

(defun blorg-route (&rest options)
  "Add a new route defined by OPTIONS to a site."
  (let* ((opt (seq-partition options 2))
         (route (make-hash-table))
         ;; all parameters the entry point takes
         (name (blorg--get opt :name))
         ;; It's also the default for :output
         (url (blorg--get opt :url "/{{ slug }}.html"))
         ;; Not using the `default' parameter in `blorg--get' because
         ;; it doesn't give the short circiut given by `or'.
         (site (or (blorg--get opt :site)
                   (blorg-site :base-url blorg--default-url)))
         ;; Prefix path for most file operations within a route
         (base-dir (blorg--get opt :base-dir default-directory))
         ;; Notice the templates directory close to `base-dir` has
         ;; higher precedence over the templates directory within
         ;; blorg's source code.
         (template-dirs (cons (expand-file-name "templates" base-dir)
                              (blorg--template-base))))
    (puthash :name name route)
    (puthash :site site route)
    (puthash :url url route)
    (puthash :base-dir base-dir route)
    (puthash :input-source (blorg--get opt :input-source) route)
    (puthash :input-pattern (blorg--get opt :input-pattern "org$") route)
    (puthash :input-exclude (blorg--get opt :input-exclude "^$") route)
    (puthash :input-filter (blorg--get opt :input-filter) route)
    (puthash :input-aggregate (blorg--get opt :input-aggregate #'blorg-input-aggregate-none) route)
    (puthash :output (blorg--get opt :output url) route)
    (puthash :template (blorg--get opt :template nil) route)
    (puthash :template-vars (blorg--get opt :template-vars nil) route)
    (puthash :template-dirs template-dirs route)
    (puthash :template-env (templatel-env-new :importfn (blorg--route-importfn route)) route)
    (puthash name route (gethash :routes site))))

(defun blorg-export ()
  "Export all sites."
  ;; Iterate over each site available in our global registry
  (maphash (lambda(_ site)
     ;; Iterate over each route of a given site
     (maphash (lambda(_ route) (blorg--export-site-route site route))
              (gethash :routes site)))
   blorg--sites))



;; ---- Pipeline Entry Points ----

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

 * `:input-pattern': Regular expression for selecting files
    within path `:base-dir'.  It defaults to \"org$\";

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

 * `:input-source': List of collections of data to be writtend
   directly to templates.  In other words, this parameter
   replaces the pipeline `pattern` > `exclude` > `filter` >
   `aggregate` and will feed data directly into the function that
   writes down templates.  This is useful for generating HTML
   files off template variables read from whatever source you
   want.

 * `:output': String with a template for generating the output
   file name.  The variables available are the variables of each
   item of a collection returned by `:input-aggregate'.

 * `:template': Name of the template that should be used to
   render a collection of files.  Notice that this is the name of
   the template, not its path (neither relative or absolute).
   The value provided here will be searched within 1. the
   directory *template* within `:base-dir' 2. the directory
   *templates* within blorg's source code.

 * `:template-vars': Association list of extra variables to be
   passed down to the template.

 * `:base-dir': Base path for `:input-pattern' and `:output'; If
    not provided, will default to the `:base-dir' of the website;

This function will not handle errors gracefully.  Please refer to
`blorg-cli' if you don't want to handle any errors yourself."
  (let* ((opt (seq-partition options 2))
         ;; all parameters the entry point takes
         (base-dir (blorg--get opt :base-dir default-directory))
         (input-source (blorg--get opt :input-source))
         (input-pattern (blorg--get opt :input-pattern "org$"))
         (input-exclude (blorg--get opt :input-exclude "^$"))
         (input-filter (blorg--get opt :input-filter))
         (input-aggregate (blorg--get opt :input-aggregate #'blorg-input-aggregate-none))
         (output (blorg--get opt :output "output/{{ slug }}.html"))
         (template (blorg--get opt :template nil))
         (template-vars (blorg--get opt :template-vars nil))
         (template-dirs
          (cons
           ;; Notice the templates directory close to `base-dir` has
           ;; higher precedence over the templates directory within
           ;; blorg's source code.
           (expand-file-name "templates" base-dir)
           (blorg--template-base)))
         ;; template environment with import function attached.
         (env
          (templatel-env-new
           :importfn (lambda(en name)
                       (templatel-env-add-template
                        en name
                        (templatel-new-from-file
                         (blorg--template-find template-dirs name))))))
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
      (blorg--template-find template-dirs template)))
    ;; Apply pipeline
    (blorg--write-collections
     blorg
     (cond ((not (null input-source)) input-source)
           (t (blorg--run-pipeline blorg))))))



;; ---- Input Filter functions ----

(defun blorg-input-filter-drafts (post)
  "Exclude POST from input list if it is a draft.

We use the DRAFT file property to define if an Org-Mode file is a
draft or not."
  (ignore-errors (not (cdr (assoc "draft" post)))))



;; ---- Aggregation functions ----


(defun blorg-input-aggregate-none (posts)
  "Aggregate each post within POSTS as a single collection.

This is the default aggregation function used by `blorg-gen' and
generate one collection per input file."
  (mapcar (lambda(p) `(("post" . ,p))) posts))

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
     (lambda(k v)
       (blorg--prepend
        output
        `(("category" . (("name" . ,k)
                         ("posts" . ,v))))))
     ht)
    output))



;; ---- Input Source: autodoc ----

(defun blorg-input-source-autodoc (pattern)
  "Pull metadata from Emacs-Lisp symbols that match PATTERN."
  `((("symbols" . ,(mapcar
                    (lambda(sym)
                      (cons
                       "symbol"
                       (cond ((functionp sym)
                              `(("type" . "function")
                                ("name" . ,sym)
                                ("docs" . ,(blorg--input-source-autodoc-documentation sym))
                                ("args" . ,(help-function-arglist sym t))))
                             (t
                              `(("type" . "variable")
                                ("name" . ,sym))))))
                    (apropos-internal pattern))))))

(defun blorg-input-source-autodoc-sections (sections)
  "Run `blorg-input-source-autodoc' for various SECTIONS."
  `((("sections" . ,(mapcar
                     (lambda(section)
                       (cons "section"
                             `(("name" . ,(car section))
                               ("slug" . ,(blorg--slugify (car section)))
                               ,@(car (blorg-input-source-autodoc (cdr section))))))
                     sections)))))

(defun blorg--input-source-autodoc-documentation (sym)
  "Generate HTML documentation of the docstring of a symbol SYM."
  (let* ((doc (documentation sym))
         (doc (replace-regexp-in-string "\n\n(fn[^)]*)$" "" doc)))
    (cdr (assoc "html" (blorg--parse-org doc)))))



;; ---- Private Functions ----

;; Site object

(defun blorg--site-get (&optional base-url)
  "Retrieve a site with key BASE-URL from `blorg--sites'."
  (gethash (or base-url blorg--default-url) blorg--sites))

(defun blorg--site-route (site route-name)
  "Retrieve ROUTE-NAME from SITE."
  (gethash route-name (gethash :routes site)))

(defun blorg--site-route-add (site route-name route-params)
  "Add ROUTE-PARAMS under ROUTE-NAME to SITE."
  (puthash route-name route-params (gethash :routes site)))

;; Crossreference

(defun blorg--url-parse (link)
  "Parse LINK components.

The LINK string has the following syntax:

   Link    <- Route ',' Vars
   Route   <- Identifier
   Vars    <- NamedParams

These are inherited from templatel's parser:

   NamedParams <- NamedParam (',' NamedParam)*
   NamedParam  <- Identifier '=' Expr
   Identifier  <- [A-Za-z_][0-9A-Za-z_]*

With the above rules, we're able to parse entries like these:
  * index
  * docs,slug=overview
  * route,param1=val,param2=10

Notice: We're using an API that isn't really intended for public
consumption from templatel."
  (let* ((scanner (templatel--scanner-new link "<string>"))
         (route (cdr (templatel--parser-identifier scanner)))
         (vars (templatel--scanner-optional
                scanner
                (lambda()
                  (templatel--token-comma scanner)
                  (mapcar
                   (lambda(np)
                     (cons (cdar np)
                           (cdadar (cdadr np))))
                   (cdar (templatel--parser-namedparams scanner)))))))
    (cons route vars)))

(defun blorg--url-for-v (route-name vars site)
  "Find ROUTE-NAME within SITE and interpolate route url with VARS."
  (concat (gethash :base-url site)
          (templatel-render-string
           (gethash :url (blorg--site-route site route-name))
           vars)))

(defun blorg--url-for (link &optional site)
  "Find route within SITE and interpolate variables found in LINK."
  (let* ((site (or site (blorg-site :base-url blorg--default-url)))
         (parsed (blorg--url-parse link)))
    (blorg--url-for-v (car parsed) (cdr parsed) site)))

;; Template Resolution

(defun blorg--template-base ()
  "Base template directory.

The template system of blorg will search for a given template
name in a list of different environments, similar to the PATH
variable a shell.  This function returns the entry that usually
sits in the bottom of that list with the lowest priority.  It
contains the `template` directory bundled with the code of the
blorg module."
  (list (expand-file-name "templates" blorg-module-dir)))

(defun blorg--template-find (directories name)
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
       (list "" "File not found" (format "Template `%s' not found" name)))
    ;; Let's see if we can find it in the next directory
    (let* ((path (expand-file-name name (car directories)))
           (attrs (file-attributes path)))
      (cond
       ;; doesn't exist; try next dir
       ((null attrs) (blorg--template-find (cdr directories) name))
       ;; is a directory
       ((not (null (file-attribute-type attrs))) nil)
       ;; we found it
       ((null (file-attribute-type attrs))
        path)))))

(defun blorg--route-importfn (route)
  "Build the import function for ROUTE.

The extension system provided by templatel takes a function which
going to be called any time one needs to find a template.  There
are mainly two good places for calling this function:

 0. An import function is needed in order to create template
    environments that support extending templates.  The provided
    function will be called once for every ~{% extends \"path\" %}~
    statement found.

 1. When a new route is added and we need to find the template
    that will be used to render the route files."
  (lambda(en name)
    (templatel-env-add-template
     en name
     (templatel-new-from-file
      (blorg--template-find (gethash :template-dirs route) name)))))

;; Exporting pipeline

(defun blorg--route-collect-and-aggregate (route)
  "Fing input files apply templates for a ROUTE."
  (let* ((input-filter (gethash :input-filter route))
         ;; Find all files that match input pattern and don't match
         ;; exclude pattern
         (input-files
          (blorg--find-source-files
           (gethash :base-dir route)
           (gethash :input-pattern route)
           (gethash :input-exclude route)))
         ;; Parse Org-mode files
         (parsed-files
          (mapcar #'blorg--parse-org-file input-files))
         ;; Apply filters that depend on data read from parser
         (filtered-files
          (if (null input-filter) parsed-files
            (seq-filter input-filter parsed-files)))
         ;; Aggregate the input list into either a single group with
         ;; all the files or multiple groups
         (aggregated-data
          (funcall (gethash :input-aggregate route) filtered-files)))
    aggregated-data))

(defun blorg--route-export (route collections)
  "Walk through COLLECTIONS & render a template for each item on it.

The settings for generating the template, like output file name,
can be found in the ROUTE."
  (dolist (data collections)
    (let* (;; Render the template
           (rendered
            (templatel-env-render
             (gethash :template-env route)
             (gethash :template route)
             (append (gethash :template-vars route) blorg-meta data)))
           ;; Render the relative output path
           (rendered-output
            (templatel-render-string
             (gethash :output route)
             (cdar data)))
           ;; Render the full path
           (final-output
            (expand-file-name rendered-output (gethash :base-dir route))))
      (blorg--log-info "writing: %s" final-output)
      (mkdir (file-name-directory final-output) t)
      (write-region rendered nil rendered-output))))

(defun blorg--export-site-route (site route)
  "Export a single ROUTE of a SITE."
  ;; Add the route's main template to the environment
  (funcall (blorg--route-importfn route)
           (gethash :template-env route)
           (gethash :template route))
  ;; Install link handlers
  (org-link-set-parameters
   "url_for"
   :export (lambda(path desc _backend)
             (format "<a href=\"%s\">%s</a>" (blorg--url-for path site) desc)))
  (templatel-env-add-filter
   (gethash :template-env route)
   "url_for"
   (lambda(route-name &optional vars)
     (blorg--url-for-v route-name vars site)))
  ;; Collect -> Aggregate -> Template -> Write
  (let ((input-source (gethash :input-source route)))
    (blorg--route-export
     route
     (if (null input-source)
         (blorg--route-collect-and-aggregate route)
       input-source))))

;; Pipeline

(defun blorg--run-pipeline (blorg)
  "Fing input files and template them up with config in BLORG."
  (let* ((input-filter (blorg--get blorg 'input-filter))
         ;; Find all files that match input pattern and don't match
         ;; exclude pattern
         (input-files
          (blorg--find-source-files
           (blorg--get blorg 'base-dir)
           (blorg--get blorg 'input-pattern)
           (blorg--get blorg 'input-exclude)))
         ;; Parse Org-mode files
         (parsed-files
          (mapcar #'blorg--parse-org-file input-files))
         ;; Apply filters that depend on data read from parser
         (filtered-files
          (if (null input-filter) parsed-files
            (seq-filter input-filter parsed-files)))
         ;; Aggregate the input list into either a single group with
         ;; all the files or multiple groups
         (aggregated-data
          (funcall (blorg--get blorg 'input-aggregate) filtered-files)))
    aggregated-data))

(defun blorg--write-collections (blorg collections)
  "Walk through COLLECTIONS & render a template for each item on it.

The settings for generating the template, like output file name,
can be found in the BLORG variable."
  (dolist (data collections)
    (let* (;; Render the template
           (rendered
            (templatel-env-render
             (blorg--get blorg 'env)
             (blorg--get blorg 'template)
             (append (blorg--get blorg 'template-vars) blorg-meta data)))
           ;; Render the relative output path
           (rendered-output
            (templatel-render-string
             (blorg--get blorg 'output)
             (cdar data)))
           ;; Render the full path
           (final-output
            (expand-file-name rendered-output (blorg--get blorg 'base-dir))))
      (blorg--log-info "writing: %s" final-output)
      (mkdir (file-name-directory final-output) t)
      (write-region rendered nil rendered-output))))

(defun blorg--parse-org-file (input-path)
  "Parse an Org-Mode file located at INPUT-PATH."
  (let* ((input-data (with-temp-buffer
                       (insert-file-contents input-path)
                       (buffer-string)))
         (keywords (blorg--parse-org input-data))
         (slug (blorg--get-cdr keywords "title" input-path)))
    (blorg--prepend keywords (cons "file" input-path))
    (blorg--prepend keywords (cons "slug" (blorg--slugify slug)))
    keywords))

(defun blorg--parse-org (input-data)
  "Parse INPUT-DATA as an Org-Mode file & generate its HTML.

An assoc will be returned with all the file properties collected
from the file, like TITLE, OPTIONS etc.  The generated HTML will
be added ad an entry to the returned assoc."
  (let (html keywords)
    ;; Replace the HTML generation code to prevent ox-html from adding
    ;; headers and stuff around the HTML generated for the `body` tag.
    (advice-add
     'org-html-template :override
     (lambda(contents _i) (setq html contents)))
    ;; Watch collection of keywords, which are file-level properties,
    ;; like #+TITLE, #+FILETAGS, etc.
    (advice-add
     'org-html-keyword :before
     (lambda(keyword _c _i)
       (blorg--prepend
        keywords
        (cons
         (downcase (org-element-property :key keyword))
         (org-element-property :value keyword)))))
    ;; Trigger Org-Mode to generate the HTML off of the input data
    (with-temp-buffer
      (insert input-data)
      (org-html-export-as-html))
    ;; Uninstall advices
    (ad-unadvise 'org-html-template)
    (ad-unadvise 'org-html-keyword)
    ;; Add the generated HTML as a property to the collected keywords
    ;; as well
    (blorg--prepend keywords (cons "html" html))
    keywords))

(defun blorg--find-source-files (directory pattern exclude)
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
                   (blorg--find-source-files (car file) pattern exclude)
                   output-files))))))
    output-files))

(defun blorg--slugify (s)
  "Make slug of S."
  (downcase
   (replace-regexp-in-string
    "\s" "-" (file-name-sans-extension (file-name-nondirectory s)))))

(defun blorg--get (seq item &optional default)
  "Pick ITEM from SEQ or return DEFAULT from list of cons."
  (or (cadr (assoc item seq)) default))

(defun blorg--get-cdr (seq item &optional default)
  "Pick ITEM from SEQ or return DEFAULT from list of cons."
  (or (cdr (assoc item seq)) default))

(defun blorg--log-info (msg &rest vars)
  "Report MSG (formatted with VARS) to log level info."
  (message
   "%s INFO %s"
   (format-time-string "%Y-%m-%d %H:%M:%S")
   (apply 'format (cons msg vars))))

(org-link-set-parameters
 "anchor"
 :export (lambda(path desc _backend)
           (format "<a href=\"#%s\">%s</a>" path desc)))

(provide 'blorg)
;;; blorg.el ends here
