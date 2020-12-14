;;; blorg.el --- Static Site Generator for org-mode; -*- lexical-binding: t -*-
;;
;; Author: Lincoln Clarete <lincoln@clarete.li>
;; URL: https://clarete.li/blorg
;; Version: 0.1.0
;; Package-Requires: ((templatel "0.1.3") (emacs "26.1"))
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
;; The API is modeled as a simplified version of a generic HTTP
;; framework.  Simplified in the meaning that it doesn't give access
;; to the request and response objects, but allow interacting with the
;; input throughout a few different points of a defined pipeline.
;;
;; Aiming to find balance between flexibility and simplicity, the
;; general principle of this software is to be designed as a set of
;; modular components with default configuration that is ready to go
;; for valuable use cases.
;;
;; 1. Use-case: transform a list of Org-Mode files in HTML:
;;
;;    (blorg-route
;;     :input-pattern "*.org"
;;     :template "post.html"
;;     :url "/{{ slug }}.html")
;;    (blorg-export)
;;
;; 2. Use-case: Link documents from different routes
;;
;;    a. publish.el looks like this:
;;
;;       (blorg-route
;;        :name "docs"
;;        :input-pattern "*.org"
;;        :input-exclude "index.org$"
;;        :template "post.html"
;;        :url "/{{ slug }}.html")
;;       (blorg-route
;;        :name "index"
;;        :input-pattern "index.org"
;;        :template "index.html"
;;        :url "/index.html")
;;       (blorg-export)
;;
;;    b. a-post.org looks something like this:
;;
;;       #+TITLE: A Post
;;       #+DATE: <2020-08-30>
;;       * Intro
;;         If you liked this, make sure you also check out
;;         [[url_for:docs,slug=another-post][Another Post]].
;;
;;    c. post.html could looks something like this
;;
;;       <h1>{{ post.title }}</h1>
;;       {{ post.html }}
;;       <hr>
;;       <a href="{{ url_for("index") }}">Home</a>
;;
;;    Behind the scenes, there's a global variable `blorg--sites' that
;;    contains a hash-table with all the blorg-sites, indexed by the
;;    `:base-url' parameter.  A default blorg site instance is created
;;    if `blorg-route' doesn't receive an explicit `:site' parameter.
;;
;;; Code:

(require 'org)
(require 'ox-html)
(require 'seq)
(require 'em-glob)
(require 'templatel)

(defvar blorg-module-dir (file-name-directory load-file-name)
  "Directory that points to the directory of blorgs source code.")

(defvar blorg-version "0.1.0"
  "The blorg's library version.")

(defvar blorg-meta
  `(("meta" ("generator" . ,(format "blorg %s (https://github.com/clarete/blorg)" blorg-version))))
  "Collection of variables that always get added to templates.")

(defvar blorg-default-url "http://localhost:8000"
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
         (base-url (blorg--get opt :base-url blorg-default-url))
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
  "Add a new route defined with parameters within OPTIONS.

A route is an abstraction to manage how Org-Mode files are found
and how they are transformed in HTML.

Examples:

 1. Route that finds all the Org-Mode files within the ~posts~
    directory, aggregate them all in one single collection made
    available to the also template called ~posts~ so it can be
    used to build summary pages

    #+BEGIN_SRC emacs-lisp
    (blorg-route
     :name \"index\"
     :input-pattern \"posts/*org\"
     :input-aggregate #'blorg-input-aggregate-all
     :template \"blog.html\"
     :output \"output/index.html\"
     :url \"/\")
    #+END_SRC

 2. Route for rendering each Org-Mode file under the directory
    ~pages~ as a separate HTML using the template ~page.html~.
    Notice the ~:output~ parameter will create all the
    directories in the path that don't exist

    #+BEGIN_SRC emacs-lisp
    (blorg-route
     :name \"pages\"
     :input-pattern \"pages/*.org\"
     :template \"page.html\"
     :output \"output/{{ slug }}/index.html\"
     :url \"/{{ slug }}\")
    #+END_SRC

Parameters in ~OPTIONS~:

 * `:input-pattern': glob expression for selecting files within
    path `:base-dir'.  It defaults to \"*.org\";

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

 * `:url': Similarly to the `:output' parameter, it takes a
   template string as input and returns the URL of an entry of a
   given entry in this route.

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

 * `:site': Instance of a blorg site created by the function
   [[anchor:symbol-blorg-site][blorg-site]].  If not provided, it
   will use a default value.  The most valuable information a
   site carries is its base URL, and that's why it's relevant for
   routes.  That way one can have multiple sites in one single
   program."
  (let* ((opt (seq-partition options 2))
         (route (make-hash-table))
         ;; all parameters the entry point takes
         (name (blorg--get opt :name))
         ;; It's also the default for :output
         (url (blorg--get opt :url "/{{ slug }}.html"))
         ;; Not using the `default' parameter in `blorg--get' because
         ;; it doesn't give the short circiut given by `or'.
         (site (or (blorg--get opt :site)
                   (blorg-site :base-url blorg-default-url)))
         ;; Prefix path for most file operations within a route
         (base-dir (blorg--get opt :base-dir default-directory))
         ;; The default theme of the site is the defacto "default"
         (theme (blorg--get opt :theme (gethash :theme site)))
         ;; Notice the templates directory close to `base-dir` has
         ;; higher precedence over the templates directory within
         ;; blorg's source code.
         (template-dirs (list (expand-file-name "templates" base-dir)
                              (blorg--theme-dir theme "templates"))))
    (puthash :name name route)
    (puthash :site site route)
    (puthash :url url route)
    (puthash :base-dir base-dir route)
    (puthash :input-source (blorg--get opt :input-source) route)
    (puthash :input-pattern (blorg--get opt :input-pattern "*.org") route)
    (puthash :input-exclude (blorg--get opt :input-exclude "^$") route)
    (puthash :input-filter (blorg--get opt :input-filter) route)
    (puthash :input-parser (blorg--get opt :input-parser #'blorg--parse-org-file) route)
    (puthash :input-aggregate (blorg--get opt :input-aggregate #'blorg-input-aggregate-each) route)
    (puthash :output (blorg--get opt :output url) route)
    (puthash :export (blorg--get opt :export #'blorg-export-template) route)
    (puthash :template (blorg--get opt :template nil) route)
    (puthash :template-vars (blorg--get opt :template-vars nil) route)
    (puthash :template-dirs template-dirs route)
    (puthash :theme theme route)
    (puthash :template-env (templatel-env-new :importfn (blorg--route-importfn route)) route)
    (puthash name route (gethash :routes site))))

(defun blorg-copy-static (&rest options)
  "Utility and Route for static assets of a blorg site.

Use this route if you want either of these two things:

 1. You want to use a built-in theme and need to copy its assets
    to the output directory of your site;

 2. You are want to copy assets of your local theme to the output
    directory of your site;

Examples:

 1. Add static route to the default site.  That will allow
    `url_for' to find the route ~\"static\"~.

    #+BEGIN_SRC emacs-lisp
    (blorg-copy-static
     :output \"output/static/{{ basename }}\"
     :url \"/static/{{ basename }}\")
    #+END_SRC

 2. This example uses a custom site parameter.  The site
    parameter points to a CDN as its Base URL.

    #+BEGIN_SRC emacs-lisp
    (blorg-copy-static
     :output \"output/public/{{ filename }}\"
     :url \"/public/{{ filename }}\"
     :site (blorg-site
            :name \"cdn\"
            :base-url \"https://cdn.example.com\"
            :theme \"autodoc\"))
    (blorg-export)
    #+END_SRC

Parameters in ~OPTIONS~:

 * `:output': String with a template for generating the output
   file name.  The variables available are the variables of each
   item of a collection returned by `:input-aggregate'.

 * `:url': Similarly to the `:output' parameter, it takes a
   template string as input and returns the URL of an entry of a
   given entry in this route.

 * `:site': Instance of a blorg site created by the function
   [[anchor:symbol-blorg-site][blorg-site]].  If not provided, it
   will use a default value.  The most valuable information a
   site carries is its base URL, and that's why it's relevant for
   routes.  That way one can have multiple sites in one single
   program.

 * `:name': name of the route.  This defaults to ~\"static\"~.
   Notice that if you are using this function to copy assets from
   a built-in theme, the template of such a theme will reference
   the route ~\"static\"~ when including assets.  Which means
   that you need at least one ~\"static\"~ route in your site."
  (let* ((opt (seq-partition options 2))
         (route (make-hash-table))
         (name (blorg--get opt :name "static"))
         (url (blorg--get opt :url "/static/{{ file }}"))
         (base-dir (blorg--get opt :base-dir default-directory))
         (site (or (blorg--get opt :site)
                   (blorg-site :base-url blorg-default-url))))
    (puthash :name name route)
    (puthash :site site route)
    (puthash :url url route)
    (puthash :base-dir base-dir route)
    (puthash :theme-dir "static/" route)
    (puthash :input-pattern (blorg--get opt :input-pattern "**/*") route)
    (puthash :input-exclude (blorg--get opt :input-exclude (regexp-opt '("/." "/.." "/output"))) route)
    (puthash :input-filter (blorg--get opt :input-filter) route)
    (puthash :input-parser #'identity route)
    (puthash :input-aggregate #'identity route)
    (puthash :output (blorg--get opt :output "output/static/{{ file }}") route)
    (puthash :export (blorg--get opt :export #'blorg-export-assets) route)
    (puthash name route (gethash :routes site))))

(defun blorg-export ()
  "Export all sites."
  (condition-case exc
      ;; Iterate over each site available in our global registry
      (maphash (lambda(_ site)
                 ;; Iterate over each route of a given site
                 (maphash (lambda(_ route) (funcall (gethash :export route) route))
                          (gethash :routes site)))
               blorg--sites)
    (templatel-error
     (message "Syntax Error: %s" (cdr exc)))
    (file-missing
     (message "%s: %s" (car (cddr exc)) (cadr (cddr exc))))))

(defun blorg-export-template (route)
  "Export a single ROUTE of a site with files to be templatized."
  ;; Add the route's main template to the environment
  (funcall (blorg--route-importfn route)
           (gethash :template-env route)
           (gethash :template route))
  ;; Install link handlers
  (let ((site (gethash :site route)))
    (org-link-set-parameters
     "url_for"
     :export (lambda(path desc _backend)
               (format "<a href=\"%s\">%s</a>" (blorg--url-for path site) desc)))
    (templatel-env-add-filter
     (gethash :template-env route)
     "url_for"
     (lambda(route-name &optional vars)
       (blorg--url-for-v route-name vars site)))
    (templatel-env-add-filter
     (gethash :template-env route)
     "strftime"
     (lambda(time format)
       (format-time-string format time))))
  ;; Collect -> Aggregate -> Template -> Write
  (let ((input-source (gethash :input-source route)))
    (blorg--export-template
     route
     (if (null input-source)
         (blorg--route-collect-and-aggregate route)
       input-source))))

(defun blorg-export-assets (route)
  "Export static assets ROUTE."
  (dolist (source-file
           (blorg--find-source-files
            (blorg--theme-dir-from-route route)
            (gethash :input-pattern route)
            (gethash :input-exclude route)))
    (let* (;; path to the theme directory the route refers to
           (base-path (blorg--theme-dir-from-route route))
           ;; file path without the base path above
           (file (replace-regexp-in-string (regexp-quote base-path) "" source-file t t))
           ;; rendered destination
           (rendered-output
            (templatel-render-string
             (gethash :output route)
             `(("file" . ,file))))
           ;; Render the full path
           (dest-file
            (expand-file-name rendered-output (gethash :base-dir route))))
      (blorg--log-info  "copying: %s -> %s" source-file dest-file)
      (mkdir (file-name-directory dest-file) t)
      (condition-case exc
          (copy-file source-file dest-file t)
        (error
         (if (not (string= (caddr exc) "Success"))
             (message "error: %s: %s" (car (cddr exc)) (cadr (cddr exc)))))))))



;; ---- Input Filter functions ----

(defun blorg-input-filter-drafts (post)
  "Exclude POST from input list if it is a draft.

We use the DRAFT file property to define if an Org-Mode file is a
draft or not."
  (ignore-errors (not (cdr (assoc "draft" post)))))



;; ---- Aggregation functions ----

(defun blorg-input-aggregate-each (posts)
  "Aggregate each post within POSTS as a single collection.

This is the default aggregation function used by `blorg-route'
and generate one collection per input file.

It returns a list in the following format:

#+BEGIN_SRC emacs-lisp
'((\"post\" . ((\"title\" . \"My post\")
               (\"slug\" . \"my-post\"))
               ...)
  (\"post\" . ((\"title\" . \"Another Post\")
               (\"slug\" . \"another-post\")
               ...))
  ...)
#+END_SRC"
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
  (gethash (or base-url blorg-default-url) blorg--sites))

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
  (let ((route (blorg--site-route site route-name)))
    (if (not (null route))
        (concat (gethash :base-url site)
                (templatel-render-string
                 (gethash :url route)
                 vars))
      (progn
        (warn "url_for: Can't find route %s" route-name)
        ""))))

(defun blorg--url-for (link &optional site)
  "Find route within SITE and interpolate variables found in LINK."
  (let* ((site (or site (blorg-site :base-url blorg-default-url)))
         (parsed (blorg--url-parse link)))
    (blorg--url-for-v (car parsed) (cdr parsed) site)))

;; Template Resolution

(defun blorg--theme-dir (theme dir)
  "Path for DIR within a THEME.

The blorg ships with a gallery of themes.  This function returns
the absolute path for THEME."
  (expand-file-name
   dir (expand-file-name
        theme (expand-file-name
               "themes" blorg-module-dir))))

(defun blorg--theme-dir-from-route (route)
  "Get the path of directory `:theme-dir' of a ROUTE."
  (blorg--theme-dir
   (gethash :theme (gethash :site route))
   (gethash :theme-dir route)))

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
  "Find input files apply templates for a ROUTE."
  (let* ((input-filter (gethash :input-filter route))
         (theme-dir (gethash :theme-dir route))
         ;; Find all files that match input pattern and don't match
         ;; exclude pattern
         (input-files
          (append
           ;; Routes can request scanner to visit the theme directory
           (unless (null theme-dir)
             (blorg--find-source-files
              (blorg--theme-dir-from-route route)
              (gethash :input-pattern route)
              (gethash :input-exclude route)))
           (blorg--find-source-files
            (gethash :base-dir route)
            (gethash :input-pattern route)
            (gethash :input-exclude route))))
         ;; Parse Org-mode files
         (parsed-files
          (mapcar (gethash :input-parser route) input-files))
         ;; Apply filters that depend on data read from parser
         (filtered-files
          (if (null input-filter) parsed-files
            (seq-filter input-filter parsed-files)))
         ;; Aggregate the input list into either a single group with
         ;; all the files or multiple groups
         (aggregated-data
          (funcall (gethash :input-aggregate route) filtered-files)))
    aggregated-data))

(defun blorg--vars-from-route (route)
  "Pick some data from ROUTE to forward rendering templates."
  `(("route" . (("name" . ,(gethash :name route))))))

(defun blorg--export-template (route collections)
  "Walk through COLLECTIONS & render a template for each item on it.

The settings for generating the template, like output file name,
can be found in the ROUTE."
  (dolist (data collections)
    (let* (;; Render the template
           (rendered
            (templatel-env-render
             (gethash :template-env route)
             (gethash :template route)
             (append (gethash :template-vars route)
                     blorg-meta
                     data
                     (blorg--vars-from-route route))))
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
  "SITE ROUTE."
  (funcall (gethash :export route) site route))

(defun blorg--parse-org-file (input-path)
  "Parse an Org-Mode file located at INPUT-PATH."
  (let* ((input-data (with-temp-buffer
                       (insert-file-contents input-path)
                       (buffer-string)))
         (keywords (blorg--parse-org input-data))
         (slug
          ;; First look for `slug` FILETAG, if it's not available, try
          ;; to use the `title` FILETAG. If both fail, use the file
          ;; name.
          (blorg--get-cdr
           keywords "slug"
           (blorg--get-cdr keywords "title" input-path))))
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
        (blorg--parse-org-keyword keyword))))
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

(defun blorg--parse-org-keyword (keyword)
  "Parse a single Org-Mode document KEYWORD.

If it's a date field, it will return a timestamp tuple instead of
the value string.  The user will have to use the `strftime`
template filter to display a nicely formatted string."
  (let ((key (downcase (org-element-property :key keyword)))
        (value (org-element-property :value keyword)))
    (cons
     key
     (if (string= key "date")
         (encode-time (org-parse-time-string value))
       value))))

(defun blorg--find-source-files (base-dir input-pattern input-exclude)
  "Find source files with parameters extracted from a route.

The BASE-DIR is a path where the search will start.  The
INPUT-PATTERN is a glob expression, compatible with eshell's
`eshell-extended-glob'.  After a list of file names is found, it
is then filtered with INPUT-EXCLUDE, which is a regular
expression (not a glob)."
  (seq-filter
   (lambda (f)
     (not (string-match input-exclude f)))
   (eshell-extended-glob
    (concat (file-name-as-directory base-dir) input-pattern))))

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
