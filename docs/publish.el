;;; publish.el --- Generate Weblorg page
;;; Commentary:
;;
;; Generate static website for weblorg
;;
;;; Code:
;;
;; Guarantee the freshest version of the weblorg
(add-to-list 'load-path "../")

;; Setup package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Install and configure dependencies
(use-package templatel)
(use-package htmlize)
(setq org-html-htmlize-output-type 'css)

(require 'weblorg)

;; Defaults to localhost:8000
(if (string= (getenv "ENV") "prod")
    (setq weblorg-default-url "https://emacs.love/weblorg"))
(if (string= (getenv "ENV") "local")
    (setq weblorg-default-url "http://localhost:8000"))

;; Set site wide configuration
(weblorg-site
 :theme #'weblorg-theme-autodoc
 :template-vars '(("project_name" . "weblorg")
                  ("project_github" . "https://github.com/emacs-love/weblorg")
                  ("project_image" . "https://emacs.love/weblorg/static/images/logo.svg")
                  ("project_description" . "A Static HTML Generator for Emacs and Org-Mode")))

;; Generate index
(weblorg-route
 :name "index"
 :input-pattern "./index.org"
 :template "index.html"
 :output "./index.html"
 :url "/")

;; Generate documentation articles
(weblorg-route
 :name "docs"
 :input-pattern "./doc/*.org"
 :template "doc.html"
 :output "./doc/{{ slug }}.html"
 :url "/doc/{{ slug }}.html")

;; Generate API reference
(weblorg-route
 :name "api"
 :input-source (weblorg-input-source-autodoc-sections
                `(("Routing" . ("^weblorg-route" "^weblorg-copy-static"))
                  ("Input Filters" . "^weblorg-input-filter-")
                  ("Input Aggregations" . "^weblorg-input-aggregate-")
                  ("Exporting" . "^weblorg-export$")
                  ("Data Sources" . "^weblorg-input-source-")
                  ("Template Filters" . "^weblorg-filters-")))
 :template "autodoc.html"
 :output "api.html"
 :url "/api.html")

;; Generate blog posts
(weblorg-route
 :name "posts"
 :input-pattern "./posts/*.org"
 :template "post.html"
 :output "./posts/{{ slug }}.html"
 :url "/posts/{{ slug }}.html")

;; Map out the static directory with an empty route
(weblorg-route
 :name "static"
 :output "static/{{ file }}"
 :url "/static/{{ file }}")

(setq debug-on-error t)

(weblorg-export)
;;; publish.el ends here
