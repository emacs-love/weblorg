;;; publish.el --- Generate a simple static HTML blog
;;; Commentary:
;;
;;    Define the routes of the static website.  Each of which
;;    containing the pattern for finding Org-Mode files, which HTML
;;    template to be used, as well as their output path and URL.
;;
;;; Code:

;; Guarantee the freshest version of the blorg
(add-to-list 'load-path "../../")

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

(require 'blorg)

;; Generate blog posts
(blorg-route
 :name "posts"
 :input-pattern "posts/.*\\.org$"
 :template "post.html"
 :output "output/posts/{{ slug }}.html"
 :url "/posts/{{ slug }}.html")

;; Generate pages
(blorg-route
 :name "pages"
 :input-pattern "pages/.*\\.org$"
 :template "page.html"
 :output "output/{{ slug }}/index.html"
 :url "/{{ slug }}")

;; Generate posts summary
(blorg-route
 :name "index"
 :input-pattern "posts/.*\\.org$"
 :input-aggregate #'blorg-input-aggregate-all
 :template "blog.html"
 :output "output/index.html"
 :url "/")

(blorg-copy-static
 :output "output/static/{{ file }}"
 :url "/static/{{ file }}")

(blorg-export)
;;; publish.el ends here
