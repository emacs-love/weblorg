(add-to-list 'load-path "../")

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package templatel)
(require 'blorg)
(use-package htmlize)
(setq org-html-htmlize-output-type 'css)

;; Generate blog posts
(blorg-cli
 :base-dir: './'
 :input-pattern "posts/.*\\.org$"
 :template "post.html"
 :output "../output/posts/{{ slug }}.html")

;; Generate pages
(blorg-cli
 :input-pattern "pages/.*\\.org$"
 :template "page.html"
 :output "../output/{{ slug }}/index.html")

;; Generate posts summary
(blorg-cli
 :input-pattern "posts/.*\\.org$"
 :input-aggregate #'blorg-input-aggregate-all
 :template "blog.html"
 :output "../output/blog/index.html")
