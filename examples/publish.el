(add-to-list 'load-path "../")

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package templatel)
(require 'blorg)

;; Generate blog posts
(blorg-cli
 :base-dir: './'
 :input-pattern ".*\\.org$"
 :template "post.html"
 :output "../output/posts/{{ slug }}.html")
