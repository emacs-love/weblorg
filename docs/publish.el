;;; publish.el --- Generate Blorg page
;;; Commentary:
;;
;; Generate static website for blorg
;;
;;; Code:
;;
;; Guarantee the freshest version of the blorg
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

(require 'blorg)

;; Defaults to localhost:8000
(setq blorg-default-url "http://localhost:8080")

;; Generate index
(blorg-route
 :name "index"
 :input-pattern "./index.org"
 :template "index.html"
 :output "./index.html"
 :url "/")

(blorg-copy-static
 :output "static/{{ file }}"
 :url "/static/{{ file }}")

(blorg-export)
;;; publish.el ends here
