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
(if (string= (getenv "ENV") "prod")
    (setq blorg-default-url "https://emacs.love/blorg"))

;; Set site wide configuration
(blorg-site :theme "site")

;; Generate index
(blorg-route
 :name "index"
 :input-pattern "./index.org"
 :template "index.html"
 :output "./index.html"
 :url "/")

(blorg-route
 :name "api"
 :input-source (blorg-input-source-autodoc-sections
                `(("Render template strings" . "^templatel-render")
                  ("Template environments" . "^templatel-env")
                  ("Filters" . "^templatel-filter")
                  ("Exceptions" . ,(concat "templatel-" (regexp-opt '("syntax-error"
                                                                      "runtime-error"
                                                                      "backtracking"))))))
 :template "autodoc.html"
 :output "api.html"
 :url "/api.html")


(blorg-copy-static
 :output "static/{{ file }}"
 :url "/static/{{ file }}")

(blorg-export)
;;; publish.el ends here
