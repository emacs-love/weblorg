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

;; Set site wide configuration
(weblorg-site :theme "site")

;; Generate index
(weblorg-route
 :name "index"
 :input-pattern "./index.org"
 :template "index.html"
 :output "./index.html"
 :url "/")

(weblorg-route
 :name "api"
 :input-source (weblorg-input-source-autodoc-sections
                `(("Render template strings" . "^templatel-render")
                  ("Template environments" . "^templatel-env")
                  ("Filters" . "^templatel-filter")
                  ("Exceptions" . ,(concat "templatel-" (regexp-opt '("syntax-error"
                                                                      "runtime-error"
                                                                      "backtracking"))))))
 :template "autodoc.html"
 :output "api.html"
 :url "/api.html")


(weblorg-copy-static
 :output "static/{{ file }}"
 :url "/static/{{ file }}")

(weblorg-export)
;;; publish.el ends here
