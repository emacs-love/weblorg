;;; init.el --- Initialize emacs environment
;;; Commentary:
;;
;; Installation and configuration and dependent packages.
;;
;;; Code:

(setq user-emacs-directory (expand-file-name ".emacs"))

;; Boostrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Bootstrap use-package.
(straight-use-package 'use-package)

(use-package org
  :straight t
  :config
  ;; This is not necessary and is only used for this example to avoid polluting
  ;; the local user's id locations file with the org files from this example.
  (setq org-id-locations-file (expand-file-name "./org-id-locations")))
(use-package org-roam
  :straight t
  :after org
  :init
  (setq org-roam-v2-ack t)
  :config
  (setq org-roam-directory (expand-file-name "./org")
        org-roam-file-extensions '("org")
        ;; This is not necessary and is only used for this example to avoid
        ;; polluting the local user's org-roam db with the org files from this
        ;; example.
        org-roam-db-location (expand-file-name "./org-roam.db")))

(use-package templatel :straight t)

(add-to-list 'load-path "../../")
(require 'weblorg)
(require 'weblorg-org-roam)
;;; init.el ends here
