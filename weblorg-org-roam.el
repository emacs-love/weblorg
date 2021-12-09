(require 'weblorg)
(require 'org-roam)

;; ---- Input Source: org-roam ----

(defvar weblorg--org-id-updated nil
  "Internal state representing if org id mapping have been updated.")

(defvar weblorg--org-roam-db-synced nil
  "Internal state representing if org-roam db has been synced.")

(defun weblorg--input-source-org-roam-nodes (&optional filter render-org)
  "Computes an alist for each org-roam file node that matches FILTER, optionally
rendering the html if RENDER-ORG is non-nil."
  ;; Make sure we are starting off with a sync org-roam db.
  (unless weblorg--org-roam-db-synced
    (org-roam-db-sync)
    (setq weblorg--org-roam-db-synced t))

  (let* ((org-roam-files (sort (mapcar (lambda (file)
                                         `(("file" . ,(nth 0 file))
                                           ("atime" . ,(nth 1 file))
                                           ("mtime" . ,(nth 2 file))))
                                       (org-roam-db-query [:select [file atime mtime] :from files]))
                               (lambda (a b)
                                 (string< (cdr (assoc "file" a))
                                          (cdr (assoc "file" b)))))))

    ;; Update id locations based on the org-roam-files.
    (unless weblorg--org-id-updated
      (org-id-update-id-locations (mapcar (lambda (file) (cdr (assoc "file" file))) org-roam-files))
      (setq weblorg--org-id-updated t))

    (delq nil (mapcar (lambda (file)
                        (let ((file-path (cdr (assoc "file" file))))
                          (org-roam-with-temp-buffer file-path
                            '(print (current-buffer)))
                          (org-roam-with-temp-buffer file-path
                            (let* ((slug (weblorg--slugify (file-name-sans-extension (file-name-nondirectory file-path))))
                                   (node (org-roam-node-at-point))
                                   (tags (car (org-roam-db-query [:select [tag] :from tags :where (= node-id $s1)]
                                                                 (org-roam-node-id node))))
                                   (backlinks (mapcar (lambda (node)
                                                        (let* ((source-node (org-roam-backlink-source-node node))
                                                               (source-slug (weblorg--slugify (file-name-sans-extension (file-name-nondirectory (org-roam-node-file source-node)))))
                                                               (source-title (org-roam-node-title source-node)))
                                                          `(("slug" . ,source-slug)
                                                            ("title" . ,source-title))))
                                                      (org-roam-backlinks-get node)))
                                   (keywords (if render-org (weblorg--parse-org (buffer-string) file-path)
                                               `(("title" . ,(org-roam-node-title node))))))
                              (weblorg--prepend keywords (cons "backlinks" backlinks))
                              (weblorg--prepend keywords (cons "tags" tags))
                              (weblorg--prepend keywords (cons "slug" slug))
                              (if filter (if (funcall filter keywords) keywords
                                           nil)
                                keywords)))))
                      org-roam-files))))

(defun weblorg-input-source-org-roam-nodes (&optional filter)
  "Find all org-roam file nodes that match the optional FILTER"
  (mapcar (lambda (node)
            `(("node" . ,node)))
          (weblorg--input-source-org-roam-nodes filter t)))

(defun weblorg-input-source-org-roam-nodes-agg (&optional filter)
  "Aggregate all org-roam file nodes that match the optional FILTER."
  `((("nodes" . ,(weblorg--input-source-org-roam-nodes filter)))))

(provide 'weblorg-org-roam)
