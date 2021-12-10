(require 'weblorg)
(require 'org-roam)

;; ---- Input Source: org-roam ----

(defvar weblorg--org-id-updated nil
  "Internal state representing if org id mapping have been updated.")

(defvar weblorg--org-roam-db-synced nil
  "Internal state representing if org-roam db has been synced.")

(defun weblorg--input-source-org-roam-nodes (&optional filter-fn sort-fn limit render-org)
  "Computes an alist for each org-roam file node and return the list of alists.

A FILTER-FN function taking a org-roam-node and returning a bool can be provided
to select which nodes to return.
A SORT-FN function taking two org-roam-nodes and returning a bool can be
provided to order the returned nodes.
A LIMIT integer value can be provided to limit how many results are returned.
A RENDER-ORG bool value can be provided to selectively enable rendering HTML for
each node."
  ;; Make sure we are starting off with a sync org-roam db.
  (unless weblorg--org-roam-db-synced
    (org-roam-db-sync)
    (setq weblorg--org-roam-db-synced t))

  (let ((org-roam-files (mapcar (lambda (result)
                                  (car result))
                                (org-roam-db-query [:select file :from files]))))
    ;; Update id locations based on the org-roam-files.
    (unless weblorg--org-id-updated
      (org-id-update-id-locations org-roam-files)
      (setq weblorg--org-id-updated t))

    (let* ((org-roam-nodes (mapcar (lambda (file)
                                     (org-roam-with-temp-buffer file
                                         (org-roam-node-at-point)))
                                   org-roam-files))
           (org-roam-nodes (seq-filter (lambda (node)
                                         (if filter-fn (funcall filter-fn node)
                                           t))
                                       org-roam-nodes))
           (org-roam-nodes (sort org-roam-nodes (or sort-fn
                                                    (lambda (a b)
                                                      (string< (org-roam-node-title a)
                                                               (org-roam-node-title b))))))
           (org-roam-nodes (if limit (seq-take org-roam-nodes limit)
                             org-roam-nodes)))
      (mapcar (lambda (node)
                (let ((slug (weblorg--slugify (file-name-sans-extension (file-name-nondirectory (org-roam-node-file node)))))
                      (tags (org-roam-node-tags node))
                      (backlinks (mapcar (lambda (node)
                                           (let* ((source-node (org-roam-backlink-source-node node))
                                                  (source-slug (weblorg--slugify (file-name-sans-extension (file-name-nondirectory (org-roam-node-file source-node)))))
                                                  (source-title (org-roam-node-title source-node)))
                                             `(("slug" . ,source-slug)
                                               ("title" . ,source-title))))
                                         (org-roam-backlinks-get node)))
                      (atime (org-roam-node-file-atime node))
                      (mtime (org-roam-node-file-mtime node))
                      (keywords (if render-org (weblorg--parse-org-file (org-roam-node-file node))
                                  `(("title" . ,(org-roam-node-title node))))))
                  (weblorg--prepend keywords (cons "backlinks" backlinks))
                  (weblorg--prepend keywords (cons "tags" tags))
                  (weblorg--prepend keywords (cons "atime" atime))
                  (weblorg--prepend keywords (cons "mtime" mtime))
                  (weblorg--prepend keywords (cons "slug" slug))
                  keywords))
              org-roam-nodes))))

(defun weblorg-input-source-org-roam-nodes (&optional filter-fn sort-fn limit)
  "Find all org-roam file nodes.

A FILTER-FN function taking a org-roam-node and returning a bool can be provided
to select which nodes to return.
A SORT-FN function taking two org-roam-nodes and returning a bool can be
provided to order the returned nodes.
A LIMIT integer value can be provided to limit how many results are returned."
  (mapcar (lambda (node)
            `(("node" . ,node)))
          (weblorg--input-source-org-roam-nodes filter-fn sort-fn limit t)))

(defun weblorg-input-source-org-roam-nodes-agg (&optional filter-fn sort-fn limit)
  "Aggregate all org-roam file nodes.

A FILTER-FN function taking a org-roam-node and returning a bool can be provided
to select which nodes to return.
A SORT-FN function taking two org-roam-nodes and returning a bool can be
provided to order the returned nodes.
A LIMIT integer value can be provided to limit how many results are returned."
  `((("nodes" . ,(weblorg--input-source-org-roam-nodes filter-fn sort-fn limit)))))

(provide 'weblorg-org-roam)
