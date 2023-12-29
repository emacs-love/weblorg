;;; publish.el --- Export org roam file nodes
;;; Commentary:
;;
;; Example configuration for generating a static site from org-roam file nodes.
;;
;;; Code:

(add-to-list 'load-path ".")
(load "init")

(org-roam-db-sync)

(let* ((site (weblorg-site
              :default-route "org-nodes"
              :theme nil))
       (org-roam-nodes-filter (lambda (node)
                                (member "public" (org-roam-node-tags node)))))
  (weblorg-route
   :name "index"
   :input-source (lambda () (weblorg-input-source-org-roam-nodes-agg org-roam-nodes-filter))
   :template "index.html"
   :output "output/index.html"
   :url "/"
   :site site)
  (weblorg-route
   :name "org-nodes"
   :input-source (lambda () (weblorg-input-source-org-roam-nodes org-roam-nodes-filter))
   :template "org-node.html"
   :output "output/{{ slug }}/index.html"
   :url "/{{ slug }}/"
   :site site)

  (setq debug-on-error t)
  (weblorg-export))
;;; publish.el ends here
