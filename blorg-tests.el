;;; blorg-tests --- Static site generator for Emacs-Lisp; -*- lexical-binding: t -*-
;;
;; Author: Lincoln Clarete <lincoln@clarete.li>
;;
;; Copyright (C) 2020  Lincoln Clarete
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;;
;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'blorg)

(ert-deftest blorg--collect-n-aggr ()
  (blorg-route
   :base-dir (expand-file-name "fixtures/test1/" default-directory)
   :name "route"
   :input-pattern ".*\\.org$"
   :input-exclude "index.org$"
   :template "post.html"
   :url "/{{ slug }}.html")

  ;; When the collection and aggregation happen
  (let* ((route (blorg--site-route (blorg--site-get) "route"))
         (collection (blorg--route-collect-and-aggregate route))
         (posts (mapcar #'cdar collection)))
    (should (equal (length collection) 2))
    (should (equal (mapcar (lambda(p) (blorg--get-cdr p "slug")) posts)
                   (list "a-simple-post" "a-draft-post"))))

  ;; reset the global to its initial state
  (clrhash blorg--sites))

(ert-deftest blorg--resolve-link ()
  ;; An implicit site gets created by this route that doesn't have a
  ;; site parameter
  (blorg-route
   :name "docs"
   :input-pattern ".*\\.org$"
   :input-exclude "index.org$"
   :template "post.html"
   :url "/documentation/{{ slug }}-{{ stuff }}.html"
   :site (blorg-site :base-url "https://example.com"))

  ;; When an URL for a given route is requested, then it should use
  ;; the `url' field of the route to interpolate the variables
  (should
   (equal (blorg--url-for "docs,slug=overview,stuff=10" (blorg-site :base-url "https://example.com"))
          "https://example.com/documentation/overview-10.html"))

  ;; reset the global to its initial state
  (clrhash blorg--sites))

;; Make sure we can register routes in a site and then retrieve them
;; later.
(ert-deftest blorg--site-route--add-and-retrieve ()
  ;; An implicit site gets created by this route that doesn't have a
  ;; site parameter
  (blorg-route
   :name "docs"
   :input-pattern ".*\\.org$"
   :input-exclude "index.org$"
   :template "post.html"
   :url "/{{ slug }}.html")

  ;; The site instance is being explicitly added to another site, so
  ;; this new route should not impact the previously defined one
  (blorg-route
   :site (blorg-site :base-url "https://example.com")
   :name "docs"
   :base-dir "/tmp/site"
   :input-pattern "docs/.*\\.org$"
   :input-exclude "index.org$"
   :template "docs.html"
   :url "docs/{{ slug }}.html")

  (let* ((site (blorg--site-get))
         (route (blorg--site-route site "docs")))
    (should (equal (gethash :name route) "docs"))
    (should (equal (gethash :input-pattern route) ".*\\.org$"))
    (should (equal (gethash :template route) "post.html"))
    (should (equal (gethash :url route) "/{{ slug }}.html"))
    (should (equal (gethash :input-exclude route) "index.org$")))

  (let* ((site (blorg--site-get "https://example.com"))
         (route (blorg--site-route site "docs")))
    (should (equal (gethash :name route) "docs"))
    (should (equal (gethash :input-pattern route) "docs/.*\\.org$"))
    (should (equal (gethash :template route) "docs.html"))
    (should (equal (gethash :url route) "docs/{{ slug }}.html"))
    (should (equal (gethash :input-exclude route) "index.org$"))
    (should (equal (gethash :template-dirs route)
                   (cons "/tmp/site/templates" (blorg--template-base)))))

  ;; reset the global to its initial state
  (clrhash blorg--sites))

;; Make sure that registering a new site works and that data
;; associated with it can be retrieved
(ert-deftest blorg--site-get--success ()
  (blorg-site :base-url "http://localhost:9000" :theme "stuff")
  (let ((the-same-blorg (blorg--site-get "http://localhost:9000")))
    (should (equal (gethash :base-url the-same-blorg) "http://localhost:9000"))
    (should (equal (gethash :theme the-same-blorg) "stuff")))
  ;; reset the global to its initial state
  (clrhash blorg--sites))

;; Make sure this lil helper works
(ert-deftest blorg--get--with-and-without-default ()
  (should (equal (blorg--get '((:base-dir "expected")) :base-dir "wrong") "expected"))
  (should (equal (blorg--get '() :base-dir "expected") "expected")))

;;; blorg-tests.el ends here
