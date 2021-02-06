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
(require 'weblorg)

(ert-deftest weblorg--url-parse ()
  (should (equal
           '("doc" . (("slug" . "how-to-skydive")
                      ("section" . "breathing")))
           (weblorg--url-parse "doc,slug=how-to-skydive,section=breathing")))
  (should (equal
           '("blog-posts" . (("slug" . "moon-phases")))
           (weblorg--url-parse "blog-posts,slug=moon-phases"))))

(ert-deftest weblorg--url-for ()
  (let ((site (weblorg-site :base-url "https://example.com")))
    (weblorg-route
     :name "docs"
     :input-pattern "*.org"
     :input-exclude "index.org$"
     :template "post.html"
     :url "/documentation/{{ slug }}-{{ stuff }}.html"
     :site site)
    (should
     (equal
      "https://example.com/documentation/something-else.html"
      (weblorg--url-for-v "docs"
                        '(("slug" . "something")
                          ("stuff" . "else"))
                        site)))))

(ert-deftest weblorg--slugify ()
  (should (equal (weblorg--slugify "!v0.1.1 - We've come a long way, friend!")
                 "v0-1-1-we-ve-come-a-long-way-friend")))

(ert-deftest weblorg--collect-n-aggr ()
  (weblorg-route
   :base-dir (expand-file-name "t/fixtures/test1/" default-directory)
   :name "route"
   :input-filter nil
   :input-pattern "src/*.org"
   :input-exclude "index.org$"
   :template "post.html"
   :url "/{{ slug }}.html")

  ;; When the collection and aggregation happen
  (let* ((route (weblorg--site-route (weblorg--site-get) "route"))
         (collection (weblorg--route-collect-and-aggregate route))
         (posts (mapcar #'cdar collection)))
    ;; we've got two posts there so far
    (should (equal (length collection) 2))

    ;; notice the list of files will be sorted
    (should (equal (mapcar (lambda(p) (weblorg--get-cdr p "slug")) posts)
                   (list "a-draft-post" "a-simple-post")))
    ;; also compare dates read and parsed from org files
    (should (equal (mapcar (lambda(p)
                             (format-time-string
                              "%Y-%m-%d"
                              (weblorg--get-cdr p "date")))
                           posts)
                   (list "2020-09-10" "2020-09-05"))))

  ;; reset the global to its initial state
  (clrhash weblorg--sites))

(ert-deftest weblorg--resolve-link ()
  ;; An implicit site gets created by this route that doesn't have a
  ;; site parameter
  (weblorg-route
   :name "docs"
   :input-pattern "*.org"
   :input-exclude "index.org$"
   :template "post.html"
   :url "/documentation/{{ slug }}-{{ stuff }}.html"
   :site (weblorg-site :base-url "https://example.com"))

  ;; When an URL for a given route is requested, then it should use
  ;; the `url' field of the route to interpolate the variables
  (should
   (equal (weblorg--url-for "docs,slug=overview,stuff=10" (weblorg-site :base-url "https://example.com"))
          "https://example.com/documentation/overview-10.html"))

  ;; Add anchor in any link it asked for
  (should
   (equal (weblorg--url-for "docs,slug=overview,stuff=10,anchor=sub-item" (weblorg-site :base-url "https://example.com"))
          "https://example.com/documentation/overview-10.html#sub-item"))

  ;; reset the global to its initial state
  (clrhash weblorg--sites))

;; Make sure we can register routes in a site and then retrieve them
;; later.
(ert-deftest weblorg--site-route--add-and-retrieve ()
  ;; An implicit site gets created by this route that doesn't have a
  ;; site parameter
  (weblorg-route
   :name "docs"
   :input-pattern ".*\\.org$"
   :input-exclude "index.org$"
   :template "post.html"
   :url "/{{ slug }}.html")

  ;; The site instance is being explicitly added to another site, so
  ;; this new route should not impact the previously defined one
  (weblorg-route
   :site (weblorg-site :base-url "https://example.com")
   :name "docs"
   :base-dir "/tmp/site"
   :theme "stuff"
   :input-pattern "docs/.*\\.org$"
   :input-exclude "index.org$"
   :template "docs.html"
   :url "docs/{{ slug }}.html")

  (let* ((site (weblorg--site-get))
         (route (weblorg--site-route site "docs")))
    (should (equal (gethash :name route) "docs"))
    (should (equal (gethash :input-pattern route) ".*\\.org$"))
    (should (equal (gethash :template route) "post.html"))
    (should (equal (gethash :url route) "/{{ slug }}.html"))
    (should (equal (gethash :input-exclude route) "index.org$")))

  (let* ((site (weblorg--site-get "https://example.com"))
         (route (weblorg--site-route site "docs")))
    (should (equal (gethash :name route) "docs"))
    (should (equal (gethash :input-pattern route) "docs/.*\\.org$"))
    (should (equal (gethash :template route) "docs.html"))
    (should (equal (gethash :url route) "docs/{{ slug }}.html"))
    (should (equal (gethash :input-exclude route) "index.org$"))
    (should (equal (gethash :theme route) "stuff"))
    (should (equal (gethash :template-dirs route)
                   (list "/tmp/site/templates" ; deprecated
                         "/tmp/site/theme/templates"
                         (weblorg--theme-dir "stuff" "templates")))))

  ;; reset the global to its initial state
  (clrhash weblorg--sites))

;; Make sure that registering a new site works and that data
;; associated with it can be retrieved
(ert-deftest weblorg--site-get--success ()
  (weblorg-site :base-url "http://localhost:9000" :theme "stuff")
  (let ((the-same-weblorg (weblorg--site-get "http://localhost:9000")))
    (should (equal (gethash :base-url the-same-weblorg) "http://localhost:9000"))
    (should (equal (gethash :theme the-same-weblorg) "stuff")))
  ;; reset the global to its initial state
  (clrhash weblorg--sites))

;; Make sure this lil helper works
(ert-deftest weblorg--get--with-and-without-default ()
  (should (equal (weblorg--get '((:base-dir "expected")) :base-dir "wrong") "expected"))
  (should (equal (weblorg--get '() :base-dir "expected") "expected")))

;;; weblorg-tests.el ends here
