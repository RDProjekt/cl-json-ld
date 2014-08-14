;;;; Copyright (c) 2014 RD Projekt Sp. z o.o. Sp. k. 
;;;; http://www.rdprojekt.pl/
;;;; Use is subject to license terms.
;;;;
;;;; This file is part of CL-JSON-LD.
;;;;
;;;; CL-JSON-LD is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; CL-JSON-LD is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with CL-JSON-LD.  If not, see <http://www.gnu.org/licenses/>.

(defpackage :cl-json-ld-system (:use :asdf :cl))
(in-package :cl-json-ld-system)

(defparameter *json-library* (cond
 ((find-system :st-json nil)
  :st-json)
 ((find-system :yason nil)
  :yason)
 (t :st-json)))

(format t "~&CHOSEN JSON LIBRARY: ~a~%" *json-library*)

(defparameter *depends*
  (cons *json-library* (list :puri :split-sequence :drakma :cl-ppcre :cl-fad :alexandria :ironclad)))

(defparameter *jsd-inner-filename* (if (eq *json-library* :st-json) "jsd-st-json" "jsd-yason"))

(eval `(asdf:defsystem :cl-json-ld
  :name "CL-JSON-LD"
  :description "JSON-LD API implementation"
  :author "RD Projekt Sp. z o.o. Sp. k."
  :licence "GPL v3 (see COPYING.txt)"
  :version "0.1"
  :depends-on ,*depends*
  :components ((:static-file "cl-json-ld.asd")
               (:static-file "COPYING.txt")
               (:static-file "README.md")
               (:module src
                :components ((:file "package")
                             (:file "utils" :depends-on("package"))
                             (:file "sha1" :depends-on("package"))
                             (:file ,*jsd-inner-filename* :depends-on("utils"))
                             (:file "jsd-hashmap" :depends-on("utils" ,*jsd-inner-filename*))
                             (:file "jsonld" :depends-on("utils" "jsd-hashmap"))
                             (:file "context" :depends-on("utils" "jsd-hashmap"))
                             (:file "compact" :depends-on("utils" "jsd-hashmap"))
                             (:file "flatten" :depends-on("utils" "jsd-hashmap"))
                             (:file "normalize" :depends-on("utils" "jsd-hashmap"))
                             (:file "frame" :depends-on("utils" "jsd-hashmap")))))))

(asdf:defsystem :cl-json-ld-tests
  :description "JSON-LD API tests"
  :author "RD Projekt Sp. z o.o. Sp. k."
  :version "0.1"
  :depends-on (:cl-fad :fiveam :cl-json-ld)
  :components ((:module src
                :components ((:file "package-tests")
                             (:file "unit-tests" :depends-on("package-tests"))))))

;; In order to execute tests first load the current asd file (e.g. (load "cl-json-ld.asd"))
;; Then execute (asdf:operate 'asdf:test-op :cl-json-ld-tests)
;; In order to use non-default manifest path use (cl-json-ld-tests:run-unit-tests <manifest-path>)

(defparameter *jsonld-tests-manifest*
  (let ((jsonld-dir (make-pathname :directory (pathname-directory *load-truename*))))
    (merge-pathnames "./json-ld.org/test-suite/manifest.jsonld" jsonld-dir)))

(defmethod perform ((op test-op) (c (eql (find-system :cl-json-ld-tests))))
  (format t "~&Running unit tests with manifest: ~s" *jsonld-tests-manifest*)
  (funcall (intern (symbol-name `#:run-unit-tests) :cl-json-ld-tests) *jsonld-tests-manifest*))


