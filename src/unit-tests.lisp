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

(in-package :cl-json-ld-tests)

(defparameter *verbose* nil)

;;;--------------------------------------------------
;;;              GENERAL FUNCTIONS
;;;--------------------------------------------------

(defun plist-update (plist new-values)
  "Update plist with values from a different plist."
  (loop for (indicator value) on new-values by #'cddr do
    (setf (getf plist indicator) value))
  plist)

(defun read-file (filepath)
  "Read whole file to string"
  (with-open-file (s filepath)
    (let* ((len (file-length s))
          (data (make-array len :element-type 'character :fill-pointer t)))
      (setf (fill-pointer data) (read-sequence data s))
      data)))

;;;--------------------------------------------------
;;;              JSONLD HELPERS
;;;--------------------------------------------------

(defun get-jsonld-values (node property)
  (let ((val (jsd-get property node)))
    (when val
      (if (typep val 'cons) val (list val)))))

(defun read-json-file (filepath)
  "Read file to json object"
  (when *verbose*
    (fresh-line)
    (format t "Loading json file from: ~a~C" filepath #\return))
  (with-open-file (stream filepath)
    (jsd-read stream)))

(defun is-jsonld-type (node type_)
  (let ((node-type (get-jsonld-values node "@type"))
        (types (if (typep type_ 'cons) type_ (list type_))))
    (intersection node-type types :test 'string=)))


;;;--------------------------------------------------
;;;              MANIFEST AND TEST-ENTRY OBJECTS
;;;--------------------------------------------------

(defclass manifest ()
  ((data
    :initarg :data)
   (filename
    :initarg :filename)))

(defmethod print-object ((obj manifest) out)
  (with-slots (data filename) obj
    (format out "Manifest(~s: ~s)" filename data)))

(defclass test-entry ()
  ((manifest
    :initarg :manifest)
  (data
    :initarg :data)
   (filename
    :initarg :filename)))

(defun test-dirname (test-entry)
  (directory-namestring (slot-value test-entry 'filename)))

(defun test-base (test-entry)
  (concatenate 'string (jsd-get "baseIri" (slot-value (slot-value test-entry 'manifest) 'data))
                       (jsd-get "input" (slot-value test-entry 'data))))

(defmethod print-object ((obj test-entry) out)
  (with-slots (data filename) obj
    (format out "TestEntry(~s: ~s)" filename data)))

(defun get-test-description (test-entry)
  (with-slots (manifest (test-data data)) test-entry
    (with-slots ((manifest-data data)) manifest
      (format nil "~a ~a ~a"
        (jsd-get "name" manifest-data)
        (subseq (jsd-get "@id" test-data) 2)
        (or (jsd-get "purpose" test-data) (jsd-get "name" test-data))))))

;;;            LOADING TEST_ENTRIES FROM MANIFEST

(defun get-filename-and-entry (entry root-dir fallback-path)
  (if (stringp entry)
      (let ((entry-path (merge-pathnames-as-file root-dir entry)))
        (cons entry-path (read-json-file entry-path)))
    (cons fallback-path entry)))

(defun load-manifest-tests-rec (manifest result)
  (with-slots ((manifest-data data) (manifest-path filename)) manifest
    (let ((manifest-dir (pathname-directory-pathname manifest-path)))
      (loop for entry in (get-jsonld-values manifest-data "sequence") do
        (destructuring-bind (entry-path . entry) (get-filename-and-entry entry manifest-dir manifest-path)
          (if (is-jsonld-type entry "mf:Manifest")
              (setf result (load-manifest-tests-rec (make-instance 'manifest :data entry :filename entry-path) result))
              (push (make-instance 'test-entry :manifest manifest :data entry :filename entry-path) result))))
      result)))


(defun load-tests (manifest-path)
  (let* ((manifest-data (read-json-file manifest-path))
         (manifest (make-instance 'manifest :data manifest-data :filename manifest-path))
         (res (load-manifest-tests-rec manifest (list))))
    (nreverse res)))

(defun read-test-url (property)
  #'(lambda (test-entry)
      (with-slots ((test-data data) manifest) test-entry
        (let ((val (jsd-get property test-data)))
          (when val
            (with-slots ((manifest-data data)) manifest
              (concatenate 'string (jsd-get "baseIri" manifest-data) val)))))))

(defun read-test-property (property)
  #'(lambda (test-entry)
      (with-slots (data filename) test-entry
        (let ((val (jsd-get property data)))
          (when val
            (let ((file-path (merge-pathnames-as-file (pathname-directory-pathname filename) val)))
              (if (ends-with-p (namestring file-path) ".jsonld")
                  (read-json-file file-path)
                (read-file file-path))))))))

(defun get-manifest-name (test-entry)
  (with-slots ((test-data data) manifest) test-entry
    (with-slots ((manifest-data data)) manifest
      (jsd-get "name" manifest-data))))

(alexandria:define-constant +test-suite-base-url+ "http://json-ld.org/test-suite" :test #'string=)
(defparameter *root-manifest-dir* nil)


(defun create-document-loader (test-entry)
  (with-slots ((test-data data) manifest) test-entry
    (labels 
     ((load-locally (url)
        (let ((context-url +jsd-null+)
              (document-url url)
              (options (jsd-get "option" test-data)))
          
          (when (and options (equal url (test-base test-entry)))
            (cond
             ((and (jsd-haskey "redirectTo" options) (>= (jsd-get "httpStatus" options) 300))
              (setf document-url (concatenate 'string (jsd-get "baseIri" (slot-value manifest 'data))
                                                      (jsd-get "redirectTo" options))))
             
             ((jsd-haskey "httpLink" options)
              (let ((content-type (jsd-get "contentType" options)))
                (when (and (null content-type) (ends-with-p url ".jsonld"))
                  (setf content-type "application/ld+json"))
                
                (let ((link-header (jsd-getdefault "httpLink" options "")))
                  (when (listp link-header)
                    (setf link-header (format nil "~{~a~^,~}" link-header)))
                  (setf link-header (gethash "http://www.w3.org/ns/json-ld#context" 
                                             (cl-json-ld::parse-link-header link-header)))
                  (when (and link-header (not (equal content-type "application/ld+json")))
                    (when (> (length link-header) 1)
                      (error "multiple context link headers"))
                    (setf context-url (gethash "target" (elt link-header 0)))))))))
                             
          (let* ((filename (merge-pathnames-as-file *root-manifest-dir*
                                                    (subseq document-url (+ (length +test-suite-base-url+) 1))))
                 (document (read-json-file filename)))
            (values context-url document-url document))))
             
             
             (local-loader (url)
                             (if (or (not (starts-with-p url +test-suite-base-url+))
                                     (string= (get-manifest-name test-entry) "Remote document"))
                                 (funcall (get-document-loader) url)
                               (load-locally url))))
        #'local-loader)))

(defun json-option->lisp (value)
  "Convert a test option to a lisp value."
  (cond
   ((jsd-bool-p value) (if (eq value +jsd-true+) t nil))
   ((typep value 'string)
    value)
   ((typep value 'fixnum)
    value)
   ((listp value)
    (mapcar #'json-option->lisp value))
   (t (error "Don't know how to convert option ~a" (type-of value)))))

(defun create-test-options (&optional opts)
  #'(lambda (test-entry)
      (let ((options '()))
        (with-slots ((test-data data)) test-entry
          (let ((http-options (list "contentType" "httpLink" "httpStatus" "redirectTo"))
                (test-options (jsd-get "option" test-data)))
            (when test-options
              (jsd-map #'(lambda (option-name value)
                           (unless (find option-name http-options :test #'equal)
                             (let* ((sym (camel-case->symbol option-name :keyword)))
                               (setf (getf options sym) (json-option->lisp value)))))
                       test-options))))
        (setf (getf options :document-loader) (create-document-loader test-entry))
        (setf options (plist-update options opts))

        (when (getf options :expand-context)
          (let ((filename (merge-pathnames (getf options :expand-context) (test-dirname test-entry))))
            (setf (getf options :expand-context) (read-json-file filename))))        
        options
        )))

(defparameter *test-types* (list))

(defun register-test-type (type func param-builders)
  (setf *test-types* (acons type (list :type type :func func :params param-builders) *test-types*)))

(register-test-type "jld:FromRDFTest" #'from-rdf 
                    (list (read-test-property "input") (create-test-options (list :format "application/nquads"))))
(register-test-type "jld:ToRDFTest" #'to-rdf 
                    (list (read-test-url "input") (create-test-options (list :format "application/nquads"))))
(register-test-type "jld:ExpandTest" #'expand 
                    (list (read-test-url "input") (create-test-options (list :format "application/nquads"))))
(register-test-type "jld:CompactTest" #'compact 
                    (list (read-test-url "input") (read-test-property "context") (create-test-options ())))
(register-test-type "jld:FlattenTest" #'flatten
                    (list (read-test-url "input") (read-test-property "context") (create-test-options ())))
(register-test-type "jld:NormalizeTest" #'normalize
                    (list (read-test-property "input") (create-test-options (list :format "application/nquads"))))
(register-test-type "jld:FrameTest" #'frame
                    (list (read-test-url "input") (read-test-property "frame") (create-test-options ())))


 
(defun get-test-type (test-entry)
  (with-slots (data) test-entry
    (loop for (type . type-desc) in *test-types* do
          (when (is-jsonld-type data type)
            (return-from get-test-type type-desc)))
    (error "No implementation found for test type ~a" (get-jsonld-values data "@type"))))

(defun run-jsonld-test (test-entry)
  "returns possibly multiple values, first value tells if the test passed, 
  optional second value is test error message and is present when test failed"
  (with-slots (manifest (test-data data)) test-entry
    (with-slots ((manifest-data data) (manifest-path filename)) manifest
      ;;; expand @id
      (setf (jsd-get "@id" test-data)
        (concatenate 'string (jsd-get "baseIri" manifest-data) 
          (file-namestring manifest-path) 
          (jsd-get "@id" test-data)))
      (let* ((positive-p (is-jsonld-type test-data "jld:PositiveEvaluationTest"))
             (negative-p (is-jsonld-type test-data "jld:NegativeEvaluationTest"))
             (type (get-test-type test-entry))
             (params (mapcar (lambda (func) (funcall func test-entry)) (getf type :params)))
             (expected-result (if positive-p 
                                  (funcall (read-test-property "expect") test-entry) 
                                  (jsd-get "expect" test-data)))
             (actual-result nil))
        
        (handler-bind
            ((error #'(lambda (exception)
                        (when negative-p
                          (let ((error-code (json-ld-error-code exception)))
                            (multiple-value-bind (passed message) (assert-equal expected-result error-code)
                              (when passed
                                (return-from run-jsonld-test (values passed message)))))))))
          (setf actual-result (apply (getf type :func) params)))
        
        (unless positive-p
          (error "Expected an error; one was not raised."))
        (assert-equal expected-result actual-result)))))

(defun assert-equal (expected actual)
  (multiple-value-bind (passed message) (objs-equal-p expected actual)
    (when (not passed)
      (format t "Different values:~&EXPECTED: ~s~&ACTUAL: ~s" expected actual)) 
    (values passed message)))

(defmacro generate-test (test-entry)
  (let ((test-name (gensym)))
    `(let ((,test-name (get-test-description ,test-entry)))
       (eval `(test ,(make-symbol ,test-name)
                    (let ((test-entry ,test-entry))
                      (multiple-value-bind (passed message) (run-jsonld-test test-entry)
                        (if passed
                            (pass)
                          (fail "~s" message)))))))))

(defun get-tests-with-ids (all-tests &rest ids)
  "Return only tests that have given IDs. IDs are usually strings in the form \"#tXXXX\", where X is a digit."
  (remove-if-not #'(lambda (entry)
                     (find (jsd-get "@id" (slot-value entry 'data)) ids :test #'string=)) all-tests))

(defun get-tests-without-ids (all-tests &rest ids)
  "Return only tests that don't have given IDs. 
  IDs are usually strings in the form \"#tXXXX\", where X is a digit."
  (remove-if #'(lambda (entry)
                 (find (jsd-get "@id" (slot-value entry 'data)) ids :test #'string=)) all-tests))


(defun run-unit-tests (manifest-path &key (debug-on-error t) (debug-on-failure nil))
  (let ((*root-manifest-dir* (pathname-directory-pathname manifest-path))
        (*debug-on-error* debug-on-error)
        (*debug-on-failure* debug-on-failure)
        (*print-names* nil)
        (*read-default-float-format* 'double-float))
    (format t "~&ROOT MANIFEST ~s~&" *root-manifest-dir*)
    (let* ((suite (make-suite :jsonld-test-suite))
          (tests (load-tests manifest-path)))
      (in-suite :jsonld-test-suite)
      (loop for test-entry in tests do
            (generate-test test-entry))
      (time (run! suite)))))
