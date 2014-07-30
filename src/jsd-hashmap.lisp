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

;;;; abstractions for manipulating json objects

(in-package :cl-json-ld-jsd)

(defun is-object (value)
  "Return T iff given value is an Object."
  (typep value 'hash-table))

(defun is-empty-object (value)
  "Return T iff given value is an empty Object."
  (and (is-object value)
       (eq 0 (hash-table-count value))))

;;;; deepcopy

(defmethod cl-json-ld-utils:deepcopy ((element hash-table))
  (let ((res (make-string-hashtable)))
    (maphash #'(lambda (k v) (setf (gethash k res) (deepcopy v))) element)
    res))

;;;; objs-equal-p

(defun cdr-assoc (key list &rest rest)
  (let ((kv (apply #'assoc key list rest)))
    (if kv (cdr kv) nil)))

(defmethod objs-equal-p ((tab1 hash-table) (tab2 hash-table))
  (if (eql (hash-table-count tab1) (hash-table-count tab2))
      (progn 
        (loop for key being the hash-keys of tab1 using (hash-value val1) do
              (cl-json-ld-utils::propagate-negative-result val1 (gethash key tab2) objs-equal-p))
        (return-from objs-equal-p t))
    (cl-json-ld-utils::lazy-message-eq nil "Different object lengths ~s != ~s" tab1 tab2)))


(defun jsd-get (key jsd)
  "Fetch a value from a JS object. Returns a second value like
gethash."
  (gethash key jsd))

(defun (setf jsd-get) (val key jsd)
  "Store a value in a JS object."
  (setf (gethash key jsd) val))

(defun jsd-count (jsd)
  "Return the number of items (unique keys) in the JSD."
  (hash-table-count jsd))

(defun jsd-map (func jsd)
  "Iterate over the key/value pairs in a JS object."
  (maphash func jsd))

(defun jsd-sorted-map (func jsd &key (test #'string<))
  "Iterate over the key/value pairs in JSD, in order of sorted keys."
  (sorted-maphash func jsd :test test))

(defun jsd-copied-map (func jsd)
  "Iterate over key/value pairs in JSD, copying the JSD beforehand,
  so that the code in func can manipulate the map."
  (let ((items (hash-table-alist jsd)))
    (loop for (key . value) in items do
          (funcall func key value))))

;;;;
;;;; JSON HELPER FUNCTIONS
;;;;

(defun jsd-haskey (key jsd)
  (multiple-value-bind (x present-p) (jsd-get key jsd)
    (declare (ignore x))
    present-p))

(defun multi-jsd-get (jsd &rest keys)
  "Return a value, descending into keys. If there is no key on the way, return nil."
  (loop for key in keys do
     (setf jsd (jsd-get key jsd))
     (unless jsd
       (return-from multi-jsd-get nil)))
  jsd)

(defmacro jsd-when-get (key jsd (value) &body body)
  "Execute body only if key exists in jsd, with value bound to value."
  (let ((present-p (gensym)))
    `(multiple-value-bind (,value ,present-p) (jsd-get ,key ,jsd)
	  (when ,present-p
	    ,@body))))

(defmacro jsd-getdefault (key jsd default)
  "Return the value under given key in a JSD.
  If the key is not present in the JSD, evaluate default and return it,
  WITHOUT adding it to the JSD."
  (let ((key-name (gensym)) (table-name (gensym)) (value (gensym)) (present (gensym)))
    `(let ((,key-name ,key) (,table-name ,jsd))
       (multiple-value-bind (,value ,present) (jsd-get ,key-name ,table-name)
         (if ,present
             ,value
             ,default)))))

(defmacro jsd-setdefault (jsd key value-form)
  "Return the value under given key in a JSD.
  If the key is not present in the JSD, evaluate value-form,
  add it to the JSD and return it."
  (let ((table-name (gensym)) (key-name (gensym)) (value (gensym)) (present (gensym)))
    `(let ((,table-name ,jsd) (,key-name ,key))
       (multiple-value-bind (,value ,present) (jsd-get ,key-name ,table-name)
         (declare (ignore ,value))
         (unless ,present (setf (jsd-get ,key-name ,table-name) ,value-form))
         (jsd-get ,key-name ,table-name)))))

(defun jsd-remove (key jsd)
  "Remove given key from jsd."
  (remhash key jsd))

(defun jsd-sorted-keys (jsd)
  "Return a sorted list of keys in a given JSD."
  (let ((keys (hash-table-keys jsd)))
    (setf keys (sort keys #'string<))
    keys))
