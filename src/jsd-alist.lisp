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

;;;; implementation using st-json internally
(defclass jsd-json ()
  ((alist
    :initarg :alist)))

(defgeneric convert-to-jsd (value))

(defmethod convert-to-jsd ((value jso))
  (let ((res nil))
    (mapjso #'(lambda (k v)
                (setf res (acons k (convert-to-jsd v) res)))
            value)
    (make-instance 'jsd-json :alist res)))

(defmethod convert-to-jsd ((value cons))
  (mapcar #'(lambda (item)
              (convert-to-jsd item))
    value))

(defmethod convert-to-jsd ((value t))
  value)

(defun jsd-read (source)
  (convert-to-jsd (read-json source)))

(defun jsd-make (&rest args)
  (convert-to-jsd (apply #'jso args)))

(defun is-object (value)
  "Return T iff given value is an Object."
  (typep value 'jsd-json))

(defun is-empty-object (value)
  "Return T iff given value is an empty Object."
  (and (is-object value)
       (with-slots (alist) value
         (eq 0 (length alist)))))

;;;; to string

(defmethod write-json-element ((element jsd-json) stream)
  (with-slots (alist) element
    (write-json-element (st-json::make-jso :alist alist) stream)))

(defun jsd-to-string (element)
  "Write a value's JSON representation to a string."
  (write-json-to-string element))

;;;; deepcopy

(defmethod cl-json-ld-utils:st-json-deepcopy ((element jsd-json))
  (with-slots (alist) element
    (make-instance 'jsd-json :alist (loop for (key . val) in alist
                                        collect (cons (copy-seq key) (cl-json-ld-utils:st-json-deepcopy val))))))

;;;; objs-equal-p

(defun cdr-assoc (key list &rest rest)
  (let ((kv (apply #'assoc key list rest)))
    (if kv (cdr kv) nil)))

(defmethod objs-equal-p ((obj1 jsd-json) (obj2 jsd-json))
  (with-slots ((list1 alist)) obj1
    (with-slots ((list2 alist)) obj2
      (if (eql (length list1) (length list2))
          (progn 
            (loop
              for (key . val1) in list1
              do 
              (let* ((val2 (cdr-assoc key list2 :test #'string=)))
                (cl-json-ld-utils::propagate-negative-result val1 val2 objs-equal-p)))
            (return-from objs-equal-p t))
        (cl-json-ld-utils::lazy-message-eq nil "Different object lengths ~s != ~s" obj1 obj2)))))


(defun jsd-get (key jsd)
  "Fetch a value from a JS object. Returns a second value like
gethash."
  (with-slots (alist) jsd
    (let ((pair (assoc key alist :test #'string=)))
      (values (cdr pair) (and pair t)))))

(defun (setf jsd-get) (val key jsd)
  "Store a value in a JS object."
  (with-slots (alist) jsd
    (let ((pair (assoc key alist :test #'string=)))
      (if pair
          (setf (cdr pair) val)
        (prog1 val (push (cons key val) alist))))))

(defun jsd-count (jsd)
  "Return the number of items (unique keys) in the JSD."
  (with-slots (alist) jsd
    (length alist)))

(defun jsd-map (func jsd)
  "Iterate over the key/value pairs in a JS object."
  (with-slots (alist) jsd 
    (loop :for (key . val) :in alist
          :do (funcall func key val))))

(defun jsd-sorted-map (func jsd &key (test #'string<))
  "Iterate over the key/value pairs in JSD, in order of sorted keys."
  (with-slots (alist) jsd
    (let ((items (copy-list alist)))
      (setf items (sort items #'(lambda (left right) (funcall test (car left) (car right)))))
      (loop for (key . value) in items do
            (funcall func key value)))))

(defun jsd-copied-map (func jsd)
  "Iterate over key/value pairs in JSD, copying the JSD beforehand,
  so that the code in func can manipulate the map."
  (with-slots (alist) jsd
    (let ((items (copy-list alist)))
      (loop for (key . value) in items do
            (funcall func key value)))))

(defun jsd-is-bool-p (value)
  (typep value 'json-bool))

(defconstant +jsd-true+ :true)
(defconstant +jsd-false+ :false)
    
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
  (with-slots (alist) jsd
    (setf alist (remove key alist :key #'car :test #'string=))))

(defun jsd-sorted-keys (jsd)
  "Return a sorted list of keys in a given JSD."
  (with-slots (alist) jsd
    (let ((keys (loop for cons in alist collect (car cons))))
      (setf keys (sort keys #'string<))
      keys)))