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

(in-package :cl-json-ld-utils)

(defun starts-with-p (string prefix)
  "Return T iff given string starts with given prefix."
  (let ((prefix-length (length prefix)))
    (and (<= prefix-length (length string))
         (equal (subseq string 0 prefix-length) prefix))))

(defun ends-with-p (string suffix)
  "Return T iff given string ends with a given suffix."
  (let ((suffix-length (length suffix)))
    (equal (subseq string (- (length string) suffix-length)) suffix)))

(defun make-string-hashtable ()
  "Create a hashtable whose keys will be strings."
  (make-hash-table :test #'equal))

;;;; Symbol manipulation

(defun camel-case->lisp-name (name)
  "Convert camel case name to dash-separated lisp name"
  (apply #'concatenate 'string
         (loop for chr across name collect (if (upper-case-p chr)
                                               (format nil "-~C" (char-downcase chr))
                                             (string chr)))))

(defun camel-case->symbol (name &optional package)
  "Convert camel case name to lisp symbol"
  (let ((lisp-name (string-upcase (camel-case->lisp-name name))))
    (intern lisp-name package)))

;;;; Hashmap extensions

(defun sorted-maphash (func hash-table &key (test #'string<))
  "Iterate over all entries in hash-table, sorted by keys.
  For each key/value pair, call func."
  (let ((keys (alexandria:hash-table-keys hash-table)))
    (setf keys (sort keys test))
    (loop for key in keys do
          (funcall func key (gethash key hash-table)))))

;;;; deepcopy

(defgeneric deepcopy (element)
  (:documentation "Deep copy data."))

(defmethod deepcopy ((element string))
  (copy-seq element))

(defmethod deepcopy ((element real))
  element)

(defmethod deepcopy ((element list))
  (mapcar #'deepcopy element))

(defmethod deepcopy ((element symbol))
  element)

;;;; objs-equal-p

(defgeneric objs-equal-p (obj1 obj2))

(defun lazy-message-eq (result-form &rest format-args)
  (let ((equal result-form))
    (values equal (when (not equal) (apply #'format nil format-args)))))

(defmethod objs-equal-p ((s1 string) (s2 string))
  (lazy-message-eq (string= s1 s2) "Different strings ~s != ~s" s1 s2))

(defmacro propagate-negative-result (obj1 obj2 return-from)
  `(multiple-value-bind (equal message) (objs-equal-p ,obj1 ,obj2)
     (when (not equal)
       (return-from ,return-from (values equal message)))))

(defmethod objs-equal-p ((list1 cons) (list2 cons))
  (when (eql (length list1) (length list2))
    (loop
      for el1 in list1
      for el2 in list2
      do
      (propagate-negative-result el1 el2 objs-equal-p)
    (return-from objs-equal-p t)))
  (lazy-message-eq nil "Different list lengths ~s != ~s" list1 list2))

(defmethod objs-equal-p ((obj1 t) (obj2 t))
  (lazy-message-eq (eql obj1 obj2) "eql comparison failed~&EXPECTED (type ~s): ~s ~&ACTUAL (type ~s): ~s" (type-of obj1) obj1 (type-of obj2) obj2))

;;; Path normalization

(defun normpath (path)
  "Normalize path, eliminating double slashes, etc.

  Converted from Python's posixpath.normpath function."
  (when (equal path "")
    (return-from normpath "."))
  
  (let ((initial-slashes (get-initial-slashes path))
        (comps (split-sequence #\/ path))
        (new-comps '()))
    (loop for comp in comps
        unless (or (equal comp "") (equal comp ".")) do
      (cond
       ((or (not (equal comp ".."))
            (and (zerop initial-slashes) (endp new-comps))
            (and (not (endp new-comps)) (equal (first new-comps) "..")))
        (push comp new-comps))
       
       ((not (endp new-comps))
        (pop new-comps))))
    
    (setf path (format nil "~{~a~^/~}" (reverse new-comps)))
    
    (when (plusp initial-slashes)
      (setf path (concatenate 'string (make-string initial-slashes :initial-element #\/) path)))
    
    (if (plusp (length path))
        path
        ".")))


(defun get-initial-slashes (path)
  ;; Posix allows one or two initial slashes, but treats three or more
  ;; as single slash.
  (cond
   ((starts-with-p path "///")
    1)
   ((starts-with-p path "//")
    2)
   ((starts-with-p path "/")
    1)
   (t
    0)))
