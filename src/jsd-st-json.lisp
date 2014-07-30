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

(in-package :cl-json-ld-jsd)

(defgeneric convert-to-jsd (value))

(defmethod convert-to-jsd ((value st-json:jso))
  (let ((res (make-string-hashtable)))
        (st-json:mapjso #'(lambda (k v)
                    (setf (gethash k res) (convert-to-jsd v)))
                value)
    res))

(defmethod convert-to-jsd ((value cons))
  (mapcar #'(lambda (item)
              (convert-to-jsd item))
    value))

(defmethod convert-to-jsd ((value t))
  value)

(defun jsd-read (source)
  (convert-to-jsd (st-json:read-json source)))

(defun jsd-make (&rest args)
  (convert-to-jsd (apply #'st-json:jso args)))

(defmethod write-json-element ((element hash-table) stream)
  (write-json-element (st-json::make-jso :alist (hash-table-alist element)) stream))

(defun jsd-to-string (element)
  "Write a value's JSON representation to a string."
  (st-json:write-json-to-string element))

(defconstant +jsd-null+ :null)
(defconstant +jsd-true+ :true)
(defconstant +jsd-false+ :false)

(defun jsd-bool-p (value)
  (typep value 'st-json:json-bool))