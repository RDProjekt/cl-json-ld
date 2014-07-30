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

(defun jsd-read (source)
  (let* ((yason:*parse-json-booleans-as-symbols* t)
         (yason:*parse-json-null-as-keyword* t))
    (yason:parse source)))

(defun jsd-make (&rest args)
  (let ((res (make-string-hashtable)))
    (loop for (key value) on args by #'cddr do
          (setf (gethash key res) value))
    res))

(defmethod yason:encode ((object (eql :null)) &optional (stream *standard-output*))
  (yason:encode 'yason:null stream))

(defun jsd-to-string (element)
  (let ((s (make-string-output-stream)))
    (yason:encode element s)
    s))

(defconstant +jsd-null+ :null)
(defconstant +jsd-true+ 'yason:true)
(defconstant +jsd-false+ 'yason:false)

(defun jsd-bool-p (value)
  (or (eql value +jsd-true+)
      (eql value +jsd-false+)))
