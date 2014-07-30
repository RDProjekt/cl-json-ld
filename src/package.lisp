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

(defpackage :cl-json-ld-sha1
  (:use cl)
  (:export
   :make-sha1-digester
   :sha1-hash-strings
   :digester-add-string
   :digester-hexdigest))

(defpackage :cl-json-ld-utils
  (:use :cl :alexandria :split-sequence)
  (:export
   :starts-with-p
   :ends-with-p
   :objs-equal-p
   :deepcopy
   :sorted-maphash
   :camel-case->symbol
   :make-string-hashtable
   :normpath))

(defpackage :cl-json-ld-jsd
  (:use :cl :cl-json-ld-utils :alexandria)
  (:export
   :jsd-json
   :jsd-read
   :jsd-make
   :jsd-get
   :is-object
   :is-empty-object
   :jsd-map
   :jsd-sorted-map
   :jsd-copied-map
   :jsd-to-string
   :jsd-count
   :jsd-when-get
   :jsd-getdefault
   :jsd-setdefault
   :jsd-haskey
   :jsd-remove
   :jsd-bool-p
   :+jsd-null+
   :+jsd-true+
   :+jsd-false+
   :multi-jsd-get
   :jsd-sorted-keys))

(defpackage :cl-json-ld
  (:use :cl :puri :cl-ppcre :cl-json-ld-utils :cl-json-ld-jsd :cl-json-ld-sha1)
  (:export
   :from-rdf
   :to-rdf
   :expand
   :compact
   :flatten
   :normalize
   :frame
   :json-ld-error
   :json-ld-error-code
   :get-document-loader
   :set-document-loader
   :json-ld-processor
   :processor-from-rdf
   :register-rdf-parser
   :processor-register-rdf-parser
   :jsd-read
   :jsd-to-string
   :jsd-make))

