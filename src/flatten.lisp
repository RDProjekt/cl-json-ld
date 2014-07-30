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

(in-package :cl-json-ld)

(defun flatten (input ctx &optional (options nil))
  "Perform JSON-LD flattening.

  Converted from pyld's flatten function."  
  (plist-setdefault options :base (if (stringp input) input ""))
  (plist-setdefault options :document-loader *default-document-loader*)
  
  ;; Expand input
  (let* ((expanded (handler-bind
                       ((error #'(lambda (cause) 
                                  (error 'json-ld-error 
                                    :message "Could not expand input before flattening."
                                    :cause cause))))
                     (expand input options)))
         ;; Do flattening
         (flattened (internal-flatten expanded)))
    
    (unless ctx
      (return-from flatten flattened))
    
    ;; Compact result (force @graph option to true, skip expansion)
    (setf (getf options :graph) t
          (getf options :skip-expansion) t)
    
    (handler-case
        (compact flattened ctx options)
      (error (cause)
        (error 'json-ld-error
          :message "Could not compact flattened output."
          :cause cause)))))
    
(defun internal-flatten (input)
  "Perform JSON-LD flattening.

  Converted from pyld's JsonLdProcessor._flatten method."
  ;; Produce a map of all subjects and name each bnode
  (let ((namer (make-namer))
        (graphs (jsd-make +default-graph-name+ (jsd-make))))
    
    (create-node-map input graphs +default-graph-name+ namer)
    
    ;; Add all non-default graphs to default graph
    (let ((default-graph (jsd-get +default-graph-name+ graphs)))
      (flet ((process-graph (graph-name node-map)
               (when (equal graph-name +default-graph-name+)
                 (return-from process-graph))
               (let ((graph-subject (jsd-setdefault default-graph graph-name
                                                    (jsd-make +id-key+ graph-name
                                                              +graph-key+ '()))))
                 (setf (jsd-get +graph-key+ graph-subject)
                       (append (jsd-getdefault +graph-key+ graph-subject '())
                               (collect-graph-nodes node-map))))))
        (jsd-map #'process-graph graphs))
      
      ;; Produce flattened output
      (collect-graph-nodes default-graph))))

(defun collect-graph-nodes (graph)
  "Return nodes from graph that are not subject references."
  (let ((result '()))
    (jsd-sorted-map #'(lambda (key value)
                        (declare (ignore key))
                        (unless (is-subject-reference value)
                          (push value result))) graph)
    (nreverse result)))