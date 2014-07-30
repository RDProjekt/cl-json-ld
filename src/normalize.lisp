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

(defun normalize (input &optional (options nil))
  "Perform RDF normalization on the given JSON-LD input.

  Converted from pyld's normalize function."
  
  ;; Set default options
  (plist-setdefault options :base (if (stringp input) input ""))
  (plist-setdefault options :document-loader *default-document-loader*)
  
  ;; Convert to RDF dataset, then do normalization.
  (let ((opts (copy-list options)))
    (remf opts :format)
    (setf (getf opts :produce-generalized-rdf) nil)
  
    (let ((dataset (handler-bind 
                       ((json-ld-error #'(lambda (cause)
                                           (error 'json-ld-error
                                             :message "Could not convert input to RDF dataset before normalization."
                                             :cause cause))))
                     (to-rdf input opts))))
      ;; do normalization
      (internal-normalize dataset options))))


(defun internal-normalize (dataset options)
  "Perform RDF normalization on the given RDF dataset.

  Converted from pyld's JsonLdProcessor._normalize method."
  ;; create quads and map bnodes to their associated quads
  (multiple-value-bind (quads bnodes) (map-quad-bnodes dataset)
    
    ;; mapping complete, start canonical naming
    (let ((namer (make-namer "_:c14n")))
      
      (assign-bnode-names bnodes namer)
      
      (let ((normalized (get-normalized-quads quads namer)))
        ;; Handle output format
        (let ((format (getf options :format)))
          (if format
              (if (equal format "application/nquads")
                  (format nil "~{~a~}" normalized)
                  (error 'json-ld-error :message (format nil "Unknown output format: ~s" format)))
              ; else
              (parse-nquads (format nil "~{~a~}" normalized))))))))


(alexandria:define-constant +quad-attrs+ (list :subject :object :name) :test #'equal)

(defun map-quad-bnodes (dataset)
  "Create quads and map bnodes to their associated quads.

  Return two values:
   - quads is a dynamic array of quads (each quad is represented by a plist)
   - bnodes is a hashtable with keys being names of bnodes and values being plists,
     whose single :quads properties keep dynamic arrays of quads associated with the bnode.
  "
  (let ((quads (make-dynamic-array))
        (bnodes (make-string-hashtable)))
    (loop for graph-name being the hash-keys of dataset using (hash-value triples) do
      (when (equal graph-name +default-graph-name+)
        (setf graph-name nil))
      (loop for triple across triples do
        (let ((quad (copy-list triple)))
          (when graph-name
            (setf (getf quad :name) 
                  (make-triple-elem (if (blank-name-p graph-name) 
                                        +blank-node-type+ 
                                        +iri-type+)
                                    graph-name)))
          (vector-push-extend quad quads)
          
          (loop for attr in +quad-attrs+
                for triple-elem = (getf quad attr)
                when (and triple-elem
                          (equal (getf triple-elem :type) +blank-node-type+)) do
            (let* ((triple-value (getf triple-elem :value))
                   (bnode (gethash triple-value bnodes '()))
                   (quads (getf bnode :quads)))
              (unless quads
                (setf quads (make-dynamic-array)
                      (getf bnode :quads) quads
                      (gethash triple-value bnodes) bnode))
              (vector-push-extend quad quads))))))
    (values quads bnodes)))
          
(defun assign-bnode-names (bnodes namer)
  "Fill namer's cache with names of bnodes."
  (let ((duplicates (assign-unique-names bnodes namer)))
    (assign-duplicate-names duplicates bnodes namer)))

(defun assign-unique-names (bnodes namer)
  "Continue to hash bnode quads while bnodes are assigned names.

  Return a hashtable of duplicates."
  (do ((unnamed (alexandria:hash-table-keys bnodes) (reverse next-unnamed))
       (next-unnamed '() '())
       (duplicates (make-string-hashtable) (make-string-hashtable))
       (unique (make-string-hashtable) (make-string-hashtable)))
      
      (nil) ; loop until explicit return
    
    (loop for bnode in unnamed
          ;; Hash quads for each unnamed bnode
          for hash = (hash-quads bnode bnodes)
          for duplicate-entry = (gethash hash duplicates)
          for unique-entry = (gethash hash unique)
          do
      ;; Store hash as unique or a duplicate    
      (cond
       (duplicate-entry
        (vector-push-extend bnode duplicate-entry)
        (push bnode next-unnamed))
       
       (unique-entry
        (let ((hash-duplicates (make-dynamic-array)))
          (vector-push-extend unique-entry hash-duplicates)
          (vector-push-extend bnode hash-duplicates)
          (setf (gethash hash duplicates) hash-duplicates)
          (push unique-entry next-unnamed)
          (push bnode next-unnamed)
          (remhash hash unique)))
       
       (t
        (setf (gethash hash unique) bnode))))
    
    ;; Name unique bnodes in sorted hash order
    (sorted-maphash #'(lambda (hash bnode)
                        (declare (ignore hash))
                        (namer-get-name namer bnode))
                    unique)
  
    ;; Done when no more bnodes named
    (when (= (length unnamed) (length next-unnamed))
      (return duplicates))))
                                 
(defun assign-duplicate-names (duplicates bnodes namer)
  "Enumerate duplicate hash groups in sorted order."
  (flet ((process-group (hash group)
           (declare (ignore hash))
           (let ((results (loop for bnode across group
                                unless (namer-is-named namer bnode) ; skip already-named bnodes
                                ;; hash bnode paths
                                collect (let ((path-namer (make-namer)))
                                          (namer-get-name path-namer bnode)
                                          (hash-paths bnode bnodes namer path-namer)))))
             ;; Name bnodes in hash order
             (setf results (sort results #'string< :key #'(lambda (x) (getf x :hash))))
             (loop for result in results do
               (loop for bnode across (namer-order (getf result :path-namer)) do
                 (namer-get-name namer bnode))))))
    (sorted-maphash #'process-group duplicates)))

(defun quad-name-value (quad)
  "Return the value of :name property of quad, if present."
  (let ((quad-name (getf quad :name)))
    (if quad-name
        (getf quad-name :value)
        nil)))

(defun get-normalized-quads (quads namer)
  "At this point all bnodes in the set of RDF quads have been
  assigned canonical names, which have been stored in the 'namer'
  object. Here each quad is updated by assigning each of its bnodes its
  new name via the 'namer' object.

  Return a list of strings representing RDF quads."
  (let ((normalized (loop for quad across quads do
                      (loop for attr in +quad-attrs+
                          for quad-attr = (getf quad attr)
                          when (and (equal (getf quad-attr :type) +blank-node-type+)
                                    (not (starts-with-p (getf quad-attr :value) "_:c14n"))) do
                            (setf (getf quad-attr :value) (namer-get-name namer (getf quad-attr :value))))
                        collect (triple->nquad quad (quad-name-value quad)))))
    (setf normalized (sort normalized #'string<))
    normalized))
                            
(defun hash-quads (id bnodes)
  "Hash all of the quads about a blank node.

  bnodes is a string hashtable whose values are plists with keys:
   :hash - cached hash
   :quads - dynamic array with quad items (plists).

  Converted from pyld's JsonLdProcessor._hash_quads method."
  (let ((bnode (gethash id bnodes)))
    
    ;; Return cached hash
    (let ((cached (getf bnode :hash)))
      (when cached
        (return-from hash-quads cached)))
    
    ;; Serialize all of bnode's quads
    (let ((nquads (loop for quad across (getf bnode :quads)
                        collect (triple->nquad quad (quad-name-value quad) :bnode id))))
      ;; Sort serialized quads
      (setf nquads (sort nquads #'string<))
      ;; Cache and return hashed quads
      (let ((quads-hash (sha1-hash-strings nquads)))
        (setf (getf bnode :hash) quads-hash
              (gethash id bnodes) bnode)
        quads-hash))))

(defun hash-paths (id bnodes namer path-namer)
  "Produce a hash for the paths of adjacent bnodes for a bnode,
  incorporating all information about its subgraph of bnodes. This
  method will recursively pick adjacent bnode permutations that produce
  the lexicographically-least 'path' serializations.

  :param id: the ID of the bnode to hash paths for.
  :param bnodes: the hashtable of bnode quads.
  :param namer: the canonical bnode namer.
  :param path_namer: the namer used to assign names to adjacent bnodes.

  Return a plist with :hash and :path-namer keys.

  Converted from pyld's JsonLdProcessor._hash_paths method."
  (let ((groups (group-bnodes-by-hash id bnodes namer path-namer))
        (digester (make-sha1-digester)))
    ;; "groups" is a hashmap with array values.
    ;; Iterate over groups in sorted hash order:
    (sorted-maphash 
     #'(lambda (group-hash group)
         (setf path-namer (update-hash-path digester group-hash group bnodes namer path-namer)))
     groups)
    
    ;; Return SHA-1 hash and path namer
    (list :hash (digester-hexdigest digester) :path-namer path-namer)))

(defun get-adjacent-bnode-and-direction (quad id)
  "Return either subject's or object's value of the quad, 
  indicating which one was taken by the 'direction' value."
  (loop for (property . direction) in '((:subject . "p") (:object . "r"))
        for bnode = (get-adjacent-bnode-name (getf quad property) id)
        when bnode do
    (return-from get-adjacent-bnode-and-direction (values bnode direction)))
  (values nil nil))
        
(defun group-bnodes-by-hash (id bnodes namer path-namer)
  "Group adjacent bnodes by hash, keep properties & references separate.
  Return a hashmap with array values."
  (let ((groups (make-string-hashtable))
        (quads (getf (gethash id bnodes) :quads)))
    (loop for quad across quads do
      ;; get adjacent bnode
      (multiple-value-bind (bnode direction) (get-adjacent-bnode-and-direction quad id)    
        (when bnode
          ;; get bnode name (try canonical, path, then hash)
          (let ((name (cond
                       ((namer-is-named namer bnode)
                        (namer-get-name namer bnode))
                       ((namer-is-named path-namer bnode)
                        (namer-get-name path-namer bnode))
                       (t
                        (hash-quads bnode bnodes))))
                (group-digester (make-sha1-digester)))
            ;; Hash direction, property and bnode name/hash
            (digester-add-string group-digester direction)
            (digester-add-string group-digester (getf (getf quad :predicate) :value))
            (digester-add-string group-digester name)
            
            ;; Add bnode to hash group
            (vector-push-extend bnode
                                (hashtable-setdefault groups (digester-hexdigest group-digester) 
                                                      (make-dynamic-array)))))))
    groups))

(defun get-adjacent-bnode-name (node id)
  "A helper function that gets the blank node name from an RDF quad
   node (subject or object). If the node is not a blank node or its
   value does not match the given blank node ID, nil will be returned."
  (if (and (equal (getf node :type) +blank-node-type+)
           (not (equal (getf node :value) id)))
      (getf node :value)
    nil))

(defun update-hash-path (digester group-hash group bnodes namer path-namer)
  "Returns new chosen path-namer."
  ;; Digest group hash
  (digester-add-string digester group-hash)
  
  ;; Choose a path and namer from the permutations
  (multiple-value-bind (chosen-path chosen-namer) (choose-path-and-namer group bnodes namer path-namer)
    ;; Digest chosen path and update namer:
    (digester-add-string digester chosen-path)
    chosen-namer))

(defun string-permutation-next (elements)
  "Change elements (an array of strings) in place 
  to produce a next permutation in lexicographic order.
  Return nil if no more permutations exist."
  (let ((elem-count (1- (length elements))))
    (loop for i from (1- elem-count) downto 0
        when (string< (elt elements i) (elt elements (1+ i))) do
          (loop for k from elem-count downto i
              when (string< (elt elements i) (elt elements k)) do
                (rotatef (elt elements i) (elt elements k))
                (setf k (1+ elem-count))
                (loop while (< (incf i) (decf k)) do
                      (rotatef (elt elements i) (elt elements k)))
                (return-from string-permutation-next elements)))))

(defun string-permutation-init (elements)
  "Initialize permutation of an array of strings."
  (sort elements #'string<))

(defun choose-path-and-namer (group bnodes namer path-namer)
  "Choose a path and namer for a given permutation (group)."
  (when (zerop (length group))
    (return-from choose-path-and-namer (values nil nil)))
  (let ((chosen-path nil)
        (chosen-namer nil)
        (group-permutation (map 'vector #'identity group)))
    (string-permutation-init group-permutation)
    (loop while group-permutation do
      (block group-permutation-loop
        (let ((path-namer-copy (namer-clone path-namer))
              (path "")
              (recurse (make-dynamic-array)))
          (loop for bnode across group-permutation do
            ;; Use canonical name if available
            (if (namer-is-named namer bnode)
                (setf path (concatenate 'string path (namer-get-name namer bnode)))
                (progn
                  ;; Recurse if bnode isn't named in the path yet
                  (unless (namer-is-named path-namer-copy bnode)
                    (vector-push-extend bnode recurse))
                  (setf path (concatenate 'string path (namer-get-name path-namer-copy bnode)))))
              
            ;; Skip permutation if path is already >= chosen path
            (when (and chosen-path    
                       (>= (length path) (length chosen-path))
                       (string> path chosen-path))
              (return-from group-permutation-loop)))
        
          ;; Recurse
          (loop for bnode across recurse
                for result = (hash-paths bnode bnodes namer path-namer-copy) do
            (setf path (concatenate 'string path (namer-get-name path-namer-copy bnode)
                                                 (format nil "<~a>" (getf result :hash))))
            (setf path-namer-copy (getf result :path-namer))
                
            ;; Skip permutation if path is already >= chosen path
            (when (and chosen-path    
                       (>= (length path) (length chosen-path))
                       (string> path chosen-path))
              (return-from group-permutation-loop)))

          (when (or (not chosen-path)
                    (string< path chosen-path))
            (setf chosen-path path
                  chosen-namer path-namer-copy))))
          
      (setf group-permutation (string-permutation-next group-permutation)))
    (values chosen-path chosen-namer)))
