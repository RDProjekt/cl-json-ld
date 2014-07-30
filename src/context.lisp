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

(defvar *cache* nil)

(defun get-initial-context (options)
  (jsd-make +base-key+ (getf options :base) "mappings" (jsd-make) "inverse" nil))

(defun get-context-value (context key type)
  "Gets the value for the given active context key and type, nil if none is set."
  ;; return :null for invalid key
  (when (eq key +jsd-null+)
    (return-from get-context-value +jsd-null+))
  
  (let ((rval +jsd-null+))
    ;; get default language
    (when (equal type +language-key+)
      (jsd-when-get type context (type-value)
        (setf rval type-value)))
    
    ;; get specific entry information
    (jsd-when-get key (jsd-get "mappings" context) (entry)
      (when (eq entry +jsd-null+)
        (return-from get-context-value +jsd-null+))
      ;; return whole entry
      (if (not type)
          (setf rval entry)
          (jsd-when-get type entry (type-value)
            (setf rval type-value))))
    rval))

(defun get-context-mapping (context mapping-key)
  (let ((mappings (jsd-get "mappings" context)))
    (jsd-get mapping-key mappings)))

(defun clone-active-context (active-ctx)
  "Clones an active context, creating a child active context."
  (let ((child (jsd-make +base-key+ (jsd-get +base-key+ active-ctx)
                         "mappings" (deepcopy (jsd-get "mappings" active-ctx))
                         "inverse" nil))
        (language-val (jsd-get +language-key+ active-ctx))
        (vocab-val (jsd-get +vocab-key+ active-ctx)))
    (when language-val
      (setf (jsd-get +language-key+ child) language-val))
    (when vocab-val
      (setf (jsd-get +vocab-key+ child) vocab-val))
    child))

(defun term-definition-handle-reverse (reverse-value value active-ctx local-ctx defined mapping)
  (when (jsd-haskey +id-key+ value)
    (error 'json-ld-error 
      :message "Invalid JSON-LD syntax; an @reverse term definition must not contain @id."
      :code "invalid reverse property"))
  (when (not (stringp reverse-value))
    (error 'json-ld-error 
      :message "Invalid JSON-LD syntax; @context @reverse value must be a string."
      :code "invalid IRI mapping"))
  
  ;; expand and add @id mapping
  (let ((id (expand-iri active-ctx reverse-value :vocab t :base nil :local-ctx local-ctx :defined defined)))
    (when (not (absolute-iri-p id))
      (error 'json-ld-error 
        :message "Invalid JSON-LD syntax; @context @reverse value must be an absolute IRI or a blank node identifier."
        :code "invalid IRI mapping"))
    (setf (jsd-get +id-key+ mapping) id)
    (setf (jsd-get "reverse" mapping) t)))

(defun create-term-definition (active-ctx local-ctx term defined)
  "Creates a term definition during context processing."
  (multiple-value-bind (defined-p present-p) (gethash term defined)
    (when present-p
      ;; term already defined
      (when defined-p
        (return-from create-term-definition))
      ;; cycle detected
      (error 'json-ld-error 
        :message "Cyclical context definition detected."
        :code "cyclic IRI mapping")))
  
  ;; now defining term
  (setf (gethash term defined) nil)
  
  (when (is-keyword term)
    (error 'json-ld-error
      :message "Invalid JSON-LD syntax; keywords cannot be overridden."
      :code "keyword redefinition"))
  
  ;; remove old mapping
  (let ((active-mappings (jsd-get "mappings" active-ctx)))
    (when (jsd-get term active-mappings)
      (jsd-remove term active-mappings))
      
    ;; get context term value
    (let ((value (jsd-get term local-ctx)))
      ;; clear context entry
      (when (or (eq value +jsd-null+)
                (and (is-object value) (multiple-value-bind (id-value id-present-p) (jsd-get +id-key+ value)
                                         (and id-present-p (eq id-value +jsd-null+)))))
        (setf (jsd-get term active-mappings) +jsd-null+)
        (setf (gethash term defined) t)
        (return-from create-term-definition))
      
      ;; convert short-hand value to object w/@id
      (when (stringp value)
        (setf value (jsd-make +id-key+ value)))
        
      (when (not (is-object value))
        (error 'json-ld-error 
          :message "Invalid JSON-LD syntax; @context property values must be strings or objects."
          :code "invalid term definition"))
        
      ;; create new mapping
      (let ((mapping (jsd-make "reverse" nil)))
        
        (setf (jsd-get term active-mappings) mapping)
        
        (multiple-value-bind (reverse-value reverse-present-p) (jsd-get +reverse-key+ value)
          (if reverse-present-p
              (term-definition-handle-reverse reverse-value value active-ctx local-ctx defined mapping)
            (multiple-value-bind (id id-present-p) (jsd-get +id-key+ value)
              (when id-present-p
                (when (not (stringp id))
                  (error 'json-ld-error 
                    :message "Invalid JSON-LD syntax; @context @id value must be a string"
                    :code "invalid IRI mapping"))
                (when (not (equal id term))
                  (setf id (expand-iri active-ctx id :vocab t :base nil :local-ctx local-ctx :defined defined))
                  (when (and (not (absolute-iri-p id)) (not (is-keyword id)))
                    (error 'json-ld-error 
                      :message "Invalid JSON-LD syntax;
 @context @id value must be an absolute IRI, a blank node identifier, or a keyword"
                      :code "invalid IRI mapping"))
                  (setf (jsd-get +id-key+ mapping) id))))))
        
        (unless (jsd-haskey +id-key+ mapping)
          ;; see if the term has a prefix
          (if (find #\: term)
              (progn
                (multiple-value-bind (prefix suffix) (split-string term #\:)
                  (when (jsd-get prefix local-ctx)
                    ;; define parent prefix
                    (create-term-definition active-ctx local-ctx prefix defined))
                  
                  ;; set @id based on prefix parent
                  (multiple-value-bind (prefix-mapping prefix-mapping-present-p) (jsd-get prefix active-mappings)
                    (setf (jsd-get +id-key+ mapping)
                          (if prefix-mapping-present-p
                              (concatenate 'string (jsd-get +id-key+ prefix-mapping) suffix)
                              ;; term is an absolute IRI
                              term)))))
              ;; else
              (progn
                (multiple-value-bind (vocab-value vocab-present-p) (jsd-get +vocab-key+ active-ctx)
                (when (not vocab-present-p)
                  (error 'json-ld-error 
                    :message "Invalid JSON-LD syntax; @context terms must define an @id."
                    :code "invalid IRI mapping"))
                ;; prepend vocab to term
                (setf (jsd-get +id-key+ mapping) (concatenate 'string vocab-value term))))))
        
        ;; IRI mapping now defined
        (setf (gethash term defined) t)
        
        (multiple-value-bind (type type-present-p) (jsd-get +type-key+ value)
          (when type-present-p
            (when (not (stringp type))
              (error 'json-ld-error 
                :message "Invalid JSON-LD syntax; @context @type value must be a string."
                :code "invalid type mapping"))
            (when (and (not (equal type +id-key+))
                       (not (equal type +vocab-key+)))
              ;; expand @type to full IRI
              (setf type (expand-iri active-ctx type :vocab t :local-ctx local-ctx :defined defined))
              
              (when (not (absolute-iri-p type))
                (error 'json-ld-error 
                  :message "Invalid JSON-LD syntax; an @context @type value must be an absolute IRI."
                  :code "invalid type mapping"))
              
              (when (blank-name-p type)
                (error 'json-ld-error 
                  :message "Invalid JSON-LD syntax; an @context @type values must be an IRI, not a blank node identifier."
                  :code "invalid type mapping")))
            ;; add @type to mapping
            (setf (jsd-get +type-key+ mapping) type))
          
          (multiple-value-bind (container container-present-p) (jsd-get +container-key+ value)
            (when container-present-p
              (when (not (find container (list +list-key+ +set-key+ +index-key+ +language-key+) :test #'string=))
                (error 'json-ld-error 
                  :message "Invalid JSON-LD syntax; @context @container value must be one of the following: @list, @set, @index, or @language."
                  :code "invalid container mapping"))
              (when (and (jsd-get "reverse" mapping)
                         (not (equal container +index-key+))
                         (not (equal container +set-key+))
                         (not (eq container +jsd-null+)))
                (error 'json-ld-error 
                  :message "Invalid JSON-LD syntax; @context @container value for an @reverse type definition must be @index or @set."
                  :code "invalid reverse property"))
              ;; add @container to mapping
              (setf (jsd-get +container-key+ mapping) container)))
          
          (multiple-value-bind (language language-present-p) (jsd-get +language-key+ value)
            (when (and language-present-p (not type-present-p))
              (when (not (or (eq language +jsd-null+) (stringp language)))
                (error 'json-ld-error 
                  :message "Invalid JSON-LD syntax; @context @language value must be a string or null."
                  :code "invalid language mapping"))
              ;; add @language to mapping
              (when (not (eq language +jsd-null+))
                (setf language (string-downcase language)))
              (setf (jsd-get +language-key+ mapping) language))))
        
        ;; disallow aliasing @context and @preserve
        (let ((id (jsd-get +id-key+ mapping)))
          (when (or (equal id +context-key+) (equal id +preserve-key+))
            (error 'json-ld-error 
              :message "Invalid JSON-LD syntax; @context and @preserve cannot be aliased"
              :code "invalid keyword alias")))))))

(defun process-context (active-ctx local-ctx options)
  "Processes a local context, retrieving any URLs as necessary, and
  returns a new active context.

  Converted from pyld's JsonLdProcessor.process_context method."
  ;; Return initial context early for null context
  (when (eq local-ctx +jsd-null+)
    (return-from process-context (get-initial-context options)))
  
  ;; Set default options
  (plist-setdefault options :base "")
  (plist-setdefault options :document-loader *default-document-loader*)
  
  ;; Retrieve URLs in local-ctx
  (setf local-ctx (deepcopy local-ctx))
  (when (or (stringp local-ctx)
            (and (is-object local-ctx) (not (jsd-haskey +context-key+ local-ctx))))
    (setf local-ctx (jsd-make +context-key+ local-ctx)))
  
  (retrieve-context-urls local-ctx nil (getf options :document-loader) :base (getf options :base))
  
  (internal-process-context active-ctx local-ctx options))


(defun internal-process-context (active-ctx local-ctx options)
  "Processes a local context and returns a new active context.

  Converted from pyld's JsonLdProcessor._process_context method."
  ;; normalize local context to an array  
  (when (is-object local-ctx)
    (multiple-value-bind (context context-present-p) (jsd-get +context-key+ local-ctx)
      (when (and context-present-p (listp context))
        (setf local-ctx (jsd-get +context-key+ local-ctx)))))
  
  (let ((ctxs (arraify local-ctx))
        (rval active-ctx)
        (active-context-cache (getf *cache* :active-ctx))
        (must-clone t))
    ;; no contexts in array, clone existing context
    (when (= (length ctxs) 0)
      (return-from internal-process-context (clone-active-context active-ctx)))
  
    ;; process each context in order
    (loop for ctx in ctxs do
          (block continue-block
            ;; reset to initial context
            (when (eq ctx +jsd-null+)
              (setf rval (get-initial-context options))
              (setf must-clone nil)
              (return-from continue-block))
          
            ;; dereference @context key if present
            (when (and (is-object ctx) (jsd-get +context-key+ ctx))
              (setf ctx (jsd-get +context-key+ ctx)))
            
            ;; context must be an object now, all URLs retrieved prior to call
            (when (not (is-object ctx))
              (error 'json-ld-error 
                :message "Invalid JSON-LD syntax; @context must be an object."
                :code "invalid local context"))
            
            ;; get context from cache if available
            
            (when active-context-cache
              (let ((cached (context-cache-get active-context-cache active-ctx ctx)))
                (when cached
                  (setf rval cached)
                  (setf must-clone t)
                  (return-from continue-block))))
            
            ;; clone context, if required, before updating
            (when must-clone
              (setf rval (clone-active-context active-ctx))
              (setf must-clone nil))
            
            (let ((defined (make-string-hashtable)))

            ;; handle @base
            (jsd-when-get +base-key+ ctx (base)
              (cond
               ((eq base +jsd-null+)
                (setf base nil))
               ((not (stringp base))
                (error 'json-ld-error 
                  :message "Invalid JSON-LD syntax; the value of \"@base\" in a @context must be a string or null."
                  :code "invalid base IRI"))
               ((and (not (equal base "")) (not (absolute-iri-p base)))
                (error 'json-ld-error 
                  :message "Invalid JSON-LD syntax; the value of \"@base\" in a @context must be an absolute IRI or the empty string."
                  :code "invalid base IRI")))
              (setf (jsd-get +base-key+ rval) base)
              (setf (gethash +base-key+ defined) t))
                        
              ;; handle @vocab
              (multiple-value-bind (vocab-value vocab-present-p) (jsd-get +vocab-key+ ctx)
                (when vocab-present-p
                  (cond ((eq vocab-value +jsd-null+) (jsd-remove +vocab-key+ rval))
                        ((not (stringp vocab-value))
                         (error 'json-ld-error 
                           :message "Invalid JSON-LD syntax; the value of \"@vocab\" in a @context must be a string or null."
                           :code "invalid vocab mapping"))
                        ((not (absolute-iri-p vocab-value))
                         (error 'json-ld-error 
                           :message "Invalid JSON-LD syntax; the value of \"@vocab\" in a @context must be an absolute IRI."
                           :code "invalid vocab mapping"))
                        (t (setf (jsd-get +vocab-key+ rval) vocab-value)))
                  (setf (gethash +vocab-key+ defined) t)))
              
              ;; handle @language
              (multiple-value-bind (language-value language-present-p) (jsd-get +language-key+ ctx)
                (when language-present-p
                  (cond ((eq language-value +jsd-null+) (jsd-remove +language-key+ rval))
                        ((not (stringp language-value))
                         (error 'json-ld-error 
                           :message "Invalid JSON-LD syntax; the value of \"@language\" in a @context must be a string or null."
                           :code "invalid default language"))
                        (t (setf (jsd-get +language-key+ rval) (string-downcase language-value))))
                  (setf (gethash +language-key+ defined) t)))
              
              ;; process all other keys
              (jsd-map #'(lambda (k v)
                          (declare (ignore v))
                          (create-term-definition rval ctx k defined))
                      ctx)
            
              ;; Cache result:
              (when active-context-cache
                (context-cache-set active-context-cache active-ctx ctx rval)))))
    rval))


(defclass limited-queue ()
  ((size :initarg :size :initform 100)
   (array :initform nil)
   (start :initform 0)
   (end :initform 0)))

(defmethod initialize-instance :after ((queue limited-queue) &key)
  (with-slots (size array) queue
    (setf array (make-array size))))

(defun queue-full (queue)
  (with-slots (size start end) queue
    (= (mod (1+ end) size) start)))

(defun queue-push (queue value)
  (when (queue-full queue)
    (error "Queue full."))
  (with-slots (array end size) queue
    (setf (elt array end) value)
    (setf end (mod (1+ end) size))))

(defun queue-pop-first (queue)
  (with-slots (array start end size) queue
    (when (= start end)
      (error "Queue empty."))
    (prog1
      (elt array start)
      (setf (elt array start) nil)
      (setf start (mod (1+ start) size)))))


(defclass active-context-cache ()
  ((order :initform (make-instance 'limited-queue :size 100))
   (cache :initform (make-string-hashtable))))

(defun context-cache-get (cache active-ctx local-ctx)
  (let* ((key1 (jsd-to-string active-ctx))
         (key2 (jsd-to-string local-ctx))
         (first-entry (gethash key1 (slot-value cache 'cache))))
    (and first-entry (gethash key2 first-entry))))

(defun context-cache-set (cache active-ctx local-ctx result)
  (with-slots (order cache) cache
    
    (when (queue-full order)
      (destructuring-bind (key1 . key2) (queue-pop-first order)
        (remhash key2 (gethash key1 cache))
        (when (zerop (hash-table-count (gethash key1 cache)))
          (remhash key1 cache))))
    
    (let ((key1 (jsd-to-string active-ctx))
          (key2 (jsd-to-string local-ctx)))
      (queue-push order (cons key1 key2))
      (setf (gethash key2 (hashtable-setdefault cache key1 (make-string-hashtable)))
        (deepcopy result)))))

(setf (getf *cache* :active-ctx) (make-instance 'active-context-cache))

