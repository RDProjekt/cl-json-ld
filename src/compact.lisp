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

(defun compact (input ctx &optional (options nil))
  "Perform JSON-LD compaction.

  Return multiple values: compacted JSON-LD and the active context.

  Converted from pyld's compact function."
  (unless ctx
    (error 'json-ld-error
      :message "The compaction context must not be nil."
      :code "invalid local context"))
  
  (when (eq input +jsd-null+)
    ;; Nothing to compact.
    (return-from compact (values nil nil)))
  
  ;; Set default options.
  (plist-setdefault options :base (if (stringp input) input ""))
  (plist-setdefault options :compact-arrays T)
  (plist-setdefault options :document-loader *default-document-loader*)
    
  (let* ((expanded (if (getf options :skip-expansion)
                       input
                       (expand input options))) ; Expand input.
         ; Process context:
         (active-ctx (process-context (get-initial-context options) ctx options))
         ; Do compaction:
         (compacted (internal-compact active-ctx nil expanded options))
         (compacted (simplify-compacted compacted options))
         ; Build output context:
         (ctx (build-output-context ctx))
         ; Remove empty contexts and remove array if only one element:
         (ctx (clean-context ctx)))
    
    ; Add context and/or @graph:
    (setf compacted (add-context-or-graph compacted active-ctx ctx))
    
    (values compacted active-ctx)))

(defun simplify-compacted (compacted options)
  "Simplify the result of compaction according to given options."
  (cond
   ((and (getf options :compact-arrays) (not (getf options :graph)) (listp compacted))
    (let ((length (length compacted)))
      (cond
       ((= length 1)
        ;; Simplify to a single item:
        (setf compacted (first compacted)))
       ((= length 0)
        ;; Simplify to an empty object:
        (setf compacted (jsd-make))))))
   ;; Always use an array if graph options is on:
   ((getf options :graph)
    (setf compacted (to-list compacted))))
  compacted)

(defun build-output-context (ctx)
  "Create an output context from the given context."
  ;; Follow context key:
  (when (and (is-object ctx) (jsd-get +context-key+ ctx))
    (setf ctx (jsd-get +context-key+ ctx)))
  
  ;; Build output context:
  (to-list (deepcopy ctx)))

(defun clean-context (ctx)
  "Remove empty context and remove array if only one element.
  Assumes ctx is a list."
  ;; Remove empty contexts:
  (setf ctx (remove-if #'(lambda (v) (and (is-object v) (= (jsd-count v) 0))) ctx))
  
  ;; Remove array if only one context:
  (when (= (length ctx) 1)
    (setf ctx (first ctx)))
  
  ctx)

(defun add-context-or-graph (compacted active-ctx ctx)
  "Add context and/or @graph to compacted output."
  (when (listp compacted)
    ;; Convert compacted to a JSO with @graph key:
    (let ((kwgraph (compact-iri active-ctx +graph-key+))
          (graph compacted))
      (setf compacted (jsd-make))
      (setf (jsd-get kwgraph compacted) graph)))
  
  (when (or (not (listp ctx)) (> (length ctx) 0))
    ;; If the context is not empty, add it to compacted:
    (setf (jsd-get +context-key+ compacted) ctx))
  
  compacted)

(defun internal-compact (active-ctx active-property element options)
  "Recursively compact an element using the given active context.
  All values must be in expanded form before this method is called.

  Converted from pyld's JsonLdProcessor._compact method."
  (when (listp element)
    ;; Recursively compact array.
    (let* 
        ((compacted-elems 
          (map 'list #'(lambda (e) (internal-compact active-ctx active-property e options)) element))
        (rval (remove nil compacted-elems)))
      (when (and (getf options :compact-arrays) 
                 (= (length rval) 1)
                 (null (get-context-value active-ctx active-property +container-key+)))
        (setf rval (first rval)))
      (return-from internal-compact rval)))
  
  (when (is-object element)
    ;; Recursively compact object.
    (when (or (is-value element) (is-subject-reference element))
      ;; Do value compaction on @values and subject references.
      (return-from internal-compact (compact-value active-ctx active-property element)))
    
    (let ((inside-reverse (equal active-property +reverse-key+))
          (rval (jsd-make)))
      ;; Recursively process element keys in order:
      (jsd-sorted-map 
       #'(lambda (expanded-property expanded-value) 
           (compact-item rval active-property expanded-property expanded-value active-ctx inside-reverse options)) element)
      (return-from internal-compact rval)))
  
  ;; Only primitives remain which are already compact:
  element)

(defun compact-item (rval active-property expanded-property expanded-value active-ctx inside-reverse options)
  "Compact a single property-value pair of a JSO."
  (cond
   ((or (equal expanded-property +id-key+) (equal expanded-property +type-key+))
    (compact-id-or-type-item rval expanded-property expanded-value active-ctx))
   ((equal expanded-property +reverse-key+)
    (compact-reverse-item rval expanded-property expanded-value active-ctx options))
   ((equal expanded-property +index-key+)
    (compact-index-item rval active-property expanded-property expanded-value active-ctx))
   (t
    ;; Expanded value must be a list due to expansion algorithm.
    (compact-list rval expanded-property expanded-value active-ctx inside-reverse options))))

(defun compact-id-or-type-item (rval expanded-property expanded-value active-ctx)
  "Compact an @id or @type."
  (let*
     ((compacted-value
       (if (stringp expanded-value)
           ;; Compact single @id
           (compact-iri active-ctx expanded-value :vocab (equal expanded-property +type-key+))
           ;; Expanded value must be a @type array:
           (map 'list #'(lambda (ev) (compact-iri active-ctx ev :vocab t)) expanded-value)))
      ;; Use keyword alias and add value:
      (alias (compact-iri active-ctx expanded-property)))
    (node-add-value rval alias compacted-value :property-is-array (empty-list-p compacted-value))))

(defun empty-list-p (value)
  "Return T iff value is an empty list (nil)."
  (and (listp value) (endp value)))

(defun compact-reverse-item (rval expanded-property expanded-value active-ctx options)
  "Compact a @reverse."
  ;; Recursively compact expanded value.
  (let ((compacted-value (internal-compact active-ctx +reverse-key+ expanded-value options)))
    ;; Handle double-reversed properties:
    (jsd-copied-map 
     #'(lambda (compacted-property value)
         (let ((mapping (jsd-get compacted-property (jsd-get "mappings" active-ctx))))
           (when (and mapping (jsd-get "reverse" mapping))
             (let* ((container (get-context-value active-ctx compacted-property +container-key+))
                    (use-array (or (equal container +set-key+) (not (getf options :compact-arrays)))))
               (node-add-value rval compacted-property value :property-is-array use-array))
             (jsd-remove compacted-property compacted-value))))
     compacted-value)
    
    (when (plusp (jsd-count compacted-value))
      ;; Use keyword alias and add value
      (node-add-value rval (compact-iri active-ctx expanded-property) compacted-value))))

(defun compact-index-item (rval active-property expanded-property expanded-value active-ctx)
  "Compact an @index."
  ;; Drop @index if inside and @index container:
  (when (equal (get-context-value active-ctx active-property +container-key+) +index-key+)
    (return-from compact-index-item))
                
  ;; Use keyword alias and add value
  (node-add-value rval (compact-iri active-ctx expanded-property) expanded-value))

(defun compact-list (rval expanded-property expanded-value active-ctx inside-reverse options)
  "Compact a list."
  ;; Preserve empty lists:
  (when (endp expanded-value)
    (let ((item-active-property (compact-iri active-ctx expanded-property :value expanded-value 
                                             :vocab t :reverse inside-reverse)))
      (node-add-value rval item-active-property '() :property-is-array t)))
  
  ;; Recursively process list values:
  (loop for expanded-item in expanded-value do
        (compact-list-item rval expanded-property expanded-item active-ctx inside-reverse options)))

(defun compact-list-item (rval expanded-property expanded-item active-ctx inside-reverse options)
  "Compact a list item."
  ;; Compact property and get container type:
  (let*
      ((item-active-property (compact-iri active-ctx expanded-property :value expanded-item
                                          :vocab t :reverse inside-reverse))
       (container (get-context-value active-ctx item-active-property +container-key+))
    
       ;; Get @list value if appropriate
       (is-list (is-list expanded-item))
       (list (if is-list (jsd-get +list-key+ expanded-item) nil))
    
       ;; Recursively compact expanded item
       (compacted-item (internal-compact active-ctx item-active-property
                                         (if is-list list expanded-item) options)))
    
    ;; Handle @list
    (when is-list
      (setf compacted-item (compact-list-item-list rval compacted-item expanded-item 
                                                      container active-ctx item-active-property)))
      
    (if (or (equal container +language-key+) (equal container +index-key+))
      ;; Handle language and index maps.
      ;; Get or create the map object
      (let ((map-object (jsd-setdefault rval item-active-property (jsd-make))))
        ;; If container is a language map, simplify compacted value to a simple string:
        (when (and (equal container +language-key+) (is-value compacted-item))
          (setf compacted-item (jsd-get +value-key+ compacted-item)))
        
        ;; Add compact value to map object using key from expanded value 
        ;; based on the container type:
        (node-add-value map-object (jsd-get container expanded-item) compacted-item))
      ;; else use an array if compact-arrays flag is false,
      ;; @container is @set or @list, value is an empty list, or key is @graph
      (let ((is-array
             (or (not (getf options :compact-arrays))
                 (equal container +set-key+)
                 (equal container +list-key+)
                 (empty-list-p compacted-item)
                 (equal expanded-property +list-key+)
                 (equal expanded-property +graph-key+))))
        ;; Add compact value:
        (node-add-value rval item-active-property compacted-item :property-is-array is-array)))))

(defun compact-list-item-list (rval compacted-item expanded-item container active-ctx item-active-property)
  "Compact a @list item."
  ;; Ensure @list is a list:
  (setf compacted-item (to-list compacted-item))
  
  (cond
   ((not (equal container +list-key+))
    ;; Wrap using @list alias:
    (let ((wrapper (jsd-make)))
      (setf (jsd-get (compact-iri active-ctx +list-key+) wrapper) compacted-item)
      (setf compacted-item wrapper))
    
    ;; Include @index from expanded @list, if any:
    (when (jsd-get +index-key+ expanded-item)
      (setf (jsd-get (compact-iri active-ctx +index-key+) compacted-item) 
            (jsd-get +index-key+ expanded-item))))
   ((jsd-get item-active-property rval)
    (error 'json-ld-error
      :message "JSON-LD compact error; property has a @list @container rule but there is more than a single
 @list that matches the compacted term in the document. Compaction might mix unwanted items into the list."
      :code "compaction to list of lists")))
  
  compacted-item)
  
(defun compact-value (active-ctx active-property value)
  "Perform value compaction on an object with @value or @id as the only property.

  Converted from pyld's JsonLdProcessor._compact_value method."
  (if (is-value value)
    (compact-value-value active-ctx active-property value)
    ;; Value is a subject reference:
    (compact-value-id active-ctx active-property value)))

(defun compact-value-value (active-ctx active-property value)
  "Compact @value."
  (let*
      ((type (get-context-value active-ctx active-property +type-key+))
       (language (get-context-value active-ctx active-property +language-key+))
       (container (get-context-value active-ctx active-property +container-key+))
       
       ;; Whether or not the value has and @index that must be preserved:
       (preserve-index (and (jsd-get +index-key+ value) (not (equal container +index-key+)))))
    
    (unless preserve-index
      ;; If there's no index to preserve 
      ;; and matching @type or @language specified in context, compact:
      (when (or (equal (jsd-get +type-key+ value) type)
                (equal (jsd-get +language-key+ value) language))
        (return-from compact-value-value (jsd-get +value-key+ value))))
    
    ;; Return just the value of @value if all are true:
    ;; 1. @value is the only key or @index isn't being preserved
    ;; 2. There is no default language or @value is not a string 
    ;;    or the key has a mapping with a null @language.
    (let*
        ((key-count (jsd-count value))
         (is-value-only-key (or (= key-count 1)
                                (and (= key-count 2) (jsd-get +index-key+ value) (not preserve-index))))
         (has-default-language (jsd-get +language-key+ active-ctx))
         (is-value-string (stringp (jsd-get +value-key+ value)))
         (has-null-mapping (and (jsd-get active-property (jsd-get "mappings" active-ctx))
                                (eq (jsd-get +language-key+ 
                                            (jsd-get active-property (jsd-get "mappings" active-ctx))) +jsd-null+))))
      (when (and is-value-only-key
                 (or (not has-default-language)
                     (not is-value-string)
                     has-null-mapping))
        (return-from compact-value-value (jsd-get +value-key+ value))))
    
    (let ((rval (jsd-make)))    
      (when preserve-index
        ;; Preserve @index:
        (setf (jsd-get (compact-iri active-ctx +index-key+) rval) (jsd-get +index-key+ value)))
    
      (cond
       ((jsd-get +type-key+ value)
        ;; Compact @type IRI:
        (setf (jsd-get (compact-iri active-ctx +type-key+) rval)
              (compact-iri active-ctx (jsd-get +type-key+ value) :vocab t)))
       ((jsd-get +language-key+ value)
        ;; Alias @language:
        (setf (jsd-get (compact-iri active-ctx +language-key+) rval)
              (jsd-get +language-key+ value))))
    
      ;; Alias @value:
      (setf (jsd-get (compact-iri active-ctx +value-key+) rval)
            (jsd-get +value-key+ value))
      rval)))

(defun compact-value-id (active-ctx active-property value)
  "Compact @id."
  (let*
      ((expanded-property (expand-iri active-ctx active-property :vocab t))
       (type (get-context-value active-ctx active-property +type-key+))
       (compacted (compact-iri active-ctx (jsd-get +id-key+ value) :vocab (equal type +vocab-key+))))
    
    ;; Compact to scalar
    (if (or (equal type +id-key+)
              (equal type +vocab-key+)
              (equal expanded-property +graph-key+))
      compacted
      (let ((rval (jsd-make)))
        (setf (jsd-get (compact-iri active-ctx +id-key+) rval) compacted)
        rval))))
              
(defun compact-iri (active-ctx iri &key value vocab reverse)
  "Compact an IRI or keyword into a term or CURIE if it can be.
  If the IRI has ana ssociated value it may be passed.

  Converted from pyld's JsonLdProcessor._compact_iri method."
  ;; Can't compact :null:
  (when (eq iri +jsd-null+)
    (return-from compact-iri iri))
  
  ;; Term is a keyword, force vocab to True
  (when (is-keyword iri)
    (setf vocab t))
  
  (when (and vocab (jsd-haskey iri (get-inverse-context active-ctx)))
    (let ((term (get-term-from-inverse-context active-ctx iri value reverse)))
      (when term
        (return-from compact-iri term))))
  
  ;; No term match, use @vocab if available:
  (when vocab
    (jsd-when-get +vocab-key+ active-ctx (vocab-value)
      (when (and (starts-with-p iri vocab-value) (not (equal iri vocab-value)))
        ;; Use suffix as relative iri if it is not a term in the active context
        (let ((suffix (subseq iri (length vocab-value))))
          (when (not (jsd-haskey suffix (jsd-get "mappings" active-ctx)))
            (return-from compact-iri suffix))))))
  
  ;; No term or @vocab match, check for possible CURIEs
  (let ((candidate (find-curie-candidate active-ctx iri value)))
    (when candidate
      (return-from compact-iri candidate)))
  
  ;; Compact IRI relative to base
  (unless vocab
    (return-from compact-iri (remove-base (jsd-get +base-key+ active-ctx) iri)))
  
  ;; Return IRI as is
  iri)    
    
(defun find-curie-candidate (active-ctx iri value)
  (let ((candidate nil))
    (flet ((process-mapping (term definition)
             (when (find #\: term)
               (return-from process-mapping))
             ;; Skip entries with @ids that are not partial matches
             (when (or (not definition)
                       (eq definition +jsd-null+)
                       (equal (jsd-get +id-key+ definition) iri)
                       (not (starts-with-p iri (jsd-get +id-key+ definition))))
               (return-from process-mapping))
                            
             ;; a CURIE is usable if:
             ;; 1. it has no mapping, OR
             ;; 2. value is None, which means we're not compacting an @value, AND
             ;;  the mapping matches the IRI.
             (let* ((curie (concatenate 'string term ":" (subseq iri (length (jsd-get +id-key+ definition)))))
                    (is-usable-curie (or (not (jsd-haskey curie (jsd-get "mappings" active-ctx)))
                                         (and (null value)
                                              (equal (multi-jsd-get active-ctx "mappings" curie +id-key+) iri)))))
               (when (and is-usable-curie
                          (or (null candidate)
                              (shorter-or-least curie candidate)))
                 (setf candidate curie)))))
      (jsd-map #'process-mapping (jsd-get "mappings" active-ctx)))
    candidate))

(defun remove-base (base iri)
  "Remove a base IRI from the given absolute IRI.

  Converted from pyld's remove_base function."
  (let* ((base (parse-uri base))
         (rel (parse-uri iri))
         (enough (enough-uri rel base))
         (removed (render-uri enough nil)))
    (when (and (null (uri-path base)) (starts-with-p removed "/"))
      (setf removed (subseq removed 1)))
    removed))

(defun uri-netloc (uri)
  "Return netloc (host:port) for an URI."
  (let ((host (uri-host uri))
        (port (uri-port uri)))
    (if port
        (format nil "~a:~a" host port)
        host)))

(defun get-inverse-context (active-ctx)
  "Generate an inverse context for use in the compaction algorithm,
  if not already generated for the given active context.

  Converted from pyld's JsonLdProcessor._get_inverse_context method."
  (let ((inversed (jsd-get "inverse" active-ctx)))
    (when inversed
      (return-from get-inverse-context inversed)))
  
  (let ((inverse (jsd-make))
        (default-language (jsd-getdefault +language-key+ active-ctx +none-key+)))
    (setf (jsd-get "inverse" active-ctx) inverse)
    
    (flet ((create-selection (term mapping)
             (when (eq mapping +jsd-null+)
               (return-from create-selection))
                             
             ;; Add term selection where it applies
             (let ((container (jsd-getdefault +container-key+ mapping +none-key+))
                     (iris (to-list (jsd-get +id-key+ mapping))))
                 (loop for iri in iris do
                   (let* ((container-map (jsd-setdefault inverse iri (jsd-make)))
                          (entry (jsd-setdefault container-map container (make-inverse-entry))))
                     (cond
                      
                      ((jsd-get "reverse" mapping)
                       ;; Term is preferred for values using @reverse
                       (jsd-setdefault (jsd-get +type-key+ entry) +reverse-key+ term))
                      
                      ((jsd-haskey +type-key+ mapping)
                       ;; Term is preferred for values using specific type
                       (jsd-setdefault (jsd-get +type-key+ entry) (jsd-get +type-key+ mapping) term))
                      
                      ((jsd-haskey +language-key+ mapping)
                       ;; Term is preferred for values using specific language
                       (let ((language (jsd-get +language-key+ mapping)))
                         (when (eq language +jsd-null+)
                           (setf language +null-key+))
                         (jsd-setdefault (jsd-get +language-key+ entry) language term)))
                      
                      (t
                       ;; Term is preferred for values w/default language or not typ and no language.
                       ;; Add an entry for the default language:
                       (jsd-setdefault (jsd-get +language-key+ entry) default-language term)
                       ;; Add entries for no type and no language:
                       (jsd-setdefault (jsd-get +type-key+ entry) +none-key+ term)
                       (jsd-setdefault (jsd-get +language-key+ entry) +none-key+ term))))))))
      ;; Create term selections for each mapping in the context,
      ;; ordered by shortest and then lexicographically least.
      (jsd-sorted-map #'create-selection (jsd-get "mappings" active-ctx) :test #'shorter-or-least))
    inverse))

(defun shorter-or-least (a b)
  "Return T iff string a is either shorter or lexicographically earlier than b."
  (let ((a-len (length a))
        (b-len (length b)))
    (if (= a-len b-len)
        (string< a b)
      (< a-len b-len))))

(defun make-inverse-entry ()
  "Create an entry for an inversed context."
  (jsd-make +language-key+ (jsd-make) +type-key+ (jsd-make)))

(defun get-term-from-inverse-context (active-ctx iri value reverse)
  "Use inverse context to pick a term if iri is relative to vocab."
  (let ((default-language (jsd-getdefault +language-key+ active-ctx +none-key+))
        (containers '())
        (type-or-language +language-key+)
        (type-or-language-value +null-key+))
    
    ;; Prefer @index if available in value
    (when (and (is-object value) (jsd-haskey +index-key+ value))
      (push +index-key+ containers))
    
    (cond
     
     (reverse
      (setf type-or-language +type-key+)
      (setf type-or-language-value +reverse-key+)
      (push +set-key+ containers))
     
     ((is-list value)
      ;; Choose most specific term that works for all elements in @list.
      (unless (jsd-haskey +index-key+ value)
        ;; Only select @list containers if @index is NOT in value.
        (push +list-key+ containers))
      (let* ((list (jsd-get +list-key+ value))
             (common-language (if (endp list) default-language nil))
             (common-type nil))
        
        (loop for item in list do
          (multiple-value-bind (item-language item-type) (get-item-language-and-type item)
            (cond
             ((null common-language)
              (setf common-language item-language))
             ((and (not (equal item-language common-language)) (is-value item))
              (setf common-language +none-key+)))
            
            (cond
             ((null common-type)
              (setf common-type item-type))
             ((not (equal item-type common-type))
              (setf common-type +none-key+)))
            
            (when (and (equal common-language +none-key+) (equal common-type +none-key+))
              ;; There are different languages and types in the list, 
              ;; so choose the most generic term, no need to keep iterating.
              (return))))
        
        (setf common-language (or common-language +none-key+))
        (setf common-type (or common-type +none-key+))
        (if (equal common-type +none-key+)
          (setf type-or-language-value common-language)
          (progn
            (setf type-or-language +type-key+)
            (setf type-or-language-value common-type)))))
     
     (t
      ;; Non-@list
      (if (is-value value)
        (cond
         ((and (jsd-haskey +language-key+ value) (not (jsd-haskey +index-key+ value)))
          (push +language-key+ containers)
          (setf type-or-language-value (jsd-get +language-key+ value)))
         
         ((jsd-haskey +type-key+ value)
          (setf type-or-language +type-key+)
          (setf type-or-language-value (jsd-get +type-key+ value))))
      ;; else
        (progn
          (setf type-or-language +type-key+)
          (setf type-or-language-value +id-key+)))
      (push +set-key+ containers)))
              
    ;; Do term selection
    (push +none-key+ containers)
    (setf containers (reverse containers))
    (select-term active-ctx iri value containers type-or-language type-or-language-value)))

(defun get-item-language-and-type (item)
  (let ((language +none-key+)
        (type +none-key+))
    (if (is-value item)
      (cond
       ((jsd-haskey +language-key+ item)
        (setf language (jsd-get +language-key+ item)))
       
       ((jsd-haskey +type-key+ item)
        (setf type (jsd-get +type-key+ item)))
       
       (t ; plain literal
        (setf language +null-key+)))
      
      ;; else
      (setf type +id-key+))
    (values language type)))

(defun select-term (active-ctx iri value containers type-or-language type-or-language-value)
  "Pick the preferred compaction term from the inverse context entry.

  Converted from pyld's JsonLdProcessor._select_term method."
  (setf type-or-language-value (or type-or-language-value +null-key+))
  
  (let ((prefs '())) ; Preferred options for the value of @type or language.
    (if (and (or (equal type-or-language-value +id-key+) (equal type-or-language-value +reverse-key+))
             (is-subject-reference value))
      ;; Determine prefs for @id based on whether value compacts to term.
      (progn 
        ;; Prefer @reverse first:
        (when (equal type-or-language-value +reverse-key+)
          (push +reverse-key+ prefs))
        
        ;; Try to compact value to a term:
        (let* ((term (compact-iri active-ctx (jsd-get +id-key+ value) :vocab t))
               (mapping (jsd-get term (jsd-get "mappings" active-ctx))))
          (if (and term
                   mapping
                   (equal (jsd-get +id-key+ mapping) (jsd-get +id-key+ value)))
            (progn
              (push +vocab-key+ prefs)
              (push +id-key+ prefs))
            ;;else
            (progn
              (push +id-key+ prefs)
              (push +vocab-key+ prefs)))))
      ;; else
      (push type-or-language-value prefs))
    
    (push +none-key+ prefs)
    (setf prefs (reverse prefs))
      
    (let ((container-map (multi-jsd-get active-ctx "inverse" iri)))
      (loop for container in containers
          when (jsd-haskey container container-map) do
        (let ((type-or-language-value-map (multi-jsd-get container-map container type-or-language)))
          (loop for pref in prefs
              when (jsd-haskey pref type-or-language-value-map) do
            (return-from select-term (jsd-get pref type-or-language-value-map)))))))
  ;; not found:
  nil)
        
    
         
