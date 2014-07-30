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

(defun frame (input frame &optional (options nil))
  "Perform JSON-LD framing.

  Converted from pyld's frame function."
  ;; Set default options
  (plist-setdefault options :base (if (stringp input) input ""))
  (plist-setdefault options :compact-arrays t)
  (plist-setdefault options :embed t)
  (plist-setdefault options :document-loader *default-document-loader*)
  
  ;; If frame is a string, attempt to dereference remote document
  (multiple-value-bind (context-url document-url document)
      (if (stringp frame)
          (funcall (getf options :document-loader) frame)
          (values +jsd-null+ nil frame))
    (declare (ignore document-url))
    (handler-bind ((json-ld-error #'(lambda (cause)
                                      (error 'json-ld-error
                                        :message "Could not retrieve a JSON-LD document from the URL."
                                        :code "loading document failed"
                                        :cause cause))))
      (unless document
        (error 'json-ld-error :message "No remote document found at the given URL."))
      (when (stringp document)
        (setf document (jsd-read document))))
    
    ;; Preserve frame context
    (let ((frame document)
          (ctx nil)
          (expanded nil)
          (expanded-frame nil))
      (when frame
        (setf ctx (jsd-get +context-key+ frame))
        (when (not (eq context-url +jsd-null+))
          (setf ctx (if ctx
                        (append (to-list ctx) (list context-url))
                        context-url)
                (jsd-get +context-key+ frame) ctx)))
      
      ;; If no context present, make an empty context.
      (setf ctx (or ctx (jsd-make)))
      
      ;; Expand input
      (handler-bind ((json-ld-error #'(lambda (cause)
                                        (error 'json-ld-error
                                          :message "Could not expand input before framing"
                                          :cause cause))))
        (setf expanded (expand input options)))
      
      ;; Expand frame
      (let ((opts (copy-list options)))
        (setf (getf opts :keep-free-floating-nodes) t)
        (handler-bind ((json-ld-error #'(lambda (cause)
                                          (error 'json-ld-error
                                            :message "Could not expand frame before framing"
                                            :cause cause))))
          (setf expanded-frame (expand frame opts))))
      
      ;; Do framing
      (let ((framed (internal-frame expanded expanded-frame options))
            (compacted nil)
            (active-ctx nil))
        
        ;; Compact result (force @graph option to T)
        (setf (getf options :graph) t
              (getf options :skip-expansion) t
              (getf options :active-ctx) t)
        
        (handler-bind ((json-ld-error #'(lambda (cause)
                                          (error 'json-ld-error
                                            :message "Could not compact framed output."
                                            :cause cause))))
          (multiple-value-bind (comp act-ctx) (compact framed ctx options)
            (setf compacted comp
                  active-ctx act-ctx)))
        
        ;; Get graph alias
        (let ((graph (compact-iri active-ctx +graph-key+)))
          ;; Remove @preserve from results
          (setf (jsd-get graph compacted) (remove-preserve active-ctx (jsd-get graph compacted) options)))
        
        compacted))))

(defun internal-frame (input frame options)
  "Perform JSON-LD framing.

  :param input_: the expanded JSON-LD to frame.
  :param frame: the expanded JSON-LD frame to use.
  :param options: the framing options.
  
  Converted from pyld's JsonLdProcessor._frame method."
  ;; Create framing state
  (let ((state (make-state :options options
                           :graphs (jsd-make +default-graph-name+ (jsd-make) +merged-key+ (jsd-make))))
        (namer (make-namer)))
    
    ;; Produce a map of all graphs and name each bnode
    (create-node-map input (state-get state :graphs) +merged-key+ namer)
    (state-set state :subjects (jsd-get +merged-key+ (state-get state :graphs)))
    
    ;; Frame the subjects
    (let ((framed '())
          (sorted-subject-keys (jsd-sorted-keys (state-get state :subjects))))
      (setf framed (match-frame state sorted-subject-keys frame framed nil))
      (nreverse framed))))

(defun make-state (&rest args)
  (let ((state (make-hash-table)))
    (loop for (key value) on args by #'cddr do
          (state-set state key value))
    state))

(defun state-set (state key value)
  (setf (gethash key state) value))

(defun state-get (state key)
  (gethash key state))

(defun match-frame (state subjects frame parent property)
  "Frame subjects according to given frame.

  Return modified parent."
  ;; Validate the frame
  (validate-frame frame)
  (setf frame (first frame))
  
  ;; Filter out subjects that match the frame
  (let* ((matches (filter-subjects state subjects frame))
         (options (state-get state :options))
         (embed-on (get-frame-flag frame options "embed"))
         (explicit-on (get-frame-flag frame options "explicit")))
       
    ;; Add matches to output
    (sorted-maphash #'(lambda (id subject)
                        (setf parent (process-frame-match id subject state frame parent 
                                                          property options embed-on explicit-on)))
                    matches))
  parent)

(defun validate-frame (frame)
  "Validate a JSON-LD frame, throwing an exception if the frame is invalid."
  (unless (and (listp frame)
               (= (length frame) 1)
               (is-object (first frame)))
    (error 'json-ld-error
      :message "Invalid JSON-LD syntax; a JSON-LD frame must be a single object.")))

(defun filter-subjects (state subjects frame)
  "Return a hashmap of all of the subjects that match a parsed frame."
  (let ((rval (make-string-hashtable)))
    (loop for id in subjects
        for subject = (jsd-get id (state-get state :subjects))
        when (filter-subject subject frame)
        do (setf (gethash id rval) subject))
    rval))

(defun filter-subject (subject frame)
  "Return T iff the given subject matches the given frame."
  ;; Check @type (object value means 'any' type, fall through to ducktyping.
  (let ((frame-types (jsd-get +type-key+ frame)))
    (when (and frame-types
               (not (and (= (length frame-types) 1)
                         (is-object (first frame-types)))))
      ;; Any matching @type is a match.
      (return-from filter-subject 
                   (some #'(lambda (type) (node-has-value-p subject +type-key+ type)) frame-types))))
  
  ;; Check ducktype
  (jsd-map #'(lambda (k v)
               (declare (ignore v))
               ;; Only not a duck if @id or non-keyword isn't in subject
               (when (and (or (equal k +id-key+)
                              (not (is-keyword k)))
                          (not (jsd-haskey k subject)))
                 (return-from filter-subject nil)))
           frame)
  t)
               
(defun get-frame-flag (frame options name)
  "Get the frame flag value (boolean) for the given flag name.

  Name should be in camelCase."
  (let* ((key (concatenate 'string "@" name))
         (value (jsd-get key frame)))
    (if value
        (eq (first value) +jsd-true+)
        ;; else take value from options
        (getf options (camel-case->symbol name :keyword)))))
        
(defun process-frame-match-set-embed (embed-on id state output)
  (let ((existing (gethash id (state-get state :embeds))))
    (when (and embed-on existing)
      ;; Only overwrite an existing embed if it has already been added
      ;; to its parent -- otherwise its parent is somewhere up
      ;; the tree from this embed and the embed would occur twice 
      ;; once the tree is added
      (setf embed-on nil)
      
      (let ((existing-parent (getf existing :parent)))
        (cond
         ((listp existing-parent)
          ;; Existing embed's parent is a list
          (setf embed-on (some #'(lambda (p) (compare-values output p)) existing-parent)))
         
         ((node-has-value-p existing-parent (getf existing :property) output)
          ;; Existing embed's parent is an object
          (setf embed-on t))))
      
      ;; Existing embed has already been added, so allow an overwrite
      (when embed-on
        (remove-embed state id))))
  embed-on)
      
      
(defun add-frame-output (parent property output)
  "Add framing output to the given parent.

  Return modified parent."
  (if (is-object parent)
      (node-add-value parent property output :property-is-array t)
      (push output parent))

  parent)

(defun remove-preserve (ctx input options)
  "Remove the @preserve keywords as the last step of the framing algorithm."
  (cond
   ((listp input)
    ;; Recurse through lists
    (return-from remove-preserve (loop for e in input
                                     for result = (remove-preserve ctx e options)
                                     ;; drop nulls from arrays
                                     unless (eq result +jsd-null+)
                                     collect result)))
   
   ((is-object input)
    ;; Remove @preserve
    (jsd-when-get +preserve-key+ input (preserve)
      (return-from remove-preserve (if (equal preserve +null-key+) +jsd-null+ preserve)))
    
    ;; Skip @values
    (when (is-value input)
      (return-from remove-preserve input))
    
    ;; Recurse through @lists
    (when (is-list input)
      (setf (jsd-get +list-key+ input) (remove-preserve ctx (jsd-get +list-key+ input) options))
      (return-from remove-preserve input))
    
    ;; Recurse through properties
    (jsd-map #'(lambda (prop v)
                 (let ((result (remove-preserve ctx v options))
                       (container (get-context-value ctx prop +container-key+)))
                   (when (and (getf options :compact-arrays)
                              (listp result)
                              (= (length result) 1)
                              (not (equal container +set-key+))
                              (not (equal container +list-key+)))
                     (setf result (first result)))
                   (setf (jsd-get prop input) result)))
             input)))
                  
  input)
                            
(defun process-frame-match (id subject state frame parent property options embed-on explicit-on)
  "Return modified parent."
  ;; In order to treat each top-level match as a compartmentalized result,
  ;; create an independent copy of the embedded subjects map when the property is nil,
  ;; which only occurs at top-level.
  (unless property
    (state-set state :embeds (make-string-hashtable)))
  
  ;; Start output
  (let* ((output (make-id-entry id))
         (embed-on (process-frame-match-set-embed embed-on id state output)))
    (when embed-on
      (setf (gethash id (state-get state :embeds)) (list :parent parent :property property))
      ;; Iterate over subject properties in order
      (jsd-sorted-map #'(lambda (prop objects)
                         (process-frame-match-process-property subject prop objects state frame output explicit-on))
                     subject)
      ;; Handle defaults in order
      (loop for prop in (jsd-sorted-keys frame)
          unless (is-keyword prop)  ; skip keywords
          do (process-frame-match-handle-frame-default prop frame output options)))
    
    ;; Add output to parent
    (add-frame-output parent property output)))
    
(defun process-frame-match-process-property (subject prop objects state frame output explicit-on)
  "Converted from part of pyld's _match_frame method."
  (cond
   ((is-keyword prop)
    ;; Copy keywords to output
    (setf (jsd-get prop output) (deepcopy (jsd-get prop subject))))
   
   ((not (jsd-haskey prop frame))
    ;; If property isn't in the frame,
    ;; If explicit is off, embed values.
    (unless explicit-on
      (embed-values state subject prop output)))
   
   (t
    ;; Add objects
    (loop for object in objects do
          (process-frame-match-add-object prop object state frame output)))))

(defun process-frame-match-add-object (prop object state frame output)
  "Converted from part of pyld's _match_frame method."
  (cond
   ((is-list object)
    ;; Recurse into list.
    ;; Add empty list.
    (let ((list (jsd-make +list-key+ '())))
      (add-frame-output output prop list)
      
      ;; Add list objects
      (loop for object in (jsd-get +list-key+ object) do
        (if (is-subject-reference object)
            ;; Recurse into subject reference
            (match-frame state (list (jsd-get +id-key+ object))
                         (jsd-get +list-key+ (first (jsd-get prop frame)))
                         list +list-key+)
            ;; else include other values automatically
            (add-frame-output list +list-key+ (deepcopy object))))))
   
   ((is-subject-reference object)
    ;; Recurse into subject reference
    (match-frame state (list (jsd-get +id-key+ object))
                 (jsd-get prop frame) output prop))
   
   (t
    ;; Include other values automatically
    (add-frame-output output prop (deepcopy object)))))

(defun embed-values (state subject property output)
  "Embed values for the given subject and property into the given output
  during the framing algorithm.

  Returns modified output.

  Converted from pyld's JsonLdProcessor._embed_values method."
  ;; Embed subject properties in output
  (loop for object in (jsd-get property subject) do
    (cond
     ((is-list object)
      ;; Recurse into @list
      (let ((list (jsd-make +list-key+ '())))
        (setf output (add-frame-output output property list)
              (jsd-get +list-key+ list) 
                (reverse (embed-values state object +list-key+ (jsd-get +list-key+ list)))))
      (return-from embed-values output))
     
     ((is-subject-reference object)
      ;; Handle subject reference
      (let ((id (jsd-get +id-key+ object))
            (state-embeds (state-get state :embeds)))
        ;; Embed full subject if isn't already embedded
        (unless (gethash id state-embeds)
          ;; Add embed
          (setf (gethash id state-embeds) (list :parent output :property property))
          ;; Recurse into subject
          (setf object (jsd-make))
          (let ((state-subject (jsd-get id (state-get state :subjects))))
            (jsd-map #'(lambda (prop value)
                         ;; Copy keywords
                         (if (is-keyword prop)
                             (setf (jsd-get prop object) (deepcopy value))
                           (embed-values state state-subject prop object)))
                     state-subject)))
        (setf output (add-frame-output output property object)
              (getf (gethash id state-embeds) :parent) output)
        output))
     
     (t
      ;; Copy non-subject value
      (add-frame-output output property (deepcopy object))))))

(defun process-frame-match-handle-frame-default (prop frame output options)
  "Converted from part of pyld's JsonLdProcessor._match_frame method."
  ;; If omit default is off, then include default values for properties
  ;; that appear in the next frame but are not in the matching subject.
  (let* ((next (first (jsd-get prop frame)))
         (omit-default-on (get-frame-flag next options "omitDefault")))
    (unless (or omit-default-on (jsd-haskey prop output))
      (let ((preserve (to-list (if (jsd-haskey +default-graph-name+ next)
                                   (deepcopy (jsd-get +default-graph-name+ next))
                                   +null-key+))))
        (setf (jsd-get prop output) (list (jsd-make +preserve-key+ preserve)))))))

(defun remove-embed (state id)
  "Remove an existing embed."
  ;; Get existing embed
  (let* ((embeds (state-get state :embeds))
         (embed (gethash id embeds))
         (property (getf embed :property))
         ;; Create reference to replace embed
         (subject (make-id-entry id)))
    
    ;; Remove existing embed
    (let ((new-parent (remove-embed-only embed subject property)))
      ;; Update parent, which is a list and must be manually updated:
      (setf (getf embed :parent) new-parent
            (gethash id embeds) embed))
    
    ;; Recursively remove dependent dangling embeds
    (labels ((remove-dependents (id)
               ;; Get embed keys as a separate array
               ;; to enable deleting keys in map
               (loop for next in (alexandria:hash-table-keys embeds)
                     for embed-next = (gethash next embeds)  ; might have been removed already
                     when (and embed-next
                               (is-object (getf embed-next :parent))
                               (equal (jsd-get +id-key+ (getf embed-next :parent)) id))
                   do (remhash next embeds)
                      (remove-dependents next))))
      (remove-dependents id))))

(defun remove-embed-only (embed subject property)
  "Remove the embed itself (without dependents).

  Return the new parent."
  (let ((old-parent (getf embed :parent)))
    (if (listp old-parent)
        ;; Replace subject with reference in list
        (mapcar #'(lambda (parent) (if (compare-values parent subject) subject parent)) old-parent)
        ;; else replace subject with reference
        (let ((use-array (listp (jsd-get property old-parent))))
          (node-remove-value old-parent property subject :property-is-array use-array)
          (node-add-value old-parent property subject :property-is-array use-array)
          old-parent))))
    
(defun node-remove-value (subject property value &key property-is-array)
  (let ((values (remove-if #'(lambda (e) (compare-values e value)) (node-get-values subject property))))
    (cond
     ((= (length values) 0)
      (remove-property subject property))
     ((and (= (length values) 1) (not property-is-array))
      (setf (jsd-get property subject) (first values)))
     (t
      (setf (jsd-get property subject) values)))))
    
(defun remove-property (subject property)
  (jsd-remove property subject))

(defun node-get-values (subject property)
  (to-list (jsd-getdefault property subject '())))
