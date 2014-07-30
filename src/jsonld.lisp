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

;;;; Global environment

(defvar *rdf-parsers* nil "Alist of pairs: format->parser function")

(defun register-rdf-parser (format parser)
  "Register a parser for a given format.
  If a parser for this format already exists, it is replaced."
  (setf *rdf-parsers* (string-alist-add *rdf-parsers* format parser)))

;;;; XSD constants

(alexandria:define-constant +xsd-string+ "http://www.w3.org/2001/XMLSchema#string" :test #'string=)
(alexandria:define-constant +xsd-boolean+ "http://www.w3.org/2001/XMLSchema#boolean" :test #'string=)
(alexandria:define-constant +xsd-integer+ "http://www.w3.org/2001/XMLSchema#integer" :test #'string=)
(alexandria:define-constant +xsd-double+ "http://www.w3.org/2001/XMLSchema#double" :test #'string=)

;;;; RDF constants

(alexandria:define-constant +rdf+ "http://www.w3.org/1999/02/22-rdf-syntax-ns#" :test #'string=)
(alexandria:define-constant +rdf-list+ (concatenate 'string +rdf+ "List") :test #'string=)
(alexandria:define-constant +rdf-first+ (concatenate 'string +rdf+ "first") :test #'string=)
(alexandria:define-constant +rdf-rest+ (concatenate 'string +rdf+ "rest") :test #'string=)
(alexandria:define-constant +rdf-nil+ (concatenate 'string +rdf+ "nil") :test #'string=)
(alexandria:define-constant +rdf-type+ (concatenate 'string +rdf+ "type") :test #'string=)
(alexandria:define-constant +rdf-langstring+ (concatenate 'string +rdf+ "langString") :test #'string=)

(alexandria:define-constant +jsonld-keywords+ (list "@base"
                                     "@context"
                                     "@container"
                                     "@default"
                                     "@embed"
                                     "@explicit"
                                     "@graph"
                                     "@id"
                                     "@index"
                                     "@language"
                                     "@list"
                                     "@omitDefault"
                                     "@preserve"
                                     "@reverse"
                                     "@set"
                                     "@type"
                                     "@value"
                                     "@vocab") :test #'equal)

;;;; JSON key constants

(alexandria:define-constant +usages-key+ "usages" :test #'string=)
(alexandria:define-constant +type-key+ "@type" :test #'string=)
(alexandria:define-constant +id-key+ "@id" :test #'string=)
(alexandria:define-constant +list-key+ "@list" :test #'string=)
(alexandria:define-constant +graph-key+ "@graph" :test #'string=)
(alexandria:define-constant +value-key+ "@value" :test #'string=)
(alexandria:define-constant +language-key+ "@language" :test #'string=)
(alexandria:define-constant +index-key+ "@index" :test #'string=)
(alexandria:define-constant +reverse-key+ "@reverse" :test #'string=)
(alexandria:define-constant +context-key+ "@context" :test #'string=)
(alexandria:define-constant +base-key+ "@base" :test #'string=)
(alexandria:define-constant +container-key+ "@container" :test #'string=)
(alexandria:define-constant +set-key+ "@set" :test #'string=)
(alexandria:define-constant +vocab-key+ "@vocab" :test #'string=)
(alexandria:define-constant +preserve-key+ "@preserve" :test #'string=)
(alexandria:define-constant +none-key+ "@none" :test #'string=)
(alexandria:define-constant +null-key+ "@null" :test #'string=)
(alexandria:define-constant +merged-key+ "@merged" :test #'string=)

;;;; Utility functions

(defun is-array (value)
  "Return T iff given value is a vector, but not a string."
  (and (not (typep value 'string)) (typep value 'vector)))

(defun to-list (value)
  "Convert a list, array or single value to a list."
  (cond
   ((listp value)
     value)
   ((is-array value)
     (coerce value 'list))
   (t
     (list value))))

(defun list->array (list)
  "Convert a list to a fixed-length array (vector)."
  (make-array (list (length list)) :initial-contents list))

(alexandria:define-constant +escape-replacements+
    (list (cons "\\\\" "\\\\")
          (cons (string #\Tab) "\\t")
          (cons (string #\Newline) "\\n")
          (cons (string #\Return) "\\r")
          (cons "\"" "\\\""))
  :test #'equal
  :documentation "Things to replace in a string to escape it.")

(alexandria:define-constant +unescape-replacements+
  (list (cons "\\\\\"" "\"")
          (cons "\\\\t" (string #\Tab))
          (cons "\\\\n" (string #\Newline))
          (cons "\\\\r" (string #\Return))
        (cons "\\\\\\\\" "\\"))
  :test #'equal
  :documentation "Things to replace in a string to unescape it.")

(defun replace-multi (string replacements)
  "Given a list of replacements, return a new string with all replacements applied.
  The original string is not modified."
  (loop for (to-replace . replace-with) in replacements
      do (setf string (cl-ppcre:regex-replace-all to-replace string replace-with))
        finally (return string)))

(defmacro hashtable-setdefault (hashtable key value-form)
  "Return the value under given key in a hashtable.
  If the key is not present in the hashtable, evaluate value-form,
  add it to the hashtable and return it."
  (let ((table-name (gensym)) (key-name (gensym)) (value (gensym)) (present (gensym)))
    `(let ((,table-name ,hashtable) (,key-name ,key))
       (multiple-value-bind (,value ,present) (gethash ,key-name ,table-name)
         (declare (ignore ,value))
         (unless ,present (setf (gethash ,key-name ,table-name) ,value-form))
         (gethash ,key-name ,table-name)))))

(defun make-dynamic-array ()
  "Create an empty array (vector) with dynamic length."
  (make-array 1 :fill-pointer 0 :adjustable t))

(defun make-single-item-array (item)
  "Create an array with one item and dynamic length."
  (let ((array (make-dynamic-array)))
    (vector-push-extend item array)
    array))

(defun string-alist-add (alist key value)
  "Return a new alist containing the key-value pair.
  Key must be a string.
  DESTRUCTIVE if the list already contained the key."
  (if (string-assoc key alist)
    (progn
      (rplacd (assoc key alist :test #'string=) value)
      alist)
    (acons key value alist)))

(defun string-assoc (key alist)
  "Return a cons pair in an alist for the given string key."
  (assoc key alist :test #'string=))

(defmacro plist-setdefault (plist key default)
  "Return the value under given key in a plist.
  If the key is not present in the plist, evaluate default,
  add it to the plist and return it."
  (let ((notfound (gensym))
        (value (gensym))
        (calculated-key (gensym)))
    `(let* ((,calculated-key ,key)
            (,notfound '#:notfound)
            (,value (getf ,plist ,calculated-key ,notfound)))
       (if (eq ,value ,notfound)
           (setf (getf ,plist ,calculated-key) ,default)
           ,value))))
  
;;;; Dataset creation

(alexandria:define-constant +default-graph-name+ "@default" :test #'string=)
(alexandria:define-constant +iri-type+ "IRI" :test #'string=)
(alexandria:define-constant +blank-node-type+ "blank node" :test #'string=)

(alexandria:define-constant +regexp-eoln+ "(?:\\r\\n)|(?:\\n)|(?:\\r)" :test #'string=)
(alexandria:define-constant +regexp-wso+ "[ \\t]*" :test #'string=
  :documentation "Optional whitespace")
(alexandria:define-constant +regexp-empty-line+ (concatenate 'string "^" +regexp-wso+ "$") :test #'string=)
(alexandria:define-constant +regexp-nquad+
             (let* ((iri "(?:<([^:]+:[^>]*)>)")
                    (bnode "(_:(?:[A-Za-z][A-Za-z0-9]*))")
                    (plain "\"([^\"\\\\]*(?:\\\\.[^\"\\\\]*)*)\"")
                    (datatype (concatenate 'string "(?:\\^\\^" iri ")"))
                    (language "(?:@([a-z]+(?:-[a-z0-9]+)*))")
                    (literal (concatenate 'string "(?:" plain "(?:" datatype "|" language ")?)"))
                    (ws "[ \\t]+")
                    
                    (subject (concatenate 'string "(?:" iri "|" bnode ")" ws))
                    (property (concatenate 'string iri ws))
                    (object (concatenate 'string "(?:" iri "|" bnode "|" literal ")" +regexp-wso+))
                    (graph (concatenate 'string "(?:\\.|(?:(?:" iri "|" bnode ")" +regexp-wso+ "\\.))")))
               (concatenate 'string "^" +regexp-wso+ subject property object graph +regexp-wso+ "$"))
             :test #'string=
             :documentation "Regular expression that parses one line containing one nquad.")

(defun match-nquad (line)
  (cl-ppcre:register-groups-bind (m0 m1 m2 m3 m4 m5 m6 m7 m8 m9) (+regexp-nquad+ line :sharedp t)
                                 (return-from match-nquad (list t m0 m1 m2 m3 m4 m5 m6 m7 m8 m9)))
  (list nil))

(defun parse-nquads (input)
  "Parse nquads from a given input (a string). Return a dataset."
  (let ((dataset (create-empty-dataset))
        (lines (cl-ppcre:split +regexp-eoln+ input)))
    (loop for line-number from 1
        for line in lines
        unless (cl-ppcre:scan +regexp-empty-line+ line) do
          (destructuring-bind (matchedp . matches) (match-nquad line)
            (when (not matchedp)
              (error 'json-ld-error :message (format nil "Error while parsing N-Quads invalid quad: ~s" line)))
            (let* ((triple (matches->triple matches))
                   (graph-name (matches->graph-name matches))
                   (graph (maybe-create-graph dataset graph-name)))             
              (add-unique-triple graph triple))))
    dataset))

(defun make-triple (subject predicate object)
  "Create a triple object."
  (list :subject subject :predicate predicate :object object))

(defun matches->triple (matches)
  "Extract a triple from nquad matches."
  (let ((subject (matches->subject matches))
        (predicate (matches->predicate matches))
        (object (matches->object matches)))
    (make-triple subject predicate object)))

(defun matches->subject (matches)
  "Extract triple subject from nquad matches."
  (if (elt matches 0)
      (make-triple-elem +iri-type+ (elt matches 0))
      (make-triple-elem +blank-node-type+ (elt matches 1))))

(defun matches->predicate (matches)
  "Extract triple predicate from nquad matches."
  (make-triple-elem +iri-type+ (elt matches 2)))

(defun matches->object (matches)
  "Extract triple object from nquad matches."
  (cond
   ((elt matches 3) (make-triple-elem +iri-type+ (elt matches 3)))
   ((elt matches 4) (make-triple-elem +blank-node-type+ (elt matches 4)))
   (t (matches->literal-object matches))))

(defun matches->literal-object (matches)
  "Extract triple literal object from nquad matches."
  (let* ((unescaped (replace-multi (elt matches 5) +unescape-replacements+))
         (object (make-triple-elem "literal" unescaped)))
    (cond
     ((elt matches 6) 
      (setf (getf object :datatype) (elt matches 6)))
     ((elt matches 7)
      (setf (getf object :datatype) +rdf-langstring+)
      (setf (getf object :language) (elt matches 7)))
     (t (setf (getf object :datatype) +xsd-string+)))
    object))

(defun matches->graph-name (matches)
  "Extract graph name from nquad matches."
  (or (elt matches 8) (elt matches 9) +default-graph-name+))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-triple-elem (type value)
    "Create a plist representing one of the triple elements in a dataset
    (subject, predicate or object)."
    (list :type type :value value)))

(defun create-empty-dataset ()
  "Create a new, empty dataset object."
  (make-string-hashtable))

(defun maybe-create-graph (dataset name)
  "Create a graph in the dataset with given name, if it
  does not exist yet."
  (hashtable-setdefault dataset name (make-empty-graph)))

(defun make-empty-graph ()
  "Create a graph that can be a part of the dataset."
  (make-dynamic-array))

(defun add-unique-triple (graph triple)
  "Add a triple to the graph if it does not exist yet."
  (unless (graph-has-triple-p graph triple)
    (vector-push-extend triple graph)))

(defun graph-has-triple-p (graph triple)
  "Return a truthy value iff the graph contains the given triple."
  (find triple graph :test #'triples-equal-p))

(defun triples-equal-p (left right)
  "Return T iff given triples are equal.
  Equivalent to pyld's 'JsonPLdProcessor._compare_rdf_triples' method."
  (flet 
      ((elems-equal (attr)
         (let ((left-elem (getf left attr))
               (right-elem (getf right attr)))
           (and (equal-params-p left-elem right-elem :type)
                (equal-params-p left-elem right-elem :value)))))
    (and (every #'elems-equal #(:subject :predicate :object))
         (let ((left-object (getf left :object))
               (right-object (getf right :object)))
           (and (equal-params-p left-object right-object :language)
                (equal-params-p left-object right-object :datatype))))))

(defun equal-params-p (left right param)
  "Return T iff two plists have 'equal' value of the given param."
  (equal (getf left param) (getf right param)))

;;;; unique-namer class

(defun make-namer (&optional (prefix "_:b"))
  (make-instance 'unique-namer :prefix prefix))

(defclass unique-namer ()
  ((prefix
    :initarg :prefix)
   (counter
    :initform 0)
   (existing
    :initform (make-string-hashtable))
   (order
    :initform (make-dynamic-array)
    :reader namer-order)))

(defgeneric namer-get-name (namer &optional old-name))

(defmethod namer-get-name ((namer unique-namer) &optional old-name)  
  (with-slots (existing prefix counter order) namer
    (when old-name
      (multiple-value-bind (value present-p) (gethash old-name existing)
        (when present-p
          (return-from namer-get-name value))))
    (let ((name (format nil "~a~a" prefix counter)))
      (incf counter)
      (when old-name
        (setf (gethash old-name existing) name)
        (vector-push-extend old-name order))
      name)))

(defgeneric namer-is-named (namer old-name))

(defmethod namer-is-named ((namer unique-namer) old-name)
  (with-slots (existing) namer
    (multiple-value-bind (value present-p) (gethash old-name existing)
      (declare (ignore value))
      present-p)))

(defun namer-clone (namer)
  "Clone (duplicate) a namer."
  (let ((cloned-namer (make-instance 'unique-namer :prefix (slot-value namer 'prefix))))
    (setf (slot-value cloned-namer 'counter) (slot-value namer 'counter)
          (slot-value cloned-namer 'existing) (clone-string-hashtable (slot-value namer 'existing))
          (slot-value cloned-namer 'order) (clone-dynamic-array (slot-value namer 'order)))
    cloned-namer))

(defun clone-string-hashtable (old-table)
  "Return a new hashtable with string keys that is a shallow copy of old-table."
  (let ((new-table (make-string-hashtable)))
    (loop for key being the hash-keys of old-table using (hash-value value) do
          (setf (gethash key new-table) value))
    new-table))

(defun clone-dynamic-array (old-array)
  "Return a new dynamic array that is a shallow copy of old-array."
  (let ((new-array (make-dynamic-array)))
    (loop for item across old-array do
          (vector-push-extend item new-array))
    new-array))

;;;; JSON-LD-PROCESSOR class

(defclass json-ld-processor ()
  ((rdf-parsers
    :initarg :rdf-parsers
    :accessor processor-rdf-parsers
    :initform nil
    :documentation "processor-specific RDF parsers")))

(defun processor-from-rdf (processor dataset options)
  "Convert given RDF dataset (dataset object or string) 
  using processor's or global parsers.

  Converted from pyld's JsonLdProcessor.from_rdf method."
  (if (and (null (getf options :format))
           (stringp dataset))
      (setf (getf options :format) "application/nquads"))
  
  (let* 
      ((format (getf options :format))
       (converted-dataset
        (if format
          (funcall (get-format-parser processor format) dataset)
          dataset)))
    (dataset->jsonld converted-dataset options)))

(defun split-string (string delimiter-char)
  (let ((split-pos (position delimiter-char string)))
    (values (subseq string 0 split-pos) (subseq string (+ split-pos 1)))))

(defun uri-path-string (uri)
  "Return uri-path or empty string."
  (or (uri-path uri) ""))

(defun prepend-base (base iri)
  "Prepends a base IRI to the given relative IRI."
  (unless base
    (return-from prepend-base iri))
  (let* ((merged (merge-uris iri base))
         (path (uri-path-string merged))
         (add-slash (ends-with-p path "/")))
    (setf path (normpath path))
    (when (and (not (ends-with-p path "/")) add-slash)
      (setf path (concatenate 'string path "/")))
    (setf (uri-path merged) path)
    (render-uri merged nil)))

(defun expand-iri (active-ctx value &key base vocab local-ctx defined)
  (when (or (eq value +jsd-null+) (is-keyword value))
    (return-from expand-iri value))
  
  ;; define dependency not if defined
  (when (and local-ctx 
             (jsd-haskey value local-ctx) 
             (not (gethash value defined)))
    (create-term-definition active-ctx local-ctx value defined))
  
  (when vocab
    (multiple-value-bind (mapping mapping-present-p) (jsd-get value (jsd-get "mappings" active-ctx))
      (when mapping-present-p
        (when (eq mapping +jsd-null+)
          (return-from expand-iri +jsd-null+))
        (return-from expand-iri (jsd-get +id-key+ mapping)))))
  
  ;; split value into prefix:suffix
  (when (find #\: value)
    (multiple-value-bind (prefix suffix) (split-string value #\:)
      ;; do not expand blank nodes (prefix of '_') or already-absolute IRIs (suffix of '//')
      (when (or (equal prefix "_") (starts-with-p suffix "//"))
        (return-from expand-iri value))
      
      ;; prefix dependency not defined, define it
      (when (and local-ctx (jsd-get prefix local-ctx))
        (create-term-definition active-ctx local-ctx prefix defined))
      
      ;; use mapping if prefix is defined
      (multiple-value-bind (mapping mapping-present-p) (jsd-get prefix (jsd-get "mappings" active-ctx))
        (when mapping-present-p
          (return-from expand-iri (concatenate 'string (jsd-get +id-key+ mapping) suffix))))
      
      (return-from expand-iri value)))
  
  ;; prepend vocab
  (when vocab
    (multiple-value-bind (vocab vocab-present-p) (jsd-get +vocab-key+ active-ctx)
      (when vocab-present-p
        (return-from expand-iri (concatenate 'string vocab value)))))
  
  ;; resolve against base
  (if base
      (prepend-base (jsd-get +base-key+ active-ctx) value)
      value))

(defun absolute-iri-p (iri)
  "Return T iff the given value is an absolute IRI."
  (find #\: iri))

(defun validate-type-value (value)
  "Raises an exception if the given value is not a valid @type value."
  (unless (or (stringp value)                 ; must be a string or
              (is-empty-object value)         ; an empty object or
              (and (listp value)              ; a list
                   (every #'stringp value)))  ;   that contains only strings
    (error 'json-ld-error 
      :message "Invalid JSON-LD syntax; \"@type\" value must a string, an array of strings, or an empty object."
      :code "invalid type value")))

(defun validate-expanded-property (expanded-property expanded-active-property value rval options)
  (when (is-keyword expanded-property)
    (when (equal expanded-active-property +reverse-key+)
      (error 'json-ld-error 
        :message "Invalid JSON-LD syntax; a keyword cannot be used as a @reverse property."
        :code "invalid reverse property map"))
    (when (jsd-get expanded-property rval)
      (error 'json-ld-error 
        :message "Invalid JSON-LD syntax; colliding keywords detected"
        :code "colliding keywords")))
  
  ;; syntax error if @id is not a string
  (when (and (equal expanded-property +id-key+) (not (stringp value)))
    (unless (getf options :is-frame)
      (error 'json-ld-error
        :message "Invalid JSON-LD syntax; @id value must be a string."
        :code "invalid @id value"))
    (unless (is-object value)
      (error 'json-ld-error
        :message "Invalid JSON-LD syntax; @id value must be a string or an object."
        :code "invalid @id value")))
  
  (when (equal expanded-property +type-key+)
    (validate-type-value value))
  
  ;; @graph must be an array or an object
  (when (and (equal expanded-property +graph-key+)
             (not (or (is-object value) (listp value))))
    (error 'json-ld-error 
      :message "Invalid JSON-LD syntax; \"@graph\" must not be an object or an array"
      :code "invalid @graph value"))
  
  ;; @value must not be an object or an array
  (when (and (equal expanded-property +value-key+)
             (or (is-object value) (listp value)))
    (error 'json-ld-error 
      :message "Invalid JSON-LD syntax; \"@value\" value must not be an object or an array."
      :code "invalid value object value"))
  
  ;; @language must be a string
  (when (equal expanded-property +language-key+)
    (when (not (stringp value))
      (error 'json-ld-error 
        :message "Invalid JSON-LD syntax; \"@language\" value must be a string."
        :code "invalid language-tagged string"))
    ;; ensure language value is lowercase
    (setf value (nstring-downcase value)))
  
  ;; @index must be a string
  (when (and (equal expanded-property +index-key+) (not (stringp value)))
    (error 'json-ld-error 
      :message "Invalid JSON-LD syntax; \"@index\" value must be a string."
      :code "invalid @index value")))

(defun expand-value (active-ctx active-property value)
  "Expands the given value by using the coercion and keyword rules in the given context."
  ;; nothing to expand
  (if (eq value +jsd-null+)
    (return-from expand-value value))
  
  (let ((expanded-property (expand-iri active-ctx active-property :vocab t)))
    ;; special-case expand @id and @type (skips '@id' expansion)
    (when (equal expanded-property +id-key+)
      (return-from expand-value (expand-iri active-ctx value :base t)))
    
    (when (equal expanded-property +type-key+)
      (return-from expand-value (expand-iri active-ctx value :vocab t :base t)))
    
    (let ((type (get-context-value active-ctx active-property +type-key+)))
      ;; do @id expansion (automatic for @graph)
      (when (or (equal type +id-key+) 
                (and (equal expanded-property +graph-key+)
                     (stringp value)))
        (return-from expand-value (jsd-make +id-key+ (expand-iri active-ctx value :base t))))
      ;; do @id expansion w/vocab
      (when (equal type +vocab-key+)
        (return-from expand-value (jsd-make +id-key+ (expand-iri active-ctx value :vocab t :base t))))
      
      ;; do not expand keyword values
      (when (is-keyword expanded-property)
        (return-from expand-value value))
      
      (let ((rval (jsd-make)))
        ;; other type        
        (if (not (eq type +jsd-null+))
            (setf (jsd-get +type-key+ rval) type)
          ;; check for language tagging
          (when (stringp value)
            (let ((language (get-context-value active-ctx active-property +language-key+)))
              (when (not (eq language +jsd-null+))
                (setf (jsd-get +language-key+ rval) language)))))
        (setf (jsd-get +value-key+ rval) value)
        rval))))

(defun expand-language-map (language-map)
  "Expands a language map."
  (let ((rval nil))
    (jsd-sorted-map 
     #'(lambda (key values)
         (setf values (arraify values))
         (loop for item in values do
               (when (not (stringp item))
                 (error 'json-ld-error 
                   :message "Invalid JSON-LD syntax; language map values must be strings."
                   :code "invalid language map value"))
               (setf rval (cons (jsd-make +value-key+ item +language-key+ (string-downcase key)) rval))))
     language-map)
    (setf rval (nreverse rval))
    rval))

(defun expand-index-map (index-map active-property active-ctx options)
  (let ((rval nil))
    (jsd-sorted-map 
     #'(lambda (key values)
         (setf values (do-expand active-ctx active-property (arraify values) options nil))
         (loop for item in values do
               (setf (jsd-get +index-key+ item) key)
               (setf rval (cons item rval))))
     index-map)
    (setf rval (nreverse rval))
    rval))

(defun get-expanded-value (container active-ctx key value options active-property expanded-active-property expanded-property)
  ;; handle language map container (skip if value is not an object)
  (when (and (equal container +language-key+)
             (is-object value))
    (return-from get-expanded-value (expand-language-map value)))
  
  ;; handle index container (skip if value is not an object)
  (when (and (equal container +index-key+) (is-object value))
    (return-from get-expanded-value (expand-index-map value key active-ctx options)))
  
  ;; language or @set
  (let ((is-list (equal expanded-property +list-key+)))
    (let ((res (if (or is-list 
            (equal expanded-property +set-key+))
        (let ((next-active-property active-property))
          (when (and is-list
                     (equal expanded-active-property +graph-key+))
            (setf next-active-property nil))
          (do-expand active-ctx next-active-property value options is-list))      
      ;; recursively expand value w/key as new active property
                 (do-expand active-ctx key value options nil))))
      res)))

(defun expand-use-array-p (prop)
  (not (find prop (list +index-key+ +id-key+  +type-key+ +value-key+ +language-key+) :test #'string=)))

(defun expanded-type-invalid-p (rval)
  (multiple-value-bind (type type-present-p) (jsd-get +type-key+ rval)
    (and type-present-p
         (or (not (absolute-iri-p type))
             (blank-name-p type)))))

(defun finalize-expansion (rval options inside-list active-property expanded-active-property)
  ;; get property count on expanded output
  (let ((count (jsd-count rval))
        (handled-p nil))
    
    (multiple-value-bind (value value-present-p) (jsd-get +value-key+ rval)
      (when value-present-p
        (setf handled-p t)
        ;; @value must only have @language or @type
        (when (and (jsd-haskey +type-key+ rval)
                   (jsd-haskey +language-key+ rval))
          (error 'json-ld-error 
            :message "Invalid JSON-LD syntax; an element containing \"@value\" may not contain both \"@type\" and \"@language\"."
            :code "invalid value object"))
        (let ((valid-count (- count 1)))
          (when (jsd-haskey +type-key+ rval)
            (decf valid-count))
          (when (jsd-haskey +index-key+ rval)
            (decf valid-count))
          (when (jsd-haskey +language-key+ rval)
            (decf valid-count))
          (when (not (eq valid-count 0))
            (error 'json-ld-error 
              :message "Invalid JSON-LD syntax; an element containing \"@value\" may only have an \"@index\" property and at most one other property which can be \"@type\" or \"@language\"."
              :code "invalid value object"))
          
          ;; drop None @values
          (cond ((eq value +jsd-null+) (setf rval +jsd-null+))
                ;; if @language is present, @value must be a string
                ((and (jsd-haskey +language-key+ rval)
                      (not (stringp value)))
                 (error 'json-ld-error 
                   :message "Invalid JSON-LD syntax; only strings may be language-tagged"
                   :code "invalid language-tagged value"))
                ((expanded-type-invalid-p rval)
                 (error 'json-ld-error 
                   :message "Invalid JSON-LD syntax; an element containing \"@value\" and \"@type\" must have an absolute IRI for the value of \"@type\"."
                   :code "invalid typed value"))))))
    ;; convert @type to an array
    (when (not handled-p)
      (multiple-value-bind (type type-present-p) (jsd-get +type-key+ rval)
        (when (and type-present-p (not (listp type)))
          (setf handled-p t)
          (setf (jsd-get +type-key+ rval) (list type)))))
    
    ;; handle @set and @list
    (when (and (not handled-p)
               (or (jsd-haskey +set-key+ rval) (jsd-haskey +list-key+ rval)))
      (setf handled-p t)
      (when (and (> count 1)
                 (not (and (eq count 2) (jsd-haskey +index-key+ rval))))
        (error 'json-ld-error 
          :message "Invalid JSON-LD syntax; if an element has the property \"@set\" or \"@list\", then it can have at most one other property, which is \"@index\"."
          :code "invalid set or list object"))
      (multiple-value-bind (set set-present-p) (jsd-get +set-key+ rval)
        ;; optimize away @set
        (when set-present-p
          (setf rval set)
          (setf count (length rval)))))
    ;; drop objects with only @language
    (when (and (not handled-p)
               (eq count 1)
               (jsd-haskey +language-key+ rval))
      (setf rval +jsd-null+))
    
    ;; drop certain top-level objects that do not occur in lists
    (when (and (is-object rval) 
               (not (getf options :keep-free-floating-nodes))
               (not inside-list)
               (or (not active-property)
                   (equal expanded-active-property +graph-key+))
               ;; drop empty object or top-level @value/@list, or object with only @id
               (or (eq count 0)
                   (jsd-haskey +value-key+ rval)
                   (jsd-haskey +list-key+ rval)
                   (and (eq count 1) (jsd-haskey +id-key+ rval))))
      (setf rval +jsd-null+)))
  rval)

(defun do-expand-array (active-ctx active-property element options inside-list)
  (let ((lists-to-append nil)
        (container (get-context-value active-ctx active-property +container-key+)))
    (setf inside-list (or inside-list (equal container +list-key+)))
    (loop for e in element do
          (setf e (do-expand active-ctx active-property e options inside-list))
          (when (and inside-list
                     (or (listp e) (is-list e)))
            (error 'json-ld-error 
              :message "Invalid JSON-LD syntax; lists of lists are not permitted"
              :code "list of lists"))
          (when (not (eq e +jsd-null+))
            (setf lists-to-append (cons (if (listp e) e (list e)) lists-to-append))))
    (setf lists-to-append (nreverse lists-to-append))
    (let ((res (apply #'append lists-to-append)))
      res)))

(defun handle-reverse-property (value active-ctx options inside-list rval)
  ;; reverse must be an object
  (when (not (is-object value))
    (error 'json-ld-error 
      :message "Invalid JSON-LD syntax; \"@reverse\" value must be an object."
      :code "invalid @reverse value"))
  
  (let ((exp-value (do-expand active-ctx +reverse-key+ value options inside-list)))
    ;; properties double-reversed
    (jsd-when-get +reverse-key+ exp-value (reverse-value)
      (jsd-map #'(lambda (rproperty rvalue)
                   (node-add-value rval rproperty rvalue :property-is-array t))
               reverse-value))
    
    ;; merge in all reversed properties
    (let ((reverse-map (jsd-get +reverse-key+ rval)))
      (jsd-map
       #'(lambda (property items)
           (block mapper-func
             (when (equal property +reverse-key+)
               (return-from mapper-func))
             (when (not reverse-map)
               (setf reverse-map (jsd-make))
               (setf (jsd-get +reverse-key+ rval) reverse-map))
             (node-add-value reverse-map property nil :property-is-array t)
             (loop for item in items do
                   (when (or (is-value item) (is-list item))
                     (error 'json-ld-error 
                       :message "Invalid JSON-LD syntax; \"@reverse\" value must not be an @value or an @list."
                       :code "invalid reverse property value"))
                   (node-add-value reverse-map property item :property-is-array t))))
         exp-value))))

(defun merge-in-reverse-properties (rval expanded-value expanded-property)
  (let ((reverse-map (jsd-setdefault rval +reverse-key+ (jsd-make)))
        (expanded-value (arraify expanded-value)))
    (loop for item in expanded-value do
          (when (or (is-value item) (is-list item))
            (error 'json-ld-error 
              :message "Invalid JSON-LD syntax; \"@reverse\" value must not be an @value or an @list."
              :code "invalid reverse property value"))
          (node-add-value reverse-map expanded-property item :property-is-array t))))

(defun do-expand (active-ctx active-property element options inside-list)
  "Recursively expands an element using the given context. Any context in the element will be removed. 
  All context URLs must have been retrieved before calling this method.

  Converted from pyld's JsonLdProcessor._expand method."

  ;; nothing to expand
  (when (eq element +jsd-null+)
    (return-from do-expand element))
  
  ;; recursively expand array
  (when (listp element)
    (return-from do-expand (do-expand-array active-ctx active-property element options inside-list)))
  
  ;; handle scalars  
  (unless (is-object element)
    ;; drop free-floating scalars that are not in lists
    (when (and (not inside-list) (or (not active-property) 
                                     (equal (expand-iri active-ctx active-property :vocab t) +graph-key+)))
      (return-from do-expand +jsd-null+))
    ;; expand element according to value expansion rules
    (return-from do-expand (expand-value active-ctx active-property element)))
  
  ;; Recursively expand object. 
  ;; If element has a context, process it.
  (when (jsd-get +context-key+ element)
    (setf active-ctx (internal-process-context active-ctx (jsd-get +context-key+ element) options)))
  
  ;; expand the active property
  (let ((expanded-active-property (expand-iri active-ctx active-property :vocab t))
        (rval (jsd-make)))
    (jsd-sorted-map 
     #'(lambda (key value)
         (block mapper-func
           (when (equal key +context-key+)
             (return-from mapper-func))
           
           ;; expand key to IRI
           (let ((expanded-property (expand-iri active-ctx key :vocab t)))
             (when (or (eq expanded-property +jsd-null+)
                       (not (or (absolute-iri-p expanded-property)
                                (is-keyword expanded-property))))
               (return-from mapper-func))
             
             (validate-expanded-property expanded-property expanded-active-property value rval options)
             
             (when (equal expanded-property +reverse-key+)
               (handle-reverse-property value active-ctx options inside-list rval)
               (return-from mapper-func))
             
             (let* ((container (get-context-value active-ctx key +container-key+))
                    (expanded-value (get-expanded-value container active-ctx key value options active-property expanded-active-property expanded-property)))
               
               ;; drop None values if property is not @value (dropped below)
               (when (and (eq expanded-value +jsd-null+) (not (equal expanded-property +value-key+)))
                 (return-from mapper-func))
               
               ;; convert expanded value to @list if container specifies it
               (when (and (not (equal expanded-property +list-key+)) 
                          (not (is-list expanded-value))
                          (equal container +list-key+))
                 ;; ensure expanded value is an array
                 (setf expanded-value (jsd-make +list-key+ (arraify expanded-value))))
               
               ;; merge in reverse properties
               (let ((mapping (get-context-mapping active-ctx key)))
                 (when (and mapping (jsd-get "reverse" mapping))
                   (merge-in-reverse-properties rval expanded-value expanded-property)
                   (return-from mapper-func)))
             
             ;; add value for property, use an array exception for certain key words
               (node-add-value rval expanded-property expanded-value 
                               :property-is-array (expand-use-array-p expanded-property))))))
     element)
    (finalize-expansion rval options inside-list active-property expanded-active-property)))
       
(defparameter *default-document-loader* nil)

(defun get-document-loader ()
  "Get the default JSON-LD document loader."
  *default-document-loader*)

(defun set-document-loader (load-document)
  "Set the default JSON-LD document loader."
  (setf *default-document-loader* load-document))

(defun arraify (value)
  (if (or (not value) (listp value))
      value
    (list value)))

;;;; JSON-LD generation

(define-condition json-ld-error (error)
  ((message :initarg :message :initform nil :reader error-message)
   (code :initarg :code :initform nil :reader error-code)
   (cause :initarg :cause :initform nil :reader error-cause)))

(defgeneric json-ld-error-code (error)
  (:documentation "Return the code for the condition."))

(defmethod json-ld-error-code ((error json-ld-error))
  (let ((code (error-code error))
        (cause (error-cause error))
        (message (error-message error)))
    (cond
     (code code)
     (cause (json-ld-error-code cause))
     (message message)
     (t "UNKNOWN JSON-LD-ERROR CODE"))))

(defmethod json-ld-error-code ((error error))
  (format nil "~a" error))         

(defmethod print-object ((err json-ld-error) out)
  (with-slots (message) err
    (format out "JsonLdError(~s)" message)))

(defun dataset->jsonld (dataset options)
  "Return a JSON representation of a given dataset."
  (destructuring-bind (graph-map . node-references) (get-graph-map-for-dataset dataset options)
    (convert-linked-lists-to-arrays graph-map node-references)
    (let* ((graphs (make-json-list))
           (default-graph (jsd-get +default-graph-name+ graph-map)))
      (jsd-sorted-map 
       #'(lambda (subject node)
           (when (jsd-get subject graph-map)
             (let ((graph (make-json-list)))
               (jsd-sorted-map
                #'(lambda (key value)
                    (declare (ignore key))
                    (setf graph (add-graph-entry value graph)))
                (jsd-get subject graph-map))
               (setf (jsd-get +graph-key+ node) (finalize-json-list graph))))
           (setf graphs (add-graph-entry node graphs)))
       default-graph)
      (finalize-json-list graphs))))

(defun add-graph-entry (node graph)
  "Add given node to given graph if it's a full subject."
  (jsd-remove +usages-key+ node)
  ; only add full subjects to top-level
  (unless (is-subject-reference node)
    (push node graph))
  graph)

(defun is-subject (value)
  "Returns True if the given value is a subject with properties."  
  (and (is-object value)
       (not (jsd-haskey +value-key+ value))
       (not (jsd-haskey +set-key+ value))
       (not (jsd-haskey +list-key+ value))
       (or (> (jsd-count value) 1) 
           (not (jsd-haskey +id-key+ value)))))

(defun is-subject-reference (value)
  "Returns T if the given value is a subject reference."
  (and (is-object value) (= (jsd-count value) 1) (jsd-haskey +id-key+ value)))

(defun get-graph-map-for-dataset (dataset options)
  "Return a JSO of graphs for the given dataset."
  (let* ((default-graph (make-node-map))
         (graph-map (jsd-make))
         (node-references (jsd-make)))
    (setf (jsd-get +default-graph-name+ graph-map) default-graph)
    (loop for name being the hash-keys in dataset using (hash-value graph)
        do (let ((node-map (jsd-setdefault graph-map name (make-node-map))))
             (when (and (not (equal name +default-graph-name+)) (not (jsd-get name default-graph)))
               (setf (jsd-get name default-graph) (make-id-entry name)))
             (fill-node-map-from-graph node-map graph node-references options))
        finally (return (cons graph-map node-references)))))

(defun make-node-map ()
  "Create a JSON graph."
  (jsd-make))

(defun make-id-entry (name)
  "Create a JSON dict with @id key of given value."
  (jsd-make +id-key+ name))

(defun fill-node-map-from-graph (node-map graph node-references options)
  "Fill a JSON node map (dict) with data from a dataset graph."
  (loop for triple across graph do
    (block continue 
      (let* ((s (getf (getf triple :subject) :value))
             (p (getf (getf triple :predicate) :value))
             (o (getf triple :object))
             (o-value (getf o :value))
             (node (jsd-setdefault node-map s (make-id-entry s)))
             (object-is-id (object-is-id-p o)))
        (when object-is-id (jsd-setdefault node-map o-value (make-id-entry o-value)))
        (when (and (equal p +rdf-type+) (not (getf options :use-rdf-type)) object-is-id)
          (node-add-value node +type-key+ o-value :property-is-array t)
          (return-from continue))
        
        (let ((value (rdf-to-object o (getf options :use-native-types))))
          (node-add-value node p value :property-is-array t)
          ;; object may be an RDF list/partial list node but we can't know easily
          ;; until all triples are read
          (when object-is-id
            (node-add-value node-references o-value (jsd-get +id-key+ node) :property-is-array t)
            (vector-push-extend 
             (make-usage-entry node p value) 
             (jsd-setdefault (jsd-get o-value node-map) +usages-key+ (make-dynamic-array)))))))))

(defun make-usage-entry (node property value)
  "Return a plist describing a usage."
  (list :node node :property property :value value))

(defun object-is-id-p (object)
  "Return T iff object is of type IRI or blank node."
  (let ((o-type (getf object :type)))
    (or (equal o-type +iri-type+) (equal o-type +blank-node-type+))))

(defun rdf-to-object (o use-native-types)
  "Convert an RDF triple object to a JSON-LD object."
  ;; Convert IRI/BlankNode object to JSON-LD
  (when (object-is-id-p o)
    (return-from rdf-to-object (make-id-entry (getf o :value))))
  
  ;; Convert literal object to JSON-LD
  (let ((rval (jsd-make))
        (o-value (getf o :value)))
    (setf (jsd-get +value-key+ rval) o-value)
    
    (let ((language (getf o :language)))
      (if language
        ;; Add language
        (setf (jsd-get +language-key+ rval) language)
        ;; Add datatype
        (let ((type (getf o :datatype)))
          (cond
           (use-native-types
            (cond
             ((equal type +xsd-boolean+)
              (setf (jsd-get +value-key+ rval) (parse-boolean o-value)))
             ((numberp o-value)
              (setf (jsd-get +value-key+ rval) (numeric-string->numeric o-value type))))
            ;; Do not add native type
            (when (not (native-type-p type))
              (setf (jsd-get +type-key+ rval) type)))
           ((not (equal type +xsd-string+))
            (setf (jsd-get +type-key+ rval) type))))))
    rval))

(alexandria:define-constant +native-types+ (list +xsd-boolean+ +xsd-integer+ +xsd-double+ +xsd-string+)
  :test #'equal)

(defun native-type-p (type)
  "Return T iff given type is an XSD native type."
  (find type +native-types+ :test #'string=))

(defun numeric-string->numeric (value type)
  "Convert a string representing an integer or a double to a Lisp number."
  (cond 
   ((equal type +xsd-integer+)
      (if (digit-string-p value) (parse-integer value) value))
   ((equal type +xsd-double+)
      (parse-float value))
   (t value)))

(defun parse-float (string)
  "Convert a string representing a float to a Lisp float."
  (with-input-from-string (in string)
    (read in)))

(defun parse-boolean (string)
  "Convert a string representing a boolean to json-ld boolean keyword."
  (cond ((equal string "true") +jsd-true+)
        ((equal string "false") +jsd-false+)
        (t string)))

(defun digit-string-p (string)
  "Return T iff given string is not empty and consists only of digits."
  (and
   (> (length string) 1)
   (every #'digit-char-p string)))

(defun merge-removing-duplicates (old-values new-values)
  "Return a new list containing all values from old-values plus values in new-values
  with duplicates removed."
  (let ((result (reverse old-values)))
    (loop for value in new-values do
      (when (not (value-exists-p result value))
        (push value result)))
    (nreverse result)))
                  
(defun value-exists-p (old-values value)
  "Return T iff value exists in old-values."
  (find value old-values :test #'compare-values))                  

(defun node-add-value (subject property values &key property-is-array (allow-duplicate t))
  "Add values to the subject.
  Subject is a JSO, property is a string, values can be an array, a string, or a JSO.

  Equivalent to pyld's JsonLdProcessor.add_value method."
  (let ((values (to-list values)))
    (multiple-value-bind (old-values had-values) (jsd-get property subject)
      (let* ((was-list (and had-values (listp old-values)))
             (old-values-list (if had-values (to-list old-values) '()))
             (new-values (if allow-duplicate 
                           (append old-values-list values)
                           (merge-removing-duplicates old-values-list values)))
             (new-elem (if (and (= (length new-values) 1) (not property-is-array) (not was-list))
                         (first new-values)
                         new-values)))
        (setf (jsd-get property subject) new-elem)))))

(defun node-has-value-p (subject property value)
  "Determines if the given value is a property of the given subject.

  Equivalent to pyld's JsonLdProcessor.has_value method."
  (if (node-has-property-p subject property)
      (let* ((val (jsd-get property subject))
             (is-list (is-list val)))
        (cond 
         ((or (listp val) is-list)
          (when is-list
            (setf val (jsd-get +list-key+ val)))
          (some #'(lambda (v) (compare-values value v)) val))
         
         ((not (listp value))
          (compare-values value val))))
    nil))

(defun node-has-property-p (subject property)
  "Return T iff subject has at least one value for the given property.

  Equivalent to pyld's JsonLdProcessor.has_property method."
  (multiple-value-bind (value has-value) (jsd-get property subject)
    (and has-value
         (or (not (listp value)) (plusp (length value))))))

(defun compare-values (v1 v2)
  (or 
   ; equal primitives
   (and (not (is-object v1)) (not (is-object v2)) (equal v1 v2))
   
   ; equal values
   (and (is-value v1) (is-value v2)
        (equal (jsd-get +value-key+ v1) (jsd-get +value-key+ v2))
        (equal (jsd-get +type-key+ v1) (jsd-get +type-key+ v2))
        (equal (jsd-get +language-key+ v1) (jsd-get +language-key+ v2))
        (equal (jsd-get +index-key+ v1) (jsd-get +index-key+ v2)))
   
   ; equal @ids
   (and (is-object v1) (is-object v2) (jsd-get +id-key+ v1) (jsd-get +id-key+ v2)
        (equal (jsd-get +id-key+ v1) (jsd-get +id-key+ v2)))))

(defun is-list (value)
  "Return T iff given value is a @list."
  (and (is-object value) (jsd-haskey +list-key+ value)))

(defun is-value (value)
  "Return T iff given value is an Object with @value key."
  (and (is-object value) (jsd-get +value-key+ value)))

(defun is-bnode (value)
  "Returns T iff the given value is a blank node."
  (and (is-object value)
       (let ((id (jsd-get +id-key+ value)))
         (if id
             (blank-name-p id)
             (or (zerop (jsd-count value))
                 (not (or (jsd-haskey +value-key+ value)
                          (jsd-haskey +set-key+ value)
                          (jsd-haskey +list-key+ value))))))))

(defun is-keyword (value)
  "Returns whether or not the given value is a keyword."
  (and (stringp value) (find value +jsonld-keywords+ :test #'string=)))

(defun blank-name-p (name)
  "Return T iff name describes a blank node."
  (starts-with-p name "_:"))

(defun convert-linked-lists-to-arrays (graph-map node-references)
  ;; convert linked lists to @list arrays
  (jsd-map #'(lambda (graph-name graph-object)
         (declare (ignore graph-name))
         (block continue
           (let ((rdf-nil (jsd-get +rdf-nil+ graph-object)))
             (unless rdf-nil 
               ; no @lists to be converted, continue
               (return-from continue))
             
             ; iterate backwargs through each RDF list
             (loop for usage across (jsd-get +usages-key+ rdf-nil)
                 do (block continue-usage
                      ;; ensure node is a well-formed list node; it must:
                      ;; 1. Be referenced only once and used only once in a list.
                      ;; 2. Have an array for rdf:first that has 1 item.
                      ;; 3. Have an array for rdf:rest that has 1 item
                      ;; 4. Have no keys other than: @id, usages, rdf:first, rdf:rest
                      ;;   and, optionally, @type where the value is rdf:List.
                       (destructuring-bind (&key node property ((:value head))) usage
                         (let ((list '())
                               (list-nodes '())
                               (node-key-count (jsd-count node)))
                           (loop while (and
                                        (equal property +rdf-rest+)
                                        (= (length (jsd-get (jsd-get +id-key+ node) node-references)) 1)
                                        (plusp (length (jsd-get +usages-key+ node)))
                                        (one-element-list-p (jsd-get +rdf-first+ node))
                                        (one-element-list-p (jsd-get +rdf-rest+ node))
                                        (or (= node-key-count 4)
                                            (and (= node-key-count 5)
                                                 (one-element-list-p (jsd-get +type-key+ node))
                                                 (equal (elt (jsd-get +type-key+ node) 0) +rdf-list+)))) do 
                                 (push (elt (jsd-get +rdf-first+ node) 0) list)
                                 (push (jsd-get +id-key+ node) list-nodes)
                                 (destructuring-bind (&key ((:node new-node)) ((:property new-property)) 
                                                            ((:value new-value))) 
                                     (elt (jsd-get +usages-key+ node) 0)
                                   (setf node new-node
                                     property new-property
                                     head new-value
                                     node-key-count (jsd-count node)))
                                 (when (not (blank-name-p (jsd-get +id-key+ node)))
                                   (return)))
                           
                           ; the list is nested in another list
                           (when (equal property +rdf-first+)
                             ; empty list
                             (when (equal (jsd-get +id-key+ node) +rdf-nil+)
                               ; can't convert rdf:nil to a @list object because it would result in
                               ; a list of lists which isn't supported
                               (return-from continue-usage))
                             ; preserve list head
                             (setf head (elt (jsd-get +rdf-rest+ (jsd-get (jsd-get +id-key+ head) graph-object)) 0))
                             (pop list)
                             (pop list-nodes))
                           
                           ; transform list into @list object
                           (jsd-remove +id-key+ head)
                           (setf (jsd-get +list-key+ head) list)
                           (loop for node-id in (reverse list-nodes) do 
                                 (jsd-remove node-id graph-object)))))))))
          graph-map))

(defun one-element-list-p (value)
  "Return T iff value is a one-element list."
  (and (listp value) (= (length value) 1)))


;;;; JSON API

(defun make-json-list ()
  '())

(defun finalize-json-list (list)
  (reverse list))
                                                                
;;;; Initial setup.

(register-rdf-parser "application/nquads" #'parse-nquads)

;;;; Testing

(defun dump-dataset (dataset)
  "Print representation of a given dataset to standard output."
  (format t "DATASET~%")
  (loop for name being the hash-keys in dataset using (hash-value graph)
      do (format t " - GRAPH ~a, entry count ~a:~%" name (length graph))
        (dump-graph graph)))

(defun dump-graph (graph)
  "Print representation of a given dataset graph to standard output."
  (loop for triple across graph
        do (format t "   ~a~%" triple)))

;;;; Public API

(defun from-rdf (input &optional (options nil))
  "Convert RDF input (in the format given in options) into  JSD format.

  Converted from pyld's 'from_rdf' global function."
  (let ((processor (make-instance 'json-ld-processor)))
    (processor-from-rdf processor input options)))

(defun to-rdf (input &optional (options nil))
  "Output the RDF dataset found in the given JSON-LD object (in JSD format).

  Converted from pyld's 'to_rdf' global function."
  (plist-setdefault options :base (if (stringp input) input ""))
  (plist-setdefault options :document-loader *default-document-loader*)
  
  (let ((expanded (expand input options))
        (namer (make-namer))
        (node-map (make-node-map)))
    (setf (jsd-get +default-graph-name+ node-map) (jsd-make))
    (create-node-map expanded node-map +default-graph-name+ namer)
        
    (let* ((dataset (node-map->dataset node-map namer options)))
      (format-dataset dataset options))))

(defun expand (input &optional (options nil))
  "Performs JSON-LD expansion.

  Converted from pyld's 'expand' global function."
  ;;; set default options
  (plist-setdefault options :document-loader *default-document-loader*)
  
  (multiple-value-bind (context-url document-url document)
      ;; if input is a string, attempt to dereference remote document
      (if (stringp input)
          (funcall (getf options :document-loader) input)
        (values +jsd-null+ nil input))

    (handler-case
      (progn
        (when (not document)
          (error 'json-ld-error :message "No remote document found at the given URL"))
        (when (stringp document)
          (setf document (jsd-read document))))
      (error (cause)
        (error 'json-ld-error
          :message "Could not retrieve a JSON-LD document from the URL."
          :code "loading document failed"
          :cause cause)))
    
    ;; Set default base:
    (plist-setdefault options :base (or document-url ""))
    
    ;;; build meta-object and retrieve all @context urls
    (let ((context-input (jsd-make "document" (deepcopy document)
                              "remoteContext" (jsd-make +context-key+ context-url))))

      (when (getf options :expand-context)
        ;; expand-context, if present, is always of type JSD
        (let ((expand-context (deepcopy (getf options :expand-context))))
          (setf (jsd-get "expandContext" context-input)
                (if (and (is-object expand-context) (jsd-haskey +context-key+ expand-context))
                    expand-context
                    (jsd-make +context-key+ expand-context)))))
      
      ;; find all URLs in the given input
      (retrieve-context-urls context-input nil (getf options :document-loader) :base (getf options :base))
    
      (let* ((active-ctx (get-initial-context options))
             (document (jsd-get "document" context-input))
             (remote-context (multi-jsd-get context-input "remoteContext" +context-key+)))
        
        ;; Process optional expandContext
        (jsd-when-get "expandContext" context-input (exp-context)
          (setf active-ctx (process-context active-ctx (jsd-get +context-key+ exp-context) options)))
        
        ;; Process remote context from HTTP link header
        (when (not (eq remote-context +jsd-null+))
          (setf active-ctx (process-context active-ctx remote-context options)))
        
        ;; Do expansion
        (let ((expanded (do-expand active-ctx nil document options nil)))
          ;; optimize away @graph with no other properties
          (cond
           ((and (is-object expanded) (jsd-haskey +graph-key+ expanded) (eq (jsd-count expanded) 1))
            (setf expanded (jsd-get +graph-key+ expanded)))
           
           ((eq expanded +jsd-null+)
            (setf expanded nil)))
          
        ;; normalize to an array
        (arraify expanded))))))





;;;; Converting JSON-LD to dataset

(defun node-map->dataset (node-map namer options)
  "Convert node map to a dataset."
  (let ((dataset (make-string-hashtable)))
    (jsd-sorted-map #'(lambda (graph-name graph)
                       (when (or (equal graph-name +default-graph-name+)
                                 (absolute-iri-p graph-name))
                         (setf (gethash graph-name dataset) (graph->rdf graph namer options))))
                   node-map)
    dataset))

(defun format-dataset (dataset options)
  "Convert dataset to output format, or return dataset if no format specified."
  (let ((format (getf options :format)))
    (if format
      (if (equal format "application/nquads")
        (dataset->nquads dataset)
        (error 'json-ld-error :message (format nil "Unknown output format: ~s" format)))
      dataset)))

(defun get-format-parser (processor format)
  "Return a function that can parse given format."
  (let ((parser (or (string-assoc format (processor-rdf-parsers processor))
                    (string-assoc format *rdf-parsers*))))
    (cdr parser)))

(defun processor-register-rdf-parser (processor content-type parser)
  "Add a new RDF parser to the processor object."
  (setf (processor-rdf-parsers processor) 
        (string-alist-add (processor-rdf-parsers processor) content-type parser)))

(defun create-node-map (input graphs graph namer &key name list)
  "Recursively flatten the subjects in the given JSON-LD expanded input into a node map.

  @type input: `JSO'
  @type graphs: `JSO'
  @type graph: `string'
  @type namer: `unique-namer'
  @type name: when not nil, a `string'
  @type list: when not nil, a resizable vector of JSD objects.

  Converted from pyld's JsonLdProcessor._create_node_map method."
  ;; Recurse through array
  (when (listp input)
    (loop for e in input do
      (create-node-map e graphs graph namer :list list))
    (return-from create-node-map))
  
  ;; Add non-object to list
  (when (not (is-object input))
    (when list
      (vector-push-extend input list))
    (return-from create-node-map))
  
  ;; Add values to list
  (when (is-value input)
    (let ((type (jsd-get +type-key+ input)))
      (when (and type (blank-name-p type))
        ; Rename @type blank node
        (setf (jsd-get +type-key+ input) (namer-get-name namer type)))
      (when list
        (vector-push-extend input list))
      (return-from create-node-map)))
  
  ;; At this point, input must be a subject.
  
  ;; Spec requires @type to be named first, so assign names early.
  (let ((types (jsd-get +type-key+ input)))
    (when types
      (loop for type in types
          when (blank-name-p type) do
            ; Ignore result, it's remembered in namer's cache.
            (namer-get-name namer type))))
  
  ;; Get name for subject.
  (when (not name)
    (setf name (jsd-get +id-key+ input))
    (when (is-bnode input)
      (setf name (namer-get-name namer name))))
  
  ;; Add subject reference to list.
  (when list
    (vector-push-extend (make-id-entry name) list))
  
  ;; Create new subject or merge into existing one.
  
  (let ((subject (jsd-setdefault (jsd-setdefault graphs graph (jsd-make)) name (make-id-entry name))))
    (jsd-sorted-map #'(lambda (property objects)
                       (add-subject-property subject property objects input graphs graph namer name)) input)
    input
    ))

(defun make-subject (name)
  (let ((result (make-string-hashtable)))
    (setf (gethash +id-key+ result) name)
    result))

(defun add-reverse-property (input graphs graph namer name)
  "Add the objects as the @reverse property of subject."
  (let ((referenced-node (make-id-entry name))
        (reverse-map (jsd-get +reverse-key+ input)))
    (flet ((handle-property (reverse-property items)
       (loop for item in items do
         (let ((item-name (jsd-get +id-key+ item)))
           (when (is-bnode item)
             (setf item-name (namer-get-name namer item-name)))
           (create-node-map item graphs graph namer :name item-name)
           (node-add-value (multi-jsd-get graphs graph item-name) reverse-property referenced-node
                           :property-is-array t :allow-duplicate nil)))))
      (jsd-map #'handle-property reverse-map))))

(defun add-property-non-type-keyword (subject property input)
  (when (and (equal property +index-key+) (jsd-haskey +index-key+ subject))
    (error 'json-ld-error 
      :message "Invalid JSON-LD syntax; conflicting @index property detected."
      :code "conflicting indexes"))
  (setf (jsd-get property subject) (jsd-get property input)))

(defun add-subject-property (subject property objects input graphs graph namer name)
  "Add the property with objects to a graph subject.

  @type graphs `JSO'

  Converted from the last part of pyld's JsonLdProcessor._create_node_map function."
  ;; Skip @id
  (when (equal property +id-key+)
    (return-from add-subject-property))
  
  ;; Handle reverse properties
  (when (equal property +reverse-key+)
    (add-reverse-property input graphs graph namer name)
    (return-from add-subject-property))
  
  ;; Recurse into graph
  (when (equal property +graph-key+)
    ;; Add graph subjects map entry
    (jsd-setdefault graphs name (jsd-make))
    (let ((g (if (equal graph +merged-key+) graph name)))
      (create-node-map objects graphs g namer))
    (return-from add-subject-property))
  
  ;; Copy non-@type keywords
  (when (and (not (equal property +type-key+)) (is-keyword property)) 
    (add-property-non-type-keyword subject property input)
    (return-from add-subject-property))
  
  ;; if property is a bnode, assign it a new id
  (when (blank-name-p property)
    (setf property (namer-get-name namer property)))
  
  ;; ensure property is added for empty arrays
  (when (eq (length objects) 0)
    (node-add-value subject property '() :property-is-array t)
    (return-from add-subject-property))
  
  (loop for obj in objects do
        (when (and (equal property +type-key+) (blank-name-p obj))
          ;; Rename @type blank nodes
          (setf obj (namer-get-name namer obj)))
        
        (cond
         ((or (is-subject obj) (is-subject-reference obj))
          ;; Handle embedded subject or subject reference.
          (let ((id (jsd-get +id-key+ obj)))
            (when (is-bnode obj)
              ;; Rename blank node @id
              (setf id (namer-get-name namer id)))
            ;; Add reference and recurse:
            (node-add-value subject property (make-id-entry id) 
                            :property-is-array t :allow-duplicate nil)
            (create-node-map obj graphs graph namer :name id)))
         
         ((is-list obj)
          ;; Handle @list
          (let ((olist (make-dynamic-array)))
            (create-node-map (jsd-get +list-key+ obj) graphs graph namer :name name :list olist)
            (let ((obj (jsd-make +list-key+ (to-list olist))))
              (node-add-value subject property obj :property-is-array t :allow-duplicate nil))))
         
         (t
          (create-node-map obj graphs graph namer :name name)
          (node-add-value subject property obj :property-is-array t :allow-duplicate nil)))
        ))

(defun graph->rdf (graph namer options)
  "Creates an array of RDF triples for the given graph.

  Converted from pyld's JsonLdProcessor._graph_to_rdf method."
  (let ((rval (make-dynamic-array)))
    (jsd-sorted-map #'(lambda (id node)
                       (jsd-sorted-map #'(lambda (property items)
                                          (collect-rdfs-from-items rval id property items namer options)) 
                                      node))
                   graph)
    rval))

(defun collect-rdfs-from-items (rval id property items namer options)
  "Type of rval is dynamic array."
  (when (equal property +type-key+)
    (setf property +rdf-type+))
  (when (is-keyword property)
    (return-from collect-rdfs-from-items))
  
  (loop for item in items
      ;; Skip relative IRI subjects and predicates:
      when (and (absolute-iri-p id) (absolute-iri-p property)) do
    (collect-rdf-from-item rval id property item namer options)))

(defun collect-rdf-from-item (rval id property item namer options)
  (let ((subject (make-triple-elem (if (blank-name-p id) 
                                       +blank-node-type+ 
                                     +iri-type+) id))
        
        (predicate (make-triple-elem (if (blank-name-p property)
                                         (progn
                                           ;; Skip bnode predicates unless producing generalized RDF:
                                           (unless (getf options :produce-generalized-rdf)
                                             (return-from collect-rdf-from-item))
                                           +blank-node-type+)
                                         +iri-type+)
                                     property)))
    
    (if (is-list item)
        ;; Convert @list to triples
        (list-to-rdf (jsd-get +list-key+ item) namer subject predicate rval)
        ;; else convert value or node object to triple
        (let ((object (object-to-rdf item)))
          ;; Skip nil objects (they are relative IRIs)
          (when object
            (vector-push-extend (make-triple subject predicate object) rval))))))

(alexandria:define-constant +list-first+ (make-triple-elem +iri-type+ +rdf-first+) :test #'equal)
(alexandria:define-constant +list-rest+ (make-triple-elem +iri-type+ +rdf-rest+) :test #'equal)
(alexandria:define-constant +list-nil+ (make-triple-elem +iri-type+ +rdf-nil+) :test #'equal)

(defun list-to-rdf (list namer subject predicate triples)
  "Convert a @list value into a linked list of blank node RDF triples (and RDF collection).

  Converted from pyld's JsonLdProcessor._list_to_rdf method."
  (loop for item in list do
    (let ((blank-node (make-triple-elem +blank-node-type+ (namer-get-name namer))))
      (vector-push-extend (make-triple subject predicate blank-node) triples)
      
      (setf subject blank-node
            predicate +list-first+)
      (let ((object (object-to-rdf item)))
        ;; Skip nil objects (they are relative IRIs)
        (when object
          (vector-push-extend (make-triple subject predicate object) triples)))
      (setf predicate +list-rest+)))
  
  (vector-push-extend (make-triple subject predicate +list-nil+) triples))

(defun object-to-rdf (item)
  "Converts a JSON-LD value object to an RDF literal or a JSON-LD string 
  or node object to and RDF resource.

  Converted from pyld's JsonLdProcessor._object_to_rdf method."
  (let ((object (if (is-value item)
                    (value-object-to-rdf item)
                    (string-or-node-object-to-rdf item))))
    ;; Skip relative IRIs
    (if (and (equal (getf object :type) +iri-type+) (not (absolute-iri-p (getf object :value))))
        nil
        object)))

(defun value-object-to-rdf (item)
  (let ((value (jsd-get +value-key+ item))
        (datatype (jsd-get +type-key+ item))
        (object (make-triple-elem "literal" nil)))
    ;; Convert to XSD datatypes as appropriate
    (cond
     
     ((jsd-bool-p value)
      (setf (getf object :value) (if (eq value +jsd-true+)
                                     "true"
                                     "false"))
      (setf (getf object :datatype) (or datatype +xsd-boolean+)))
     
     ((or (floatp value) (equal datatype +xsd-double+))
      ;; Canonical double representation
      (setf (getf object :value) (canonical-double-representation value))
      (setf (getf object :datatype) (or datatype +xsd-double+)))
     
     ((integerp value)
      (setf (getf object :value) (format nil "~a" value))
      (setf (getf object :datatype) (or datatype +xsd-integer+)))
     
     ((jsd-haskey +language-key+ item)
      (setf (getf object :value) value)
      (setf (getf object :datatype) (or datatype +rdf-langstring+))
      (setf (getf object :language) (jsd-get +language-key+ item)))
     
     (t
      (setf (getf object :value) value)
      (setf (getf object :datatype) (or datatype +xsd-string+))))
    
    object))

(defun canonical-double-representation (value)
  (let ((formatted (format nil "~1,14e" value)))
    (cl-ppcre:regex-replace "\([0-9]\)0*[DEFLSdefls]\\+0*\([0-9]\)" formatted "\\1E\\2")))

(defun string-or-node-object-to-rdf (item)
  "Convert string/node object to RDF"
  (let ((id (if (is-object item) (jsd-get +id-key+ item) item)))
    (make-triple-elem (if (blank-name-p id) +blank-node-type+ +iri-type+) id)))

(defun dataset->nquads (dataset)
  "Convert an RDF dataset to N-Quads.

  Dataset is a hashmap with string keys and dynamic array values.
  Elements of dynamic array are plists representing triples.

  Converted from pyld's JsonLdProcessor.to_nquads method."
  (let ((quads '()))
    (loop for graph-name being the hash-keys of dataset using (hash-value triples) do
          (when (equal graph-name +default-graph-name+)
            (setf graph-name nil))
          (loop for triple across triples do
            (push (triple->nquad triple graph-name) quads)))
    (setf quads (sort quads #'string<))
    (format nil "~{~a~}" quads)))

(defun triple->nquad (triple graph-name &key bnode)
  "Convert an RDF triple and graph name to an N-Quad string (a single quad).

  Converted from pyld's JsonLdProcessor.to_nquad method."
  (destructuring-bind (&key subject predicate object &allow-other-keys) triple
    (with-output-to-string (quad)
      (write-nquad-subject subject bnode quad)
      (write-string " " quad)
      (write-nquad-predicate predicate bnode quad)
      (write-string " " quad)
      (write-nquad-object object bnode quad)
      (when graph-name
        (write-string " " quad)
        (write-nquad-graph graph-name bnode quad))
      (format quad " .~%"))))

(defun write-nquad-subject (subject bnode quad)
  (cond
   ((equal (getf subject :type) +iri-type+)
    (format quad "<~a>" (getf subject :value)))
   
   (bnode
    (write-string (if (equal (getf subject :value) bnode) "_:a" "_:z") quad))
   
   (t
    (write-string (getf subject :value) quad))))

(defun write-nquad-predicate (predicate bnode quad)
  (cond
   ((equal (getf predicate :type) +iri-type+)
    (format quad "<~a>" (getf predicate :value)))
   
   ;; FIXME: TBD what to do with bnode predicates during normalization
   (bnode
    (write-string "_:p" quad))
   
   (t
    (write-string (getf predicate :value) quad))))

(defun write-nquad-object (object bnode quad)
  (cond
   ((equal (getf object :type) +iri-type+)
    (format quad "<~a>" (getf object :value)))
   
   ((equal (getf object :type) +blank-node-type+)
    (if bnode
        (write-string (if (equal (getf object :value) bnode) "_:a" "_:z") quad)
        (write-string (getf object :value) quad)))
   
   (t
    (let ((escaped (replace-multi (getf object :value) +escape-replacements+)))
      (format quad "\"~a\"" escaped))
    (cond
     ((equal (getf object :datatype) +rdf-langstring+)
      (when (getf object :language)
        (format quad "@~a" (getf object :language))))
     ((not (equal (getf object :datatype) +xsd-string+))
      (format quad "^^<~a>" (getf object :datatype)))))))

(defun write-nquad-graph (graph-name bnode quad)
  (cond
   ((not (blank-name-p graph-name))
    (format quad "<~a>" graph-name))
   
   (bnode
    (write-string "_:g" quad))
   
   (t
    (write-string graph-name quad))))

(defun load-document (url)
    (handler-bind 
        ((error #'(lambda (e) 
                    (unless (typep e 'json-ld-error)
                      (error 'json-ld-error
                        :message "Could not retrieve a JSON-LD document from the URL."
                        :code "loading document failed"
                        :cause e)))))
      (try-load-document url)))

(set-document-loader #'load-document)

(defun try-load-document (url)
  "Retrieve JSON-LD at the given URL."
  (multiple-value-bind (body status-code headers uri) (drakma:http-request url)
    (unless (= status-code 200)
      (error "Bad status when retrieving url ~a: ~a" url status-code))
    (let ((document (if (stringp body)
                        body
                        (map 'string #'code-char body)))
          (document-url (render-uri uri nil))
          (context-url (get-context-url headers)))
      (values context-url document-url document))))

(defun get-context-url (headers)
  "Extract context url, if present, from HTTP request headers."
  (let ((link-header (cdr (assoc :link headers)))
        (content-type (cdr (assoc :content-type headers))))
    (if (and link-header (not (equal content-type "application/ld+json")))
        (get-context-url-from-link-header link-header)
        +jsd-null+)))

(alexandria:define-constant +link-header-rel+ "http://www.w3.org/ns/json-ld#context"
  :test #'string= :documentation "JSON-LD link header rel")

(defun get-context-url-from-link-header (link-header)
  (let* ((parsed (parse-link-header link-header))
         (rels (gethash +link-header-rel+ parsed)))
    (when (> (length rels) 1)
      (error 'json-ld-error 
        :message "URL could not be dereferenced, it has more then one associated HTTP Link Header."
        :code "multiple context link headers"))
    (or (and rels (gethash "target" (elt rels 0)))
        +jsd-null+)))

(defun parse-link-header (header)
  "Parses a link header. The results will be key'd by the value of 'rel'."
  (let ((rval (make-string-hashtable))
        (entries (cl-ppcre:all-matches-as-strings "(?:<[^>]*?>|\"[^\"]*?\"|[^,])+" header)))
    (loop for entry in entries do
      (parse-link-header-entry entry rval))
    rval))

(defun parse-link-header-entry (entry rval)
  "Parse a single link header entry and save result in rval."
  (cl-ppcre:register-groups-bind (target params) ("\\s*<([^>]*?)>\\s*(?:;\\s*(.*))?" entry)
    (let ((result (make-string-hashtable)))
      (setf (gethash "target" result) target)
      (cl-ppcre:do-register-groups (key value-one value-two)
                                   ("(.*?)=(?:(?:\"([^\"]*?)\")|([^\"]*?))\\s*(?:(?:;\\s*)|$)" params)
        (setf (gethash key result) (or value-one value-two)))
      (let* ((rel (gethash "rel" result ""))
             (rval-rel (hashtable-setdefault rval rel (make-dynamic-array))))
        (vector-push-extend result rval-rel)))))

(defconstant +max-context-urls+ 10)

(defun retrieve-context-urls (input cycles load-document &key (base ""))
  "Retrieves external @context URLs using the given document loader. Each
  instance of @context in the input that refers to a URL will be
  replaced with the JSON @context found at that URL.

  Converted from pyld's JsonLdProcessor._retrieve_context_urls method."
  
  (when (> (length cycles) +max-context-urls+)
    (error 'json-ld-error 
      :message "Maximum number of @context URLs exceeded."
      :code "loading remote context failed"))
  
  ;; For tracking URLs to retrieve
  (let ((urls (make-string-hashtable)))
    
    ;; Find all URLs in the given input
    (find-context-urls input urls nil base)
    
    ;; Queue all unretrieved URLs
    (let ((queue (make-unretrieved-url-queue urls)))
      ;; Retrieve URLs in queue
      (loop for url in queue do
        (retrieve-queued-context-url urls url cycles load-document)))
    
    ;; Replace all URLs in the input
    (find-context-urls input urls t base)))

(defun find-context-urls (input urls replace base)
  "Find all @context URLs in the given JSON-LD input."
  (cond
   ((listp input)
    (loop for e in input do
      (find-context-urls e urls replace base)))
   
   ((is-object input)
    (jsd-map #'(lambda (k v)
                (find-context-urls-in-object-entry k v input urls replace base))
            input))))

(defun find-context-urls-in-object-entry (k v input urls replace base)
  (unless (equal k +context-key+)
    (find-context-urls v urls replace base)
    (return-from find-context-urls-in-object-entry))
  
  (cond
   ((listp v)
    ;; array @context
    (setf (jsd-get k input)
      (find-context-urls-in-list-context v urls replace base)))
   ((stringp v)
    ;; string @context
    (find-context-urls-in-string-context k v input urls replace base))))

(defun find-context-urls-in-string-context (k v input urls replace base)
  (let ((v (prepend-base base v)))
    (cond
     (replace
      ;; Replace w/@context if requested
      (setf (jsd-get k input) (gethash v urls)))
     
     ;; @context URL found
     ((not (gethash v urls))
      (setf (gethash v urls) +jsd-false+)))))

(defun find-context-urls-in-list-context (v urls replace base)
  "Return a new list of contexts."
  (flet
      ((process-url (url)
         (if (stringp url)
             (progn
               (let ((url (prepend-base base url)))
                 (cond
                  (replace
                   (let ((ctx (gethash url urls)))
                     (if (listp ctx)
                         ;; Add flattened context
                         (copy-list ctx)
                         (list ctx))))
                  
                  ((not (gethash url urls))
                   (setf (gethash url urls) +jsd-false+)
                   (list url)))))
             ;;else
             (list url))))
    (mapcan #'process-url v)))
  

(defun make-unretrieved-url-queue (urls)
  (let ((queue '()))
    (loop for url being the hash-keys of urls using (hash-value ctx)
        when (eq ctx +jsd-false+) do
      (let* ((pieces (parse-uri url))
             (scheme (uri-scheme pieces))
             (netloc (uri-netloc pieces)))
        (unless (and scheme
                     netloc
                     (or (equal scheme :http) (equal scheme :https))
                     (valid-netloc-p netloc))
          (error 'json-ld-error
            :message "Malformed or unsupported URL."
            :code "loading remote context failed"))
        (push url queue)))
    (nreverse queue)))

(defun valid-netloc-p (netloc)
  (not (cl-ppcre:scan "[^a-zA-Z0-9-.:]" netloc)))

(defun retrieve-queued-context-url (urls url cycles load-document)
  ;; Check for context URL cycle
  (when (member url cycles :test #'string=)
    (error 'json-ld-error
      :message "Cyclical @context URLs detected."
      :code "recursive context inclusion"))
  (setf cycles (pushnew url cycles :test #'string=))
  
  ;; Retrieve URL
  (multiple-value-bind (context-url document-url ctx) (load-document url)
    (declare (ignore document-url))
    
    ;; Parse string context as JSON
    (when (stringp ctx)
      (setf ctx (jsd-read ctx)))
    
    ;; Ensure ctx is an object
    (unless (is-object ctx)
      (error 'json-ld-error 
        :message "Dereferencing a URL did not result in a valid JSON-LD object."
        :code "invalid remote context"))
    
    ;; Use empty context if no @context key is present
    (setf ctx (jsd-make +context-key+ (if (jsd-haskey +context-key+ ctx) 
                                          (jsd-get +context-key+ ctx) 
                                        (jsd-make))))
    
    ;; Append context URL to context if given
    (unless (eq context-url +jsd-null+)
      (when (null context-url)
        (error "Context url is supposed to be +jsd-null+ if empty."))
      (let ((ctx-list (to-list (jsd-get +context-key+ ctx))))
        (setf (jsd-get +context-key+ ctx) (append ctx-list (list context-url)))))

    ;; Recurse
    (retrieve-context-urls ctx cycles load-document :base url)
    (setf (gethash url urls) (jsd-get +context-key+ ctx))))
    
    
    
    
