;;;;  HTML5 parser for Common Lisp
;;;;
;;;;  Copyright (C) 2012 Thomas Bakketun <thomas.bakketun@copyleft.no>
;;;;  Copyright (C) 2012 Asgeir Bjørlykke <asgeir@copyleft.no>
;;;;  Copyright (C) 2012 Mathias Hellevang
;;;;  Copyright (C) 2012 Stian Sletner <stian@copyleft.no>
;;;;
;;;;  This library is free software: you can redistribute it and/or modify
;;;;  it under the terms of the GNU Lesser General Public License as published
;;;;  by the Free Software Foundation, either version 3 of the License, or
;;;;  (at your option) any later version.
;;;;
;;;;  This library is distributed in the hope that it will be useful,
;;;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;  GNU General Public License for more details.
;;;;
;;;;  You should have received a copy of the GNU General Public License
;;;;  along with this library.  If not, see <http://www.gnu.org/licenses/>.

(in-package :html5-parser)

;; external interface

(defun parse-html5 (source &key encoding strictp container dom)
  (parse-html5-from-source source
                           :encoding encoding
                           :strictp strictp
                           :container container
                           :dom dom))

(defun parse-html5-fragment (source &key encoding strictp (container "div") dom)
  (parse-html5-from-source source
                           :encoding encoding
                           :strictp strictp
                           :container container
                           :dom dom))

(defgeneric transform-html5-dom (to-type node &key)
  (:method ((to-type cons) node &key)
    (apply #'transform-html5-dom (car to-type) node (cdr to-type)))
  (:method (to-type node &key &allow-other-keys)
    (error "No TRANSFORM-HTML5-DOM method defined for dom type ~S." to-type)))


;; internal

(defun parse-html5-from-source (source &key container encoding strictp dom)
  (let ((*parser* (make-instance 'html-parser
                                 :strict strictp)))
    (parser-parse source
                  :fragment-p container
                  :encoding encoding)
    (with-slots (open-elements errors) *parser*
      (let ((document
             (if container
                 (let ((fragment (make-fragment (document*))))
                   (node-reparent-children (first open-elements) fragment)
                   fragment)
                 (document*))))
        (values (if dom
                    (transform-html5-dom dom document)
                    document)
                (reverse errors))))))

(defvar *phase*)

(defun ascii-ichar= (char1 char2)
  "ASCII case-insensitive char="
  (or (char= char1 char2)
      (and (or (char<= #\A char1 #\Z)
               (char<= #\A char2 #\Z))
           (char= (char-downcase char1)
                  (char-downcase char2)))))

(defun ascii-istring= (string1 string2)
  "ASCII case-insensitive string="
  (every #'ascii-ichar= string1 string2))

(defun cdata-switch-helper ()
  (and (last-open-element)
       (not (equal (node-namespace (last-open-element))
                   (slot-value *parser* 'html-namespace)))))

(defun parser-parse (source &key fragment-p encoding)
  (with-slots (inner-html-mode container tokenizer)
      *parser*
    (setf inner-html-mode fragment-p)
    (when (stringp fragment-p)
      (setf container fragment-p))
    (setf tokenizer (make-html-tokenizer source
                                         :encoding encoding

                                         :cdata-switch-helper #'cdata-switch-helper))
    (parser-reset)
    (loop
          ;; The input stream will throw please-reparse with result true
          ;; if the encoding is changed
          while (catch 'please-reparse
                  (main-loop)
                  nil)
          do (parser-reset))))

(defun parser-reset ()
  (with-slots (open-elements active-formatting-elements
                    head-pointer form-pointer insert-from-table
                    first-start-tag errors compat-mode inner-html-mode
                    inner-html container tokenizer phase last-phase
                    before-rcdata-phase frameset-ok
                    html-namespace)
      *parser*
    (setf open-elements '())
    (setf active-formatting-elements '())
    (setf head-pointer nil)
    (setf form-pointer nil)
    (setf insert-from-table nil)
    (setf first-start-tag nil)
    (setf errors '())
    (setf compat-mode :no-quirks)
    (cond (inner-html-mode
           (setf inner-html (string-downcase container))
           (cond ((member inner-html +cdata-elements+ :test #'string=)
                  (setf (slot-value tokenizer 'state) :rcdata-state))
                 ((member inner-html +rcdata-elements+ :test #'string=)
                  (setf (slot-value tokenizer 'state) :rawtext-state))
                 ((string= inner-html "plaintext")
                  (setf (slot-value tokenizer 'state) :plaintext-state)))
           (insert-root (implied-tag-token "html" :start-tag))
           (setf phase :before-head)
           (reset-insertion-mode))
          (t
           (setf inner-html nil)
           (setf phase :initial)))

    (setf last-phase nil)
    (setf before-rcdata-phase nil)
    (setf frameset-ok t)))

(defun is-html-integration-point (element)
  (if (and (string= (node-name element) "annotation-xml")
           (string= (node-namespace element) (find-namespace "mathml")))
      (and (element-attribute element "encoding")
           (member (ascii-upper-2-lower (element-attribute element "encoding"))
                   '("text/html" "application/xhtml+xml")
                   :test #'string=))
      (member (node-name-tuple element)
              +html-integration-point-elements+
              :test #'equal)))

(defun is-math-ml-text-integration-point (element)
  (member (node-name-tuple element)
          +mathml-text-integration-point-elements+
          :test #'equal))

(defun main-loop ()
  (with-slots (tokenizer phase)
      *parser*
    (map-tokens tokenizer (lambda (token)
                            (process-token (normalize-token token))))
    (loop with reprocess = t
       with phases = '()
       while reprocess do
         (push phase phases)
         (setf reprocess (process-eof nil :phase phase))
         (when reprocess
           (assert (not (member phase phases)))))))

(defun process-token (token)
  (with-slots (tokenizer last-open-element html-namespace)
      *parser*
    (let ((new-token token)
          (type))
      (loop while new-token do
           (let* ((current-node (last-open-element))
                  (current-node-namespace (if current-node (node-namespace current-node)))
                  (current-node-name (if current-node (node-name current-node))))

             (setf type (getf new-token :type))

             (cond ((eql type :parse-error)
                    (parser-parse-error (getf token :data) (getf token :datavars))
                    (setf new-token nil))
                   (t
                    (let (phase)
                      (if (or (null (slot-value *parser* 'open-elements))
                              (equal current-node-namespace html-namespace)
                              (and (is-math-ml-text-integration-point current-node)
                                   (or (and (eql type :start-tag)
                                            (not (member (getf token :name) '("mglyph" "malignmark") :test #'string=)))
                                       (eql type :characters)
                                       (eql type :space-characters)))
                              (and (equal current-node-namespace (find-namespace "mathml"))
                                   (equal current-node-name "annotation-xml")
                                   (eql type :start-tag)
                                   (equal (getf token :name) "svg"))
                              (and (is-html-integration-point current-node)
                                   (member type '(:start-tag :characters :space-characters))))
                          (setf phase (slot-value *parser* 'phase))
                          (setf phase :in-foreign-content))
                      ;(format t "~&phase ~S token ~S~%" phase new-token)
                      (setf new-token
                            (ecase type
                              (:characters
                               (process-characters new-token :phase phase))
                              (:space-characters
                               (process-space-characters new-token :phase phase))
                              (:start-tag
                               (process-start-tag new-token :phase phase))
                              (:end-tag
                               (process-end-tag new-token :phase phase))
                              (:comment
                               (process-comment new-token :phase phase))
                              (:doctype
                               (process-doctype new-token :phase phase))))
                      ;(format t "   phase returned ~S new-token ~S~%" phase new-token)
                                            ))))
           (when (and (eql type :start-tag)
                      (getf token :self-closing)
                      (not (getf token :self-closing-acknowledged)))
             (parser-parse-error :non-void-element-with-trailing-solidus
                                 `(:name ,(getf token :name))))))))

(defun parser-parse-error (error-code &optional datavars)
  (with-slots (errors) *parser*
    (push (list error-code datavars) errors)))

;; TODO rename to a longer and more descriptive name when we are done writing the code
(defun perror (error-code &rest datavars)
  (parser-parse-error error-code datavars))

(defun normalize-token (token)
  (when (getf token :start-tag)
    ;; Remove duplicate attributes
    (setf (getf token :data) (remove-duplicates (getf token :data)
                                                :key #'car
                                                :test #'string=
                                                :from-end t)))
  token)

(defun adjust-attributes (token replacements)
  (setf (getf token :data)
        (loop for (name . value) in (getf token :data)
           collect (cons (or (cdr (assoc name replacements :test #'string=))
                             name)
                         value))))

(defun adjust-math-ml-attributes (token)
  (adjust-attributes token '(("definitionurl" ."definitionURL"))))

(defun adjust-svg-attributes (token)
  (adjust-attributes token '(("attributename" . "attributeName")
                             ("attributetype" . "attributeType")
                             ("basefrequency" . "baseFrequency")
                             ("baseprofile" . "baseProfile")
                             ("calcmode" . "calcMode")
                             ("clippathunits" . "clipPathUnits")
                             ("contentscripttype" . "contentScriptType")
                             ("contentstyletype" . "contentStyleType")
                             ("diffuseconstant" . "diffuseConstant")
                             ("edgemode" . "edgeMode")
                             ("externalresourcesrequired" . "externalResourcesRequired")
                             ("filterres" . "filterRes")
                             ("filterunits" . "filterUnits")
                             ("glyphref" . "glyphRef")
                             ("gradienttransform" . "gradientTransform")
                             ("gradientunits" . "gradientUnits")
                             ("kernelmatrix" . "kernelMatrix")
                             ("kernelunitlength" . "kernelUnitLength")
                             ("keypoints" . "keyPoints")
                             ("keysplines" . "keySplines")
                             ("keytimes" . "keyTimes")
                             ("lengthadjust" . "lengthAdjust")
                             ("limitingconeangle" . "limitingConeAngle")
                             ("markerheight" . "markerHeight")
                             ("markerunits" . "markerUnits")
                             ("markerwidth" . "markerWidth")
                             ("maskcontentunits" . "maskContentUnits")
                             ("maskunits" . "maskUnits")
                             ("numoctaves" . "numOctaves")
                             ("pathlength" . "pathLength")
                             ("patterncontentunits" . "patternContentUnits")
                             ("patterntransform" . "patternTransform")
                             ("patternunits" . "patternUnits")
                             ("pointsatx" . "pointsAtX")
                             ("pointsaty" . "pointsAtY")
                             ("pointsatz" . "pointsAtZ")
                             ("preservealpha" . "preserveAlpha")
                             ("preserveaspectratio" . "preserveAspectRatio")
                             ("primitiveunits" . "primitiveUnits")
                             ("refx" . "refX")
                             ("refy" . "refY")
                             ("repeatcount" . "repeatCount")
                             ("repeatdur" . "repeatDur")
                             ("requiredextensions" . "requiredExtensions")
                             ("requiredfeatures" . "requiredFeatures")
                             ("specularconstant" . "specularConstant")
                             ("specularexponent" . "specularExponent")
                             ("spreadmethod" . "spreadMethod")
                             ("startoffset" . "startOffset")
                             ("stddeviation" . "stdDeviation")
                             ("stitchtiles" . "stitchTiles")
                             ("surfacescale" . "surfaceScale")
                             ("systemlanguage" . "systemLanguage")
                             ("tablevalues" . "tableValues")
                             ("targetx" . "targetX")
                             ("targety" . "targetY")
                             ("textlength" . "textLength")
                             ("viewbox" . "viewBox")
                             ("viewtarget" . "viewTarget")
                             ("xchannelselector" . "xChannelSelector")
                             ("ychannelselector" . "yChannelSelector")
                             ("zoomandpan" . "zoomAndPan"))))

(defun adjust-foreign-attributes (token)
  (adjust-attributes token `(("xlink:actuate" . ("xlink" "actuate" ,(find-namespace "xlink")))
                             ("xlink:arcrole" . ("xlink" "arcrole" ,(find-namespace "xlink")))
                             ("xlink:href" . ("xlink" "href" ,(find-namespace "xlink")))
                             ("xlink:role" . ("xlink" "role" ,(find-namespace "xlink")))
                             ("xlink:show" . ("xlink" "show" ,(find-namespace "xlink")))
                             ("xlink:title" . ("xlink" "title" ,(find-namespace "xlink")))
                             ("xlink:type" . ("xlink" "type" ,(find-namespace "xlink")))
                             ("xml:base" . ("xml" "base" ,(find-namespace "xml")))
                             ("xml:lang" . ("xml" "lang" ,(find-namespace "xml")))
                             ("xml:space" . ("xml" "space" ,(find-namespace "xml")))
                             ("xmlns" . (nil "xmlns" (find-namespace "xmlns")))
                             ("xmlns:xlink" . ("xmlns" "xlink" ,(find-namespace "xmlns"))))))

(defun reset-insertion-mode ()
  (with-slots (inner-html html-namespace phase open-elements) *parser*
    (let ((last nil)
          (new-phase nil)
          (new-modes '(("select" . :in-select)
                       ("td" . :in-cell)
                       ("th" . :in-cell)
                       ("tr" . :in-row)
                       ("tbody" . :in-table-body)
                       ("thead" . :in-table-body)
                       ("tfoot" . :in-table-body)
                       ("caption" . :in-caption)
                       ("colgroup" . :in-column-group)
                       ("table" . :in-table)
                       ("head" . :in-body)
                       ("body" . :in-body)
                       ("frameset" . :in-frameset)
                       ("html" . :before-head))))
      (loop for node in (reverse open-elements)
         for node-name = (node-name node)
         do
         (when (eql node (first open-elements))
           (assert inner-html)
           (setf last t)
           (setf node-name inner-html))
         ;; Check for conditions that should only happen in the innerHTML
         ;; case
         (when (member node-name '("select" "colgroup" "head" "html") :test #'string=)
           (assert inner-html))

         (unless (and (not last)
                      (string/= (node-namespace node) html-namespace))
           (let ((match (cdr (assoc node-name new-modes :test #'string=))))
             (when match
               (setf new-phase match)
               (return))
             (when last
               (setf new-phase :in-body)
               (return)))))
      (setf phase new-phase))))

(defun parse-rc-data-raw-text (token content-type)
  (assert (member content-type '(:rawtext :rcdata)))
  (with-slots (tokenizer original-phase phase) *parser*
    (insert-element token)
    (setf (tokenizer-state tokenizer) (ecase content-type
                                        (:rawtext :rawtext-state)
                                        (:rcdata :rcdata-state)))
    (setf original-phase phase)
    (setf phase :text)
    nil))


;; Phases --------------------------------------------------------------------

(defun implied-tag-token (name &optional (type :end-tag))
  (list :type type :name name :data '() :self-closing nil))

(defun implied-tag-token/full (name type
                               &key (attributes '()) (self-closing nil))
  (list :type type :name name :data attributes :self-closing self-closing))

(eval-when (:compile-toplevel :execute)
  (defun phase-process-method-name (function-name)
    (intern (concatenate 'string
                         "%"
                         (symbol-name function-name))
            (symbol-package function-name))))

(defvar *phase-indent* 0)

(defun call-phase-method (name phase token)
  ;(format *trace-output* "~&~vTcall: ~S ~S ~S" *phase-indent* name phase token)
  ;(break)
  (let ((result (let ((*phase-indent* (+ 4 *phase-indent*)))
                  (funcall name phase token))))
    ;(format *trace-output* "~&~vTreturn: ~S ~S" *phase-indent* name result)
    result))

(defmacro define-phase-process-functions (&body defs)
  `(progn
     ,@(loop for function-name in defs
          for method-name = (phase-process-method-name function-name)
          collect `(defgeneric ,method-name (phase token))
          collect `(defun ,function-name (token &key (phase *phase*))
                     (call-phase-method #',method-name phase token)))))

(define-phase-process-functions
  add-formatting-element
  end-tag-applet-marquee-object
  end-tag-block
  end-tag-body
  end-tag-br
  end-tag-caption
  end-tag-col
  end-tag-colgroup
  end-tag-form
  end-tag-formatting
  end-tag-frameset
  end-tag-head
  end-tag-heading
  end-tag-html
  end-tag-html-body-br
  end-tag-ignore
  end-tag-imply
  end-tag-imply-head
  end-tag-list-item
  end-tag-optgroup
  end-tag-option
  end-tag-other
  end-tag-p
  end-tag-script
  end-tag-select
  end-tag-table
  end-tag-table-cell
  end-tag-table-row-group
  end-tag-tr
  insert-text
  process-characters
  process-comment
  process-doctype
  process-end-tag
  process-eof
  process-space-characters
  process-start-tag
  start-tag-a
  start-tag-applet-marquee-object
  start-tag-base-link-command
  start-tag-body
  start-tag-button
  start-tag-caption
  start-tag-close-p
  start-tag-col
  start-tag-colgroup
  start-tag-form
  start-tag-formatting
  start-tag-frame
  start-tag-frameset
  start-tag-from-head
  start-tag-head
  start-tag-heading
  start-tag-hr
  start-tag-html
  start-tag-i-frame
  start-tag-image
  start-tag-imply-tbody
  start-tag-input
  start-tag-is-index
  start-tag-list-item
  start-tag-math
  start-tag-meta
  start-tag-misplaced
  start-tag-no-script-no-frames-style
  start-tag-nobr
  start-tag-noframes
  start-tag-opt
  start-tag-optgroup
  start-tag-option
  start-tag-other
  start-tag-param-source
  start-tag-plaintext
  start-tag-pre-listing
  start-tag-process-in-head
  start-tag-rawtext
  start-tag-row-group
  start-tag-rp-rt
  start-tag-script
  start-tag-select
  start-tag-style-script
  start-tag-svg
  start-tag-table
  start-tag-table-cell
  start-tag-table-element
  start-tag-table-other
  start-tag-textarea
  start-tag-title
  start-tag-tr
  start-tag-void-formatting
  start-tag-xmp)

(defmacro def (phase name (&rest slots) &body body)
  `(defmethod ,(phase-process-method-name name) ((*phase* (eql ,phase)) token)
     (with-slots (,@slots) *parser*
       ,@body)))

(defmacro tagname-dispatch (phase name &body cases)
  `(def ,phase ,name ()
     (let ((tagname (getf token :name)))
       (declare (ignorable tagname))
       ,(let* ((default '(error "Unhandled tag ~S" tagname))
               (string-cases
                 (loop for (tagnames function) in cases
                       append (cond ((stringp tagnames)
                                     `((,tagnames (,function token))))
                                    ((consp tagnames)
                                     (loop for tag in tagnames
                                           collect `(,tag (,function token))))
                                    ((eql 'default tagnames)
                                     (setf default `(,function token))
                                     nil)
                                    (t (error "Invalid tag name clause ~S" tagnames))))))
          (if (not string-cases)
              default
              `(string-case:string-case
                   (tagname :default ,default)
                 ,@string-cases))))))

;; Default methods

(defmethod %process-comment (*phase* token)
  ;; For most phases the following is correct. Where it's not it will be
  ;; overridden.
  (insert-comment token (last-open-element))
  nil)

(defmethod %process-doctype (*phase* token)
  (parser-parse-error :unexpected-doctype)
  nil)

(defmethod %process-characters (*phase* token)
  (parser-insert-text (getf token :data))
  nil)

(defmethod %process-space-characters (*phase* token)
  (parser-insert-text (getf token :data))
  nil)

(defmethod %start-tag-html (*phase* token)
  (with-slots (first-start-tag open-elements)
      *parser*
    (when (and (not first-start-tag)
               (string= (getf token :name) "html"))
      (parser-parse-error :non-html-root))
    ;; XXX Need a check here to see if the first start tag token emitted is
    ;; this token... If it's not, invoke self.parser.parseError().
    (let ((root-element (first open-elements)))
      (loop for (name . value) in (getf token :data)
            do (unless (element-attribute root-element name)
                 (setf (element-attribute root-element name) value))))
    (setf first-start-tag nil)
    nil))


;; InitialPhase

(def :initial process-space-characters ()
  nil)

(def :initial process-comment ()
  (insert-comment token (document*))
  nil)

(def :initial process-doctype (compat-mode phase)
  (destructuring-bind (&key name public-id system-id correct &allow-other-keys)
      token

    (when (or (string/= name "html")
              public-id
              (and system-id (string/= system-id "about:legacy-compat")))
      (parser-parse-error :unknown-doctype))

    (unless public-id
      (setf public-id ""))

    (insert-doctype token)

    (setf public-id (ascii-upper-2-lower public-id))

    (cond ((or (not correct)
               (string/= name "html")
               (cl-ppcre:scan +quirks-mode-doctypes-regexp+ public-id)
               (member public-id '("-//w3o//dtd w3 html strict 3.0//en//"
                                   "-/w3c/dtd html 4.0 transitional/en"
                                   "html")
                       :test #'string=)
               (and (not system-id)
                    (cl-ppcre:scan '(:sequence :start-anchor (:alternation
                                                              "-//w3c//dtd html 4.01 frameset//"
                                                              "-//w3c//dtd html 4.01 transitional//"))
                                   public-id))
               (and system-id
                    (equal (ascii-upper-2-lower system-id)
                           "http://www.ibm.com/data/dtd/v11/ibmxhtml1-transitional.dtd")))
           (setf compat-mode :quirks))
          ((or (cl-ppcre:scan '(:sequence :start-anchor (:alternation
                                                         "-//w3c//dtd xhtml 1.0 frameset//"
                                                         "-//w3c//dtd xhtml 1.0 transitional//"))
                              public-id)
               (and system-id
                    (cl-ppcre:scan '(:sequence :start-anchor (:alternation
                                                              "-//w3c//dtd html 4.01 frameset//"
                                                              "-//w3c//dtd html 4.01 transitional//"))
                                   public-id)))
           (setf compat-mode :limited-quirks)))
    (setf phase :before-html)
    nil))

(flet ((anything-else ()
         (with-slots (compat-mode phase)
             *parser*
           (setf compat-mode :quirks)
           (setf phase :before-html))))

  (def :initial process-characters ()
    (parser-parse-error :expected-doctype-but-got-chars)
    (anything-else)
    token)

  (def :initial process-start-tag ()
    (parser-parse-error :expected-doctype-but-got-start-tag
                        (list :name (getf token :name)))
    (anything-else)
    token)

  (def :initial process-end-tag ()
    (parser-parse-error :expected-doctype-but-got-end-tag
                        (list :name (getf token :name)))
    (anything-else)
    token)

  (def :initial process-eof ()
    (parser-parse-error :expected-doctype-but-got-eof)
    (anything-else)
    t))


;; BeforeHtmlPhase

(flet ((insert-html-element ()
         (insert-root (implied-tag-token "html" :start-tag))
         (setf (parser-phase *parser*) :before-head)))


  (def :before-html process-eof ()
    (insert-html-element)
    t)

  (def :before-html process-comment ()
    (insert-comment token (document*))
    nil)

  (def :before-html process-space-characters ()
    nil)

  (def :before-html process-characters ()
    (insert-html-element)
    token)

  (def :before-html process-start-tag (first-start-tag)
    (when (string= (getf token :name) "html")
      (setf first-start-tag t))
    (insert-html-element)
    token)

  (def :before-html process-end-tag ()
    (cond ((not (member (getf token :name) '("head" "body" "html" "br") :test #'string=))
           (parser-parse-error :unexpected-end-tag-before-html `(:name ,(getf token :name)))
           nil)
          (t
           (insert-html-element)
           token))))

;; BeforeHeadPhase

(tagname-dispatch :before-head process-start-tag
  ("html" start-tag-html)
  ("head" start-tag-head token)
  (default start-tag-other))

(tagname-dispatch :before-head process-end-tag
  (("head" "body" "html" "br") end-tag-imply-head)
  (default end-tag-other))

(def :before-head process-eof ()
  (start-tag-head (implied-tag-token "head" :start-tag))
  t)

(def :before-head process-space-characters ()
  nil)

(def :before-head process-characters ()
  (start-tag-head (implied-tag-token "head" :start-tag))
  token)

(def :before-head start-tag-html ()
  (process-start-tag token :phase :in-body))

(def :before-head start-tag-head (head-pointer)
  (insert-element token)
  (setf head-pointer (last-open-element))
  (setf (parser-phase *parser*) :in-head)
  nil)

(def :before-head start-tag-other ()
  (start-tag-head (implied-tag-token "head" :start-tag))
  token)

(def :before-head end-tag-imply-head ()
  (start-tag-head (implied-tag-token "head" :start-tag))
  token)

(def :before-head end-tag-other ()
  (parser-parse-error :end-tag-after-implied-root `(:name ,(getf token :name)))
  nil)

;; InHeadPhase

(tagname-dispatch :in-head process-start-tag
  ("html" start-tag-html)
  ("title" start-tag-title)
  (("noscript" "noframes" "style") start-tag-no-script-no-frames-style)
  ("script" start-tag-script)
  (("base" "basefont" "bgsound" "command" "link") start-tag-base-link-command)
  ("meta" start-tag-meta)
  ("head" start-tag-head)
  (default start-tag-other))

(tagname-dispatch :in-head process-end-tag
  ("head" end-tag-head)
  (("br" "html" "body") end-tag-html-body-br)
  (default end-tag-other))

(flet ((anything-else ()
         (end-tag-head (implied-tag-token "head"))))

  ;; the real thing
  (def :in-head process-eof ()
    (anything-else)
    t)

  (def :in-head process-characters ()
    (anything-else)
    token)

  (def :in-head start-tag-html ()
    (process-start-tag token :phase :in-body))

  (def :in-head start-tag-head ()
    (parser-parse-error :two-heads-are-not-better-than-one)
    nil)

  (def :in-head start-tag-base-link-command (open-elements)
    (insert-element token)
    (pop-end open-elements)
    (setf (getf token :self-closing-acknowledged) t)
    nil)

  (defun parse-content-attr (string)
    "The algorithm for extracting an encoding from a meta element"
    (let ((position 0))                 ; Step 1
      (labels ((char-at (index)
                 (and (< position (length string))
                      (char string index)))
               (skip-space ()
                 (loop while (member (char-at position) +space-characters+)
                       do (incf position))))
        ;; Step 2
        (loop
          (setf position (search "charset" string :start2 position))
          (unless position
            (return-from parse-content-attr))
          ;; Set position to after charset
          (incf position 7)
          ;; Step 3
          (skip-space)
          ;; Step 4
          (when (eql (char-at position) #\=)
            (return))
          (decf position))
        ;; Step 5
        (incf position)
        (skip-space)
        ;; Step 6
        (let ((next-char (char-at position)))
          (cond ((or (eql #\' next-char)
                     (eql #\" next-char))
                 (incf position)
                 (let ((end (position next-char string :start position)))
                   (when end
                     (subseq string position end))))
                (next-char
                 (let ((start position))
                   (loop until (or (= position (length string))
                                   (member (char-at position) +space-characters+))
                         do (incf position))
                   (subseq string start position))))))))


  (def :in-head start-tag-meta (tokenizer open-elements)
    (insert-element token)
    (pop-end open-elements)
    (setf (getf token :self-closing-acknowledged) t)

    (let ((attributes (getf token :data)))
      (when (eql (cdr (html5-stream-encoding (tokenizer-stream tokenizer))) :tentative)
        (cond ((assoc "charset" attributes :test #'string=)
               (html5-stream-change-encoding (tokenizer-stream tokenizer)
                                             (cdr (assoc "charset" attributes :test #'string=))))
              ((and (assoc "http-equiv" attributes :test #'string=)
                    (ascii-istring= (cdr (assoc "http-equiv" attributes :test #'string=))
                                    "Content-Type")
                    (assoc "content" attributes :test #'string=))
               (let* ((content (cdr (assoc "content" attributes :test #'string=)))
                      (new-encoding (parse-content-attr content)))
                 (if new-encoding
                     (html5-stream-change-encoding (tokenizer-stream tokenizer)
                                                   new-encoding)
                     (parser-parse-error :invalid-encoding-declaration
                                         `(:content ,content))))))))
    nil)

  (def :in-head start-tag-title ()
    (parse-rc-data-raw-text token :rcdata)
    nil)

  (def :in-head start-tag-no-script-no-frames-style ()
    ;; Need to decide whether to implement the scripting-disabled case
    (parse-rc-data-raw-text token :rawtext))

  (def :in-head start-tag-script (tokenizer original-phase phase)
    (insert-element token)
    (setf (tokenizer-state tokenizer) :script-data-state)
    (setf original-phase phase)
    (setf phase :text)
    nil)

  (def :in-head start-tag-other ()
    (anything-else)
    token)

  (def :in-head end-tag-head (phase open-elements)
    (let ((node (pop-end open-elements)))
      (assert (string= (node-name node) "head") ()  "Expected head got ~S" (node-name node))
      (setf phase :after-head)
      nil))

  (def :in-head end-tag-html-body-br ()
    (anything-else)
    token)

  (def :in-head end-tag-other ()
    (parser-parse-error :unexpected-end-tag `(:name ,(getf token :name)))
    nil))

;; XXX If we implement a parser for which scripting is disabled we need to
;; implement this phase.
;;
;; InHeadNoScriptPhase

;; AfterHeadPhase

(tagname-dispatch :after-head process-start-tag
  ("html" start-tag-html)
  ("body" start-tag-body)
  ("frameset" start-tag-frameset)
  (("base" "basefont" "bgsound" "link" "meta"
           "noframes" "script" "style" "title")
   start-tag-from-head)
  ("head" start-tag-head)
  (default start-tag-other))

(tagname-dispatch :after-head process-end-tag
  (("body" "html" "br") end-tag-html-body-br)
  (default end-tag-other))

(flet ((anything-else ()
         (with-slots (phase frameset-ok) *parser*
           (insert-element (implied-tag-token "body" :start-tag))
           (setf phase :in-body)
           (setf frameset-ok t))))

  (def :after-head process-eof ()
    (anything-else)
    t)

  (def :after-head process-characters ()
    (anything-else)
    token)

  (def :after-head start-tag-html ()
    (process-start-tag token :phase :in-body))

  (def :after-head start-tag-body (phase frameset-ok)
    (setf frameset-ok nil)
    (insert-element token)
    (setf phase :in-body)
    nil)

  (def :after-head start-tag-frameset (phase)
    (insert-element token)
    (setf phase :in-frameset)
    nil)

  (def :after-head start-tag-from-head (head-pointer open-elements)
    (parser-parse-error :unexpected-start-tag-out-of-my-head
                        `(:name ,(getf token :name)))
    (push-end head-pointer open-elements)
    (process-start-tag token :phase :in-head)
    (loop for node in (reverse open-elements)
          do (when (string= "head" (node-name node))
               (setf open-elements
                     (remove node open-elements :test #'equal))
               (return)))
    nil)

  (def :after-head start-tag-head ()
    (parser-parse-error :unexpected-start-tag
                        `(:name ,(getf token :name)))
    nil)

  (def :after-head start-tag-other ()
    (anything-else)
    token)

  (def :after-head end-tag-html-body-br ()
    (anything-else)
    token)

  (def :after-head end-tag-other ()
    (parser-parse-error :unexpected-end-tag
                        `(:name ,(getf token :name)))
    nil))

;; InBodyPhase

(tagname-dispatch :in-body process-start-tag
  ("html" start-tag-html)
  (("base" "basefont" "bgsound" "command" "link"
           "meta" "noframes" "script" "style" "title")
   start-tag-process-in-head)
  ("body" start-tag-body)
  ("frameset" start-tag-frameset)
  (("address" "article" "aside" "blockquote" "center" "details"
              "dir" "div" "dl" "fieldset" "figcaption" "figure"
              "footer" "header" "hgroup" "menu" "nav" "ol" "p"
              "section" "summary" "ul")
   start-tag-close-p)
  (#.+heading-elements+ start-tag-heading)
  (("pre" "listing") start-tag-pre-listing)
  ("form" start-tag-form)
  (("li" "dd" "dt") start-tag-list-item)
  ("plaintext" start-tag-plaintext)
  ("a" start-tag-a)
  (("b" "big" "code" "em" "font" "i" "s" "small" "strike"
        "strong" "tt" "u")
   start-tag-formatting)
  ("nobr" start-tag-nobr)
  ("button" start-tag-button)
  (("applet" "marquee" "object") start-tag-applet-marquee-object)
  ("xmp" start-tag-xmp)
  ("table" start-tag-table)
  (("area" "br" "embed" "img" "keygen" "wbr")
   start-tag-void-formatting)
  (("param" "source" "track") start-tag-param-source)
  ("input" start-tag-input)
  ("hr" start-tag-hr)
  ("image" start-tag-image)
  ("isindex" start-tag-is-index)
  ("textarea" start-tag-textarea)
  ("iframe" start-tag-i-frame)
  (("noembed" "noscript") start-tag-rawtext)
  ("select" start-tag-select)
  (("rp" "rt") start-tag-rp-rt)
  (("option" "optgroup") start-tag-opt)
  (("math") start-tag-math)
  (("svg") start-tag-svg)
  (("caption" "col" "colgroup" "frame" "head"
              "tbody" "td" "tfoot" "th" "thead"
              "tr")
   start-tag-misplaced)
  (default start-tag-other))

(tagname-dispatch :in-body process-end-tag
  ("body" end-tag-body)
  ("html" end-tag-html)
  (("address" "article" "aside" "blockquote" "button" "center"
              "details" "dir" "div" "dl" "fieldset" "figcaption" "figure"
              "footer" "header" "hgroup" "listing" "menu" "nav" "ol" "pre"
              "section" "summary" "ul")
   end-tag-block)
  ("form" end-tag-form)
  ("p" end-tag-p)
  (("dd" "dt" "li") end-tag-list-item)
  (#.+heading-elements+ end-tag-heading)
  (("a" "b" "big" "code" "em" "font" "i" "nobr" "s" "small"
        "strike" "strong" "tt" "u")
   end-tag-formatting)
  (("applet" "marquee" "object") end-tag-applet-marquee-object)
  ("br" end-tag-br)
  (default end-tag-other))

(flet ((is-matching-formatting-element (node1 node2)
         (and (equal (node-name node1) (node-name node2))
              (equal (node-namespace node1) (node-namespace node2))
              (node-attributes= node1 node2))))

  (def :in-body add-formatting-element (reverse active-formatting-elements)
    (insert-element token)
    (let ((element (last-open-element))
          matching-elements)
      (loop for node in (reverse active-formatting-elements)
            do (if (eq node :marker)
                   (return)
                   (when (is-matching-formatting-element node element)
                     (push-end node matching-elements))))
      (assert (<= (length matching-elements) 3))
      (when (= (length matching-elements) 3)
        (setf active-formatting-elements
              (remove (car (last matching-elements))
                      active-formatting-elements)))
      (assert element)
      (push-end element active-formatting-elements))
    nil))

(def :in-body process-eof (open-elements)
  (let ((allowed-elements '("dd" "dt" "li" "p" "tbody" "td"
                            "tfoot" "th" "thead" "tr" "body" "html")))
    (loop for node in (reverse open-elements)
          do (when (not (member (node-name node)
                                allowed-elements
                                :test #'string=))
               (parser-parse-error :expected-closing-tag-but-got-eof)
               (return))))
  nil)

(def :in-body process-characters (frameset-ok)
  (let ((data (getf token :data)))
    (if (equal data (string #\u0000))
        nil
        (progn
          (reconstruct-active-formatting-elements)
          (parser-insert-text data)
          ;;This must be bad for performance
          (when (and frameset-ok
                     (notevery (lambda (char)
                                 (find char +space-characters+))
                               data))
            (setf frameset-ok nil))
          nil))))

(def :in-body process-space-characters (in-body-process-space-characters-mode)
  (ecase in-body-process-space-characters-mode
    (:non-pre
     (reconstruct-active-formatting-elements)
     (parser-insert-text (getf token :data)))
    (:drop-newline
     (let ((data (getf token :data)))
       (setf in-body-process-space-characters-mode :non-pre)
       (when (and (plusp (length data))
                  (char= #\Newline (char data 0))
                  (member (node-name (last-open-element))
                          '("pre" "listing" "textarea")
                          :test #'string=)
                  (not (node-has-content (last-open-element))))
         (setf data (subseq data 1)))
       (when (plusp (length data))
         (reconstruct-active-formatting-elements)
         (parser-insert-text data)))))
  nil)

(def :in-body start-tag-process-in-head ()
  (process-start-tag token :phase :in-head))

(def :in-body start-tag-body (frameset-ok open-elements)
  (parser-parse-error :unexpected-start-tag
                      `(:name ,(getf token :name)))
  (if (or (= 1 (length open-elements))
          (string/= (node-name (second open-elements)) "body"))
      (assert (slot-value *parser* 'inner-html))
      (progn
        (setf frameset-ok nil)
        (loop for (name . value) in (getf token :data)
              do (unless (element-attribute (second open-elements) name)
                   (setf (element-attribute (second open-elements) name) value)))))
  nil)

(def :in-body start-tag-frameset (frameset-ok phase open-elements)
  (parser-parse-error :unexpected-start-tag
                      `(:name ,(getf token :name)))
  (cond ((or (= 1 (length open-elements))
             (string/= (node-name (second open-elements)) "body"))
         (assert (slot-value *parser* 'inner-html)))
        ((not frameset-ok)
         nil)
        (t
         (when (node-parent (second open-elements))
           (node-remove-child (node-parent (second open-elements))
                              (second open-elements)))
         (loop until (string= (node-name (last-open-element))
                              "html")
               do (pop-end open-elements))
         (insert-element token)
         (setf phase :in-frameset)))
  nil)

(def :in-body start-tag-close-p ()
  (when (element-in-scope "p" "button")
    (end-tag-p (implied-tag-token "p")))
  (insert-element token)
  nil)

(def :in-body start-tag-pre-listing (in-body-process-space-characters-mode frameset-ok)
  (when (element-in-scope "p" "button")
    (end-tag-p (implied-tag-token "p")))
  (insert-element token)
  (setf frameset-ok nil)
  (setf in-body-process-space-characters-mode :drop-newline)
  nil)

(def :in-body start-tag-form (form-pointer)
  (if form-pointer
      (parser-parse-error :unexpected-start-tag
                          `(:name ,(getf token :name)))
      (progn
        (when (element-in-scope "p" "button")
          (end-tag-p (implied-tag-token "p")))
        (insert-element token)
        (setf form-pointer (last-open-element))))
  nil)

(def :in-body start-tag-list-item (phase frameset-ok open-elements)
  (setf frameset-ok nil)
  (let ((stop-names (cond ((string= (getf token :name) "li")
                           '("li"))
                          ((string= (getf token :name) "dt")
                           '("dt" "dd"))
                          ((string= (getf token :name) "dd")
                           '("dt" "dd")))))
    (loop for node in (reverse open-elements)
          do (cond ((member (node-name node) stop-names :test #'string=)
                    (process-end-tag (implied-tag-token (node-name node)) :phase phase)
                    (return))
                   ((and (member (node-name-tuple node) +special-elements+
                                 :test #'equal)
                         (not (member (node-name node)
                                      '("address" "div" "p")
                                      :test #'string=)))
                    (return)))))
  (when (element-in-scope "p" "button")
    (process-end-tag (implied-tag-token "p") :phase phase))
  (insert-element token)
  nil)

(def :in-body start-tag-plaintext (tokenizer)
  (when (element-in-scope "p" "button")
    (end-tag-p (implied-tag-token "p")))
  (insert-element token)
  (setf (tokenizer-state tokenizer) :plaintext-state)
  nil)

(def :in-body start-tag-heading (open-elements)
  (when (element-in-scope "p" "button")
    (end-tag-p (implied-tag-token "p")))
  (when (member (node-name (last-open-element)) +heading-elements+
                :test #'string=)
    (perror :unexpected-start-tag :name (getf token :name))
    (pop-end open-elements))
  (insert-element token)
  nil)

(def :in-body start-tag-a (open-elements active-formatting-elements)
  (let ((afe-a-element (element-in-active-formatting-elements "a")))
    (when afe-a-element
      (perror :unexpected-start-tag-implies-end-tag
              :start-name "a" :end-name "a")
      (end-tag-formatting (implied-tag-token "a"))
      (when (member afe-a-element open-elements)
        (setf open-elements
              (remove afe-a-element open-elements)))
      (when (member afe-a-element active-formatting-elements)
        (setf active-formatting-elements
              (remove afe-a-element active-formatting-elements))))
    (reconstruct-active-formatting-elements)
    (add-formatting-element token))
  nil)

(def :in-body start-tag-formatting ()
  (reconstruct-active-formatting-elements)
  (add-formatting-element token)
  nil)

(def :in-body start-tag-nobr ()
  (reconstruct-active-formatting-elements)
  (when (element-in-scope "nobr")
    (perror :unexpected-start-tag-implies-end-tag
            :start-name "nobr" :end-name "nobr")
    (process-end-tag (implied-tag-token "nobr"))
    ;; XXX Need tests that trigger the following
    (reconstruct-active-formatting-elements))
  (add-formatting-element token)
  nil)

(def :in-body start-tag-button (frameset-ok)
  (cond ((element-in-scope "button")
         (perror :unexpected-start-tag-implies-end-tag
                 :start-name "button" :end-name "button")
         (process-end-tag (implied-tag-token "button"))
         token)
        (t
         (reconstruct-active-formatting-elements)
         (insert-element token)
         (setf frameset-ok nil)
         nil)))

(def :in-body start-tag-applet-marquee-object (frameset-ok active-formatting-elements)
  (reconstruct-active-formatting-elements)
  (insert-element token)
  (push-end :marker active-formatting-elements)
  (setf frameset-ok nil)
  nil)

(def :in-body start-tag-xmp (frameset-ok)
  (when (element-in-scope "p" "button")
    (end-tag-p (implied-tag-token "p")))
  (reconstruct-active-formatting-elements)
  (setf frameset-ok nil)
  (parse-rc-data-raw-text token :rawtext)
  nil)

(def :in-body start-tag-table (frameset-ok compat-mode phase)
  (when (not (eq compat-mode :quirks))
    (when (element-in-scope "p" "button")
      (end-tag-p (implied-tag-token "p"))))
  (insert-element token)
  (setf frameset-ok nil)
  (setf phase :in-table)
  nil)

(def :in-body start-tag-void-formatting (frameset-ok open-elements)
  (reconstruct-active-formatting-elements)
  (insert-element token)
  (pop-end open-elements)
  (setf (getf token :self-closing-acknowledged) t)
  (setf frameset-ok nil)
  nil)

(def :in-body start-tag-input (frameset-ok)
  (let ((old-frameset-ok frameset-ok))
    (start-tag-void-formatting token)
    (let ((type (assoc "type" (getf token :data) :test #'string=)))
      (when (and type
                 (string= (ascii-upper-2-lower (cdr type)) "hidden"))
        ;;input type=hidden doesn't change framesetOK
        (setf frameset-ok old-frameset-ok))))
  nil)

(def :in-body start-tag-param-source (open-elements)
  (insert-element token)
  (pop-end open-elements)
  (setf (getf token :self-closing-acknowledged) t)
  nil)

(def :in-body start-tag-hr (frameset-ok open-elements)
  (when (element-in-scope "p" "button")
    (end-tag-p (implied-tag-token "p")))
  (insert-element token)
  (pop-end open-elements)
  (setf (getf token :self-closing-acknowledged) t)
  (setf frameset-ok nil)
  nil)

(def :in-body start-tag-image ()
  (perror :unexpected-start-tag-treated-as
          :original-name "image" :new-name "img")
  (process-start-tag (implied-tag-token/full
                      "img" :start-tag
                      :attributes (getf token :data)
                      :self-closing (getf token :self-closing)))
  nil)

(def :in-body start-tag-is-index (form-pointer)
  (block nil
    (perror :deprecated-tag :name "isindex")
    (when form-pointer
      (return nil))
    (let (attrs)
      (when (assoc "action" (getf token :data) :test #'string=)
        (setf attrs (list (assoc "action" (getf token :data) :test #'string=))))
      (process-start-tag (implied-tag-token/full "form" :start-tag
                                                 :attributes attrs)))
    (process-start-tag (implied-tag-token "hr" :start-tag))
    (process-start-tag (implied-tag-token "label" :start-tag))
    ;; XXX Localization ...
    (let ((prompt (if (assoc "prompt" (getf token :data) :test #'string=)
                      (cdr (assoc "prompt" (getf token :data) :test #'string=))
                      "This is a searchable index. Enter search keywords: ")))
      (process-characters (list :type :characters :data prompt)))
    (let ((attrs (append (remove-if (lambda (el)
                                      (member (car el) '("action" "prompt" "name")
                                              :test #'string=))
                                    (copy-list (getf token :data)))
                         (copy-list '(("name" . "isindex"))))))
      (process-start-tag (implied-tag-token/full "input" :start-tag
                                                 :attributes attrs
                                                 :self-closing
                                                 (getf token :self-closing))))
    (process-end-tag (implied-tag-token "label"))
    (process-start-tag (implied-tag-token "hr" :start-tag))
    (process-end-tag (implied-tag-token "form")))
  nil)

(def :in-body start-tag-textarea (tokenizer
                                  in-body-process-space-characters-mode
                                  frameset-ok)
  (insert-element token)
  (setf (tokenizer-state tokenizer) :rcdata-state)
  (setf in-body-process-space-characters-mode :drop-newline)
  (setf frameset-ok nil)
  nil)

(def :in-body start-tag-i-frame (frameset-ok)
  (setf frameset-ok nil)
  (start-tag-rawtext token)
  nil)

(def :in-body start-tag-rawtext ()
  ;;;iframe, noembed noframes, noscript(if scripting enabled)
  (parse-rc-data-raw-text token :rawtext)
  nil)

(def :in-body start-tag-opt (phase)
  (when (string= (node-name (last-open-element)) "option")
    (process-end-tag (implied-tag-token "option") :phase phase))
  (reconstruct-active-formatting-elements)
  (insert-element token)
  nil)

(def :in-body start-tag-select (frameset-ok)
  (reconstruct-active-formatting-elements)
  (insert-element token)
  (setf frameset-ok nil)
  (if (member (parser-phase *parser*) '(:in-table :in-caption :in-column-group
                                        :in-table-body :in-row :in-cell))
      (setf (parser-phase *parser*) :in-select-in-table)
      (setf (parser-phase *parser*) :in-select))
  nil)

(def :in-body start-tag-rp-rt ()
  (when (element-in-scope "ruby")
    (generate-implied-end-tags)
    (when (string/= (node-name (last-open-element)) "ruby")
      (perror :expected-ruby-tag)))
  (insert-element token)
  nil)

(def :in-body start-tag-math (open-elements)
  (reconstruct-active-formatting-elements)
  (adjust-math-ml-attributes token)
  (adjust-foreign-attributes token)
  (setf (getf token :namespace) (find-namespace "mathml"))
  (insert-element token)
  ;;Need to get the parse error right for the case where the token
  ;;has a namespace not equal to the xmlns attribute
  (when (getf token :self-closing)
    (pop-end open-elements)
    (setf (getf token :self-closing-acknowledged) t))
  nil)

(def :in-body start-tag-svg (open-elements)
  (reconstruct-active-formatting-elements)
  (adjust-svg-attributes token)
  (adjust-foreign-attributes token)
  (setf (getf token :namespace) (find-namespace "svg"))
  (insert-element token)
  ;;Need to get the parse error right for the case where the token
  ;;has a namespace not equal to the xmlns attribute
  (when (getf token :self-closing)
    (pop-end open-elements)
    (setf (getf token :self-closing-acknowledged) t))
  nil)

(def :in-body start-tag-misplaced ()
  ;;; Elements that should be children of other elements that have a
  ;;; different insertion mode; here they are ignored
  ;;; "caption", "col", "colgroup", "frame", "frameset", "head",
  ;;; "option", "optgroup", "tbody", "td", "tfoot", "th", "thead",
  ;;; "tr", "noscript"
  (perror :unexpected-start-tag-ignored :name (getf token :name))
  nil)

(def :in-body start-tag-other ()
  (reconstruct-active-formatting-elements)
  (insert-element token)
  nil)

(def :in-body end-tag-p (open-elements)
  (cond ((not (element-in-scope "p" "button"))
         (start-tag-close-p (implied-tag-token "p" :start-tag))
         (perror :unexpected-end-tag :name "p")
         (end-tag-p (implied-tag-token "p")))
        (t
         (generate-implied-end-tags "p")
         (when (string/= (node-name (last-open-element)) "p")
           (perror :unexpected-end-tag :name "p"))
         (let ((node (pop-end open-elements)))
           (loop until (string= (node-name node) "p")
                 do (setf node (pop-end open-elements))))))
  nil)

(def :in-body end-tag-body (open-elements)
  (block nil
    (when (not (element-in-scope "body"))
      (perror :unexpected-scope)
      (return nil))
    (when (string/= (node-name (last-open-element)) "body")
      (loop for node in (cddr open-elements)
            do (when (member (node-name node)
                             '("dd" "dt" "li" "optgroup" "option" "p" "rp"
                               "rt" "tbody" "td" "tfoot" "th" "thead" "tr"
                               "body" "html")
                             :test #'string=)
                 ;;Not sure this is the correct name for the parse error
                 (perror :expected-one-end-tag-but-got-another
                         :expected-name "body" :got-name (node-name node))
                 (return)))))
  (setf (parser-phase *parser*) :after-body)
  nil)

(def :in-body end-tag-html ()
  ;;We repeat the test for the body end tag token being ignored here
  (cond ((element-in-scope "body")
         (end-tag-body (implied-tag-token "body"))
         token)
        (t nil)))

(def :in-body end-tag-block (in-body-process-space-characters-mode open-elements)
  ;;Put us back in the right whitespace handling mode
  (when (string= (getf token :name) "pre")
    (setf in-body-process-space-characters-mode :non-pre))
  (let ((in-scope (element-in-scope (getf token :name))))
    (when in-scope
      (generate-implied-end-tags))
    (when (string/= (node-name (last-open-element))
                    (getf token :name))
      (perror :end-tag-too-early :name (getf token :name)))
    (when in-scope
      (let ((node (pop-end open-elements)))
        (loop until (string= (node-name node) (getf token :name))
              do (setf node (pop-end open-elements))))))
  nil)

(def :in-body end-tag-form (form-pointer open-elements)
  (let ((node form-pointer))
    (setf form-pointer nil)
    (if (or (null node) (not (element-in-scope (node-name node))))
        (perror :unexpected-end-tag :name "form")
        (progn
          (generate-implied-end-tags)
          (when (not (equal (last-open-element) node))
            (perror :end-tag-too-early-ignored :name "form"))
          (setf open-elements
                (remove node open-elements)))))
  nil)

;;; Note to self:
;;;   - A token is a plist.
;;;   - A property is an alist.
;;;   - A node is an object.
;;;   - An element is a node.

(def :in-body end-tag-list-item (open-elements)
  (let ((variant (if (string= (getf token :name) "li")
                     "list"
                     nil)))
    (if (not (element-in-scope (getf token :name) variant))
        (perror :unexpected-end-tag :name (getf token :name))
        (progn
          (generate-implied-end-tags (getf token :name))
          (when (string/= (node-name (last-open-element))
                          (getf token :name))
            (perror :end-tag-too-early :name (getf token :name)))
          (let ((node (pop-end open-elements)))
            (loop until (string= (node-name node) (getf token :name))
                  do (setf node (pop-end open-elements)))))))
  nil)

(def :in-body end-tag-heading (open-elements)
  (loop for item in +heading-elements+
        do (when (element-in-scope item)
             (generate-implied-end-tags)
             (return)))
  (when (string/= (node-name (last-open-element))
                  (getf token :name))
    (perror :end-tag-too-early :name (getf token :name)))
  (loop for item in +heading-elements+
        do (when (element-in-scope item)
             (let ((item (pop-end open-elements)))
               (loop until (member (node-name item) +heading-elements+
                                   :test #'string=)
                     do (setf item (pop-end open-elements))))))
  nil)

(defmacro insert-elt-at (object index place)
  (let ((tmp (gensym "TMP"))
        (object-symbol (gensym "OBJECT"))
        (index-symbol (gensym "INDEX")))
    `(let ((,object-symbol ,object)
           (,index-symbol ,index)
           (,tmp ,place))
       (setf ,place (append (subseq ,tmp 0 (min ,index-symbol (length ,tmp)))
                            (list ,object-symbol)
                            (nthcdr ,index-symbol ,tmp))))))

(def :in-body end-tag-formatting (active-formatting-elements open-elements)
  ;; The much-feared adoption agency algorithm
  ;; http://www.whatwg.org/specs/web-apps/current-work/#adoptionAgency
  ;; XXX Better parseError messages appreciated.
  (loop named outer
        with name = (getf token :name)
        with outer-loop-counter = 0
        with formatting-element
        with afe-index
        with furthest-block
        with bookmark
        with last-node
        with inner-loop-counter
        with index
        with node
        with common-ancestor
        with clone
        while (< outer-loop-counter 8)
        do
        (incf outer-loop-counter)

        ;; Step 1 paragraph 1
        (setf formatting-element
              (element-in-active-formatting-elements name))
        (cond ((or (not formatting-element)
                   (and (member formatting-element
                                open-elements)
                        (not (element-in-scope
                              (node-name formatting-element)))))
               (perror :adoption-agency-1.1 :name name)
               (return-from outer nil))

              ;; Step 1 paragraph 2
              ((not (member formatting-element
                            open-elements))
               (perror :adoption-agency-1.2 :name name)
               (setf active-formatting-elements
                     (remove formatting-element active-formatting-elements))
               (return-from outer nil)))

        ;; Step 1 paragraph 3
        (unless (eql formatting-element
                     (last-open-element))
          (perror :adoption-agency-1.3 :name name))


        ;; Step 2
        ;; Start of the adoption agency algorithm proper
        (setf afe-index (position formatting-element
                                  open-elements))
        (setf furthest-block nil)
        (loop for element in (subseq open-elements
                                     afe-index)
              do (when (member (node-name-tuple element)
                               +special-elements+
                               :test #'equal)
                   (setf furthest-block element)
                   (return)))
        ;; Step 3
        (when (null furthest-block)
          (loop for element = (pop-end open-elements)
                until (eql formatting-element element)
                finally (setf active-formatting-elements
                              (remove element
                                      active-formatting-elements)))
          (return-from outer nil))
        (setf common-ancestor (elt open-elements (- afe-index 1)))

        ;; Step 5
        ;;if furthestBlock.parent:
        ;;    furthestBlock.parent.removeChild(furthestBlock)

        ;; Step 5
        ;; The bookmark is supposed to help us
        ;; identify where to reinsert nodes in step
        ;; 12. We have to ensure that we reinsert
        ;; nodes after the node before the active
        ;; formatting element.  Note the bookmark can
        ;; move in step 7.4
        (setf bookmark (position formatting-element
                                 active-formatting-elements))

        ;; Step 6
        (setf node furthest-block)
        (setf last-node node)
        (setf inner-loop-counter 0)

        (setf index (position node open-elements))
        (loop named inner
              while (< inner-loop-counter 3)
              do
              (block continue
                (incf inner-loop-counter)
                ;; Node is element before node in open elements
                (decf index)
                (setf node (elt open-elements index))
                (when (not (member node active-formatting-elements))
                  (setf open-elements
                        (remove node open-elements))
                  (return-from continue))
                ;; Step 6.3
                (when (eql node formatting-element)
                  (return-from inner))
                ;; Step 6.4
                (when (eql last-node furthest-block)
                  (setf bookmark (1+ (position node
                                               active-formatting-elements))))
                ;; Step 6.5
                (setf clone (node-clone* node))
                ;; Replace node with clone
                (symbol-macrolet
                    ((af active-formatting-elements)
                     (oe open-elements))
                  (setf (elt af (position node af)) clone)
                  (setf (elt oe (position node oe)) clone))
                (setf node clone)

                ;; Step 6.6
                ;; Remove lastNode from its parents, if any
                (when (node-parent last-node)
                  (node-remove-child (node-parent last-node)
                                     last-node))
                (node-append-child node last-node)

                ;; Step 7.7
                (setf last-node node)
                ;; End of inner loop
                ))

        ;; Step 7
        ;; Foster parent lastNode if commonAncestor is a
        ;; table, tbody, tfoot, thead, or tr we need to
        ;; foster parent the lastNode
        (when (node-parent last-node)
          (node-remove-child (node-parent last-node)
                             last-node))

        (if (member (node-name common-ancestor)
                    '("table" "tbody" "tfoot" "thead" "tr")
                    :test #'string=)
            (multiple-value-bind (parent insert-before)
                (get-table-misnested-nodeposition)
              (node-insert-before* parent last-node insert-before))
            (node-append-child* common-ancestor last-node))

        ;; Step 8
        (setf clone (node-clone* formatting-element))

        ;; Step 9
        (node-reparent-children furthest-block clone)

        ;; Step 10
        (node-append-child* furthest-block clone)

        ;; Step 11
        (setf active-formatting-elements
              (remove formatting-element
                      active-formatting-elements))
        (insert-elt-at clone bookmark active-formatting-elements)

        ;; Step 12
        (setf open-elements
              (remove formatting-element
                      open-elements))
        (insert-elt-at clone
                       (1+ (position furthest-block
                                     open-elements))
                       open-elements))
  nil)

(def :in-body end-tag-applet-marquee-object (open-elements)
  (when (element-in-scope (getf token :name))
    (generate-implied-end-tags))
  (when (string/= (node-name (last-open-element))
                  (getf token :name))
    (perror :end-tag-too-early :name (getf token :name)))
  (when (element-in-scope (getf token :name))
    (let ((element (pop-end open-elements)))
      (loop until (string= (node-name element) (getf token :name))
            do (setf element (pop-end open-elements))))
    (clear-active-formatting-elements))
  nil)

(def :in-body end-tag-br (open-elements)
  (perror :unexpected-end-tag-treated-as
          :original-name "br" :new-name "br element")
  (reconstruct-active-formatting-elements)
  (insert-element (implied-tag-token "br" :start-tag))
  (pop-end open-elements)
  nil)

(def :in-body end-tag-other (open-elements)
  (loop for node in (reverse open-elements)
        do (cond ((string= (node-name node) (getf token :name))
                  (generate-implied-end-tags (getf token :name))
                  (when (string/= (node-name (last-open-element))
                                  (getf token :name))
                    (perror :unexpected-end-tag :name (getf token :name)))
                  (loop while (not (eq node
                                       (pop-end open-elements))))
                  (return))
                 (t
                  (when (member (node-name-tuple node) +special-elements+
                                :test #'equal)
                    (perror :unexpected-end-tag :name (getf token :name))
                    (return)))))
  nil)


;; TextPhase

(tagname-dispatch :text process-start-tag
  (default start-tag-other))

(tagname-dispatch :text process-end-tag
  ("script" end-tag-script)
  (default end-tag-other))

(def :text process-characters ()
  (parser-insert-text (getf token :data))
  nil)

(def :text process-eof (phase original-phase open-elements)
  (perror :expected-named-closing-tag-but-got-eof
          (node-name (last-open-element)))
  (pop-end open-elements)
  (setf phase original-phase)
  t)

(def :text start-tag-other ()
  (error "Tried to process start tag ~S in RCDATA/RAWTEXT mode" (getf token :name)))

(def :text end-tag-script (phase original-phase open-elements)
  (assert (string= (node-name (pop-end open-elements))
                   "script"))
  (setf phase original-phase)
  ;; The rest of this method is all stuff that only happens if
  ;; document.write works
  nil)

(def :text end-tag-other (phase original-phase open-elements)
  (pop-end open-elements)
  (setf phase original-phase)
  nil)


;; InTablePhase
;; http://www.whatwg.org/specs/web-apps/current-work/#in-table

(tagname-dispatch :in-table process-start-tag
  ("html" start-tag-html)
  ("caption" start-tag-caption)
  ("colgroup" start-tag-colgroup)
  ("col" start-tag-col)
  (("tbody" "tfoot" "thead") start-tag-row-group)
  (("td" "th" "tr") start-tag-imply-tbody)
  ("table" start-tag-table)
  (("style" "script") start-tag-style-script)
  ("input" start-tag-input)
  ("form" start-tag-form)
  (default start-tag-other))

(tagname-dispatch :in-table process-end-tag
  ("table" end-Tag-Table)
  (("body" "caption" "col" "colgroup" "html" "tbody" "td"
           "tfoot" "th" "thead" "tr") end-Tag-Ignore)
  (default end-tag-other))

(flet ((clear-stack-to-table-context ()
         ;; clear the stack back to a table context
         (loop until (member (node-name (last-open-element))
                             '("table" "html")
                             :test #'string=)
            do
              ;;(perror :unexpected-implied-end-tag-in-table
              ;;        :name (node-name* (last-open-element)))
              (pop-end (slot-value *parser* 'open-elements)))
         ;; When the current node is <html> it's an innerHTML case
         ))

  (def :in-table process-eof (inner-html)
    (if (string/= (node-name (last-open-element)) "html")
        (perror :eof-in-table)
        (assert inner-html))
    ;; Stop parsing
    nil)

  (def :in-table process-space-characters (phase original-phase)
    (setf original-phase phase)
    (setf phase :in-table-text)
    (process-space-characters token :phase phase)
    nil)

  (def :in-table process-characters (phase original-phase)
    (setf original-phase phase)
    (setf phase :in-table-text)
    (process-characters token :phase phase)
    nil)

  (def :in-table insert-text (insert-from-table)
    ;; If we get here there must be at least one non-whitespace character
    ;; Do the table magic!
    (setf insert-from-table t)
    (process-characters token :phase :in-body)
    (setf insert-from-table nil)
    nil)

  (def :in-table start-tag-caption (phase active-formatting-elements)
    (clear-stack-to-table-context)
    (push-end :marker active-formatting-elements)
    (insert-element token)
    (setf phase :in-caption)
    nil)

  (def :in-table start-tag-colgroup (phase)
    (clear-stack-to-table-context)
    (insert-element token)
    (setf phase :in-column-group)
    nil)

  (def :in-table start-tag-col ()
    (start-tag-colgroup (implied-tag-token "colgroup" :start-tag))
    token)

  (def :in-table start-tag-row-group (phase)
    (clear-stack-to-table-context)
    (insert-element token)
    (setf phase :in-table-body)
    nil)

  (def :in-table start-tag-imply-tbody ()
    (start-tag-row-group (implied-tag-token "tbody" :start-tag))
    token)

  (def :in-table start-tag-table (phase inner-html)
    (perror :unexpected-start-tag-implies-end-tag
            :start-name "table"
            :end-name "table")
    (process-end-tag (implied-tag-token "table") :phase phase)
    (unless inner-html
      token))

  (def :in-table start-tag-style-script ()
    (process-start-tag token :phase :in-head))

  (def :in-table start-tag-input (open-elements)
    (let ((type (assoc "type" (getf token :data) :test #'string=)))
      (cond ((and type
                  (string= (ascii-upper-2-lower (cdr type)) "hidden"))
             (perror :unexpected-hidden-input-in-table)
             (insert-element token)
              ;; XXX associate with form
             (pop-end open-elements))
            (t
             (start-tag-other token))))
    nil)

  (def :in-table start-tag-form (form-pointer open-elements)
    (perror :unexpected-form-in-table)
    (unless form-pointer
      (insert-element token)
      (setf form-pointer (last-open-element))
      (pop-end open-elements))
    nil)

  (def :in-table start-tag-other (insert-from-table)
    (perror :unexpected-start-tag-implies-table-voodoo :name (getf token :name))
    ;; Do the table magic!
    (setf insert-from-table t)
    (process-start-tag token :phase :in-body)
    (setf insert-from-table nil)
    nil)

  (def :in-table end-tag-table (inner-html open-elements)
    (cond ((element-in-scope "table" "table")
           (generate-implied-end-tags)
           (unless (equal (node-name (last-open-element)) "table")
             (perror :end-tag-too-early-named
                     :got-name "table"
                     :expected-name (node-name (last-open-element))))
           (loop until (equal (node-name (last-open-element)) "table")
              do (pop-end open-elements))
           (pop-end open-elements)
           (reset-insertion-mode))
          (t
           ;; innerHTML case
           (assert inner-html)
           (perror :end-tag-table-in-table-inner-html-case)))
    nil)

  (def :in-table end-tag-ignore ()
    (perror :unexpected-end-tag :name (getf token :name))
    nil)

  (def :in-table end-tag-other (insert-from-table)
    (perror :unexpected-end-tag-implies-table-voodoo :name (getf token :name))
    ;; Do the table magic!
    (setf insert-from-table t)
    (process-end-tag token :phase :in-body)
    (setf insert-from-table nil)
    nil))


;; InTableTextPhase

(defun flush-characters ()
  (with-slots (character-tokens) *parser*
    (let ((data (apply #'concatenate 'string
                       (loop for item in (reverse character-tokens)
                             collect (getf item :data)))))
      (if (not (only-space-characters-p data))
          (insert-text (list :type :characters
                             :data data)
                       :phase :in-table)
          (parser-insert-text data)))
    (setf character-tokens nil)))

(def :in-table-text process-comment (phase original-phase)
  (flush-characters)
  (setf phase original-phase)
  token)

(def :in-table-text process-eof (phase original-phase)
  (flush-characters)
  (setf phase original-phase)
  t)

(def :in-table-text process-characters (character-tokens)
  (unless (equal (getf token :data) (string #\u0000))
    (push token character-tokens))
  nil)

(def :in-table-text process-space-characters (character-tokens)
  ;; pretty sure we should never reach here
  (push token character-tokens)
  nil)

(def :in-table-text process-start-tag (phase original-phase)
  (flush-characters)
  (setf phase original-phase)
  token)

(def :in-table-text process-end-tag (phase original-phase)
  (flush-characters)
  (setf phase original-phase)
  token)


;; InCaptionPhase
;; http://www.whatwg.org/specs/web-apps/current-work/#in-caption

(tagname-dispatch :in-caption process-start-tag
  ("html" start-tag-html)
  (("caption" "col" "colgroup" "tbody" "td" "tfoot" "th"
              "thead" "tr") start-tag-table-element)
  (default start-tag-other))

(tagname-dispatch :in-caption process-end-tag
  ("caption" end-tag-caption)
  ("table" end-tag-table)
  (("body" "col" "colgroup" "html" "tbody" "td" "tfoot" "th"
           "thead" "tr") end-tag-ignore)
  (default end-tag-other))

(flet ((ignore-end-tag-caption ()
         (not (element-in-scope "caption" "table"))))

  (def :in-caption process-eof ()
    (process-eof token :phase :in-body))

  (def :in-caption process-characters ()
    (process-characters token :phase :in-body))

  (def :in-caption start-tag-table-element (phase)
    (perror :start-tag-table-element-in-caption)
    ;; XXX Have to duplicate logic here to find out if the tag is ignored
    (prog1 (unless (ignore-end-tag-caption)
             token)
      (process-end-tag (implied-tag-token "caption") :phase phase)))

  (def :in-caption start-tag-other ()
    (process-start-tag token :phase :in-body))

  (def :in-caption end-tag-caption (phase inner-html open-elements)
    (cond ((not (ignore-end-tag-caption))
           ;; AT this code is quite similar to endTagTable in "InTable"
           (generate-implied-end-tags)
           (unless (equal (node-name (last-open-element)) "caption")
             (perror :expected-one-end-tag-but-got-another
                     :got-name "caption"
                     :expected-name (node-name (last-open-element))))
           (loop until (equal (node-name (last-open-element)) "caption")
              do (pop-end open-elements))
           (clear-active-formatting-elements)
           (setf phase :in-table))
          (t
           ;; innerHTML case
           (assert inner-html)
           (perror :end-tag-caption-in-caption-inner-html-mode)))
    nil)

  (def :in-caption end-tag-table (phase)
    (perror :end-tag-table-in-caption)
    (prog1 (unless (ignore-end-tag-caption)
             token)
      (process-end-tag (implied-tag-token "caption") :phase phase)))

  (def :in-caption end-tag-ignore ()
    (perror :unexpected-end-tag :name (getf token :name))
    nil)

  (def :in-caption end-tag-other ()
    (process-end-tag token :phase :in-body)))


;; InColumnGroupPhase
;; http://www.whatwg.org/specs/web-apps/current-work/#in-column

(tagname-dispatch :in-column-group process-start-tag
  ("html" start-tag-html)
  ("col" start-tag-col)
  (default start-tag-other))

(tagname-dispatch :in-column-group process-end-tag
  ("colgroup" end-tag-colgroup)
  ("col" end-tag-col)
  (default end-tag-other))


(flet ((ignore-end-tag-colgroup ()
         (string= (node-name (last-open-element)) "html")))

  (def :in-column-group process-eof (inner-html)
    (cond ((string= (node-name (last-open-element)) "html")
           (assert inner-html)
           nil)
          (t
           (let ((ignore-end-tag (ignore-end-tag-colgroup)))
             (end-tag-colgroup (implied-tag-token "colgroup"))
             (not ignore-end-tag)))))

  (def :in-column-group process-characters ()
    (prog1 (unless (ignore-end-tag-colgroup)
             token)
      (end-tag-colgroup (implied-tag-token "colgroup"))))

  (def :in-column-group start-tag-col (open-elements)
    (insert-element token)
    (pop-end open-elements)
    nil)

  (def :in-column-group start-tag-other ()
    (prog1 (unless (ignore-end-tag-colgroup)
             token)
      (end-tag-colgroup (implied-tag-token "colgroup"))))

  (def :in-column-group end-tag-colgroup (phase open-elements)
    (cond ((ignore-end-tag-colgroup)
           ;; innerHTML case
           (perror :end-tag-colgroup-in-column-group-inner-html-mode))
          (t
           (pop-end open-elements)
           (setf phase :in-table)))
    nil)

  (def :in-column-group end-tag-col ()
    (perror :no-end-tag :name "col")
    nil)

  (def :in-column-group end-tag-other ()
    (prog1 (unless (ignore-end-tag-colgroup)
             token)
      (end-tag-colgroup (implied-tag-token "colgroup")))))


;; InTableBodyPhase
;; http://www.whatwg.org/specs/web-apps/current-work/#in-table0

(tagname-dispatch :in-table-body process-start-tag
  ("html" start-tag-html)
  ("tr" start-tag-tr)
  (("td" "th") start-tag-table-cell)
  (("caption" "col" "colgroup" "tbody" "tfoot" "thead") start-tag-table-other)
  (default start-tag-other))

(tagname-dispatch :in-table-body process-end-tag
  (("tbody" "tfoot" "thead") end-Tag-Table-Row-Group)
  ("table" end-Tag-Table)
  (("body" "caption" "col" "colgroup" "html" "td" "th" "tr") end-Tag-Ignore)
  (default end-tag-other))

(flet ((clear-stack-to-table-body-context ()
         (loop until (member (node-name (last-open-element))
                             '("tbody" "tfoot" "thead" "html")
                             :test #'string=)
            do
              ;;(perror :unexpected-implied-end-tag-in-table
              ;;        :name (node-name (last-open-element)))
              (pop-end (slot-value *parser* 'open-elements)))
         (when (string= (node-name (last-open-element)) "html")
           (assert (slot-value *parser* 'inner-html)))))

  (def :in-table-body process-eof ()
    (process-eof token :phase :in-table))

  (def :in-table-body process-space-characters ()
    (process-space-characters token :phase :in-table))

  (def :in-table-body process-characters ()
    (process-characters token :phase :in-table))

  (def :in-table-body start-tag-tr (phase)
    (clear-stack-to-table-body-context)
    (insert-element token)
    (setf phase :in-row)
    nil)

  (def :in-table-body start-tag-table-cell ()
    (perror :unexpected-cell-in-table-body :name (getf token :name))
    (start-tag-tr (implied-tag-token "tr" :start-tag))
    token)

  (def :in-table-body start-tag-table-other (inner-html)
    ;; XXX AT Any ideas on how to share this with endTagTable?
    (cond ((or (element-in-scope "tbody" "table")
               (element-in-scope "thead" "table")
               (element-in-scope "tfoot" "table"))
           (clear-stack-to-table-body-context)
           (end-tag-table-row-group
            (implied-tag-token (node-name (last-open-element))))
           token)
          (t
           ;; innerHTML case
           (assert inner-html)
           (perror :start-tag-table-other-in-table-body-inner-html-mode)
           nil)))

  (def :in-table-body start-tag-other ()
    (process-start-tag token :phase :in-table))

  (def :in-table-body end-tag-table-row-group (phase open-elements)
    (cond ((element-in-scope (getf token :name) "table")
           (clear-stack-to-table-body-context)
           (pop-end open-elements)
           (setf phase :in-table))
          (t
           (perror :unexpected-end-tag-in-table-body :name (getf token :name))))
    nil)

  (def :in-table-body end-tag-table (inner-html)
    (cond ((or (element-in-scope "tbody" "table")
                (element-in-scope "thead" "table")
                (element-in-scope "tfoot" "table"))
           (clear-stack-to-table-body-context)
           (end-tag-table-row-group
            (implied-tag-token (node-name (last-open-element))))
           token)
          (t
           ;; innerHTML case
           (assert inner-html)
           (perror :end-tag-table-other-in-table-body-inner-html-mode)
           nil)))

  (def :in-table-body end-tag-ignore ()
    (perror :unexpected-end-tag-in-table-body :name (getf token :name))
    nil)

  (def :in-table-body end-tag-other ()
    (process-end-tag token :phase :in-table)))

;; InRowPhase
;; http://www.whatwg.org/specs/web-apps/current-work/#in-row

(tagname-dispatch :in-row process-start-tag
  ("html" start-tag-html)
  (("td" "th") start-tag-table-cell)
  (("caption" "col" "colgroup" "tbody" "tfoot" "thead" "tr")
   start-tag-table-other)
  (default start-tag-other))

(tagname-dispatch :in-row process-end-tag
  ("tr" end-tag-tr)
  ("table" end-tag-table)
  (("tbody" "tfoot" "thead") end-tag-table-row-group)
  (("body" "caption" "col" "colgroup" "html" "td" "th") end-tag-ignore)
  (default end-tag-other))


;; helper methods (XXX unify this with other table helper methods)
(flet ((clear-stack-to-table-row-context ()
         (loop until (member (node-name (last-open-element))
                             '("tr" "html")
                             :test #'string=)
            do
              (perror :unexpected-implied-end-tag-in-table-row
                      :name (node-name (last-open-element)))
              (pop-end (slot-value *parser* 'open-elements))))

       (ignore-end-tag-tr ()
         (not (element-in-scope "tr" "table"))))

  ;; the rest
  (def :in-row process-eof ()
    (process-eof token :phase :in-table)
    nil)

  (def :in-row process-space-characters ()
    (process-space-characters token :phase :in-table))

  (def :in-row process-characters ()
    (process-characters token :phase :in-table))

  (def :in-row start-tag-table-cell (phase active-formatting-elements)
    (clear-stack-to-table-row-context)
    (insert-element token)
    (setf phase :in-cell)
    (push-end :marker active-formatting-elements)
    nil)

  (def :in-row start-tag-table-other ()
    (let ((ignore-end-tag (ignore-end-tag-tr)))
      (end-tag-tr (implied-tag-token "tr"))
       ;; XXX how are we sure it's always ignored in the innerHTML case?
      (unless ignore-end-tag
        token)))

  (def :in-row start-tag-other ()
    (process-start-tag token :phase :in-table))

  (def :in-row end-tag-tr (phase inner-html open-elements)
    (cond ((not (ignore-end-tag-tr))
           (clear-stack-to-table-row-context)
           (pop-end open-elements)
           (setf phase :in-table-body))
          (t
           ;; innerHTML case
           (assert inner-html)
           (perror :end-tag-tr-inner-html-mode)))
    nil)

  (def :in-row end-tag-table ()
    (let ((ignore-end-tag (ignore-end-tag-tr)))
      (end-tag-tr (implied-tag-token "tr"))
      ;; Reprocess the current tag if the tr end tag was not ignored
      ;; XXX how are we sure it's always ignored in the innerHTML case?
      (unless ignore-end-tag
        token)))

  (def :in-row end-tag-table-row-group ()
    (cond ((element-in-scope (getf token :name) "table")
           (end-tag-tr (implied-tag-token "tr"))
           token)
          (t
           (perror :end-tag-table-row-group-something-wrong)
           nil)))

  (def :in-row end-tag-ignore ()
    (perror :unexpected-end-tag-in-table-row (getf token :name))
    nil)

  (def :in-row end-tag-other ()
    (process-end-tag token :phase :in-table)))


;; InCellPhase
;; http://www.whatwg.org/specs/web-apps/current-work/#in-cell

(tagname-dispatch :in-cell process-start-tag
  ("html" start-tag-html)
  (("caption" "col" "colgroup" "tbody" "td" "tfoot" "th" "thead" "tr")
   start-tag-table-other)
  (default start-tag-other))

(tagname-dispatch :in-cell process-end-tag
  (("td" "th") end-tag-table-cell)
  (("body" "caption" "col" "colgroup" "html") end-tag-ignore)
  (("table" "tbody" "tfoot" "thead" "tr") end-tag-imply)
  (default end-tag-other))

(flet ((close-cell ()
         (if (element-in-scope "td" "table")
             (end-tag-table-cell (implied-tag-token "td"))
             (if (element-in-scope "th" "table")
                 (end-tag-table-cell (implied-tag-token "th"))))))

  (def :in-cell process-eof ()
    (process-eof token :phase :in-body)
    nil)

  (def :in-cell process-characters ()
    (process-characters token :phase :in-body))

  (def :in-cell start-tag-table-other (inner-html)
    (cond ((or (element-in-scope "td" "table")
               (element-in-scope "th" "table"))
           (close-cell)
           token)
          (t
           ;; innerHTML case
           (assert inner-html)
           (perror :start-tag-table-other-in-inner-html-mode)
           nil)))

  (def :in-cell start-tag-other ()
    (process-start-tag token :phase :in-body))

  (def :in-cell end-tag-table-cell (phase open-elements)
    (cond ((element-in-scope (getf token :name) "table")
           (generate-implied-end-tags (getf token :name))
           (cond ((not (equal (node-name (last-open-element))
                              (getf token :name)))
                  (perror :unexpected-cell-end-tag :name (getf token :name))
                  (loop until (equal (node-name (pop-end open-elements))
                                     (getf token :name))))
                 (t
                  (pop-end open-elements)))
           (clear-active-formatting-elements)
           (setf phase :in-row))
          (t
           (perror :unexpected-end-tag :name (getf token :name))))
    nil)

  (def :in-cell end-tag-ignore ()
    (perror :unexpected-end-tag :name (getf token :name))
    nil)

  (def :in-cell end-tag-imply ()
    (cond ((element-in-scope (getf token :name) "table")
           (close-cell)
           token)
          (t
           ;; sometimes innerHTML case
           (perror :end-tag-imply-sometimes-inner-html-case)
           nil)))

  (def :in-cell end-tag-other ()
    (process-end-tag token :phase :in-body)))


;; InSelectPhase

(tagname-dispatch :in-select process-start-tag
  ("html" start-tag-html)
  ("option" start-tag-option)
  ("optgroup" start-tag-optgroup)
  ("select" start-tag-select)
  (("input" "keygen" "textarea") start-tag-input)
  ("script" start-tag-script)
  (default start-tag-other))

(tagname-dispatch :in-select process-end-tag
  ("option" end-tag-option)
  ("optgroup" end-tag-optgroup)
  ("select" end-tag-select)
  (default end-tag-other))

;; http://www.whatwg.org/specs/web-apps/current-work/#in-select
(def :in-select process-eof (inner-html)
  (if (not (equal (node-name (last-open-element)) "html"))
      (perror :eof-in-select)
      (assert inner-html))
  nil)

(def :in-select process-characters ()
  (unless (equal (getf token :data) (string #\u0000))
    (parser-insert-text (getf token :data)))
  nil)

(def :in-select start-tag-option (open-elements)
  ;; We need to imply </option> if <option> is the current node.
  (when (equal (node-name (last-open-element)) "option")
    (pop-end open-elements))
  (insert-element token)
  nil)

(def :in-select start-tag-optgroup (open-elements)
  (when (equal (node-name (last-open-element)) "option")
    (pop-end open-elements))
  (when (equal (node-name (last-open-element)) "optgroup")
    (pop-end open-elements))
  (insert-element token)
  nil)

(def :in-select start-tag-select ()
  (perror :unexpected-select-in-select)
  (end-tag-select (implied-tag-token "select"))
  nil)

(def :in-select start-tag-input (inner-html)
  (perror :unexpected-input-in-select)
  (cond ((element-in-scope "select" "select")
         (end-tag-select (implied-tag-token "select"))
         token)
        (t
         (assert inner-html)
         nil)))

(def :in-select start-tag-script ()
  (process-start-tag token :phase :in-head))

(def :in-select start-tag-other ()
  (perror :unexpected-start-tag-in-select :name (getf token :name))
  nil)

(def :in-select end-tag-option (open-elements)
  (if (equal (node-name (last-open-element)) "option")
      (pop-end open-elements)
      (perror :unexpected-end-tag-in-select :name (getf token :name)))
  nil)

(def :in-select end-tag-optgroup (open-elements)
  ;; </optgroup> implicitly closes <option>
  (when  (and (equal (node-name (last-open-element)) "option")
              (equal (node-name (elt open-elements
                                     (- (length open-elements) 2)))
                     "optgroup"))
    (pop-end open-elements))
  ;; It also closes </optgroup>
  (if (equal (node-name (last-open-element)) "optgroup")
      (pop-end open-elements)
      ;; But nothing else
      (perror :unexpected-end-tag-in-select :name (getf token :name)))
  nil)

(def :in-select end-tag-select (inner-html open-elements)
  (cond ((element-in-scope "select" "select")
         (loop until (equal (node-name (pop-end open-elements))
                            "select"))
         (reset-insertion-mode))
        (t
         ;; innerHTML case
         (assert inner-html)
         (perror :end-tag-select-in-inner-html-mode)))
  nil)

(def :in-select end-tag-other ()
  (perror :unexpected-end-tag-in-select :name (getf token :name))
  nil)


;; InSelectInTablePhase

(tagname-dispatch :in-select-in-table process-start-tag
  (("caption" "table" "tbody" "tfoot" "thead" "tr" "td" "th") start-tag-table)
  (default start-tag-other))

(tagname-dispatch :in-select-in-table process-end-tag
  (("caption" "table" "tbody" "tfoot" "thead" "tr" "td" "th") end-tag-table)
  (default end-tag-other))

(def :in-select-in-table process-eof ()
  (process-eof token :phase :in-select)
  nil)

(def :in-select-in-table process-characters ()
  (process-characters token :phase :in-select))

(def :in-select-in-table start-tag-table ()
  (perror :unexpected-table-element-start-tag-in-select-in-table :name (getf token :name))
  (end-tag-other (implied-tag-token "select"))
  token)

(def :in-select-in-table start-tag-other ()
  (process-start-tag token :phase :in-select))

(def :in-select-in-table end-tag-table ()
  (perror :unexpected-table-element-end-tag-in-select-in-table :name (getf token :name))
  (cond ((element-in-scope (getf token :name) "table")
         (end-tag-other (implied-tag-token "select"))
         token)
        (t
         nil)))

(def :in-select-in-table end-tag-other ()
  (process-end-tag token :phase :in-select))


;; InForeignContentPhase

(defparameter +breakout-elements+
  '("b" "big" "blockquote" "body" "br"
    "center" "code" "dd" "div" "dl" "dt"
    "em" "embed" "h1" "h2" "h3"
    "h4" "h5" "h6" "head" "hr" "i" "img"
    "li" "listing" "menu" "meta" "nobr"
    "ol" "p" "pre" "ruby" "s"  "small"
    "span" "strong" "strike"  "sub" "sup"
    "table" "tt" "u" "ul" "var"))


(defun adjust-svg-tag-names (token)
  (let ((replacement (cdr
                      (assoc (getf token :name)
                             '(("altglyph" . "altGlyph")
                               ("altglyphdef" . "altGlyphDef")
                               ("altglyphitem" . "altGlyphItem")
                               ("animatecolor" . "animateColor")
                               ("animatemotion" . "animateMotion")
                               ("animatetransform" . "animateTransform")
                               ("clippath" . "clipPath")
                               ("feblend" . "feBlend")
                               ("fecolormatrix" . "feColorMatrix")
                               ("fecomponenttransfer" . "feComponentTransfer")
                               ("fecomposite" . "feComposite")
                               ("feconvolvematrix" . "feConvolveMatrix")
                               ("fediffuselighting" . "feDiffuseLighting")
                               ("fedisplacementmap" . "feDisplacementMap")
                               ("fedistantlight" . "feDistantLight")
                               ("feflood" . "feFlood")
                               ("fefunca" . "feFuncA")
                               ("fefuncb" . "feFuncB")
                               ("fefuncg" . "feFuncG")
                               ("fefuncr" . "feFuncR")
                               ("fegaussianblur" . "feGaussianBlur")
                               ("feimage" . "feImage")
                               ("femerge" . "feMerge")
                               ("femergenode" . "feMergeNode")
                               ("femorphology" . "feMorphology")
                               ("feoffset" . "feOffset")
                               ("fepointlight" . "fePointLight")
                               ("fespecularlighting" . "feSpecularLighting")
                               ("fespotlight" . "feSpotLight")
                               ("fetile" . "feTile")
                               ("feturbulence" . "feTurbulence")
                               ("foreignobject" . "foreignObject")
                               ("glyphref" . "glyphRef")
                               ("lineargradient" . "linearGradient")
                               ("radialgradient" . "radialGradient")
                               ("textpath" . "textPath"))
                             :test #'string=))))
    (when replacement
      (setf (getf token :name) replacement))))


(defparameter +only-space-characters-regexp+
  (cl-ppcre:create-scanner `(:sequence :start-anchor
                                       (:greedy-repetition
                                        0 nil
                                        (:alternation ,@(coerce +space-characters+ 'list)))
                                       :end-anchor)
                           :multi-line-mode t))

(defun only-space-characters-p (string)
  (cl-ppcre:scan +only-space-characters-regexp+ string))

(def :in-foreign-content process-characters (frameset-ok)
  (cond ((equal (getf token :data) (string #\u0000))
         (setf (getf token :data) (string #\uFFFD)))
        ((and frameset-ok
              (not (only-space-characters-p (getf token :data))))
         (setf frameset-ok nil)))
  (process-characters token :phase nil)
  nil)

(def :in-foreign-content process-start-tag (html-namespace open-elements)
  (block nil
    (let ((current-node (last-open-element)))
      (cond ((or (member (getf token :name) +breakout-elements+ :test #'string=)
                 (and (string= (getf token :name) "font")
                      (intersection (mapcar #'car (getf token :data))
                                    '("color" "face" "size")
                                    :test #'string=)))
             (parser-parse-error :unexpected-html-element-in-foreign-content
                                 (getf token :name))
             (loop until (or (is-html-integration-point (last-open-element))
                             (is-math-ml-text-integration-point (last-open-element))
                             (equal (node-namespace (last-open-element))
                                    html-namespace))
                   do (pop-end open-elements))
             (return token))
            (t
             (cond ((equal (node-namespace current-node) (find-namespace "mathml"))
                    (adjust-math-ml-attributes token))
                   ((equal (node-namespace current-node) (find-namespace "svg"))
                    (adjust-svg-tag-names token)
                    (adjust-svg-attributes token)))
             (adjust-foreign-attributes token)
             (setf (getf token :namespace) (node-namespace current-node))
             (insert-element token)
             (when (getf token :self-closing)
               (pop-end open-elements)
               (setf (getf token :self-closing-acknowledged) t)))))
    nil))

(def :in-foreign-content process-end-tag (phase original-phase html-namespace open-elements)
  (let ((new-token)
        (node-index (1- (length open-elements)))
        (node (last-open-element)))
    (unless (string= (node-name node) (getf token :name))
      (parser-parse-error :unexpected-end-tag (getf token :name)))

    (loop
     (when (string= (ascii-upper-2-lower (node-name node)) (getf token :name))
       ;; XXX this isn't in the spec but it seems necessary
       (when (eql phase :in-table-text)
         (flush-characters)
         (setf phase original-phase))
       (loop until (eql (pop-end open-elements) node)
             do (assert open-elements))
       (setf new-token nil)
       (return))
     (decf node-index)

     (setf node (elt open-elements node-index))
     (when (equal (node-namespace node)
                  html-namespace)
       (setf new-token (process-end-tag token :phase phase))
       (return)))
    new-token))

;; AfterBodyPhase

(tagname-dispatch :after-body process-start-tag
  ("html" start-tag-html)
  (default start-tag-other))

(tagname-dispatch :after-body process-end-tag
  ("html" end-tag-html)
  (default end-tag-other))

(def :after-body process-eof ()
  ;; Stop parsing
  nil)

(def :after-body process-comment (open-elements)
  ;; This is needed because data is to be appended to the <html> element
  ;; here and not to whatever is currently open.
  (insert-comment token (first open-elements))
  nil)

(def :after-body process-characters (phase)
  (parser-parse-error :unexpected-char-after-body)
  (setf phase :in-body)
  token)

(def :after-body start-tag-html ()
  (process-start-tag token :phase :in-body))

(def :after-body start-tag-other (phase)
   (parser-parse-error :unexpected-start-tag-after-body
                       `(:name ,(getf token :name)))
   (setf phase :in-body)
   token)

(def :after-body end-tag-html (inner-html phase)
  (if inner-html
      (parser-parse-error :unexpected-end-tag-after-body-innerhtml)
      (setf phase :after-after-body))
  nil)

(def :after-body end-tag-other (phase)
  (parser-parse-error :unexpected-end-tag-after-body
                      `(:name ,(getf token :name)))
  (setf phase :in-body)
  token)

;; InFramesetPhase

(tagname-dispatch :in-frameset process-start-tag
  ("html" start-tag-html)
  ("frameset" start-tag-frameset)
  ("frame" start-tag-frame)
  ("noframes"start-tag-noframes)
  (default start-tag-other))

(tagname-dispatch :in-frameset process-end-tag
  ("frameset" end-tag-frameset)
  (default end-tag-other))


(def :in-frameset process-eof (inner-html)
  (if (string/= (node-name (last-open-element)) "html")
      (parser-parse-error :eof-in-frameset)
      (assert inner-html))
  nil)

(def :in-frameset process-characters ()
  (parser-parse-error :unexpected-char-in-frameset)
  nil)

(def :in-frameset start-tag-frameset ()
  (insert-element token)
  nil)

(def :in-frameset start-tag-frame (open-elements)
  (insert-element token)
  (pop-end open-elements)
  nil)

(def :in-frameset start-tag-noframes ()
  (process-start-tag token :phase :in-body))

(def :in-frameset start-tag-other ()
  (parser-parse-error :unexpected-start-tag-in-frameset
                      `(:name ,(getf token :name)))
  nil)

(def :in-frameset end-tag-frameset (phase inner-html open-elements)
  (if (string= (node-name (last-open-element)) "html")
      ;; innerHTML case
      (parser-parse-error :unexpected-frameset-in-frameset-innerhtml)
      (pop-end open-elements))

  (when (and (not inner-html)
             (string/= (node-name (last-open-element)) "frameset"))
    ;; If we're not in innerHTML mode and the the current node is not a
    ;; "frameset" element (anymore) then switch.
    (setf phase :after-frameset))
  nil)

(def :in-frameset end-tag-other ()
  (parser-parse-error :unexpected-end-tag-in-frameset
                      `(:name ,(getf token :name)))
  nil)


;; AfterFramesetPhase

(tagname-dispatch :after-frameset process-start-tag
  ("html" start-tag-html)
  ("noframes" start-tag-noframes)
  (default start-tag-other))

(tagname-dispatch :after-frameset process-end-tag
  ("html" end-tag-html)
  (default end-tag-other))

(def :after-frameset process-eof ()
  ;; Stop parsing
  nil)

(def :after-frameset process-characters ()
  (parser-parse-error :unexpected-char-after-frameset)
  nil)

(def :after-frameset start-tag-noframes ()
  (process-start-tag token :phase :in-head))

(def :after-frameset start-tag-other ()
  (parser-parse-error :unexpected-start-tag-after-frameset
                      `(:name ,(getf token :name)))
  nil)

(def :after-frameset end-tag-html (phase)
  (setf phase :after-after-frameset)
  nil)

(def :after-frameset end-tag-other ()
  (parser-parse-error :unexpected-end-tag-after-frameset
                      `(:name ,(getf token :name)))
  nil)

;; AfterAfterBodyPhase

(tagname-dispatch :after-after-body process-start-tag
  ("html" start-tag-html)
  (default start-tag-other))

(def :after-after-body process-eof ()
  nil)

(def :after-after-body process-comment ()
  (insert-comment token (document*))
  nil)

(def :after-after-body process-space-characters ()
  (process-space-characters token :phase :in-body))

(def :after-after-body process-characters (phase)
  (parser-parse-error :expected-eof-but-got-char)
  (setf phase :in-body)
  token)

(def :after-after-body start-tag-html ()
  (process-start-tag token :phase :in-body))

(def :after-after-body start-tag-other (phase)
  (parser-parse-error :expected-eof-but-got-start-tag
                      `(:name (getf token :name)))
  (setf phase :in-body)
  token)

(def :after-after-body process-end-tag (phase)
  (parser-parse-error :expected-eof-but-got-end-tag
                      `(:name (getf token :name)))
  (setf phase :in-body)
  token)

;; AfterAfterFramesetPhase

(tagname-dispatch :after-after-frameset process-start-tag
  ("html" start-tag-html)
  ("noframes" start-tag-noframes)
  (default start-tag-other))

(def :after-after-frameset process-eof ()
  nil)

(def :after-after-frameset process-comment ()
  (insert-comment token (document*))
  nil)

(def :after-after-frameset process-space-characters ()
  (process-space-characters token :phase :in-body))

(def :after-after-frameset process-characters ()
  (parser-parse-error :expected-eof-but-got-char)
  nil)

(def :after-after-frameset start-tag-html ()
  (process-start-tag token :phase :in-body))

(def :after-after-frameset start-tag-noframes ()
  (process-start-tag token :phase :in-head))

(def :after-after-frameset start-tag-other ()
  (parser-parse-error :expected-eof-but-got-start-tag
                      `(:name (getf token :name)))
  nil)

(def :after-after-frameset process-end-tag ()
  (parser-parse-error :expected-eof-but-got-end-tag
                      `(:name (getf token :name)))
  nil)

(defun concat-strings (list)
  (reduce (lambda (a b)
            (concatenate 'string a b))
          (remove-if-not #'stringp list)))

(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun parse-web-page(url)
  (let ((webpage-string ""))
    
	   (setq webpage-string (concat-strings(get-file url)))
	    (html5-parser:parse-html5-fragment webpage-string :dom :xmls)))
