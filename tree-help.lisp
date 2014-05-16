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

(defmacro pop-end (place)
  "Pop from the end of list"
  (let ((old-list (gensym)))
    `(let ((,old-list ,place))
       (prog1 (car (last ,old-list))
         (setf ,place (butlast ,old-list))))))

(defmacro push-end (object place)
  "Push to the end of list"
  `(progn
     ;(format t "~&push ~S to ~S" ',object ',place)
     (setf ,place (append ,place (list ,object)))))


(defvar *parser*)

(defun document* ()
  (slot-value *parser* 'document))

(defun node-clone* (node)
  (ecase (node-type node)
    (:document
      (make-document))
    (:document-fragment
     (make-fragment (document*)))
    (:document-type
     (make-doctype (document*)
                   (node-name node)
                   (node-public-id node)
                   (node-system-id node)))
    (:comment
      (make-comment (document*) (node-value node)))
    (:text
     (make-text-node (document*) (node-value node)))
    (:element
     (let ((clone (make-element (document*) (node-name node) (node-namespace node))))
       (element-map-attributes
        (lambda (name namespace value)
          (setf (element-attribute clone name namespace) value))
        node)
       clone))))

(defun node-name-tuple (node)
  (cons (or (node-namespace node)
            (find-namespace "html"))
        (node-name node)))

(defun node-name-tuple-values (node)
  (values (or (node-namespace node)
              (find-namespace "html"))
          (node-name node)))

(defun node-has-content (node)
  (not (null (node-first-child node))))

(defun node-attributes= (node1 node2)
  (labels ((has-all-attributes-of (node1 node2)
             (element-map-attributes
              (lambda (name namespace value)
                (unless (equal value
                               (element-attribute node2 name namespace))
                  (return-from has-all-attributes-of nil)))
              node1)
             t))
    (and (has-all-attributes-of node1 node2)
         (has-all-attributes-of node2 node1))))

(defun node-append-child* (node child)
  (let ((last-child (node-last-child node)))
    (if (and (eql :text (node-type child))
             last-child
             (eql :text (node-type last-child)))
        (nconcatf (node-value last-child)
                  (node-value child))
        (node-append-child node child))))

(defun node-insert-before* (node child insert-before)
  (when (eql :text (node-type child))
    (let ((prev-child (node-previous-sibling insert-before)))
      (when (and prev-child
                 (eql :text (node-type prev-child)))
        (node-remove-child node prev-child)
        (setf child (make-text-node
                     (document*)
                     (concatenate 'string
                                  (node-value prev-child)
                                  (node-value child)))))))
  (node-insert-before node child insert-before))

(defun node-reparent-children (node new-parent)
  (element-map-children (lambda (child)
                          (node-append-child new-parent child))
                        node))

(defun node-insert-text (node data &optional insert-before)
  (if insert-before
      (node-insert-before* node (make-text-node (document*) data) insert-before)
      (node-append-child* node (make-text-node (document*) data))))

(defun last-open-element ()
  (with-slots (open-elements) *parser*
    (car (last open-elements))))

(defun create-element (token)
  "Create an element but don't insert it anywhere"
  (with-slots (html-namespace) *parser*
    (let ((element (make-element (document*)
                                 (getf token :name)
                                 (or (getf token :namespace)
                                     html-namespace))))
      (loop for (name . value) in (getf token :data)
            do (if (consp name)
                   (setf (element-attribute element (second name) (third name)) value)
                   (setf (element-attribute element name) value)))
      element)))


(defun insert-root (token)
  (with-slots (open-elements) *parser*
    (let ((element (create-element token)))
      (assert element)
      (push-end element open-elements)
      (node-append-child (document*) element))))

(defun insert-doctype (token)
  (node-append-child (document*)
                     (make-doctype (document*)
                                   (getf token :name)
                                   (getf token :public-id)
                                   (getf token :system-id))))

(defun insert-comment (token &optional parent)
  (with-slots (open-elements) *parser*
    (unless parent
      (setf parent (car (last open-elements))))
    (node-append-child parent (make-comment (document*) (getf token :data)))))

(defun insert-element-normal (token)
  (with-slots (open-elements) *parser*
   (let ((element (create-element token)))
     (node-append-child (last-open-element) element)
     (push-end element open-elements)
     element)))

(defun insert-element-table (token)
  (with-slots (open-elements) *parser*
    (if (not (member (node-name (last-open-element))
                     +table-insert-mode-elements+ :test #'string=))
        (insert-element-normal token)
        (let ((element (create-element token)))
          ;; We should be in the InTable mode. This means we want to do
          ;; special magic element rearranging
          (multiple-value-bind (parent insert-before)
              (get-table-misnested-nodeposition)
            (if (not insert-before)
                (node-append-child* parent element)
                (node-insert-before* parent element insert-before))
            (push-end element open-elements))
          element))))

(defun insert-element (token)
  (with-slots (insert-from-table) *parser*
    (if insert-from-table
        (insert-element-table token)
        (insert-element-normal token))))

(defun parser-insert-text (data &optional parent)
  "Insert text data."
  (with-slots (open-elements insert-from-table) *parser*
    (unless parent
      (setf parent (car (last open-elements))))
    (cond ((or (not insert-from-table)
               (and insert-from-table
                    (not (member (node-name (last-open-element))
                                 +table-insert-mode-elements+ :test #'string=))))
           (node-insert-text parent data))
          (t
           ;; We should be in the InTable mode. This means we want to do
           ;; special magic element rearranging
           (multiple-value-bind (parent insert-before)
               (get-table-misnested-nodeposition)
             (node-insert-text parent data insert-before))))))

(defun get-table-misnested-nodeposition ()
  "Get the foster parent element, and sibling to insert before
    (or None) when inserting a misnested table node"
  (with-slots (open-elements) *parser*
    ;; The foster parent element is the one which comes before the most
    ;; recently opened table element
    (let ((last-table (find "table" open-elements :key #'node-name :test #'string= :from-end t))
          (foster-parent nil)
          (insert-before nil))

      (cond (last-table
              ;; XXX - we should really check that this parent is actually a
              ;; node here
              (if (node-parent last-table)
                  (setf foster-parent (node-parent last-table)
                        insert-before last-table)
                  (setf foster-parent (elt open-elements (1- (position last-table open-elements))))))
             (t
              (setf foster-parent (first open-elements))))
      (values foster-parent insert-before))))

(defun generate-implied-end-tags (&optional exclude)
  (with-slots (open-elements) *parser*
    (let ((name (node-name (last-open-element))))
      ;; XXX td, th and tr are not actually needed
      (when (and (member name '("dd" "dt" "li" "option" "optgroup" "p" "rp" "rt") :test #'string=)
                 (not (equal name exclude)))
        (pop-end open-elements)
        ;; XXX This is not entirely what the specification says. We should
        ;; investigate it more closely.
        (generate-implied-end-tags exclude)))))

(defun reconstruct-active-formatting-elements ()
  ;; Within this algorithm the order of steps described in the
  ;; specification is not quite the same as the order of steps in the
  ;; code. It should still do the same though.
  (with-slots (active-formatting-elements open-elements) *parser*

    ;; Step 1: stop the algorithm when there's nothing to do.
    (unless active-formatting-elements
      (return-from reconstruct-active-formatting-elements))

    ;; Step 2 and step 3: we start with the last element. So i is -1.
    (let* ((i (1- (length active-formatting-elements)))
           (entry (elt active-formatting-elements i)))
      (when (or (eql entry :marker)
                (member entry open-elements))
        (return-from reconstruct-active-formatting-elements))

      ;; Step 6
      (loop while (and (not (eql entry :marker))
                       (not (member entry open-elements))) do
           (when (zerop i)
             ;; This will be reset to 0 below
             (setf i -1)
             (return))
           (decf i)
         ;; Step 5: let entry be one earlier in the list.
           (setf entry (elt active-formatting-elements i)))

      (loop
         ;; Step 7
         (incf i)

         ;; Step 8
         (setf entry (elt active-formatting-elements i))

         ;; Step 9
         (let* ((element (insert-element (list :type :start-tag
                                               :name (node-name entry)
                                               :namespace (node-namespace entry)))))
           (element-map-attributes (lambda (name namespace value)
                                     (setf (element-attribute element name namespace) value))
                                   entry)

           ;; Step 10
           (setf (elt active-formatting-elements i) element)

           ;; Step 11
           (when (eql element (car (last active-formatting-elements)))
             (return)))))))

(defun clear-active-formatting-elements ()
 (with-slots (active-formatting-elements) *parser*
   (loop for entry = (pop-end active-formatting-elements)
      while (and active-formatting-elements
                 (not (eql entry :marker))))))

(defun element-in-active-formatting-elements (name)
  "Check if an element exists between the end of the active
   formatting elements and the last marker. If it does, return it, else
   return false"
  (with-slots (active-formatting-elements) *parser*
    (loop for item in (reverse active-formatting-elements) do
       ;; Check for Marker first because if it's a Marker it doesn't have a
       ;; name attribute.
         (when (eql item :marker)
           (return nil))
         (when (string= (node-name item) name)
           (return item)))))

(defun scope-tree ()
  (load-time-value
   (flet ((unflatten (alist)
            "Turn an alist into a tree."
            (let ((alist2
                    (mapcar #'list
                            (remove-duplicates (mapcar #'car alist)
                                               :test #'equal))))
              (loop for (key . value) in alist
                    do (push value (cdr (assoc key alist2
                                               :test #'equal))))
              ;; Put the XHTML ns first.
              (sort alist2 #'<
                    :key (lambda (pair)
                           (position (car pair)
                                     '("http://www.w3.org/1999/xhtml"
                                       "http://www.w3.org/2000/svg"
                                       "http://www.w3.org/1998/Math/MathML")
                                     :test #'string=))))))
     (let ((html (find-namespace "html")))
       `((nil . ,(unflatten +scoping-elements+))
         ("button" . ,(unflatten
                       `(,@+scoping-elements+
                         (,html . "button"))))
         ("list" . ,(unflatten
                     `(,@+scoping-elements+
                       (,html . "ol")
                       (,html . "ul"))))
         ("table" . ((,html "html" "table")))
         ("select" . ((,html "optgroup" "option"))))))))

(defun element-in-scope (target &optional variant)
  (let ((list-elements
          (cdr (assoc variant (scope-tree) :test #'equal)))
        (invert (equal "select" variant)))
    (dolist (node (reverse (slot-value *parser* 'open-elements)))
      (when (or (and (stringp target)
                     (string= (node-name node) target))
                (eql node target))
        (return-from element-in-scope t))

      (multiple-value-bind (ns name)
          (node-name-tuple-values node)
        (let ((found (member name (cdr (assoc ns list-elements :test #'string=))
                             :test #'string=)))
          (when invert
            (setf found (not found)))
          (when found
            (return-from element-in-scope nil)))))

    (error "We should never reach this point")))
