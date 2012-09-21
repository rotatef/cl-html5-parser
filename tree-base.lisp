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

(in-package #:html5-tree)

;; high level API

(defgeneric tree-current-node (tree))
(defgeneric tree-node-is-in-html-namespace (tree node))


;; the rest

(defgeneric node-name (node))
(defgeneric node-namespace (node))
(defgeneric node-name-tuple (node)) ; move to parser,  a cons of (namespace . name)
(defgeneric node-attributes (node))
(defgeneric (setf node-attributes) (new-value node))
(defgeneric node-value (node))
(defgeneric node-clone (node)) ; FIXME: should children be cloned?
(defgeneric node-append-child (node child))
(defgeneric node-remove-child (node child))
(defgeneric node-insert-before (node child insert-before))
(defgeneric node-insert-text (node data &optional insert-before))
(defgeneric node-parent (node))
(defgeneric node-reparent-children (node new-parent))
(defgeneric node-child-nodes (node))
(defgeneric (setf node-child-nodes) (new-value node))
(defgeneric node-has-content (node)) ; FIXME: better name node-has-children?
(defgeneric node-print-tree (node &optional stream indent))

(defun node-attribute (node attribute) ;; need setf also
  "Returns the value of the named attribute or NIL if not present"
  (cdr (assoc attribute (node-attributes node) :test #'string=)))

(defmethod node-reparent-children (node new-parent)
  (dolist (child (node-child-nodes node))
    (node-append-child new-parent child))
  (setf (node-child-nodes node) '()))

(defclass tree-builder ()
  ((default-namespace :initarg :default-namespace :reader tree-default-namespace) ;; FIXME: rename to html-namespace
   (open-elements :accessor tree-open-elements)
   (active-formatting-elements :accessor tree-active-formatting-elements)
   (head-pointer :accessor tree-head-pointer)
   (form-pointer :accessor tree-form-pointer)
   (insert-from-table :accessor tree-insert-from-table)
   (document :accessor tree-document)))

; helpers for the parser code
(defun tree-last-open-element (tree)
  (car (last (tree-open-elements tree))))


;; Sub classes must specialize on the following methods to create objects
;; for the relevant dom implementation
(defgeneric tree-make-document (tree))
(defgeneric tree-make-element (tree name namespace))
(defgeneric tree-make-comment (tree data))
(defgeneric tree-make-doctype (tree name public-id system-id))
(defgeneric tree-make-fragment (tree))

(defun tree-reset (tree)
  (with-slots (open-elements active-formatting-elements head-pointer form-pointer
                             insert-from-table document)
      tree
    (setf open-elements '())
    (setf active-formatting-elements '())
    (setf head-pointer nil)
    (setf form-pointer nil)
    (setf insert-from-table nil)
    (setf document (tree-make-document tree))))

(defun eassoc (string alist)
  (cdr (assoc string alist :test #'equal)))

(defmacro pop-end (place)
  "Pop from the end of list"
  (let ((old-list (gensym)))
    `(let ((,old-list ,place))
       (prog1 (car (last ,old-list))
         (setf ,place (butlast ,old-list))))))

(defmacro push-end (object place)
  "Push to the end of list"
  `(setf ,place (append ,place (list ,object))))

(defun tree-element-in-scope (tree target &optional variant)
  (let ((list-elements
         (cdr (assoc variant
                     `((nil . ,+scoping-elements+)
                       ("button" . (,@+scoping-elements+
                                    (,(find-namespace "html") . "button")))
                       ("list" . (,@+scoping-elements+
                                  (,(find-namespace "html") . "ol")
                                  (,(find-namespace "html") . "ul")))
                       ("table" . ((,(find-namespace "html") . "html")
                              (,(find-namespace "html") . "table")))
                       ("select" . ((,(find-namespace "html") . "optgroup")
                                    (,(find-namespace "html") . "option"))))
                     :test #'equal)))
        (invert (equal "select" variant)))
    (dolist (node (reverse (slot-value tree 'open-elements)))
      (when (or (and (stringp target)
                     (string= (node-name node) target))
                (eql node target))
        (return-from tree-element-in-scope t))

      (let ((found (member (node-name-tuple node) list-elements :test #'equal)))
        (when invert
          (setf found (not found)))
        (when found
          (return-from tree-element-in-scope nil))))

    (error "We should never reach this point")))

(defun tree-reconstruct-active-formatting-elements (tree)
  ;; Within this algorithm the order of steps described in the
  ;; specification is not quite the same as the order of steps in the
  ;; code. It should still do the same though.
  (with-slots (active-formatting-elements open-elements) tree

    ;; Step 1: stop the algorithm when there's nothing to do.
    (unless active-formatting-elements
      (return-from tree-reconstruct-active-formatting-elements))

    ;; Step 2 and step 3: we start with the last element. So i is -1.
    (let* ((i (1- (length active-formatting-elements)))
           (entry (elt active-formatting-elements i)))
      (when (or (eql entry :marker)
                (member entry open-elements))
        (return-from tree-reconstruct-active-formatting-elements))

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

         (let* ((clone (node-clone entry)) ; Mainly to get a new copy of the attributes
                ;; Step 9
                (element (tree-insert-element tree
                                              (list :type :start-tag
                                                    :name (node-name clone)
                                                    :namespace (node-namespace clone)
                                                    :data (node-attributes clone)))))
           ;; Step 10
           (setf (elt active-formatting-elements i) element)

           ;; Step 11
           (when (eql element (car (last active-formatting-elements)))
             (return)))))))

(defun tree-clear-active-formatting-elements (tree)
 (with-slots (active-formatting-elements) tree
   (loop for entry = (pop-end active-formatting-elements)
      while (and active-formatting-elements
                 (not (eql entry :marker))))))

(defun tree-element-in-active-formatting-elements (tree name)
  "Check if an element exists between the end of the active
   formatting elements and the last marker. If it does, return it, else
   return false"
  (with-slots (active-formatting-elements) tree
    (loop for item in (reverse active-formatting-elements) do
       ;; Check for Marker first because if it's a Marker it doesn't have a
       ;; name attribute.
         (when (eql item :marker)
           (return nil))
         (when (string= (node-name item) name)
           (return item)))))

(defun tree-insert-root (tree token)
  (with-slots (open-elements document) tree
    (let ((element (tree-create-element tree token)))
      (push-end element open-elements)
      (node-append-child document element))))

(defun tree-insert-doctype (tree token)
  (with-slots (document) tree
    (node-append-child document (tree-make-doctype tree
                                                   (getf token :name)
                                                   (getf token :public-id)
                                                   (getf token :system-id)))))

(defun tree-insert-comment (tree token &optional parent)
  (with-slots (open-elements) tree
    (unless parent
      (setf parent (car (last open-elements))))
    (node-append-child parent (tree-make-comment tree (getf token :data)))))

(defun tree-create-element (tree token)
  "Create an element but don't insert it anywhere"
  (with-slots (default-namespace) tree
    (let ((element (tree-make-element tree
                                      (getf token :name)
                                      (or (getf token :namespace)
                                          default-namespace))))
      (setf (node-attributes element) (getf token :data))
      element)))

(defun tree-insert-element (tree token)
  (if (tree-insert-from-table tree)
      (tree-insert-element-table tree token)
      (tree-insert-element-normal tree token)))

(defun tree-insert-element-normal (tree token)
 (with-slots (open-elements) tree
   (let ((element (tree-create-element tree token)))
     (node-append-child (car (last open-elements)) element)
     (push-end element open-elements)
     element)))

(defun tree-insert-element-table (tree token)
  (with-slots (open-elements) tree
    (if (not (member (node-name (car (last open-elements)))
                     +table-insert-mode-elements+ :test #'string=))
        (tree-insert-element-normal tree token)
        (let ((element (tree-create-element tree token)))
          ;; We should be in the InTable mode. This means we want to do
          ;; special magic element rearranging
          (multiple-value-bind (parent insert-before)
              (tree-get-table-misnested-nodeposition tree)
            (if (not insert-before)
                (node-append-child parent element)
                (node-insert-before parent element insert-before))
            (push-end element open-elements))
          element))))

(defun tree-insert-text (tree data &optional parent)
  "Insert text data."
  (with-slots (open-elements) tree
    (unless parent
      (setf parent (car (last open-elements))))
    (cond ((or (not (tree-insert-from-table tree))
               (and (tree-insert-from-table tree)
                    (not (member (node-name (car (last open-elements)))
                                 +table-insert-mode-elements+ :test #'string=))))
           (node-insert-text parent data))
          (t
           ;; We should be in the InTable mode. This means we want to do
           ;; special magic element rearranging
           (multiple-value-bind (parent insert-before)
               (tree-get-table-misnested-nodeposition tree)
             (node-insert-text parent data insert-before))))))

(defun tree-get-table-misnested-nodeposition (tree)
  "Get the foster parent element, and sibling to insert before
    (or None) when inserting a misnested table node"
  (with-slots (open-elements) tree
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

(defun tree-generate-implied-end-tags (tree &optional exclude)
  (with-slots (open-elements) tree
    (let ((name (node-name (car (last open-elements)))))
      ;; XXX td, th and tr are not actually needed
      (when (and (member name '("dd" "dt" "li" "option" "optgroup" "p" "rp" "rt") :test #'string=)
                 (not (equal name exclude)))
        (pop-end open-elements)
        ;; XXX This is not entirely what the specification says. We should
        ;; investigate it more closely.
        (tree-generate-implied-end-tags tree exclude)))))

(defun tree-get-document (tree)
  "Return the final tree"
  (slot-value tree 'document))

(defun tree-get-fragment (tree)
  "Return the final fragment"
  (with-slots (open-elements) tree
    (let ((fragment (tree-make-fragment tree)))
      (node-reparent-children (first open-elements) fragment)
      fragment)))
