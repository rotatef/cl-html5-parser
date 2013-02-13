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

(defvar *default-tree-builder*)

;;;
;;; Getting the document node
;;;

(defgeneric tree-document (tree)
  (:documentation "Return the document node of the tree builder."))

;;;
;;; Creating nodes
;;;

(defgeneric tree-make-document (tree)
  (:documentation "Create a document node for use in the supplied tree builder."))

(defgeneric tree-make-fragment (tree)
  (:documentation "Create a fragment node for use in the supplied tree builder."))

(defgeneric tree-make-doctype (tree name public-id system-id)
  (:documentation "Create a document type node for use in the supplied tree builder."))

(defgeneric tree-make-comment (tree data)
  (:documentation "Create a comment node for use in the supplied tree builder."))

(defgeneric tree-make-element (tree name namespace)
  (:documentation "Create a element node for use in the supplied tree builder."))

(defgeneric tree-make-text-node (tree data)
  (:documentation "Create a text node for use in the supplied tree builder."))

(defgeneric node-clone (tree node)
  (:documentation "Returns a clone of this node. Child nodes are not cloned."))

;;;
;;; Properties of nodes
;;;

(defgeneric node-type (tree node)
    (:documentation "Returns the type of the supplied node.
The valid types are: :DOCUMENT :FRAGMENT :DOCTYPE  :ELEMENT :COMMENT :TEXT"))

(defgeneric node-name (tree node))
(defgeneric node-namespace (tree node))

(defgeneric node-value (tree node))

(defgeneric node-public-id (tree node))
(defgeneric node-system-id (tree node))

(defgeneric node-attribute (tree node attribute &optional namespace))
(defgeneric (setf node-attribute) (new-value tree node attribute &optional namespace))

;;;
;;; Adding and removing child nodes
;;;

(defgeneric node-append-child (tree node child))
(defgeneric node-insert-before (tree node child insert-before))
(defgeneric node-remove-child (tree node child))

;;;
;;; Navigating the document tree
;;;

(defgeneric node-parent (tree node))
(defgeneric node-first-child (tree node))
(defgeneric node-last-child (tree node))
(defgeneric node-previous-sibling (tree node))
(defgeneric node-next-sibling (tree node))

(defgeneric node-map-attributes (tree function node)
  (:documentation "Calls function on all attributes of the supplied node.
The function is called with three parameters:
  attribute name, namespace or NIL, value"))

(defgeneric node-map-children (tree function node)
  (:documentation "Calls function on all child nodes of the supplied node.
The function is called with one paramater, a child node."))

;;;
;;; Other stuff
;;;

(defun tree-to-xmls (tree node &optional include-namespace-p)
  "Convert a tree into an XMLS-compatible tree of conses, starting at
   NODE.  Does not support fragments."
  (ecase (node-type tree node)
    (:document
     (let (root)
       (node-map-children tree
                          (lambda (n)
                            (when (string= (node-name tree n) "html")
                              (setf root n)))
                          node)
       (assert root)
       (tree-to-xmls tree root include-namespace-p)))
    (:fragment
     (let (xmls-nodes)
       (node-map-children tree (lambda (node)
                                 (push (tree-to-xmls tree node include-namespace-p)
                                       xmls-nodes))
                          node)
       (nreverse xmls-nodes)))
    (:element
     (let (attrs children)
       (node-map-attributes tree
                            (lambda (name namespace value)
                              (declare (ignore namespace))
                              (push (list name value) attrs))
                            node)
       (node-map-children tree
                          (lambda (c)
                            (push c children))
                          node)

       (apply #'list
              (if include-namespace-p
                  (cons (node-name tree node) (node-namespace tree node))
                  (node-name tree node))
              attrs
              (mapcar (lambda (c)
                        (tree-to-xmls tree c include-namespace-p))
                      (nreverse children)))))
    ((:text :comment)
     (node-value tree node))))
