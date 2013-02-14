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

(in-package :html5-cxml)

(defclass cxml-tree-builder ()
  ((document :reader tree-document)))

(defmethod initialize-instance :after ((tree cxml-tree-builder) &key)
  (setf (slot-value tree 'document) (cxml-dom:create-document)))

;;;
;;; Creating nodes
;;;

(defmethod tree-make-document ((tree cxml-tree-builder))
  (cxml-dom:create-document))

(defmethod tree-make-fragment ((tree cxml-tree-builder))
  (dom:create-document-fragment (tree-document tree)))

(defmethod tree-make-doctype ((tree cxml-tree-builder) name public-id system-id)
  (dom:create-document-type (dom:implementation (tree-document tree))
                            name public-id system-id))

(defmethod tree-make-comment ((tree cxml-tree-builder) data)
  (dom:create-comment (tree-document tree) data))

(defmethod tree-make-element ((tree cxml-tree-builder) name namespace)
  (dom:create-element-ns (tree-document tree) namespace name))

(defmethod tree-make-text-node ((tree cxml-tree-builder) data)
  (dom:create-text-node (tree-document tree) data))

;;;
;;; Node methods
;;;

(defmethod node-name ((tree cxml-tree-builder) (node dom:node))
  (dom:node-name node))

(defmethod node-namespace ((tree cxml-tree-builder) (node dom:node))
  (dom:namespace-uri node))

(defmethod node-value ((tree cxml-tree-builder) (node dom:node))
  (dom:node-value node))

(defmethod node-public-id ((tree cxml-tree-builder) (node dom:node))
  (dom:public-id node))

(defmethod node-system-id ((tree cxml-tree-builder) (node dom:node))
  (dom:system-id node))

(defmethod node-parent ((tree cxml-tree-builder) (node dom:node))
  (dom:parent-node node))

(defmethod node-first-child ((tree cxml-tree-builder) (node dom:node))
  (dom:first-child node))

(defmethod node-last-child ((tree cxml-tree-builder) (node dom:node))
  (dom:last-child node))

(defmethod node-previous-sibling ((tree cxml-tree-builder) (node dom:node))
  (dom:previous-sibling node))

(defmethod node-next-sibling ((tree cxml-tree-builder) (node dom:node))
  (dom:next-sibling node))

(defmethod node-append-child ((tree cxml-tree-builder) (node dom:node) child)
  (dom:append-child node child))

(defmethod node-remove-child ((tree cxml-tree-builder) (node dom:node) child)
  (dom:remove-child node child))

(defmethod node-insert-before ((tree cxml-tree-builder) (node dom:node) child insert-before)
  (dom:insert-before node child insert-before))

(defmethod node-attribute ((tree cxml-tree-builder) (node dom:element) attribute &optional namespace)
  (dom:get-attribute-ns node namespace attribute))

(defmethod (setf node-attribute) (new-value (tree cxml-tree-builder) (node dom:element) attribute
                                  &optional namespace)
  (check-type attribute string)
  (check-type new-value string)
  (dom:set-attribute-ns node namespace attribute new-value))

;;;
;;; Traversing
;;;

(defmethod node-type ((tree cxml-tree-builder) (node dom:node))
  (let ((type (dom:node-type node)))
  (ecase type
    ((:document :text :comment :element) type)
    (:document-fragment :fragment)
    (:document-type :doctype))))

(defmethod node-map-children ((tree cxml-tree-builder) function (node dom:node))
  (dom:map-node-list function (dom:child-nodes node)))

(defmethod node-map-attributes ((tree cxml-tree-builder) function (node dom:element))
  (dom:do-node-map (attr (dom:attributes node))
    (funcall function
             (dom:node-name attr)
             (dom:namespace-uri attr)
             (dom:node-value attr))))
