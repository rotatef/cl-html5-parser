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

(in-package :html5-simple-tree)

;; A basic implementation of a DOM-core like thing

(defclass simple-tree-builder ()
  ((document :reader tree-document)))

(defvar *default-tree-builder* 'simple-tree-builder)

(defmethod initialize-instance :after ((tree simple-tree-builder) &key)
  (setf (slot-value tree 'document) (tree-make-document tree)))

(defclass node ()
  ((type :initform :node :allocation :class :reader %node-type)
   (name :initarg :name :initform nil :reader %node-name)
   (namespace :initarg :namespace :initform nil :reader %node-namespace)
   (parent :initform nil :reader %node-parent)
   (value :initform nil :initarg :value :reader %node-value)
   (child-nodes :initform nil :accessor %node-child-nodes)))

(defclass document (node)
  ((type :initform :document :allocation :class)))

(defclass document-fragment (document)
  ((type :initform :fragment :allocation :class)))

(defclass document-type (node)
  ((type :initform :doctype :allocation :class)
   (public-id :initarg :public-id :reader %node-public-id)
   (system-id :initarg :system-id :reader %node-system-id)))

(defclass text-node (node)
  ((type :initform :text :allocation :class)))

(defclass element (node)
  ((type :initform :element :allocation :class)
   (attributes :initform nil :accessor %node-attributes)))

(defclass comment-node (node)
  ((type :initform :comment :allocation :class)))

;;;
;;; Creating nodes
;;;


(defmethod tree-make-document ((tree simple-tree-builder))
  (make-instance 'document))

(defmethod tree-make-fragment ((tree simple-tree-builder))
  (make-instance 'document-fragment))

(defmethod tree-make-doctype ((tree simple-tree-builder) name public-id system-id)
  (make-instance 'document-type :name name :public-id public-id :system-id system-id))

(defmethod tree-make-comment ((tree simple-tree-builder) data)
  (make-instance 'comment-node :value data))

(defmethod tree-make-element ((tree simple-tree-builder) name namespace)
  (make-instance 'element :name name :namespace namespace))

(defmethod tree-make-text-node ((tree simple-tree-builder) data)
  (make-instance 'text-node :value data))

;;;
;;; Node methods
;;;

(defmethod node-name ((tree simple-tree-builder) (node node))
  (%node-name node))

(defmethod node-namespace ((tree simple-tree-builder) (node node))
  (%node-namespace node))

(defmethod node-value ((tree simple-tree-builder) (node node))
  (%node-value node))

(defmethod node-public-id ((tree simple-tree-builder) (node node))
  (%node-public-id node))

(defmethod node-system-id ((tree simple-tree-builder) (node node))
  (%node-system-id node))

(defmethod node-parent ((tree simple-tree-builder) (node node))
  (%node-parent node))

(defmethod node-first-child ((tree simple-tree-builder) (node node))
  (car (%node-child-nodes node)))

(defmethod node-last-child ((tree simple-tree-builder) (node node))
  (car (last (%node-child-nodes node))))

(defmethod node-previous-sibling ((tree simple-tree-builder) (node node))
  (loop for (this next) on (%node-child-nodes (%node-parent node))
        when (eql next node) do (return this)))

(defmethod node-next-sibling ((tree simple-tree-builder) (node node))
  (loop for (this next) on (%node-child-nodes (%node-parent node))
        when (eql this node) do (return next)))

(defmethod node-append-child ((tree simple-tree-builder) (node node) child)
  (when (%node-parent child)
    (node-remove-child tree (%node-parent child) child))
  (setf (slot-value child 'parent) node)
  (setf (%node-child-nodes node)
        (append (%node-child-nodes node) (list child))))

(defmethod node-remove-child ((tree simple-tree-builder) (node node) child)
  (setf (%node-child-nodes node)
        (remove child (%node-child-nodes node)))
  (setf (slot-value child 'parent) nil))

(defmethod node-insert-before ((tree simple-tree-builder) (node node) child insert-before)
  (let ((child-nodes (%node-child-nodes node)))
    (setf (slot-value child 'parent) node)
    (setf (%node-child-nodes node) ())
    (dolist (kid child-nodes)
      (when (eql kid insert-before)
        (node-append-child tree node child))
      (node-append-child tree node kid))))

(defmethod node-attribute ((tree simple-tree-builder) (node element) attribute &optional namespace)
  (cdr (assoc (cons attribute namespace)
              (%node-attributes node)
              :test #'equal)))

(defmethod (setf node-attribute) (new-value (tree simple-tree-builder) (node node) attribute
                                  &optional namespace)
  (check-type attribute string)
  (check-type new-value string)
  (let ((old-attr (assoc (cons attribute namespace)
                         (%node-attributes node)
                         :test #'equal)))
    (if old-attr
        (setf (cdr old-attr) new-value)
        (push (cons (cons attribute namespace) new-value) (%node-attributes node)))))

;;;
;;; Traversing
;;;

(defmethod node-type ((tree simple-tree-builder) (node node))
  (%node-type node))

(defmethod node-map-children ((tree simple-tree-builder) function (node node))
  (map nil function (%node-child-nodes node)))

(defmethod node-map-attributes ((tree simple-tree-builder) function (node element))
  (loop for ((name . namespace) . value) in (%node-attributes node)
        do (funcall function name namespace value)))


;;
;; Printing for the ease of debugging
;;

(defun node-count (tree)
  (typecase tree
    (element (1+ (apply #'+ (mapcar #'node-count (%node-child-nodes tree)))))
    ((or document document-fragment)
     (apply #'+ (mapcar #'node-count (%node-child-nodes tree))))
    (t 0)))

(defmethod print-object ((node document) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format stream "nodes: ~A" (node-count node))))

(defmethod print-object ((node node) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format stream "~A" (%node-name node))))

(defmethod print-object ((node text-node) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (write (%node-value node) :stream stream :length 30)))
