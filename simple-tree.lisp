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

;; A basic implementation of a DOM-core like thing

(defclass node ()
  ((type :initform :node :allocation :class :reader node-type)
   (name :initarg :name :initform nil :reader node-name)
   (namespace :initarg :namespace :initform nil :reader node-namespace)
   (parent :initform nil :reader node-parent)
   (value :initform nil :initarg :value :reader node-value)
   (child-nodes :initform nil :accessor %node-child-nodes)))

(defclass document (node)
  ((type :initform :document :allocation :class)))

(defclass document-fragment (document)
  ((type :initform :fragment :allocation :class)))

(defclass document-type (node)
  ((type :initform :doctype :allocation :class)
   (public-id :initarg :public-id :reader node-public-id)
   (system-id :initarg :system-id :reader node-system-id)))

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

(defun make-document ()
  (make-instance 'document))

(defun make-fragment ()
  (make-instance 'document-fragment))

(defun make-doctype (name public-id system-id)
  (make-instance 'document-type :name name :public-id public-id :system-id system-id))

(defun make-comment (data)
  (make-instance 'comment-node :value data))

(defun make-element (name namespace)
  (make-instance 'element :name name :namespace namespace))

(defun make-text-node (data)
  (make-instance 'text-node :value data))

;;;
;;; Node methods
;;;

(defun node-first-child (node)
  (car (%node-child-nodes node)))

(defun node-last-child (node)
  (car (last (%node-child-nodes node))))

(defun node-previous-sibling (node)
  (loop for (this next) on (%node-child-nodes (node-parent node))
        when (eql next node) do (return this)))

(defun node-next-sibling (node)
  (loop for (this next) on (%node-child-nodes (node-parent node))
        when (eql this node) do (return next)))

(defun node-append-child (node child)
  (when (node-parent child)
    (node-remove-child (node-parent child) child))
  (setf (slot-value child 'parent) node)
  (setf (%node-child-nodes node)
        (append (%node-child-nodes node) (list child))))

(defun node-remove-child (node child)
  (setf (%node-child-nodes node)
        (remove child (%node-child-nodes node)))
  (setf (slot-value child 'parent) nil))

(defun node-insert-before (node child insert-before)
  (let ((child-nodes (%node-child-nodes node)))
    (setf (slot-value child 'parent) node)
    (labels ((insert-before (child-nodes)
               (cond ((endp child-nodes)
                      (cons child nil))
                     ((eql (car child-nodes) insert-before)
                      (cons child child-nodes))
                     (t (rplacd child-nodes (insert-before (cdr child-nodes)))))))
      (setf (%node-child-nodes node)
            (insert-before child-nodes)))))

(defun element-attribute (node attribute &optional namespace)
  (cdr (assoc (cons attribute namespace)
              (%node-attributes node)
              :test #'equal)))

(defun (setf element-attribute) (new-value node attribute
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

(defun element-map-children (function node)
  (map nil function (%node-child-nodes node)))

(defun element-map-attributes (function node)
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
    (t 1)))

(defmethod print-object ((node document) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format stream "nodes: ~A" (node-count node))))

(defmethod print-object ((node node) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format stream "~A" (node-name node))))

(defmethod print-object ((node text-node) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (write (node-value node) :stream stream :length 30)))
