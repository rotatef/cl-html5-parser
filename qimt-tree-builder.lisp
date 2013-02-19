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

(defpackage :html5-qimt-tree
  (:use
   :common-lisp
   :html5-tree
   :qimt
   :quad)
  (:export
   #:qimt-tree-builder))

(in-package :html5-qimt-tree)

(defclass qimt-tree-builder ()
  ((document :reader tree-document)
   (symbol-table :accessor symbol-table
                 :initform (make-hash-table :test #'equal))))

(defmethod initialize-instance :after ((tree qimt-tree-builder) &key)
  (setf (slot-value tree 'document) (document)))

;;;
;;; Creating nodes
;;;

(defvar *html-symbols*)
(setf *html-symbols* (make-hash-table :test #'equal))

(do-external-symbols (symbol :html)
  (when (get symbol 'qimt::tag-name)
    (setf (gethash (list (get symbol 'qimt::tag-name)
                         "http://www.w3.org/1999/xhtml"
                         (get symbol 'qimt::attributep))
                       *html-symbols*)
          symbol)))

(defun intern-symbol (tree name namespace attributep)
  (let* ((key (list (list name namespace attributep)))
         (symbol (or (gethash key *html-symbols*)
                     (gethash key (symbol-table tree)))))
    (unless symbol
      (setf symbol (make-symbol (if attributep
                                    (format nil "~A=" (string-upcase name))
                                    (string-upcase name))))
      (setf (gethash key (symbol-table tree)) symbol)
      (setf (get symbol 'qimt::tag-name) name)
      (setf (get symbol 'qimt::namespace) namespace)
      (setf (get symbol 'qimt::attributep) attributep))
    symbol))

(defmethod tree-make-document ((tree qimt-tree-builder))
  (document))

(defmethod tree-make-fragment ((tree qimt-tree-builder))
  (document-fragment))

(setf (get 'public-id= 'attribute-node-p) t)
(setf (get 'system-id= 'attribute-node-p) t)

(defmethod tree-make-doctype ((tree qimt-tree-builder) name public-id system-id)
  ;; TODO: fix proper doctype support in qimt
  (with-single-node
    (with-node '<!doctype
      (with-node 'name=
        (xd name))
      (when public-id
        (with-node 'public-id=
          (xd public-id)))
      (when system-id
        (with-node 'system-id=
          (xd system-id))))))

(defmethod tree-make-comment ((tree qimt-tree-builder) data)
  (with-single-node
    (comment (xd data))))

(defmethod tree-make-element ((tree qimt-tree-builder) name namespace)
  (make-node (intern-symbol tree name namespace nil)))

(defmethod tree-make-text-node ((tree qimt-tree-builder) data)
  (make-node nil data))

;;;
;;; Properties of nodes
;;;

(defmethod node-type ((tree qimt-tree-builder) (node quad))
  (cond ((null (qar node))
         :text)
        ((eql 'document (qar node))
         :document)
        ((eql 'document-fragment (qar node))
         :fragment)
        ((eql '<!doctype (qar node))
         :doctype)
        ((eql 'comment (qar node))
         :comment)
        ((attribute-node-p node)
         :attribute)
        (t
         :element)))

(defmethod node-name ((tree qimt-tree-builder) (node quad))
  (if (eql '<!doctype (qar node))
      (qccr (find-child node 'name=))
      (node-type-tag-name (qar node))))

(defmethod node-namespace ((tree qimt-tree-builder) (node quad))
  (or (get (qar node) 'qimt::namespace)
      "http://www.w3.org/1999/xhtml"))

(defmethod node-value ((tree qimt-tree-builder) (node quad))
  (if (comment-node-p node)
      (qccr node)
      (qcr node)))

(defmethod node-public-id ((tree qimt-tree-builder) (node quad))
  ;; TODO move to qimt
  (qccr (find-child node 'public-id=)))

(defmethod node-system-id ((tree qimt-tree-builder) (node quad))
  ;; TODO move to qimt
  (qccr (find-child node 'system-id=)))

(defun find-attr-symbol (tree name namespace)
  (intern-symbol tree name namespace t))

(defmethod node-attribute ((tree qimt-tree-builder) (node quad) attribute &optional namespace)
  (qccr (find-child node (find-attr-symbol tree attribute namespace))))

(defmethod (setf node-attribute) (new-value (tree qimt-tree-builder) (node quad) attribute
                                  &optional namespace)
  (check-type attribute string)
  (check-type new-value string)
  (let ((old-attr (find-child node (find-attr-symbol tree attribute namespace))))
    (if old-attr
        (setf (qccr old-attr) new-value)
        (within-node (node)
          (with-node (find-attr-symbol tree attribute namespace)
            (xd new-value))))))

;;;
;;; Adding and removing child nodes
;;;

(defmethod node-append-child ((tree qimt-tree-builder) (node quad) child)
  (when (qbr child)
    (node-remove-child tree (qbr child) child))
  (within-node (node :append)
    (insert-node child)))

(defmethod node-insert-before ((tree qimt-tree-builder) (node quad) child insert-before)
  (within-node (node (search-document insert-before :children node :key #'qdr))
    (insert-node child)))

(defmethod node-remove-child ((tree qimt-tree-builder) (node quad) child)
  (let ((prev-child (node-previous-sibling tree child)))
    (if prev-child
        (setf (qdr prev-child) (qdr child))
        (setf (qcr node) (qdr child)))
    (setf (qbr child) nil)
    (setf (qdr child) nil)))

;;;
;;; Navigating the document tree
;;;

(defmethod node-parent ((tree qimt-tree-builder) (node quad))
  (qbr node))

(defmethod node-first-child ((tree qimt-tree-builder) (node quad))
  (qcr node))

(defmethod node-last-child ((tree qimt-tree-builder) (node quad))
  (last-child node))

(defmethod node-previous-sibling ((tree qimt-tree-builder) (node quad))
  (if (eq node (qcbr node))
      nil ; it's the first
      (search-document node :children (qbr node) :key #'qdr)))

(defmethod node-next-sibling ((tree qimt-tree-builder) (node quad))
  (qdr node))

(defmethod node-map-attributes ((tree qimt-tree-builder) function (node quad))
  (walk-document nil :children node
                 (lambda (child)
                   (when (attribute-node-p child)
                     (funcall function
                              (node-type-tag-name (qar child))
                              (get (qar child) 'qimt::namespace)
                              (qccr child))))))

(defmethod node-map-children ((tree qimt-tree-builder) function (node quad))
  (let ((children))
    (walk-document 'list :children node
                   (lambda (child)
                     (unless (attribute-node-p child)
                       (push child children))))
    (map nil function (nreverse children))))
