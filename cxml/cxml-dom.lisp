;;;;  HTML5 parser for Common Lisp
;;;;
;;;;  Copyright (C) 2014 Joe Taylor <joekarma@gmail.com>
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

(in-package #:html5-parser)


(defmethod transform-html5-dom ((to-type (eql :cxml)) node &key)
  (let ((document-type)
        (document)
        (document-fragment))
    (labels ((walk (node &optional parent xlink-defined)
               (ecase (node-type node)
                 (:document-type
                  (setf document-type (dom:create-document-type 'rune-dom:implementation
                                                                (xml-escape-name (node-name node))
                                                                (node-public-id node)
                                                                (node-system-id node))))
                 (:document
                  (element-map-children #'walk node))
                 (:document-fragment
                  (setf document (dom:create-document 'rune-dom:implementation nil nil nil))
                  (setf document-fragment (dom:create-document-fragment document))
                  (element-map-children (lambda (c) (walk c document-fragment xlink-defined)) node))
                 (:element
                  (let ((element
                          (if document
                              (dom:create-element-ns document (node-namespace node) (xml-escape-name (node-name node)))
                              (dom:document-element
                               (setf document (dom:create-document 'rune-dom:implementation
                                                                   (node-namespace node)
                                                                   (xml-escape-name (node-name node))
                                                                   document-type))))))
                    (unless (and parent
                                 (equal (node-namespace node) (dom:namespace-uri parent)))
                      (dom:set-attribute-ns element (html5-constants:find-namespace "xmlns")
                                            "xmlns" (node-namespace node)))
                    (element-map-attributes (lambda (name namespace value)
                                              (when (and (not xlink-defined)
                                                         (equal namespace (html5-constants:find-namespace "xlink")))
                                                (dom:set-attribute element "xmlns:xlink" (html5-constants:find-namespace "xlink"))
                                                (setf xlink-defined t))
                                              (if namespace
                                                  (dom:set-attribute-ns element namespace name value)
                                                  (dom:set-attribute element (xml-escape-name name) value)))
                                            node)
                    (element-map-children (lambda (c) (walk c element xlink-defined)) node)
                    (dom:append-child (or parent document) element)))
                 (:text
                  (dom:append-child (or parent document)
                                    (dom:create-text-node document (node-value node))))
                 (:comment
                  (dom:append-child (or parent document)
                                    (dom:create-comment document (node-value node)))))))
      (walk node))
    (or document-fragment document)))
