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

(in-package #:html5-parser)


(defmethod transform-html5-dom ((to-type (eql :xmls)) node
                                &key namespace comments)
  "Convert a node into an XMLS-compatible tree of conses, starting
at. If the node is a document-fragement a list of XMLS trees is returned."
  (labels ((node-to-xmls (node parent-ns xlink-defined)
           (ecase (node-type node)
             (:document
              (let (root)
                (element-map-children (lambda (n)
                                        (when (string= (node-name n) "html")
                                          (setf root n)))
                                      node)
                (assert root)
                (node-to-xmls root parent-ns xlink-defined)))
             (:document-fragment
              (let (xmls-nodes)
                (element-map-children (lambda (node)
                                        (push (node-to-xmls node parent-ns xlink-defined)
                                              xmls-nodes))
                                      node)
                (nreverse xmls-nodes)))
             (:element
              (let (attrs children)
                (element-map-attributes (lambda (name node-namespace value)
                                          (when (and namespace
                                                     (not xlink-defined)
                                                     (equal node-namespace (html5-constants:find-namespace "xlink")))
                                            (push '#.(list "xmlns:xlink" (html5-constants:find-namespace "xlink")) attrs)
                                            (setf xlink-defined t))
                                          (push (list (if node-namespace
                                                          name
                                                          (xml-escape-name name))
                                                      value)
                                                attrs))
                                        node)
                (element-map-children (lambda (c)
                                        (push c children))
                                      node)

                (apply #'list
                       (if (and namespace
                                (not (equal parent-ns (node-namespace node))))
                           (cons (node-name node) (node-namespace node))
                           (xml-escape-name (node-name node)))
                       attrs
                       (mapcar (lambda (c)
                                 (node-to-xmls c (node-namespace node) xlink-defined))
                               (nreverse children)))))
             (:text
              (node-value node))
             (:comment
              (when comments
                (list :comment nil (node-value node)))))))
    (node-to-xmls node nil nil)))


(defmethod transform-html5-dom ((to-type (eql :xmls-ns)) node &key)
  (transform-html5-dom :xmls node :namespace t))
