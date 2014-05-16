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

(in-package :html5-parser-tests)

(in-root-suite)
(defsuite tree-builder-tests)
(in-suite tree-builder-tests)

(deftest test-make-document ()
  (is (eq :document (node-type (make-document)))))

(deftest test-append-child ()
  (let* ((doc (make-document))
         (child (make-element doc "test" nil)))
    (node-append-child doc child)
    (element-map-children (lambda (kid)
                              (is (eq kid child)))
                          doc)))

(deftest test-reappend-child ()
  (let* ((doc (make-document))
         (parent1 (make-element doc "parent1" nil))
         (parent2 (make-element doc "parent2" nil))
         (child (make-element doc "child" nil)))
    (node-append-child parent1 child)
    (is (eq parent1 (node-parent child)))
    (node-append-child parent2 child)
    (is (eq parent2 (node-parent child)))
    (element-map-children (lambda (kid)
                            (error "parent1 should not have children now ~S" kid))
                          parent1)))

(deftest test-navigate ()
  (let* ((doc (make-document))
         (parent (make-element doc "parent" nil))
         (child1 (make-element doc "child1" nil))
         (child2 (make-element doc "child2" nil))
         (child3 (make-element doc "child3" nil))
         (child4 (make-element doc "child4" nil)))
    (node-append-child parent child1)
    (node-append-child parent child2)
    (node-append-child parent child3)
    (node-append-child parent child4)
    (is (eq child1 (node-first-child parent)))
    (is (eq child4 (node-last-child parent)))
    (is (eq child2 (node-next-sibling child1)))
    (is (eq nil (node-next-sibling child4)))
    (is (eq child1 (node-previous-sibling child2)))
    (is (eq nil (node-previous-sibling child1)))))

(deftest test-remove-child ()
  (let* ((doc (make-document))
         (parent (make-element doc "parent" nil))
         (child1 (make-element doc "child1" nil))
         (child2 (make-element doc "child2" nil))
         (child3 (make-element doc "child3" nil))
         (child4 (make-element doc "child4" nil)))
    (node-append-child parent child1)
    (node-append-child parent child2)
    (node-append-child parent child3)
    (node-append-child parent child4)

    (node-remove-child parent child2)
    (is (eq child3 (node-next-sibling child1)))))

(deftest test-set-attribute ()
  (let* ((doc (make-document))
         (element (make-element doc "test" nil)))
    (setf (element-attribute element "hello") "world")
    (is (string= (element-attribute element "hello") "world"))))

(deftest test-append-text ()
  (let* ((doc (make-document))
         (parent (make-element doc "parent" nil)))
    (html5-parser::node-append-child* parent (make-text-node doc "hello"))
    (html5-parser::node-append-child* parent (make-text-node doc "world"))
    (is (string= "helloworld" (node-value (node-first-child parent))))))

;; (deftest test-node-clone ()
;;   (let* ((tree (make-tree))
;;          (parent (tree-make-element tree "parent" nil))
;;          (element (tree-make-element tree "test" nil)))
;;     (node-append-child tree parent element)
;;     (setf (node-attribute tree element "hello") "world")
;;     (let ((clone (node-clone tree element)))
;;       (is (null (node-parent tree clone)))
;;       (is (string= (node-attribute tree clone "hello") "world")))))
