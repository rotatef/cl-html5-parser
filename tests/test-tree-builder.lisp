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

(defun make-tree ()
  (make-instance *default-tree-builder*))

(deftest test-make-tree ()
  (is (not (null (tree-document (make-tree))))))

(deftest test-append-child ()
  (let* ((tree (make-tree))
         (child (tree-make-element tree "test" nil)))
    (node-append-child tree (tree-document tree) child)
    (node-map-children tree (lambda (kid)
                              (is (eq kid child)))
                       (tree-document tree))))

(deftest test-reappend-child ()
  (let* ((tree (make-tree))
         (parent1 (tree-make-element tree "parent1" nil))
         (parent2 (tree-make-element tree "parent2" nil))
         (child (tree-make-element tree "child" nil)))
    (node-append-child tree parent1 child)
    (is (eq parent1 (node-parent tree child)))
    (node-append-child tree parent2 child)
    (is (eq parent2 (node-parent tree child)))
    (node-map-children tree (lambda (kid)
                              (error "parent1 should not have children now ~S" kid))
                       parent1)))


(deftest test-navigate ()
  (let* ((tree (make-tree))
         (parent (tree-make-element tree "parent" nil))
         (child1 (tree-make-element tree "child1" nil))
         (child2 (tree-make-element tree "child2" nil))
         (child3 (tree-make-element tree "child3" nil))
         (child4 (tree-make-element tree "child4" nil)))
    (node-append-child tree parent child1)
    (node-append-child tree parent child2)
    (node-append-child tree parent child3)
    (node-append-child tree parent child4)
    (is (eq child1 (node-first-child tree parent)))
    (is (eq child4 (node-last-child tree parent)))
    (is (eq child2 (node-next-sibling tree child1)))
    (is (eq nil (node-next-sibling tree child4)))
    (is (eq child1 (node-previous-sibling tree child2)))
    (is (eq nil (node-previous-sibling tree child1)))))

(deftest test-remove-child ()
  (let* ((tree (make-tree))
         (parent (tree-make-element tree "parent" nil))
         (child1 (tree-make-element tree "child1" nil))
         (child2 (tree-make-element tree "child2" nil))
         (child3 (tree-make-element tree "child3" nil))
         (child4 (tree-make-element tree "child4" nil)))
    (node-append-child tree parent child1)
    (node-append-child tree parent child2)
    (node-append-child tree parent child3)
    (node-append-child tree parent child4)

    (node-remove-child tree parent child2)
    (is (eq child3 (node-next-sibling tree child1)))))

(deftest test-set-attribute ()
  (let* ((tree (make-tree))
         (element (tree-make-element tree "test" nil)))
    (setf (node-attribute tree element "hello") "world")
    (is (string= (node-attribute tree element "hello") "world"))))

;; (deftest test-node-clone ()
;;   (let* ((tree (make-tree))
;;          (parent (tree-make-element tree "parent" nil))
;;          (element (tree-make-element tree "test" nil)))
;;     (node-append-child tree parent element)
;;     (setf (node-attribute tree element "hello") "world")
;;     (let ((clone (node-clone tree element)))
;;       (is (null (node-parent tree clone)))
;;       (is (string= (node-attribute tree clone "hello") "world")))))
