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

;; Printing for tests

(defun print-node (node stream)
  (ecase (node-type node)
    (:document-type
     (format stream "<!DOCTYPE ~A" (node-name node))
     (when (or (node-public-id node)
               (node-system-id node))
       (format stream " \"~A\" \"~A\""
               (or (node-public-id node) "")
               (or (node-system-id node) "")))
     (format stream ">"))
    (:comment
     (format stream "<!-- ~A -->" (node-value node)))
    (:element
     (if (and (node-namespace node)
              (string/= (node-namespace node)
                        (html5-constants::find-namespace "html")))
         (format stream "<~A ~A>"
                 (html5-constants::find-prefix (node-namespace node))
                 (node-name node))
         (format stream "<~A>" (node-name node))))
    (:text
     (format stream "\"~A\"" (node-value node)))))

(defun print-tree (node &key (stream *standard-output*) (indent 0))
  (ecase (node-type node)
    ((:document :document-fragment)
     (element-map-children (lambda (child)
                             (print-tree child
                                         :stream stream
                                         :indent (+ indent 2)))
                           node))
    (:element
     (format stream "~&|~vT" indent)
     (print-node node stream)
     (incf indent 2)
     (let ((attributes))
       (element-map-attributes* (lambda (name namespace value)
                                  (push (cons (cons name namespace) value) attributes))
                                node)
       (when attributes
         (loop for (name . value) in (sort attributes #'string<
                                           :key (lambda (attr)
                                                  (if (consp (car attr))
                                                      (caar attr)
                                                      (car attr))))
               do
               (format stream "~&|~vT" indent)
               (if (cdr name)
                   (format stream "~A ~A" (html5-constants:find-prefix (cdr name)) (car name))
                   (format stream "~A" (car name)))
               (format stream "=\"~A\"" value)))
       (element-map-children (lambda (child)
                               (print-tree child
                                           :stream stream
                                           :indent indent))
                          node)))
     ((:text :comment :document-type)
      (format stream "~&|~vT" indent)
      (print-node node stream)))
  node)


(defparameter *parser-tests-to-skip*
  ())

(defun do-parser-test (&key test-name data errors document document-fragment)
  (with-simple-restart (skip "Skip test ~A ~A"
                             test-name
                             data)
    (format t "~&Test ~A: ~A~%" test-name data)
    (setf document (string-right-trim '(#\Newline) document))
    (when (member data *parser-tests-to-skip* :test #'string=)
      (format t " skipped")
      (return-from do-parser-test))
    (multiple-value-bind (result-document got-errors)
        (if document-fragment
            (parse-html5-fragment data :container document-fragment)
            (parse-html5 data))
      (let ((result (with-output-to-string (out)
                      (print-tree result-document :stream out))))
        (unless (string= document result)
          (error "Input:~%~A~%Got:~%~A~%Expected:~%~A" data result document))
        (setf errors (split-sequence:split-sequence #\Newline errors
                                                    :remove-empty-subseqs t))
        (when (and errors
                   (/= (length errors) (length got-errors)))
          (warn "Errors mismatch~&Input:~%~A~%Got:~%~{~&~A~}~%Expected:~%~{~&~A~}"
                data got-errors errors)))
      result-document)))


(defun test-parser ()
  (let ((files (html5lib-test-files "tree-construction")))
    (dolist (file files)
      (let ((test-name (pathname-name file))
            (tests (parse-test-data file)))
        (dolist (test tests)
          (apply #'do-parser-test :test-name test-name test))))))


(in-root-suite)
(defsuite parser-tests)
(in-suite parser-tests)

(deftest test-parse-content-attr ()
  (is (eql nil (html5-parser::parse-content-attr "garble")))
  (is (eql nil (html5-parser::parse-content-attr "charset")))
  (is (string= "utf-8" (html5-parser::parse-content-attr "charset=utf-8")))
  (is (string= "utf-8" (html5-parser::parse-content-attr "charset = utf-8")))
  (is (string= "utf-8" (html5-parser::parse-content-attr "	charset = utf-8  ")))
  (is (string= " utf-8  " (html5-parser::parse-content-attr "	charset =' utf-8  '")))
  (is (eql nil (html5-parser::parse-content-attr "	charset =\"utf-8  '")))
  (is (string= "utf-8" (html5-parser::parse-content-attr "	charset =\"utf-8\"")))
  (is (string= "utf-8" (html5-parser::parse-content-attr "	charset =\"utf-8\"   "))))
