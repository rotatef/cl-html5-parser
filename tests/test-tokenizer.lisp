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

(defun run-tokenizer-test-parser (initial-state last-start-tag source encoding)
  (let ((tokenizer (html5-parser::make-html-tokenizer source :encoding encoding))
        (output-tokens '()))
    (setf (slot-value tokenizer 'html5-parser::state) initial-state)
    (when last-start-tag
      (setf (slot-value tokenizer 'html5-parser::current-token)
            (list :type :start-tag
                  :name last-start-tag)))
    (html5-parser::map-tokens
     tokenizer
     (lambda (token)
;       (print token)
       (push (ecase (getf token :type)
               (:doctype
                (list :type :doctype
                      :name (getf token :name)
                      :public-id (getf token :public-id)
                      :system-id (getf token :system-id)
                      :correct (getf token :correct)))
               ((:start-tag :empty-tag)
                (list :type (getf token :type)
                      :name (getf token :name)
                      :data (remove-duplicates (getf token :data)
                                               :key #'car
                                               :test #'string=
                                               :from-end t)
                      :self-closing (getf token :self-closing)))
               (:end-tag
                (list :type :end-tag
                      :name (getf token :name)))
               (:comment
                (list :type :comment
                      :data (getf token :data)))
               ((:space-characters :characters)
                (list :type :characters
                      :data (getf token :data)))
               (:parse-error
                (list :type :parse-error)))
             output-tokens)))
    (nreverse output-tokens)))

(defun concatenate-character-tokens (tokens)
  (let ((output-tokens '()))
    (dolist (token tokens)
      (if (and (eql :characters (getf (car output-tokens) :type))
               (eql :characters (getf token :type)))
          (setf (getf (car output-tokens) :data)
                (concatenate 'string
                             (getf (car output-tokens) :data)
                             (getf token :data)))
          (push token output-tokens)))
    (nreverse output-tokens)))

(defun tokens-match (expected-tokens received-tokens ignore-error-order)
  "Test whether the test has passed or failed

   If the ignoreErrorOrder flag is set to true we don't test the relative
   positions of parse errors and non parse errors
  "
  (labels ((group (tokens)
             ;; Sort the tokens into two groups; non-parse errors and parse errors
             (cons (remove-if (lambda (x)
                                (eql (getf x :type) :parse-error))
                              tokens)
                   (remove-if-not (lambda (x)
                                    (eql (getf x :type) :parse-error))
                                  tokens))))
    (when ignore-error-order
      (setf expected-tokens (group expected-tokens))
      (setf received-tokens (group received-tokens)))
    (equal expected-tokens received-tokens)))

(defun temp-fix (data)
  (flex:octets-to-string data
                         :external-format :utf-16le))

(defun run-tokenizer-test (test-name initial-state test)
  (with-simple-restart (skip "Skip test ~A ~A: ~A"
                             test-name
                             initial-state
                             (getf test :description))
    (let* ((expected (getf test :output))
           (tokens (run-tokenizer-test-parser initial-state
                                              (getf test :last-start-tag)
                                              (getf test :input)
                                              :utf-16le))
           (received (concatenate-character-tokens tokens)))
      (unless (tokens-match expected received (getf test :ignore-error-order))
        (error "Test failed ~S ~%Expected: ~S~%Received: ~S" test expected received)))))

(defun utf16-string-to-octets (string)
  (when string
    (coerce (loop for c across string
               for i = (char-code c)
               collect (ldb (byte 8 0) i)
               collect (ldb (byte 8 8) i))
            '(vector (unsigned-byte 8)))))

(defun double-unescape (string)
  (when string
    (json:decode-json-from-string (format nil "\"~A\"" string))))

(defun data-to-octects (string double-escaped)
  (when string
    (utf16-string-to-octets (if double-escaped
                                (double-unescape string)
                                string))))

(defun fix-output (output double-escaped)
  (flet ((unescape (string)
           (when string
             (flex:octets-to-string (data-to-octects string double-escaped)
                                    :external-format :utf-16le))))
    (loop for value in output collect
         (if (consp value)
             (cond ((string= "Character" (car value))
                    (assert (= 2 (length value)))
                    (list :type :characters :data (unescape (second value))))
                   ((string= "Comment" (car value))
                    (assert (= 2 (length value)))
                    (list :type :comment :data (unescape (second value))))
                   ((string= "StartTag" (car value))
                    (assert (<= 3 (length value) 4))
                    (list :type :start-tag
                          :name (unescape (second value))
                          :data (loop for (attr . attr-value) in (third value)
                                   collect (cons (unescape (string attr))
                                                 (unescape attr-value)))
                          :self-closing (fourth value)))
                   ((string= "EndTag" (car value))
                    (assert (= 2 (length value)))
                    (list :type :end-tag :name (unescape (second value))))
                   ((string= "DOCTYPE" (car value))
                    (assert (= 5 (length value)))
                    (list :type :doctype
                          :name (unescape (second value))
                          :public-id (unescape (third value))
                          :system-id (unescape (fourth value))
                          :correct (fifth value)))
                   (t (error "Unexpected token type ~S" (car value))))
             (if (equal "ParseError" value)
                 (list :type :parse-error)
                 (error "Unexpected token type ~S" value))))))

(defun find-state-symbol (string)
  (let ((symbol (find-symbol (substitute #\- #\Space (string-upcase string)) :keyword)))
    (assert symbol () "Unkown state ~S" string)
    symbol))

(defun load-tests (filename)
  (loop for test in (cdr (assoc :|tests|
                                (let ((json:*json-identifier-name-to-lisp* #'identity))
                                  (json:decode-json-from-source filename))))
     for double-escaped = (cdr (assoc :|doubleEscaped| test))
     for parsed =
       (loop for (key . value) in test
          append (ecase key
                   (:|description|
                     (list :description value))
                   (:|initialStates|
                     (list :initial-states (mapcar #'find-state-symbol value)))
                   (:|lastStartTag|
                     (list :last-start-tag value))
                   (:|input|
                     (list :input (data-to-octects value double-escaped)))
                   (:|output|
                     (list :output
                           (fix-output value double-escaped)))
                   (:|doubleEscaped|
                     (list :double-escaped value))
                   (:|ignoreErrorOrder|
                     (list :ignore-error-order value))))
       do (unless (getf parsed :initial-states)
            (setf parsed `(,@parsed :initial-states (:data-state))))
       collect parsed))

(defparameter *skip-tests*
  '(("domjs"
     "--!NUL in comment ") ; to many ParseErrors. Wrong test?
    ("unicodeCharsProblematic" :skip) ; flexi-streams locks up on some of these tests.
    ))

(defun test-tokenizer ()
  (loop for filename in (html5lib-test-files "tokenizer" :type "test")
     for test-name = (pathname-name filename)
     for tests = (load-tests filename)
     for skip = (cdr (assoc test-name *skip-tests* :test #'string=))
     unless (eql (first skip) :skip)
     do
       (dolist (test tests)
         (unless (find (getf test :description) skip :test #'string=)
           (dolist (initial-state (getf test :initial-states))
             (run-tokenizer-test test-name initial-state test))))))
