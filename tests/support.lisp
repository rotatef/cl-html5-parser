;;;;  HTML5 parser for Common Lisp
;;;;
;;;;  Copyright (C) 2017 Thomas Bakketun <thomas.bakketun@copyleft.no>
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

(defun html5lib-test-files (subdirectory &key (type "dat"))
  (directory (merge-pathnames (make-pathname :directory `(:relative ,subdirectory)
                                             :name :wild
                                             :type type)
                              (asdf:system-relative-pathname :cl-html5-parser-tests "testdata/"))))

(defun parse-test-part (in)
  (let ((line (read-line in nil)))
    (when line
      (assert (char= #\# (char line 0)))
      (let ((name (intern (string-upcase (subseq line 1)) :keyword))
            (value (with-output-to-string (out)
                     (loop for next-char = (peek-char nil in nil)
                          while (and next-char (char/= #\# next-char))
                          do (write-line (read-line in) out)))))
        (list name (subseq value 0 (max 0 (1- (length value)))))))))


(defun parse-one-test (in)
  (loop for part = (parse-test-part in)
     while part
     append part
     until (eql (car part) :document)))

(defun parse-test-data (filename)
  (with-open-file (in filename)
    (loop for test = (parse-one-test in)
         while test
         collect test)))

