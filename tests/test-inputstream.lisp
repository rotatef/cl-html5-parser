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
(defsuite input-stream-tests)
(in-suite input-stream-tests)

(deftest test-read-char ()
  (let ((stream (html5-parser::make-html-input-stream "hello")))
    (is (eql #\h (html5-parser::html5-stream-char stream)))
    (is (eql #\e (html5-parser::html5-stream-char stream)))))

(deftest test-unget ()
  (let ((stream (html5-parser::make-html-input-stream "hei")))
    (is (eql #\h (html5-parser::html5-stream-char stream)))
    (is (eql #\e (html5-parser::html5-stream-char stream)))
    (is (eql #\i (html5-parser::html5-stream-char stream)))
    (is (eql html5-constants::+eof+ (html5-parser::html5-stream-char stream)))
    (html5-parser::html5-stream-unget stream html5-constants::+eof+)
    (html5-parser::html5-stream-unget stream #\i)
    (is (eql #\i (html5-parser::html5-stream-char stream)))
    (is (eql html5-constants::+eof+ (html5-parser::html5-stream-char stream)))))

(deftest test-chars-until ()
  (let ((stream (html5-parser::make-html-input-stream "hello<--__-->a")))
    (is (equal "hello" (html5-parser::html5-stream-chars-until stream "><")))
    (is (eql #\< (html5-parser::html5-stream-char stream)))
    (is (equal "--__-->" (html5-parser::html5-stream-chars-until stream "<>-_" t)))
    (is (eql #\a (html5-parser::html5-stream-char stream)))))

(deftest test-chars-until-eof ()
  (let ((stream (html5-parser::make-html-input-stream "hello")))
    (is (equal "hello" (html5-parser::html5-stream-chars-until stream "?")))
    (is (eql html5-constants::+eof+ (html5-parser::html5-stream-char stream)))))

(deftest test-line-ending-fix ()
  (let ((stream (html5-parser::make-html-input-stream (coerce #(#\a #\Newline
                                                                #\b #\Return
                                                                #\c #\Return #\Newline
                                                                #\d)
                                                              'string))))
    (is (eql #\a (html5-parser::html5-stream-char stream)))
    (is (eql #\Newline (html5-parser::html5-stream-char stream)))
    (is (eql #\b (html5-parser::html5-stream-char stream)))
    (is (eql #\Newline (html5-parser::html5-stream-char stream)))
    (is (eql #\c (html5-parser::html5-stream-char stream)))
    (is (eql #\Newline (html5-parser::html5-stream-char stream)))
    (is (eql #\d (html5-parser::html5-stream-char stream)))
    (is (eql html5-constants::+eof+ (html5-parser::html5-stream-char stream)))))

(deftest test-line-ending-fix2 ()
  (let ((stream (html5-parser::make-html-input-stream (coerce #(#\< #\? #\Return)
                                                              'string))))
    (is (eql #\< (html5-parser::html5-stream-char stream)))
    (is (eql #\? (html5-parser::html5-stream-char stream)))
    (is (eql #\Newline (html5-parser::html5-stream-char stream)))
    (is (eql html5-constants::+eof+ (html5-parser::html5-stream-char stream)))))


(deftest test-bom ()
  (let ((stream (html5-parser::make-html-input-stream #(#xef #xbb #xbf 39))))
    (is (eql (car (html5-parser::html5-stream-encoding stream))
             :utf-8))
    (is (eql (html5-parser::html5-stream-char stream)
             #\'))))

