;;;;  HTML5 parser for Common Lisp
;;;;
;;;;  Copyright (C) 2017 Thomas Bakketun <thomas.bakketun@copyleft.no>
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


(defun xml-escape-name (name)
  "Escapes a node name (element, attribute, doctype) by replacing any
character not valid in XML name by Uxxxxxx, where x is the code point
as six hex digits. This encoding is reversable, since the HTML parser
down cases all characters in names.

See: https://www.w3.org/TR/html5/syntax.html#coercing-an-html-dom-into-an-infoset"
  (if (and (xml-name-start-char-p (char name 0))
           (every #'xml-name-char-p name))
      name
      (with-output-to-string (out)
        (loop for first = t then nil
              for c across name do
                (if (if first
                        (xml-name-start-char-p c)
                        (xml-name-char-p c))
                    (princ c out)
                    (format out "U~:@(~6,'0X~)" (char-code c)))))))


(defun xml-unescape-name (name)
  "Reverert escaping done by xml-unescape-name."
  (cl-ppcre:regex-replace-all
   "U[0-9A-F]{6}"
   name
   (lambda (u)
     (string (code-char (parse-integer u :start 1 :radix 16))))
   :simple-calls t))


(defun xml-name-start-char-p (c)
  (or (char<= #\a c #\z)
      (char= #\_ c)
      (char<= #\A c #\Z)
      (char<= (code-char #xC0) c (code-char #xD6))
      (char<= (code-char #xD8) c (code-char #xF6))
      (char<= (code-char #xF8) c (code-char #x2FF))
      (char<= (code-char #x370) c (code-char #x37D))
      (char<= (code-char #x37F) c (code-char #x1FFF))
      (char<= (code-char #x200C) c (code-char #x200D))
      (char<= (code-char #x2070) c (code-char #x218F))
      (char<= (code-char #x2C00) c (code-char #x2FEF))
      (char<= (code-char #x3001) c (code-char #xD7FF))
      (char<= (code-char #xF900) c (code-char #xFDCF))
      (char<= (code-char #xFDF0) c (code-char #xFFFD))
      (char<= (code-char #x10000) c (code-char #xEFFFF))))


(defun xml-name-char-p (c)
  (or (xml-name-start-char-p c)
      (char= #\- c)
      (char= #\. c)
      (char<= #\0 c #\9)
      (char= (code-char #xB7) c)
      (char<= (code-char #x0300) c (code-char #x036F))
      (char<= (code-char #x203F) c (code-char #x2040))))
