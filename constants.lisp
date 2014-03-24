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

(in-package :html5-constants)

(defmacro defglobal (name value &optional docstring)
  (let ((docstring (and docstring (list docstring))))
    ;; SBCL evaluates the init value of a global at compile time.
    #+sbcl `(progn (sb-ext:defglobal ,name nil ,@docstring)
                   (setq ,name ,value)
                   ',name)
    #+ccl `(ccl:defstatic ,name ,value ,@docstring)
    #-(or sbcl ccl) `(defparameter ,name ,value ,@docstring)))

(defglobal +namespaces+
  '(("html" . "http://www.w3.org/1999/xhtml")
    ("mathml" ."http://www.w3.org/1998/Math/MathML")
    ("svg" . "http://www.w3.org/2000/svg")
    ("xlink" . "http://www.w3.org/1999/xlink")
    ("xml" . "http://www.w3.org/XML/1998/namespace")
    ("xmlns" . "http://www.w3.org/2000/xmlns/")))

(defun find-namespace (prefix)
  (cdr (assoc prefix +namespaces+ :test #'string=)))

(defun find-prefix (namespace)
  (car (find namespace +namespaces+ :test #'string= :key #'cdr)))

(defglobal +scoping-elements+
  `((,(find-namespace "html") . "applet")
    (,(find-namespace "html") . "caption")
    (,(find-namespace "html") . "html")
    (,(find-namespace "html") . "marquee")
    (,(find-namespace "html") . "object")
    (,(find-namespace "html") . "table")
    (,(find-namespace "html") . "td")
    (,(find-namespace "html") . "th")
    (,(find-namespace "mathml") . "mi")
    (,(find-namespace "mathml") . "mo")
    (,(find-namespace "mathml") . "mn")
    (,(find-namespace "mathml") . "ms")
    (,(find-namespace "mathml") . "mtext")
    (,(find-namespace "mathml") . "annotation-xml")
    (,(find-namespace "svg") . "foreignObject")
    (,(find-namespace "svg") . "desc")
    (,(find-namespace "svg") . "title")))


(defglobal +formatting-elements+
  `((,(find-namespace "html") . "a")
    (,(find-namespace "html") . "b")
    (,(find-namespace "html") . "big")
    (,(find-namespace "html") . "code")
    (,(find-namespace "html") . "em")
    (,(find-namespace "html") . "font")
    (,(find-namespace "html") . "i")
    (,(find-namespace "html") . "nobr")
    (,(find-namespace "html") . "s")
    (,(find-namespace "html") . "small")
    (,(find-namespace "html") . "strike")
    (,(find-namespace "html") . "strong")
    (,(find-namespace "html") . "tt")
    (,(find-namespace "html") . "u")))

(defglobal +special-elements+
  `((,(find-namespace "html") . "address")
    (,(find-namespace "html") . "applet")
    (,(find-namespace "html") . "area")
    (,(find-namespace "html") . "article")
    (,(find-namespace "html") . "aside")
    (,(find-namespace "html") . "base")
    (,(find-namespace "html") . "basefont")
    (,(find-namespace "html") . "bgsound")
    (,(find-namespace "html") . "blockquote")
    (,(find-namespace "html") . "body")
    (,(find-namespace "html") . "br")
    (,(find-namespace "html") . "button")
    (,(find-namespace "html") . "caption")
    (,(find-namespace "html") . "center")
    (,(find-namespace "html") . "col")
    (,(find-namespace "html") . "colgroup")
    (,(find-namespace "html") . "command")
    (,(find-namespace "html") . "dd")
    (,(find-namespace "html") . "details")
    (,(find-namespace "html") . "dir")
    (,(find-namespace "html") . "div")
    (,(find-namespace "html") . "dl")
    (,(find-namespace "html") . "dt")
    (,(find-namespace "html") . "embed")
    (,(find-namespace "html") . "fieldset")
    (,(find-namespace "html") . "figure")
    (,(find-namespace "html") . "footer")
    (,(find-namespace "html") . "form")
    (,(find-namespace "html") . "frame")
    (,(find-namespace "html") . "frameset")
    (,(find-namespace "html") . "h1")
    (,(find-namespace "html") . "h2")
    (,(find-namespace "html") . "h3")
    (,(find-namespace "html") . "h4")
    (,(find-namespace "html") . "h5")
    (,(find-namespace "html") . "h6")
    (,(find-namespace "html") . "head")
    (,(find-namespace "html") . "header")
    (,(find-namespace "html") . "hr")
    (,(find-namespace "html") . "html")
    (,(find-namespace "html") . "iframe")
    ;; Note that image is commented out in the spec as "this isn't an
    ;; element that can end up on the stack, so it doesn't matter,"
    (,(find-namespace "html") . "image")
    (,(find-namespace "html") . "img")
    (,(find-namespace "html") . "input")
    (,(find-namespace "html") . "isindex")
    (,(find-namespace "html") . "li")
    (,(find-namespace "html") . "link")
    (,(find-namespace "html") . "listing")
    (,(find-namespace "html") . "marquee")
    (,(find-namespace "html") . "menu")
    (,(find-namespace "html") . "meta")
    (,(find-namespace "html") . "nav")
    (,(find-namespace "html") . "noembed")
    (,(find-namespace "html") . "noframes")
    (,(find-namespace "html") . "noscript")
    (,(find-namespace "html") . "object")
    (,(find-namespace "html") . "ol")
    (,(find-namespace "html") . "p")
    (,(find-namespace "html") . "param")
    (,(find-namespace "html") . "plaintext")
    (,(find-namespace "html") . "pre")
    (,(find-namespace "html") . "script")
    (,(find-namespace "html") . "section")
    (,(find-namespace "html") . "select")
    (,(find-namespace "html") . "style")
    (,(find-namespace "html") . "table")
    (,(find-namespace "html") . "tbody")
    (,(find-namespace "html") . "td")
    (,(find-namespace "html") . "textarea")
    (,(find-namespace "html") . "tfoot")
    (,(find-namespace "html") . "th")
    (,(find-namespace "html") . "thead")
    (,(find-namespace "html") . "title")
    (,(find-namespace "html") . "tr")
    (,(find-namespace "html") . "ul")
    (,(find-namespace "html") . "wbr")
    (,(find-namespace "html") . "xmp")
    (,(find-namespace "svg") . "foreignObject")))

(defglobal +html-integration-point-elements+
  `((,(find-namespace "mathml") . "annotation-xml")
    (,(find-namespace "svg") . "foreignObject")
    (,(find-namespace "svg") . "desc")
    (,(find-namespace "svg") . "title")))

(defglobal +mathml-text-integration-point-elements+
  `((,(find-namespace "mathml") . "mi")
    (,(find-namespace "mathml") . "mo")
    (,(find-namespace "mathml") . "mn")
    (,(find-namespace "mathml") . "ms")
    (,(find-namespace "mathml") . "mtext")))

(defconstant +eof+ '+eof+)

(defglobal +token-types+
  '(:doctype 0
    :characters 1
    :space-characters 2
    :star-ttag 3
    :end-tag 4
    :empty-tag 5
    :comment 6
    :parse-error 7))

(defglobal +tag-token-types+
  '(:start-tag :end-tag :empty-tag))

(defglobal +space-characters+
  '(#\Tab
    #\Newline
    #\u000C
    #\Space
    #\Return))

(defglobal +table-insert-mode-elements+
  '("table"
    "tbody"
    "tfoot"
    "thead"
    "tr"))

(defglobal +ascii-lowercase+ "abcdefghijklmnopqrstuvwxyz")
(defglobal +ascii-uppercase+ "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(defglobal +ascii-letters+ "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
(defglobal +digits+ "0123456789")
(defglobal +hex-digits+ "0123456789abcdefABCDEF")

(defun ascii-letter-p (c)
  (let ((code (char-code c)))
    (or (<= #.(char-code #\a) code #.(char-code #\z))
        (<= #.(char-code #\A) code #.(char-code #\Z)))))

(defun ascii-upper-2-lower (string)
  (let ((out (copy-seq string)))
    (dotimes (i (length string))
      (let ((p (position (char out i) +ascii-uppercase+)))
        (when p
          (setf (char out i) (char +ascii-lowercase+ p)))))
    out))


(defglobal +replacement-characters+
  '(#x0 #\uFFFD
    #x0d #\u000D
    #x80 #\u20AC
    #x81 #\u0081
    #x81 #\u0081
    #x82 #\u201A
    #x83 #\u0192
    #x84 #\u201E
    #x85 #\u2026
    #x86 #\u2020
    #x87 #\u2021
    #x88 #\u02C6
    #x89 #\u2030
    #x8A #\u0160
    #x8B #\u2039
    #x8C #\u0152
    #x8D #\u008D
    #x8E #\u017D
    #x8F #\u008F
    #x90 #\u0090
    #x91 #\u2018
    #x92 #\u2019
    #x93 #\u201C
    #x94 #\u201D
    #x95 #\u2022
    #x96 #\u2013
    #x97 #\u2014
    #x98 #\u02DC
    #x99 #\u2122
    #x9A #\u0161
    #x9B #\u203A
    #x9C #\u0153
    #x9D #\u009D
    #x9E #\u017E
    #x9F #\u0178))


(defglobal +cdata-elements+
  '("title"
    "textarea"))

(defglobal +rcdata-elements+
  '("style"
    "script"
    "xmp"
    "iframe"
    "noembed"
    "noframes"
    "noscript"))

(defglobal +html-integration-point-elements+
  `((,(find-namespace "mathml") . "annotation-xml")
    (,(find-namespace "svg") . "foreignObject")
    (,(find-namespace "svg") . "desc")
    (,(find-namespace "svg") . "title")))

(defglobal +mathml-text-integration-point-elements+
  `((,(find-namespace "mathml") . "mi")
    (,(find-namespace "mathml") . "mo")
    (,(find-namespace "mathml") . "mn")
    (,(find-namespace "mathml") . "ms")
    (,(find-namespace "mathml") . "mtext")))

(defun make-hash-lookup (replacements)
  (let ((rhash (make-hash-table :test #'equalp)))
    (loop for (from to) in replacements
       do (setf (gethash from rhash) to))))


(defglobal +quirks-mode-doctypes-regexp+
  (cl-ppcre:create-scanner
   '(:sequence :start-anchor
     (:alternation
      "+//silmaril//dtd html pro v0r11 19970101//"
      "-//advasoft ltd//dtd html 3.0 aswedit + extensions//"
      "-//as//dtd html 3.0 aswedit + extensions//"
      "-//ietf//dtd html 2.0 level 1//"
      "-//ietf//dtd html 2.0 level 2//"
      "-//ietf//dtd html 2.0 strict level 1//"
      "-//ietf//dtd html 2.0 strict level 2//"
      "-//ietf//dtd html 2.0 strict//"
      "-//ietf//dtd html 2.0//"
      "-//ietf//dtd html 2.1e//"
      "-//ietf//dtd html 3.0//"
      "-//ietf//dtd html 3.2 final//"
      "-//ietf//dtd html 3.2//"
      "-//ietf//dtd html 3//"
      "-//ietf//dtd html level 0//"
      "-//ietf//dtd html level 1//"
      "-//ietf//dtd html level 2//"
      "-//ietf//dtd html level 3//"
      "-//ietf//dtd html strict level 0//"
      "-//ietf//dtd html strict level 1//"
      "-//ietf//dtd html strict level 2//"
      "-//ietf//dtd html strict level 3//"
      "-//ietf//dtd html strict//"
      "-//ietf//dtd html//"
      "-//metrius//dtd metrius presentational//"
      "-//microsoft//dtd internet explorer 2.0 html strict//"
      "-//microsoft//dtd internet explorer 2.0 html//"
      "-//microsoft//dtd internet explorer 2.0 tables//"
      "-//microsoft//dtd internet explorer 3.0 html strict//"
      "-//microsoft//dtd internet explorer 3.0 html//"
      "-//microsoft//dtd internet explorer 3.0 tables//"
      "-//netscape comm. corp.//dtd html//"
      "-//netscape comm. corp.//dtd strict html//"
      "-//o'reilly and associates//dtd html 2.0//"
      "-//o'reilly and associates//dtd html extended 1.0//"
      "-//o'reilly and associates//dtd html extended relaxed 1.0//"
      "-//softquad software//dtd hotmetal pro 6.0::19990601::extensions to html 4.0//"
      "-//softquad//dtd hotmetal pro 4.0::19971010::extensions to html 4.0//"
      "-//spyglass//dtd html 2.0 extended//"
      "-//sq//dtd html 2.0 hotmetal + extensions//"
      "-//sun microsystems corp.//dtd hotjava html//"
      "-//sun microsystems corp.//dtd hotjava strict html//"
      "-//w3c//dtd html 3 1995-03-24//"
      "-//w3c//dtd html 3.2 draft//"
      "-//w3c//dtd html 3.2 final//"
      "-//w3c//dtd html 3.2//"
      "-//w3c//dtd html 3.2s draft//"
      "-//w3c//dtd html 4.0 frameset//"
      "-//w3c//dtd html 4.0 transitional//"
      "-//w3c//dtd html experimental 19960712//"
      "-//w3c//dtd html experimental 970421//"
      "-//w3c//dtd w3 html//"
      "-//w3o//dtd w3 html 3.0//"
      "-//webtechs//dtd mozilla html 2.0//"
      "-//webtechs//dtd mozilla html//"))))

(defglobal +heading-elements+
  '("h1"
    "h2"
    "h3"
    "h4"
    "h5"
    "h6"))
