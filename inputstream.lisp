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

(in-package :html5-parser)

(deftype array-length ()
  "Type of an array index."
  '(integer 0 #.array-dimension-limit))

(deftype chunk ()
  "Type of the input stream buffer."
  '(vector character *))

(defparameter *default-encoding* :utf-8)

(defclass html-input-stream ()
  ((source :initarg :source)
   (encoding :reader html5-stream-encoding)
   (char-stream :initform nil)
   (chunk)
   (chunk-offset)
   (pending-cr)
   (errors :initform nil :accessor html5-stream-errors)))

(defun make-html-input-stream (source &key override-encoding fallback-encoding)
  (when (stringp source)
    ;; Encoding is not relevant when input is a string,
    ;; but we set it utf-8 here to avoid auto detecting taking place.
    (setf override-encoding :utf-8))
  (let ((self (make-instance 'html-input-stream :source source)))
    (with-slots (encoding stream) self
      (setf encoding (detect-encoding self
                                      (find-encoding override-encoding)
                                      (find-encoding fallback-encoding)))
      (open-char-stream self))
    self))

;; 12.2.2.2 Character encodings
(defun find-encoding (encoding-name)
  ;; Normalize the string designator
  (setf encoding-name (string-upcase (substitute #\- #\_ (string-trim +space-characters+ (string encoding-name)))))
  ;; All known encoding will already be interned in the keyword package so find-symbol is fine here
  (setf encoding-name (find-symbol encoding-name :keyword))

  (handler-case
      ;; Verfiy that flexi-streams knows the encoding and resolve aliases
      (case (flex:external-format-name (flex:make-external-format encoding-name))
        ;; Some encoding should be replaced by some other.
        ;; Only those supported by flexi-streams are listed here.
        ;; iso-8859-11 should be replaced by windows-874, but flexi-streams doesn't that encoding.
        (:iso-8859-1 :windows-1252)
        (:iso-8859-9 :windows-1254)
        (:us-ascii :windows-1252)
        (otherwise encoding-name))
    (flex:external-format-error ())))

;; 12.2.2.1 Determining the character encoding
(defun detect-encoding (stream override-encoding fallback-encoding)
  (with-slots (encoding) stream
    (block nil
      ;; 1. and 2. encoding overridden by user or transport layer
      (when override-encoding
        (return (cons override-encoding :certain)))

      ;; 3. wait for 1024 bytes, not implemented

      ;; 4. Detect BOM
      (let ((bom-encoding (detect-bom stream)))
        (when bom-encoding
          (return (cons bom-encoding :certain))))

      ;; 5. Prescan not implemented

      ;; 6. Use fallback encoding
      (when fallback-encoding
        (return (cons encoding :tentative)))

      ;; 7. Autodect not implemented

      ;; 8. Implementation-defined default
      (return (cons *default-encoding* :tentative)))))

(defmacro handle-encoding-errors (stream &body body)
  `(handler-bind ((flex:external-format-encoding-error
                   (lambda (x)
                     (declare (ignore x))
                     (push :invalid-codepoint (html5-stream-errors ,stream))
                     (use-value #\uFFFD))))
     ,@body))

(defun open-char-stream (self)
  (with-slots (source encoding char-stream chunk chunk-offset pending-cr) self
    (setf chunk (make-array (* 10 1024) :element-type 'character :fill-pointer 0))
    (setf chunk-offset 0)
    (setf pending-cr nil)
    (when char-stream
      (close char-stream))
    (setf char-stream
          (if (stringp source)
              (make-string-input-stream source)
              (flex:make-flexi-stream
               (etypecase source
                 (pathname
                  (open source :element-type '(unsigned-byte 8)))
                 (stream
                  source)
                 (vector
                  (flex:make-in-memory-input-stream source)))
               :external-format (flex:make-external-format (car encoding) :eol-style :lf))))
    ;; 12.2.2.4 says we should always skip the first byte order mark
    (handle-encoding-errors self
      (let ((first-char (peek-char nil char-stream nil)))
        (when (eql first-char #\ufeff)
          (read-char char-stream))))))

(defun detect-bom (self)
  (with-slots (source) self
    (let (byte-0 byte-1 byte-2)
      (etypecase source
        (vector
         (when (> (length source) 0) (setf byte-0 (aref source 0)))
         (when (> (length source) 1) (setf byte-1 (aref source 1)))
         (when (> (length source) 2) (setf byte-2 (aref source 2))))
        (pathname
         (with-open-file (in source :element-type '(unsigned-byte 8))
           (setf byte-0 (read-byte in nil))
           (setf byte-1 (read-byte in nil))
           (setf byte-2 (read-byte in nil))))
        (stream
         (error "Can't detect encoding when source is a stream.")))
      (cond ((and (eql byte-0 #xfe)
                  (eql byte-1 #xff))
             :utf-16be)
            ((and (eql byte-0 #xff)
                  (eql byte-1 #xfe))
             :utf-16le)
            ((and (eql byte-0 #xef)
                  (eql byte-1 #xbb)
                  (eql byte-1 #xbf))
             :utf-8)))))

;; 12.2.2.3 Changing the encoding while parsing
(defun html5-stream-change-encoding (stream new-encoding)
  (setf new-encoding (find-encoding new-encoding))
  (with-slots (encoding char-stream) stream
    ;; 1.
    (when (member (car encoding) '(:utf-16le :utf-16be))
      (setf encoding (cons (car encoding) :certain))
      (return-from html5-stream-change-encoding))

    ;; 2.
    (when (member new-encoding '(:utf-16le :utf-16be))
      (setf new-encoding :utf-8))

    ;; 3.
    (when (eql (car encoding) new-encoding)
      (setf encoding (cons (car encoding) :certain))
      (return-from html5-stream-change-encoding))

    ;; 4. Not impleneted

    ;; 5. Restart paring from scratch
    (setf encoding (cons new-encoding :certain))
    (open-char-stream stream)
    (throw 'please-reparse t)))

(defun html5-stream-char (stream)
  (with-slots (chunk chunk-offset) stream
    (when (>= chunk-offset (length chunk))
      (unless (read-chunk stream)
        (return-from html5-stream-char +eof+)))
    (prog1 (char chunk chunk-offset)
      (incf chunk-offset))))

(defun our-scan (chars opposite-p chunk &key start)
  (loop for i from start below (length chunk)
        for char = (char chunk i)
        while (if opposite-p
                  (position char chars)
                  (not (position char chars)))
        finally (return i)))

(defun html5-stream-chars-until (stream characters &optional opposite-p)
  "Returns a string of characters from the stream up to but not
   including any character in characters or end of file.
   "
  (with-slots (chunk chunk-offset) stream
    (declare (array-length chunk-offset) (chunk chunk))
    (with-output-to-string (data)
      (loop for end = (our-scan characters opposite-p chunk :start chunk-offset) do
            ;; If nothing matched, and it wasn't because we ran out of chunk,
            ;; then stop
            (when (and (not end)
                       (/= chunk-offset (length chunk)))
              (return))
            ;; If not the whole chunk matched, return everything
            ;; up to the part that didn't match
            (when (and end
                       (/= chunk-offset (length chunk)))
              (write-string chunk data :start chunk-offset :end end)
              (setf chunk-offset end)
              (return))
            ;; If the whole remainder of the chunk matched,
            ;; use it all and read the next chunk
            (write-string chunk data :start chunk-offset)
            (unless (read-chunk stream)
              (return))))))

(defun html5-stream-unget (stream char)
  (with-slots (chunk chunk-offset) stream
    (unless (eql char +eof+)
      (cond ((zerop chunk-offset)
             (cond ((< (fill-pointer chunk) (array-dimension chunk 0))
                    (incf (fill-pointer chunk))
                    (replace chunk chunk :start1 1))
                   (t
                    (let ((new-chunk (make-array (1+ (array-dimension chunk 0))
                                                 :element-type 'character
                                                 :fill-pointer (1+ (fill-pointer chunk)))))
                      (replace new-chunk chunk :start1 1)
                      (setf chunk new-chunk))))
             (setf (char chunk 0) char))
            (t
             (decf chunk-offset)
             (assert (char= char (char chunk chunk-offset))))))))

(defun read-chunk (stream)
  (declare (optimize speed))
  (with-slots (char-stream chunk chunk-offset pending-cr) stream
    (declare (array-length chunk-offset)
             (chunk chunk))
    (setf chunk-offset 0)
    (let ((start 0))
      (when pending-cr
        (setf (char chunk 0) #\Return)
        (setf start 1)
        (setf pending-cr nil))

      (setf (fill-pointer chunk) (array-dimension chunk 0))
      (handle-encoding-errors stream
        (setf (fill-pointer chunk) (read-sequence chunk char-stream :start start)))

      (unless (zerop (length chunk))

        ;; check if last char is CR and EOF was not reached
        (when (and (= (length chunk) (array-dimension chunk 0))
                   (eql (char chunk (1- (length chunk))) #\Return))
          (setf pending-cr t)
          (decf (fill-pointer chunk)))

        (report-character-errors stream chunk)

        ;; Python code replaces surrugate pairs with U+FFFD here. Why?

        ;; Normalize line endings (CR LF)
        (loop for previous = nil then current
           for current across chunk
           for index of-type array-length from 0
           with offset of-type array-length = 0
           do (unless (and (eql previous #\Return)
                           (eql current #\Newline))
                (unless (= index offset)
                  (setf (char chunk offset) current))
                (when (eql current #\Return)
                  (setf (char chunk offset) #\Newline))
                (incf offset))
           finally (setf (fill-pointer chunk) offset))

        t))))

(defun char-range (char1 char2)
  (loop for i from (char-code char1) to (char-code char2)
        collect (code-char i)))

(defparameter *invalid-unicode*
  `(,@(char-range #\u0001 #\u0008)
    #\u000B
    ,@(char-range #\u000E #\u001F)
    ,@(char-range #\u007F #\u009F)
    ;; The following are noncharacter as defined by Unicode.
    ;; Clozure Common Lisp doesn't like them.
    #-(or abcl ccl) ,@`(
    ,@(char-range #\uD800 #\uDFFF)
    ,@(char-range #\uFDD0 #\uFDEF)
    #\uFFFE
    #\uFFFF
    #\u0001FFFE
    #\u0001FFFF
    #\u0002FFFE
    #\u0002FFFF
    #\u0003FFFE
    #\u0003FFFF
    #\u0004FFFE
    #\u0004FFFF
    #\u0005FFFE
    #\u0005FFFF
    #\u0006FFFE
    #\u0006FFFF
    #\u0007FFFE
    #\u0007FFFF
    #\u0008FFFE
    #\u0008FFFF
    #\u0009FFFE
    #\u0009FFFF
    #\u000AFFFE
    #\u000AFFFF
    #\u000BFFFE
    #\u000BFFFF
    #\u000CFFFE
    #\u000CFFFF
    #\u000DFFFE
    #\u000DFFFF
    #\u000EFFFE
    #\u000EFFFF
    #\u000FFFFE
    #\u000FFFFF
    #\u0010FFFE
    #\u0010FFFF)))

(defparameter *invalid-unicode-hash* (make-hash-table))
(dolist (char *invalid-unicode*)
  (setf (gethash char *invalid-unicode-hash*) char))

(defun report-character-errors (stream data)
  (loop for char across data
        when (gethash char *invalid-unicode-hash*)
          do (push :invalid-codepoint (html5-stream-errors stream))))
