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

(defclass html-tokenizer ()
  ((stream :initarg :stream :reader tokenizer-stream)
   (cdata-switch-helper :initarg :cdata-switch-helper
                        :initform (constantly nil))
   (lowercase-element-name :initform t)
   (lowercase-attr-name :initform t)
   (escape-flag :initform nil)
   (last-four-chars :initform nil)
   (state :initform :data-state :accessor tokenizer-state)
   (escape :initform nil)
   (current-token :initform nil)
   (token-queue :initform nil)
   (temporary-buffer :initform nil)))

(defun make-html-tokenizer (source &key encoding cdata-switch-helper)
  (make-instance 'html-tokenizer
                 :stream (make-html-input-stream source :override-encoding encoding)
                 :cdata-switch-helper cdata-switch-helper))

(defun map-tokens (tokenizer function)
  "Return next token or NIL on eof"
  (with-slots (token-queue stream) tokenizer
    (loop while (run-state tokenizer) do
         (setf token-queue (nreverse token-queue))
         (loop while (html5-stream-errors stream)
            do (funcall function (list :type :parse-error :data (pop (html5-stream-errors stream)))))
         (loop while token-queue
            do (funcall function (pop token-queue))))))

(defun run-state (tokenizer)
  (run-state* tokenizer (slot-value tokenizer 'state)))

(defgeneric run-state* (tokenizer state))

(defmacro defstate (state (&rest slots) &body body)
  `(defmethod run-state* (self (state (eql ,state)))
     (with-slots (,@slots) self
       (block nil
         ,@body
         t))))

(defun push-token (self token)
  (with-slots (token-queue) self
    (push token token-queue)))

(defun make-growable-string (&optional (init ""))
  "Make an adjustable string with a fill pointer.
Given INIT, a string, return an adjustable version of it with the fill
pointer at the end."
  (let ((string
          (make-array (max 5 (length init))
                      :element-type 'character
                      :adjustable t
                      :fill-pointer (length init))))
    (when init
      (replace string init))
    string))

(defun nconcat (string &rest data)
  "Destructively concatenate DATA, string designators, to STRING."
  (declare (optimize speed))
  (unless (array-has-fill-pointer-p string)
    (setf string (make-growable-string string)))
  (labels ((conc (string x)
             (typecase x
               (character
                (vector-push-extend x string))
               (string
                (let ((len (length x)))
                  (loop for c across x do
                    (vector-push-extend c string len))))
               (symbol (conc string (string x))))))
    (dolist (x data string)
      (conc string x))))

(define-modify-macro nconcatf (&rest data) nconcat)

(defun push-token* (self type &rest data)
  "Push a token with :type type and :data the a string concatenation of data"
  (push-token self (list :type type
                         :data (apply #'nconcat (make-growable-string) data))))

(defun add-attribute (token name)
  (setf (getf token :data) (append (getf token :data)
                                   (list (cons (make-growable-string (string name))
                                               (make-growable-string))))))

(defun add-to-attr-name (token &rest data)
  (setf (caar (last (getf token :data)))
        (apply #'nconcat
               (caar (last (getf token :data)))
               data)))

(defun add-to-attr-value (token &rest data)
  (setf (cdar (last (getf token :data)))
        (apply #'nconcat
               (cdar (last (getf token :data)))
               data)))

(defun add-to (token indicator &rest data)
  (setf (getf token indicator)
        (apply #'nconcat
               (getf token indicator)
               data)))

(defun consume-number-entity (self is-hex)
  "This function returns either U+FFFD or the character based on the
   decimal or hexadecimal representation. It also discards \";\" if present.
   If not present a token (:type :parse-error) is emitted.
  "
  (with-slots (stream) self
    (let ((allowed +digits+)
          (radix 10)
          (char-stack)
          (c)
          (char-as-int)
          (char))
      (when is-hex
        (setf allowed +hex-digits+)
        (setf radix 16))

      ;; Consume all the characters that are in range while making sure we
      ;; don't hit an EOF.
      (setf c (html5-stream-char stream))
      (loop while (and (find c allowed) (not (eql c +eof+))) do
           (push c char-stack)
           (setf c (html5-stream-char stream)))

      ;; Convert the set of characters consumed to an int.
      (setf char-as-int (parse-integer (coerce (nreverse char-stack) 'string) :radix radix))

      ;; Certain characters get replaced with others
      (cond ((find char-as-int +replacement-characters+)
             (setf char (getf +replacement-characters+ char-as-int))
             (push-token self `(:type :parse-error
                                      :data :illegal-codepoint-for-numeric-entity
                                      :datavars '(:char-as-int ,char-as-int))))
            ((or (<= #xD800 char-as-int #xDFFF)
                 (> char-as-int #x10FFFF))
             (setf char #\uFFFD)
             (push-token self `(:type :parse-error
                                      :data :illegal-codepoint-for-numeric-entity
                                      :datavars '(:char-as-int ,char-as-int))))
            (t
             ;; Python comment: Should speed up this check somehow (e.g. move the set to a constant)
             (when (or (<= #x0001 char-as-int #x0008)
                       (<= #x000E char-as-int #x001F)
                       (<= #x007F char-as-int #x009F)
                       (<= #xFDD0 char-as-int #xFDEF)
                       (find char-as-int
                             #(#x000B #xFFFE #xFFFF #x1FFFE
                               #x1FFFF #x2FFFE #x2FFFF #x3FFFE
                               #x3FFFF #x4FFFE #x4FFFF #x5FFFE
                               #x5FFFF #x6FFFE #x6FFFF #x7FFFE
                               #x7FFFF #x8FFFE #x8FFFF #x9FFFE
                               #x9FFFF #xAFFFE #xAFFFF #xBFFFE
                               #xBFFFF #xCFFFE #xCFFFF #xDFFFE
                               #xDFFFF #xEFFFE #xEFFFF #xFFFFE
                               #xFFFFF #x10FFFE #x10FFFF)))
               (push-token self `(:type :parse-error
                                        :data :illegal-codepoint-for-numeric-entity
                                        :datavars '(:char-as-int ,char-as-int))))
             ;; Assume char-code-limit >= 1114112
             (setf char (code-char char-as-int))))

      ;; Discard the ; if present. Otherwise, put it back on the queue and
      ;; invoke parseError on parser.
      (unless (eql c #\;)
        (push-token self `(:type :parse-error :data :numeric-entity-without-semicolon))
        (html5-stream-unget stream c))

      (string char))))

(defun consume-entity (self &key allowed-char from-attribute)
  (with-slots (stream current-token) self
    (let ((output "&")
          (stack (list (html5-stream-char stream))))
      (cond ((or (find (car stack) +space-characters+)
                 (find (car stack) '(+eof+ #\< #\&))
                 (and allowed-char (eql allowed-char (car stack))))
             (html5-stream-unget stream (car stack)))
            ((eql (car stack) #\#)
             (push (html5-stream-char stream) stack)
             (let ((is-hex (find (car stack) "xX")))
               (when is-hex
                 (push (html5-stream-char stream) stack))
               (cond ((find (car stack) (if is-hex +hex-digits+ +digits+))
                      (html5-stream-unget stream (car stack))
                      (setf output (consume-number-entity self is-hex)))
                     (t
                      (push-token self '(:type :parse-error :data :expected-numeric-entity))
                      (html5-stream-unget stream (pop stack))
                      (when is-hex
                        (html5-stream-unget stream (pop stack)))
                      (html5-stream-unget stream (pop stack))))))
            (t
             ;; Consume the maximum number of characters possible, with the
             ;; consumed characters matching one of the identifiers in the first
             ;; column of the named character references table
             ;; (in a case-sensitive manner).
             (let ((entity)
                   (match-at 0))
               (loop with node = *entities-tree*
                     for char = (car stack) then (car (push (html5-stream-char stream)
                                                            stack))
                     for next-node = (assoc char node)
                     while next-node
                     do (when (second next-node)
                          (setf entity (second next-node))
                          (setf match-at (length stack)))
                     do (setf node (cddr next-node)))
               (let ((next-char))
                 ;; Unconsume those characters that are not part of the match
                 ;; This unconsumes everything if there where no match
                 (loop until (= (length stack) match-at) do
                      (setf next-char (car stack))
                      (html5-stream-unget stream (pop stack)))
                 (cond ((not entity)
                        ;; If no match can be made, then no characters are consumed, and nothing is returned.
                        ;; Is this always a parse error really?
                        (push-token self '(:type :parse-error :data :expected-named-entity)))
                       ((and from-attribute
                             (not (eql #\; (car stack)))
                             (or (eql next-char #\=)
                                 (find next-char +digits+)
                                 (ascii-letter-p next-char)))
                        ; Is this a parse error really?
                        (push-token self '(:type :parse-error :data :bogus))
                        (setf output (concatenate 'string "&" (reverse stack))))
                       (t
                        (unless (eql #\; (car stack))
                          (push-token self '(:type :parse-error
                                             :data :named-entity-without-semicolon)))
                        (setf output entity)))))))

      (cond (from-attribute
             (add-to-attr-value current-token output))
            (t
             (push-token* self (if (find (char output 0) +space-characters+)
                                   :space-characters
                                   :characters)
                          output))))))

(defun process-entity-in-attribute (self &key allowed-char)
  (consume-entity self :allowed-char allowed-char :from-attribute t))

(defun emit-current-token (self)
  "This method is a generic handler for emitting the tags. It also sets
   the state to :data because that's what's needed after a token has been
   emitted.
  "
  (with-slots (current-token state lowercase-element-name) self
    (let ((token current-token))
      ;; Add token to the queue to be yielded
      (when (find (getf token :type) +tag-token-types+)
        (when lowercase-element-name
          (setf (getf token :name) (ascii-upper-2-lower (getf token :name))))
        (when (eql (getf token :type) :end-tag)
          (when (getf token :data)
            (push-token self '(:type :parse-error :data :attributes-in-end-tag)))
          (when (getf token :self-closing)
            (push-token self '(:type :parse-error :data :self-closing-flag-on-end-tag)))))
      (push-token self token)
      (setf state :data-state))))

;;;
;;; Below are the various tokenizer states worked out.
;;;

(defstate :data-state (stream state)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\&)
           (setf state :entity-data-state))
          ((eql data #\<)
           (setf state :tag-open-state))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (push-token* self :characters #\u0000))
          ((eql data +eof+)
           ;; Tokenization ends.
           (return nil))
          ((find data +space-characters+)
           ;; Directly after emitting a token you switch back to the "data
           ;; state". At that point spaceCharacters are important so they are
           ;; emitted separately.
           (push-token* self :space-characters
                        data
                        (html5-stream-chars-until stream +space-characters+ t))
           ;; No need to update lastFourChars here, since the first space will
           ;; have already been appended to lastFourChars and will have broken
           ;; any <!-- or --> sequences
           )
          (t
           (push-token* self :characters
                        data
                        (html5-stream-chars-until stream '(#\& #\< #\u0000)))))))

(defstate :entity-data-state (state)
  (consume-entity self)
  (setf state :data-state))

(defstate :rcdata-state (stream state)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\&)
           (setf state :character-reference-in-rcdata))
          ((eql data #\<)
           (setf state :rcdata-less-than-sign-state))
          ((eql data +eof+)
           ;; Tokenization ends.
           (return nil))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (push-token* self :characters #\uFFFD))
          ((find data +space-characters+)
           ;; Directly after emitting a token you switch back to the "data
           ;; state". At that point spaceCharacters are important so they are
           ;; emitted separately.
           (push-token* self :space-characters
                        data
                        (html5-stream-chars-until stream +space-characters+ t))
           ;; No need to update lastFourChars here, since the first space will
           ;; have already been appended to lastFourChars and will have broken
           ;; any <!-- or --> sequences
           )
          (t
           (push-token* self :characters
                        data
                        (html5-stream-chars-until stream '(#\& #\<)))))))

(defstate :character-reference-in-rcdata (state)
  (consume-entity self)
  (setf state :rcdata-state))

(defstate :rawtext-state (stream state)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\<)
           (setf state :rawtext-less-than-sign-state))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (push-token* self :characters #\uFFFD))
          ((eql data +eof+)
           ;; Tokenization ends.
           (return nil))
          (t
           (push-token* self :characters
                        data
                        (html5-stream-chars-until stream '(#\< #\u0000)))))))

(defstate :script-data-state (stream state)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\<)
           (setf state :script-data-less-than-sign-state))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (push-token* self :characters #\uFFFD))
          ((eql data +eof+)
           ;; Tokenization ends.
           (return nil))
          (t
           (push-token* self :characters
                        data
                        (html5-stream-chars-until stream '(#\< #\u0000)))))))

(defstate :plaintext-state (stream)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data +eof+)
           ;; Tokenization ends.
           (return nil))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (push-token* self :characters #\uFFFD))
          (t
           (push-token* self :characters
                        data
                        (html5-stream-chars-until stream '(#\u0000)))))))

(defstate :tag-open-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\!)
            (setf state :markup-declaration-open-state))
          ((eql data #\/)
            (setf state :close-tag-open-state))
          ((ascii-letter-p data)
           (setf current-token (list :type :start-tag
                                     :name (make-array 1 :element-type 'character
                                                          :initial-element data
                                                          :fill-pointer t
                                                          :adjustable t)
                                     :data '()
                                     :self-closing nil
                                     :self-closing-acknowledged nil))
            (setf state :tag-name-state))
          ((eql data #\>)
           ;; XXX In theory it could be something besides a tag name. But
           ;; do we really care?
           (push-token self '(:type :parse-error :data :expected-tag-name-but-got-right-bracket))
           (push-token* self :characters "<>")
           (setf state :data-state))
          ((eql data #\?)
           ;; XXX In theory it could be something besides a tag name. But
           ;; do we really care?
           (push-token self '(:type :parse-error :data :expected-tag-name-but-got-question-mark))
           (html5-stream-unget stream data)
           (setf state :bogus-comment-state))
          (t
           ;; XXX
           (push-token self '(:type :parse-error :data :expected-tag-name))
           (push-token* self :characters "<")
           (html5-stream-unget stream data)
           (setf state :data-state)))))

(defstate :close-tag-open-state
    (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((ascii-letter-p data)
           (setf current-token (list :type :end-tag
                                     :name (make-array 1 :element-type 'character
                                                          :initial-element data
                                                          :fill-pointer t
                                                          :adjustable t)
                                     :data '()
                                     :self-closing nil))
           (setf state :tag-name-state))
          ((eql data #\>)
           (push-token self '(:type :parse-error :data :expected-closing-tag-but-got-right-bracket))
           (setf state :data-state))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :expected-closing-tag-but-got-eof))
           (push-token* self :characters "</")
           (setf state :data-state))
          (t
           ;; XXX data can be _'_...
           (push-token self `(:type :parse-error :data :expected-closing-tag-but-got-char
                                    :datavars (:data ,data)))
           (html5-stream-unget stream data)
           (setf state :bogus-comment-state))))
  t)

(defstate :tag-name-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((find data +space-characters+)
           (setf state :before-attribute-name-state))
          ((eql data #\>)
           (emit-current-token self))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :eof-in-tag-name))
           (setf state :data-state))
          ((eql data #\/)
           (setf state :self-closing-start-tag-state))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (vector-push-extend #\uFFFD (getf current-token :name)))
          (t
           (vector-push-extend data (getf current-token :name))
           ;; (Don't use charsUntil here, because tag names are
           ;; very short and it's faster to not do anything fancy)
           ))))

(defstate :rcdata-less-than-sign-state (stream state temporary-buffer)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\/)
           (setf temporary-buffer (make-growable-string))
           (setf state :rcdata-end-tag-open-state))
          (t
           (push-token* self :characters "<")
           (html5-stream-unget stream data)
           (setf state :rcdata-state)))))

(defstate :rcdata-end-tag-open-state (stream state temporary-buffer)
  (let ((data (html5-stream-char stream)))
    (cond ((ascii-letter-p data)
           (nconcatf temporary-buffer (string data))
           (setf state :rcdata-end-tag-name-state))
          (t
           (push-token* self :characters "</")
           (html5-stream-unget stream data)
           (setf state :rcdata-state)))))

(defstate :rcdata-end-tag-name-state (stream state temporary-buffer current-token)
  (let ((appropriate (and current-token
                          (string-equal (getf current-token :name)
                                        temporary-buffer)))
        (data (html5-stream-char stream)))
    (cond ((and (find data +space-characters+)
                appropriate)
           (setf current-token (list :type :end-tag
                                     :name temporary-buffer
                                     :data '()
                                     :self-closing nil))
           (setf state :before-attribute-name-state))
          ((and (eql data #\/)
                appropriate)
           (setf current-token (list :type :end-tag
                                     :name temporary-buffer
                                     :data '()
                                     :self-closing nil))
           (setf state :self-closing-start-tag-state))
          ((and (eql data #\>)
                appropriate)
           (setf current-token (list :type :end-tag
                                     :name temporary-buffer
                                     :data '()
                                     :self-closing nil))
           (emit-current-token self)
           (setf state :data-state))
          ((ascii-letter-p data)
           (nconcatf temporary-buffer data))
          (t
           (push-token* self :characters "</" temporary-buffer)
           (html5-stream-unget stream data)
           (setf state :rcdata-state)))))

(defstate :rawtext-less-than-sign-state (stream state temporary-buffer)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\/)
           (setf temporary-buffer (make-growable-string))
           (setf state :rawtext-end-tag-open-state))
          (t
           (push-token* self :characters "<")
           (html5-stream-unget stream data)
           (setf state :rawtext-state)))))

(defstate :rawtext-end-tag-open-state (stream state temporary-buffer)
  (let ((data (html5-stream-char stream)))
    (cond ((ascii-letter-p data)
           (nconcatf temporary-buffer (string data))
           (setf state :rawtext-end-tag-name-state))
          (t
           (push-token* self :characters "</")
           (html5-stream-unget stream data)
           (setf state :rawtext-state)))))

(defstate :rawtext-end-tag-name-state (stream state temporary-buffer current-token)
  (let ((appropriate (and current-token
                          (string-equal (getf current-token :name)
                                        temporary-buffer)))
        (data (html5-stream-char stream)))
    (cond ((and (find data +space-characters+)
                appropriate)
           (setf current-token (list :type :end-tag
                                     :name temporary-buffer
                                     :data '()
                                     :self-closing nil))
           (setf state :before-attribute-name-state))
          ((and (eql data #\/)
                appropriate)
           (setf current-token (list :type :end-tag
                                     :name temporary-buffer
                                     :data '()
                                     :self-closing nil))
           (setf state :self-closing-start-tag-state))
          ((and (eql data #\>)
                appropriate)
           (setf current-token (list :type :end-tag
                                     :name temporary-buffer
                                     :data '()
                                     :self-closing nil))
           (emit-current-token self)
           (setf state :data-state))
          ((ascii-letter-p data)
           (nconcatf temporary-buffer data))
          (t
           (push-token* self :characters "</" temporary-buffer)
           (html5-stream-unget stream data)
           (setf state :rawtext-state)))))

(defstate :script-data-less-than-sign-state (stream state temporary-buffer)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\/)
           (setf temporary-buffer (make-growable-string))
           (setf state :script-data-end-tag-open-state))
          ((eql data #\!)
           (push-token* self :characters "<!")
           (setf state :script-data-escape-start-state))
          (t
           (push-token* self :characters "<")
           (html5-stream-unget stream data)
           (setf state :script-data-state)))))

(defstate :script-data-end-tag-open-state (stream state temporary-buffer)
  (let ((data (html5-stream-char stream)))
    (cond ((ascii-letter-p data)
           (nconcatf temporary-buffer data)
           (setf state :script-data-end-tag-name-state))
          (t
           (push-token* self :characters "</")
           (html5-stream-unget stream data)
           (setf state :script-data-state)))))

(defstate :script-data-end-tag-name-state (stream state temporary-buffer current-token)
  (let ((appropriate (and current-token
                          (string-equal (getf current-token :name)
                                        temporary-buffer)))
        (data (html5-stream-char stream)))
    (cond ((and (find data +space-characters+)
                appropriate)
           (setf current-token (list :type :end-tag
                                     :name temporary-buffer
                                     :data '()
                                     :self-closing nil))
           (setf state :before-attribute-name-state))
          ((and (eql data #\/)
                appropriate)
           (setf current-token (list :type :end-tag
                                     :name temporary-buffer
                                     :data '()
                                     :self-closing nil))
           (setf state :self-closing-start-tag-state))
          ((and (eql data #\>)
                appropriate)
           (setf current-token (list :type :end-tag
                                     :name temporary-buffer
                                     :data '()
                                     :self-closing nil))
           (emit-current-token self)
           (setf state :data-state))
          ((ascii-letter-p data)
           (nconcatf temporary-buffer data))
          (t
           (push-token* self :characters "</" temporary-buffer)
           (html5-stream-unget stream data)
           (setf state :script-data-state)))))

(defstate :script-data-escape-start-state (stream state)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\-)
           (push-token* self :characters "-")
           (setf state :script-data-escape-start-dash-state))
          (t
           (html5-stream-unget stream data)
           (setf state :script-data-state)))))

(defstate :script-data-escape-start-dash-state (stream state)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\-)
           (push-token* self :characters "-")
           (setf state :script-data-escaped-dash-dash-state))
          (t
           (html5-stream-unget stream data)
           (setf state :script-data-state)))))

(defstate :script-data-escaped-state (stream state)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\-)
           (push-token* self :characters "-")
           (setf state :script-data-escaped-dash-state))
          ((eql data #\<)
           (setf state :script-data-escaped-less-than-sign-state))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (push-token* self :characters #\uFFFD))
          ((eql data +eof+)
           (setf state :data-state))
          (t
           (push-token* self :characters data (html5-stream-chars-until stream '(#\< #\- #\u0000)))))))

(defstate :script-data-escaped-dash-state (stream state)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\-)
           (push-token* self :characters "-")
           (setf state :script-data-escaped-dash-dash-state))
          ((eql data #\<)
           (setf state :script-data-escaped-less-than-sign-state))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (push-token* self :characters #\uFFFD)
           (setf state :script-data-escaped-state))
          ((eql data +eof+)
           (setf state :data-state))
          (t
           (push-token* self :characters data (html5-stream-chars-until stream '(#\< #\- #\u0000)))
           (setf state :script-data-escaped-state)))))

(defstate :script-data-escaped-dash-dash-state (stream state)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\-)
           (push-token* self :characters "-"))
          ((eql data #\<)
           (setf state :script-data-escaped-less-than-sign-state))
          ((eql data #\>)
           (push-token* self :characters ">")
           (setf state :script-data-state))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (push-token* self :characters #\uFFFD)
           (setf state :script-data-escaped-state))
          ((eql data +eof+)
           (setf state :data-state))
          (t
           (push-token* self :characters data (html5-stream-chars-until stream '(#\< #\- #\u0000)))
           (setf state :script-data-escaped-state)))))

(defstate :script-data-escaped-less-than-sign-state (stream state temporary-buffer)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\/)
           (setf temporary-buffer (make-growable-string))
           (setf state :script-data-escaped-end-tag-open-state))
          ((ascii-letter-p data)
           (push-token* self :characters "<" data)
           (setf temporary-buffer (ascii-upper-2-lower (string data)))
           (setf state :script-data-double-escape-start-state))
          (t
           (push-token* self :characters "<")
           (html5-stream-unget stream data)
           (setf state :script-data-escaped-state)))))

(defstate :script-data-escaped-end-tag-open-state (stream state temporary-buffer)
  (let ((data (html5-stream-char stream)))
    (cond ((ascii-letter-p data)
           (setf temporary-buffer (string data))
           (setf state :script-data-escaped-end-tag-name-state))
          (t
           (push-token* self :characters "</")
           (html5-stream-unget stream data)
           (setf state :script-data-escaped-state)))))

(defstate :script-data-escaped-end-tag-name-state (stream state temporary-buffer current-token)
  (let ((appropriate (and current-token
                          (string-equal (getf current-token :name)
                                        temporary-buffer)))
        (data (html5-stream-char stream)))
    (cond ((and (find data +space-characters+)
                appropriate)
           (setf current-token (list :type :end-tag
                                     :name temporary-buffer
                                     :data '()
                                     :self-closing nil))
           (setf state :before-attribute-name-state))
          ((and (eql data #\/)
                appropriate)
           (setf current-token (list :type :end-tag
                                     :name temporary-buffer
                                     :data '()
                                     :self-closing nil))
           (setf state :self-closing-start-tag-state))
          ((and (eql data #\>)
                appropriate)
           (setf current-token (list :type :end-tag
                                     :name temporary-buffer
                                     :data '()
                                     :self-closing nil))
           (emit-current-token self)
           (setf state :data-state))
          ((ascii-letter-p data)
           (nconcatf temporary-buffer data))
          (t
           (push-token* self :characters "</" temporary-buffer)
           (html5-stream-unget stream data)
           (setf state :script-data-escaped-state)))))

(defstate :script-data-double-escape-start-state (stream state temporary-buffer)
  (let ((data (html5-stream-char stream)))
    (cond ((or (find data +space-characters+)
               (find data '(#\/ #\>)))
           (push-token* self :characters data)
           (if (string= (string-downcase temporary-buffer) "script")
               (setf state :script-data-double-escaped-state)
               (setf state :script-data-escaped-state)))
          ((ascii-letter-p data)
           (push-token* self :characters data)
           (nconcatf temporary-buffer (string data)))
          (t
           (html5-stream-unget stream data)
           (setf state :script-data-escaped-state)))))

(defstate :script-data-double-escaped-state (stream state)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\-)
           (push-token* self :characters "-")
           (setf state :script-data-double-escaped-dash-state))
          ((eql data #\<)
           (push-token* self :characters "<")
           (setf state :script-data-double-escaped-less-than-sign-state))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (push-token* self :characters #\uFFFD))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :eof-in-script-in-script))
           (setf state :data-state))
          (t
           (push-token* self :characters data)))))

(defstate :script-data-double-escaped-dash-state (stream state)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\-)
           (push-token* self :characters "-")
           (setf state :script-data-double-escaped-dash-dash-state))
          ((eql data #\<)
           (push-token* self :characters "<")
           (setf state :script-data-double-escaped-less-than-sign-state))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (push-token* self :characters #\uFFFD)
           (setf state :script-data-double-escaped-state))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :eof-in-script-in-script))
           (setf state :data-state))
          (t
           (push-token* self :characters data)
           (setf state :script-data-double-escaped-state)))))

;; FIXME: Incorrectly named in Python code: scriptDataDoubleEscapedDashState (same the one above)
(defstate :script-data-double-escaped-dash-dash-state (stream state)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\-)
           (push-token* self :characters "-")
           (setf state :script-data-double-escaped-dash-dash-state))
          ((eql data #\<)
           (push-token* self :characters "<")
           (setf state :script-data-double-escaped-less-than-sign-state))
          ((eql data #\>)
           (push-token* self :characters ">")
           (setf state :script-data-state))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (push-token* self :characters #\uFFFD)
           (setf state :script-data-double-escaped-state))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :eof-in-script-in-script))
           (setf state :data-state))
          (t
           (push-token* self :characters data)
           (setf state :script-data-double-escaped-state)))))

(defstate :script-data-double-escaped-less-than-sign-state (stream state temporary-buffer)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\/)
           (push-token* self :characters "/")
           (setf temporary-buffer (make-growable-string))
           (setf state :script-data-double-escape-end-state))
          (t
           (html5-stream-unget stream data)
           (setf state :script-data-double-escaped-state)))))

(defstate :script-data-double-escape-end-state (stream state temporary-buffer)
  (let ((data (html5-stream-char stream)))
    (cond ((or (find data +space-characters+)
               (find data '(#\/ #\>)))
           (push-token* self :characters data)
           (if (string= (string-downcase temporary-buffer) "script")
               (setf state :script-data-escaped-state)
               (setf state :script-data-double-escaped-state)))
          ((ascii-letter-p data)
           (push-token* self :characters data)
           (nconcatf temporary-buffer data))
          (t
           (html5-stream-unget stream data)
           (setf state :script-data-double-escaped-state)))))

(defstate :before-attribute-name-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((find data +space-characters+)
           (html5-stream-chars-until stream +space-characters+ t))
          ((ascii-letter-p data)
           (add-attribute current-token data)
           (setf state :attribute-name-state))
          ((eql data #\>)
           (emit-current-token self))
          ((eql data #\/)
           (setf state :self-closing-start-tag-state))
          ((find data '(#\' #\" #\= #\<))
           (push-token self '(:type :parse-error :data :invalid-character-in-attribute-name))
           (add-attribute current-token data)
           (setf state :attribute-name-state))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (add-attribute current-token #\uFFFD)
           (setf state :attribute-name-state))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :expected-attribute-name-but-got-eof))
           (setf state :data-state))
          (t
           (add-attribute current-token data)
           (setf state :attribute-name-state)))))

(defstate :attribute-name-state (stream state current-token lowercase-attr-name)
  (let ((data (html5-stream-char stream))
        (leaving-this-state t)
        (emit-token nil))
    (cond ((eql data #\=)
           (setf state :before-attribute-value-state))
          ((ascii-letter-p data)
           (add-to-attr-name current-token data
                             (html5-stream-chars-until stream +ascii-letters+ t))
           (setf leaving-this-state nil))
          ((eql data #\>)
           ;; XXX If we emit here the attributes are converted to a dict
           ;; without being checked and when the code below runs we error
           ;; because data is a dict not a list
           (setf emit-token t))
          ((find data +space-characters+)
           (setf state :after-attribute-name-state))
          ((eql data #\/)
           (setf state :self-closing-start-tag-state))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (add-to-attr-name current-token #\uFFFD)
           (setf leaving-this-state nil))
          ((find data '(#\' #\" #\<))
            (push-token self '(:type :parse-error :data :invalid-character-in-attribute-name))
           (add-to-attr-name current-token data)
           (setf leaving-this-state nil))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :eof-in-attribute-name))
           (setf state :data-state))
          (t
           (add-to-attr-name current-token data)
           (setf leaving-this-state nil)))
    (when leaving-this-state
      ;; Attributes are not dropped at this stage. That happens when the
      ;; start tag token is emitted so values can still be safely appended
      ;; to attributes, but we do want to report the parse error in time.
      (when lowercase-attr-name
        (setf (caar (last (getf current-token :data)))
              (ascii-upper-2-lower (caar (last (getf current-token :data))))))
      (loop for (name . value) in (butlast (getf current-token :data)) do
           (when (string= (caar (last (getf current-token :data))) name)
             (push-token self '(:type :parse-error :data :duplicate-attribute))
             (return)))
      ;; XXX Fix for above XXX
      (when emit-token
        (emit-current-token self)))))

(defstate :after-attribute-name-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((find data +space-characters+)
           (html5-stream-chars-until stream +space-characters+ t))
          ((eql data #\=)
           (setf state :before-attribute-value-state))
          ((eql data #\>)
           (emit-current-token self))
          ((ascii-letter-p data)
           (add-attribute current-token data)
           (setf state :attribute-name-state))
          ((eql data #\/)
           (setf state :self-closing-start-tag-state))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (add-attribute current-token #\uFFFD)
           (setf state :attribute-name-state))
          ((find data '(#\' #\" #\<))
           (push-token self '(:type :parse-error :data :invalid-character-after-attribute-name))
           (add-attribute current-token data)
           (setf state :attribute-name-state))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :expected-end-of-tag-but-got-eof))
           (setf state :data-state))
          (t
           (add-attribute current-token data)
           (setf state :attribute-name-state)))))

(defstate :before-attribute-value-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((find data +space-characters+)
           (html5-stream-chars-until stream +space-characters+ t))
          ((eql data #\")
           (setf state :attribute-value-double-quoted-state))
          ((eql data #\&)
           (setf state :attribute-value-un-quoted-state)
           (html5-stream-unget stream data))
          ((eql data #\')
           (setf state :attribute-value-single-quoted-state))
          ((eql data #\>)
           (push-token self '(:type :parse-error :data :expected-attribute-value-but-got-right-bracket))
           (emit-current-token self))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (add-to-attr-value current-token #\uFFFD)
           (setf state :attribute-value-un-quoted-state))
          ((find data '(#\= #\< #\`))
           (push-token self '(:type :parse-error :data :equals-in-unquoted-attribute-value))
           (add-to-attr-value current-token data)
           (setf state :attribute-value-un-quoted-state))
          ((eql data +eof+)
            (push-token self '(:type :parse-error :data :expected-attribute-value-but-got-eof))
            (setf state :data-state))
          (t
           (add-to-attr-value current-token data)
           (setf state :attribute-value-un-quoted-state)))))

(defstate :attribute-value-double-quoted-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\")
           (setf state :after-attribute-value-state))
          ((eql data #\&)
           (process-entity-in-attribute self :allowed-char #\"))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (add-to-attr-value current-token #\uFFFD))
          ((eql data +eof+)
            (push-token self '(:type :parse-error :data :eof-in-attribute-value-double-quote))
            (setf state :data-state))
          (t
           (add-to-attr-value current-token
                              data
                              (html5-stream-chars-until stream '(#\" #\&)))))))

(defstate :attribute-value-single-quoted-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\')
           (setf state :after-attribute-value-state))
          ((eql data #\&)
           (process-entity-in-attribute self :allowed-char #\'))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (add-to-attr-value current-token #\uFFFD))
          ((eql data +eof+)
            (push-token self '(:type :parse-error :data :eof-in-attribute-value-single-quote))
            (setf state :data-state))
          (t
           (add-to-attr-value current-token
                              data
                              (html5-stream-chars-until stream '(#\' #\&)))))))

(defstate :attribute-value-un-quoted-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((find data +space-characters+)
           (setf state :before-attribute-name-state))
          ((eql data #\&)
           (process-entity-in-attribute self :allowed-char #\>))
          ((eql data #\>)
           (emit-current-token self))
          ((find data '(#\" #\' #\= #\< #\`))
           (push-token self '(:type :parse-error :data :unexpected-character-in-unquoted-attribute-value))
           (add-to-attr-value current-token data))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (add-to-attr-value current-token #\uFFFD))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :eof-in-attribute-value-no-quotes))
           (setf state :data-state))
          (t
           (add-to-attr-value current-token
                              data
                              (html5-stream-chars-until stream `(#\& #\> #\" #\' #\= #\< #\`
                                                              ,@+space-characters+)))))))

(defstate :after-attribute-value-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((find data +space-characters+)
           (setf state :before-attribute-name-state))
          ((eql data #\>)
           (emit-current-token self))
          ((eql data #\/)
           (setf state :self-closing-start-tag-state))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :unexpected-EOF-after-attribute-value))
           (html5-stream-unget stream data)
           (setf state :data-state))
          (t
           (push-token self '(:type :parse-error :data :unexpected-character-after-attribute-value))
           (html5-stream-unget stream data)
           (setf state :before-attribute-name-state)))))

(defstate :self-closing-start-tag-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\>)
           (setf (getf current-token :self-closing) t)
           (emit-current-token self))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :unexpected-EOF-after-solidus-in-tag))
           (html5-stream-unget stream data)
           (setf state :data-state))
          (t
           (push-token self '(:type :parse-error :data :unexpected-character-after-soldius-in-tag))
           (html5-stream-unget stream data)
           (setf state :before-attribute-name-state)))))

(defstate :bogus-comment-state (stream state current-token)
  ;; Make a new comment token and give it as value all the characters
  ;; until the first > or EOF (charsUntil checks for EOF automatically)
  ;; and emit it.
  (let ((data (html5-stream-chars-until stream '(#\>))))
    (setf data (substitute #\uFFFD #\u0000 data))
    (push-token* self :comment data)
    ;; Eat the character directly after the bogus comment which is either a
    ;; ">" or an EOF.
    (html5-stream-char stream)
    (setf state :data-state)))

(defstate :markup-declaration-open-state (stream state current-token
                                                 cdata-switch-helper)
  (let ((char-stack (make-array 1
                                :initial-element (html5-stream-char stream)
                                :fill-pointer 1
                                :adjustable t)))
    (cond ((eql (aref char-stack (1- (length char-stack))) #\-)
           (vector-push-extend (html5-stream-char stream) char-stack)
           (when (eql (aref char-stack (1- (length char-stack))) #\-)
             (setf current-token (list :type :comment :data ""))
             (setf state :comment-start-state)
             (return t)))
          ((find (aref char-stack (1- (length char-stack))) '(#\d #\D))
           (let ((matched t))
             (loop for expected in '((#\o #\O) (#\c #\C) (#\t #\T) (#\y #\Y) (#\p #\P) (#\e #\E)) do
                  (vector-push-extend (html5-stream-char stream) char-stack)
                  (unless (find (aref char-stack (1- (length char-stack))) expected)
                    (setf matched nil)
                    (return)))
             (when matched
               (setf current-token (list :type :doctype
                                         :name ""
                                         :public-id nil
                                         :system-id nil
                                         :correct t))
               (setf state :doctype-state)
               (return t))))
          ((and (eql (aref char-stack (1- (length char-stack))) #\[)
                (funcall cdata-switch-helper))
           (let ((matched t))
             (loop for expected across "CDATA[" do
                  (vector-push-extend (html5-stream-char stream) char-stack)
                  (unless (eql (aref char-stack (1- (length char-stack))) expected)
                    (setf matched nil)
                    (return)))
             (when matched
               (setf state :cdata-section-state)
               (return t)))))
    (push-token self '(:type :parse-error :data :expected-dashes-or-doctype))
    (loop while (plusp (length char-stack)) do
         (html5-stream-unget stream (vector-pop char-stack)))
    (setf state :bogus-comment-state)))

(defstate :comment-start-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\-)
           (setf state :comment-start-dash-state))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (add-to current-token :data #\uFFFD))
          ((eql data #\>)
           (push-token self '(:type :parse-error :data :incorrect-comment))
           (push-token self current-token)
           (setf state :data-state))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :eof-in-comment))
           (push-token self current-token)
           (setf state :data-state))
          (t
           (add-to current-token :data data)
           (setf state :comment-state)))))

(defstate :comment-start-dash-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\-)
           (setf state :comment-end-state))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (add-to current-token :data "-" #\uFFFD))
          ((eql data #\>)
           (push-token self '(:type :parse-error :data :incorrect-comment))
           (push-token self current-token)
           (setf state :data-state))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :eof-in-comment))
           (push-token self current-token)
           (setf state :data-state))
          (t
           (add-to current-token :data "-" data)
           (setf state :comment-state)))))

(defstate :comment-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\-)
           (setf state :comment-end-dash-state))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (add-to current-token :data #\uFFFD))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :eof-in-comment))
           (push-token self current-token)
           (setf state :data-state))
          (t
           (add-to current-token :data data
                        (html5-stream-chars-until stream '(#\- #\u0000)))))))

(defstate :comment-end-dash-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\-)
           (setf state :comment-end-state))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (add-to current-token :data "-" #\uFFFD))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :eof-in-comment-end-dash))
           (push-token self current-token)
           (setf state :data-state))
          (t
           (add-to current-token :data "-" data)
           (setf state :comment-state)))))

(defstate :comment-end-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\>)
           (push-token self current-token)
           (setf state :data-state))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (add-to current-token :data "--" #\uFFFD)
           (setf state :comment-state))
          ((eql data #\!)
           (push-token self '(:type :parse-error :data :unexpected-bang-after-double-dash-in-comment))
           (setf state :comment-end-bang-state))
          ((eql data #\-)
           (push-token self '(:type :parse-error :data :unexpected-dash-after-double-dash-in-comment))
           (add-to current-token :data data))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :eof-in-comment-double-dash))
           (push-token self current-token)
           (setf state :data-state))
          (t
           ;; XXX
           (push-token self '(:type :parse-error :data :unexpected-char-in-comment))
           (add-to current-token :data "--" data)
           (setf state :comment-state)))))

(defstate :comment-end-bang-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\>)
           (push-token self current-token)
           (setf state :data-state))
          ((eql data #\-)
           (add-to current-token :data "--!")
           (setf state :comment-end-dash-state))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (add-to current-token :data "--!" #\uFFFD)
           (setf state :comment-state))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :eof-in-comment-end-bang-state))
           (push-token self current-token)
           (setf state :data-state))
          (t
           (add-to current-token :data "--!" data)
           (setf state :comment-state)))))

(defstate :doctype-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((find data +space-characters+)
           (setf state :before-doctype-name-state))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :expected-doctype-name-but-got-eof))
           (setf (getf current-token :correct) nil)
           (push-token self current-token)
           (setf state :data-state))
          (t
           (push-token self '(:type :parse-error :data :need-space-after-doctype))
           (html5-stream-unget stream data)
           (setf state :before-doctype-name-state)))))

(defstate :before-doctype-name-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((find data +space-characters+)
           ;; pass
           )
          ((eql data #\>)
           (push-token self '(:type :parse-error :data :expected-doctype-name-but-got-right-bracket))
           (setf (getf current-token :correct) nil)
           (push-token self current-token)
           (setf state :data-state))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (add-to current-token :name #\uFFFD)
           (setf state :doctype-name-state))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :expected-doctype-name-but-got-eof))
           (setf (getf current-token :correct) nil)
           (push-token self current-token)
           (setf state :data-state))
          (t
           (setf (getf current-token :name) (string data))
           (setf state :doctype-name-state)))))

(defstate :doctype-name-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((find data +space-characters+)
           (setf (getf current-token :name) (ascii-upper-2-lower (getf current-token :name)))
           (setf state :after-doctype-name-state))
          ((eql data #\>)
           (setf (getf current-token :name) (ascii-upper-2-lower (getf current-token :name)))
           (push-token self current-token)
           (setf state :data-state))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (add-to current-token :name #\uFFFD)
           (setf state :doctype-name-state))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :eof-in-doctype-name))
           (setf (getf current-token :correct) nil)
           (setf (getf current-token :name) (ascii-upper-2-lower (getf current-token :name)))
           (push-token self current-token)
           (setf state :data-state))
          (t
           (add-to current-token :name data)))))

(defstate :after-doctype-name-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((find data +space-characters+)
           ;; pass
           )
          ((eql data #\>)
           (push-token self current-token)
           (setf state :data-state))
          ((eql data +eof+)
           (setf (getf current-token :correct) nil)
           (html5-stream-unget stream data)
           (push-token self '(:type :parse-error :data :eof-in-doctype))
           (push-token self current-token)
           (setf state :data-state))
          (t
           (cond ((find data '(#\p #\P))
                  (let ((matched t))
                    (loop for expected in '((#\u #\U) (#\b #\B) (#\l #\L) (#\i #\I) (#\c #\C)) do
                         (setf data (html5-stream-char stream))
                         (unless (find data expected)
                           (setf matched nil)
                           (return)))
                    (when matched
                      (setf state :after-doctype-public-keyword-state)
                      (return t))))
                 ((find data '(#\s #\S))
                  (let ((matched t))
                    (loop for expected in '((#\y #\Y) (#\s #\S) (#\t #\T) (#\e #\E) (#\m #\M)) do
                         (setf data (html5-stream-char stream))
                         (unless (find data expected)
                           (setf matched nil)
                           (return)))
                    (when matched
                      (setf state :after-doctype-system-keyword-state)
                      (return t)))))
           ;; All the characters read before the current 'data' will be
           ;; [a-zA-Z], so they're garbage in the bogus doctype and can be
           ;; discarded; only the latest character might be '>' or EOF
           ;; and needs to be ungetted
           (html5-stream-unget stream data)
           (push-token self `(:type :parse-error :data :expected-space-or-right-bracket-in-doctype
                                    :datavars (:data ,data)))
           (setf (getf current-token :correct) nil)
           (setf state :bogus-doctype-state)))))

(defstate :after-doctype-public-keyword-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((find data +space-characters+)
           (setf state :before-doctype-public-identifier-state))
          ((find data '(#\' #\"))
           (push-token self '(:type :parse-error :data :unexpected-char-in-doctype))
           (html5-stream-unget stream data)
           (setf state :before-doctype-public-identifier-state))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :eof-in-doctype))
           (setf (getf current-token :correct) nil)
           (push-token self current-token)
           (setf state :data-state))
          (t
           (html5-stream-unget stream data)
           (setf state :before-doctype-public-identifier-state)))))

(defstate :before-doctype-public-identifier-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((find data +space-characters+)
           ;; pass
           )
          ((eql data #\")
           (setf (getf current-token :public-id) "")
           (setf state :doctype-public-identifier-double-quoted-state))
          ((eql data #\')
           (setf (getf current-token :public-id) "")
           (setf state :doctype-public-identifier-single-quoted-state))
          ((eql data #\>)
           (push-token self '(:type :parse-error :data :unexpected-end-of-doctype))
           (setf (getf current-token :correct) nil)
           (push-token self current-token)
           (setf state :data-state))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :eof-in-doctype))
           (setf (getf current-token :correct) nil)
           (push-token self current-token)
           (setf state :data-state))
          (t
           (push-token self '(:type :parse-error :data :unexpected-char-in-doctype))
           (setf (getf current-token :correct) nil)
           (setf state :bogus-doctype-state)))))

(defstate :doctype-public-identifier-double-quoted-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\")
           (setf state :after-doctype-public-identifier-state))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (add-to current-token :public-id #\uFFFD))
          ((eql data #\>)
           (push-token self '(:type :parse-error :data :unexpected-end-of-doctype))
           (setf (getf current-token :correct) nil)
           (push-token self current-token)
           (setf state :data-state))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :eof-in-doctype))
           (setf (getf current-token :correct) nil)
           (push-token self current-token)
           (setf state :data-state))
          (t
           (add-to current-token :public-id data)))))

(defstate :doctype-public-identifier-single-quoted-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\')
           (setf state :after-doctype-public-identifier-state))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (add-to current-token :public-id #\uFFFD))
          ((eql data #\>)
           (push-token self '(:type :parse-error :data :unexpected-end-of-doctype))
           (setf (getf current-token :correct) nil)
           (push-token self current-token)
           (setf state :data-state))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :eof-in-doctype))
           (setf (getf current-token :correct) nil)
           (push-token self current-token)
           (setf state :data-state))
          (t
           (add-to current-token :public-id data)))))

(defstate :after-doctype-public-identifier-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((find data +space-characters+)
           (setf state :between-doctype-public-and-system-identifiers-state))
          ((eql data #\>)
           (push-token self current-token)
           (setf state :data-state))
          ((eql data #\")
           (push-token self '(:type :parse-error :data :unexpected-char-in-doctype))
           (setf (getf current-token :system-id) "")
           (setf state :doctype-system-identifier-double-quoted-state))
          ((eql data #\')
           (push-token self '(:type :parse-error :data :unexpected-char-in-doctype))
           (setf (getf current-token :system-id) "")
           (setf state :doctype-system-identifier-single-quoted-state))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :eof-in-doctype))
           (setf (getf current-token :correct) nil)
           (push-token self current-token)
           (setf state :data-state))
          (t
           (push-token self '(:type :parse-error :data :unexpected-char-in-doctype))
           (setf (getf current-token :correct) nil)
           (setf state :bogus-doctype-state)))))

(defstate :between-doctype-public-and-system-identifiers-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((find data +space-characters+)
           ;; pass
           )
          ((eql data #\>)
           (push-token self current-token)
           (setf state :data-state))
          ((eql data #\")
           (setf (getf current-token :system-id) "")
           (setf state :doctype-system-identifier-double-quoted-state))
          ((eql data #\')
           (setf (getf current-token :system-id) "")
           (setf state :doctype-system-identifier-single-quoted-state))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :eof-in-doctype))
           (setf (getf current-token :correct) nil)
           (push-token self current-token)
           (setf state :data-state))
          (t
           (push-token self '(:type :parse-error :data :unexpected-char-in-doctype))
           (setf (getf current-token :correct) nil)
           (setf state :bogus-doctype-state)))))

(defstate :after-doctype-system-keyword-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((find data +space-characters+)
           (setf state :before-doctype-system-identifier-state))
          ((find data '(#\' #\"))
           (push-token self '(:type :parse-error :data :unexpected-char-in-doctype))
           (html5-stream-unget stream data)
           (setf state :before-doctype-system-identifier-state))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :eof-in-doctype))
           (setf (getf current-token :correct) nil)
           (push-token self current-token)
           (setf state :data-state))
          (t
           (html5-stream-unget stream data)
           (setf state :before-doctype-system-identifier-state)))))

(defstate :before-doctype-system-identifier-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((find data +space-characters+)
           ;; pass
           )
          ((eql data #\")
           (setf (getf current-token :system-id) "")
           (setf state :doctype-system-identifier-double-quoted-state))
          ((eql data #\')
           (setf (getf current-token :system-id) "")
           (setf state :doctype-system-identifier-single-quoted-state))
          ((eql data #\>)
           (push-token self '(:type :parse-error :data :unexpected-end-of-doctype))
           (setf (getf current-token :correct) nil)
           (push-token self current-token)
           (setf state :data-state))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :eof-in-doctype))
           (setf (getf current-token :correct) nil)
           (push-token self current-token)
           (setf state :data-state))
          (t
           (push-token self '(:type :parse-error :data :unexpected-char-in-doctype))
           (setf (getf current-token :correct) nil)
           (setf state :bogus-doctype-state)))))

(defstate :doctype-system-identifier-double-quoted-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\")
           (setf state :after-doctype-system-identifier-state))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (add-to current-token :system-id #\uFFFD))
          ((eql data #\>)
           (push-token self '(:type :parse-error :data :unexpected-end-of-doctype))
           (setf (getf current-token :correct) nil)
           (push-token self current-token)
           (setf state :data-state))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :eof-in-doctype))
           (setf (getf current-token :correct) nil)
           (push-token self current-token)
           (setf state :data-state))
          (t
           (add-to current-token :system-id data)))))

(defstate :doctype-system-identifier-single-quoted-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\')
           (setf state :after-doctype-system-identifier-state))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (add-to current-token :system-id #\uFFFD))
          ((eql data #\>)
           (push-token self '(:type :parse-error :data :unexpected-end-of-doctype))
           (setf (getf current-token :correct) nil)
           (push-token self current-token)
           (setf state :data-state))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :eof-in-doctype))
           (setf (getf current-token :correct) nil)
           (push-token self current-token)
           (setf state :data-state))
          (t
           (add-to current-token :system-id data)))))

(defstate :after-doctype-system-identifier-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((find data +space-characters+)
           ;; pass
           )
          ((eql data #\>)
           (push-token self current-token)
           (setf state :data-state))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :eof-in-doctype))
           (setf (getf current-token :correct) nil)
           (push-token self current-token)
           (setf state :data-state))
          (t
           (push-token self '(:type :parse-error :data :unexpected-char-in-doctype))
           (setf state :bogus-doctype-state)))))

(defstate :bogus-doctype-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\>)
           (push-token self current-token)
           (setf state :data-state))
          ((eql data +eof+)
           ;; XXX EMIT
           (html5-stream-unget stream data)
           (push-token self current-token)
           (setf state :data-state))
          (t
           ;; pass
           ))))

(defstate :cdata-section-state (stream state current-token)
  (let ((data '()))
    (loop
       (push (html5-stream-chars-until stream '(#\])) data)
      (let ((char-stack '())
            (matched t))
        (loop for expected across "]]>" do
             (push (html5-stream-char stream) char-stack)
             (cond ((eql (car char-stack) +eof+)
                    (pop char-stack)
                    (setf data (append char-stack data))
                    (return))
                   ((not (eql (car char-stack) expected))
                    (setf matched nil)
                    (setf data (append char-stack data))
                    (return))))
        (when matched
          (return))))
    (setf data (apply #'concatenate 'string (mapcar #'string (nreverse data))))
    ;; Deal with null here rather than in the parser
    (let ((null-count (count #\u0000 data)))
      (when (plusp null-count)
        (push-token self '(:type :parse-error :data :invalid-codepoint))
        (setf data (nsubstitute #\uFFFD #\u0000 data))))
    (when (plusp (length data))
      (push-token* self :characters data))
    (setf state :data-state)))
