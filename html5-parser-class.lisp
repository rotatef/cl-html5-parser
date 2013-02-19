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

(defvar *parser*)

(defclass html-parser ()
  ((html-namespace :initform (find-namespace "html"))
   (strict :initarg :strict)
   (inner-html-mode)
   (container :initform "div")
   (tokenizer)
   (document :initform (make-instance 'document))
   (errors :initform '())
   (phase :accessor parser-phase)
   first-start-tag
   compat-mode
   inner-html
   last-phase
   original-phase
   before-rcdata-phase
   (character-tokens :initform nil)
   frameset-ok
   open-elements
   active-formatting-elements
   head-pointer
   form-pointer
   insert-from-table
   (in-body-process-space-characters-mode :initform :non-pre)))
