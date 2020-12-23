;;;-*- Mode: common-lisp; syntax: common-lisp; package: line-reader; base: 10 -*-
;;;
;;;; Line Based Reader Module
;;;
;;; Copyright (c) 2014-2020 Seiji Koide <koide@ontolonomy.co.jp>
;;; All rights reserved.
;;; 
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met: 
;;; 
;;; 1. Redistributions of source code must retain the above copyright notice,
;;;    this list of conditions and the following disclaimer. 
;;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;;    this list of conditions and the following disclaimer in the documentation
;;;    and/or other materials provided with the distribution. 
;;; 
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
;;; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;; 
;;; The views and conclusions contained in the software and documentation are those
;;; of the authors and should not be interpreted as representing official policies, 
;;; either expressed or implied, of the FreeBSD Project.
;;;
;; History
;; -------
;; 2020/11/30  added new part for sbcl, which has a bug of latin-1 default external 
;;             format in case of user-defined stream.
;; 2014/05/06  File created.
;;

(cl:provide :line-reader)

(cl:defpackage :line-reader
  (:nicknames :line)
  (:use :common-lisp)
  (:shadow #:stream #:with-open-file #:read-line)
  (:export #:stream #:with-open-file #:read-line #:line-count #:line-position
           #:expose-one-line-buf #:expose-three-lines-buf 
           #:white-char-p #:skipbl #:next-token #:with-buffered-open-file
           #:simple-stream-read-line #:read-string
           #:peeknext-char #:getnext-char #:putback-char
           #:null-line-p #:update-line-count 
   ))

(in-package :line-reader)
#|
:cd /home/seiji/allegro-projects/drawio
(line:with-open-file (stream "test10.drawio" :external-format :utf-8)
  (loop for line-count = (line:line-count stream)
        for line-position = (line:line-position stream)
        for line = (line:read-line stream nil nil)
        for i = 0 then (1+ i)
      while line
      do (format t "~2D ~2D;~3D: ~A~%" i line-count line-position line)))

:cd /usr/share/mecab/dic/juman
(line:with-open-file (stream "juman.allsorted.csv" :external-format :utf-8)
  (loop for line-count = (line:line-count stream)
        for line-position = (line:line-position stream)
        for line = (line:read-line stream nil nil)
        for i = 0 then (1+ i)
      while line
      do (format t "~2D ~2D;~3D: ~A~%" i line-count line-position line)))
      
:cd \\LS220D95A\ontologies\BFO
(line:with-open-file (stream "bfo.owl" :external-format :utf-8)
  (loop for line-count = (line:line-count stream)
        for line-position = (line:line-position stream)
        for line = (line:read-line stream nil nil)
      while line
      do (format t "~2D;~3D: ~A~%" line-count line-position line)))

:cd \\LS220D95A\ontologies\Freebase
(line:with-open-file (stream "freebase-rdf-latest" :external-format :utf-8)
  (loop for line-count = (line:line-count stream)
        for line-position = (line:line-position stream)
        for line = (line:read-line stream nil nil)
        for i = 0 then (1+ i)
      while line
      do (format t "~2D ~2D;~3D: ~A~%" i line-count line-position line)))
|#
;;;
;;;; Line Counting Stream 
;;;
;;; N-Triples are line-based data format. Namely, one triple per one line. 
;;; Even though Turtles and JSON-LD are not line-based, it is important 
;;; to notice errors with line numbers in order for users to fix them.
;;;
;;; In the original SWCLOS, peeking one and more characters has been realized 
;;; in order to parse RDF/XML format data instead of making a state-transition network. 
;;; However, characters to be read actually exist in streams at hand, if the streams 
;;; come from files. The fact is that peeking characters is needless in the case, if it 
;;; is possible to go back to any file position which you want.
;;;
;;; This module provides line count numbers and file positions for every line to users. 
;;; This module will be base of new parsers for N-Triples, Turtles, JSON-LD, and RDF/XML. 
;;;

;;;
;;; <stream> is a stream that counts and provides line numbers of files. 
;;;
#+allegro
(defclass stream (excl::file-simple-stream)
  ((line-count :initform 0 :accessor line-count
               :documentation "placeholder of line count, line number starts at 0.")
   (line-position :initform 0 :accessor line-position
                  :documentation "placeholder of current line position.")
   (previous-line-position :initform 0 :accessor previous-line-position
                           :documentation "placeholder of previous line position.")
   )
  (:documentation "This stream has a sequence of line positions in the file.")
  )

#+allegro
(defun update-line-count (stream)
  "when a newline is read, this function should be called imediately.
   before any newline line-count 0, file-position 0,
   after the first newline line-count 1 and current position."
  (setf (previous-line-position stream) (line-position stream))
  (setf (line-position stream) (file-position stream))
  (incf (line-count stream))
  )
#+allegro
(defun expose-one-line-buf (stream)
  "exposes one line of the current file position."
  (let ((this-pos (file-position stream)))
    (file-position stream (line-position stream))
    (prog1 (cl:read-line stream)
      (file-position stream this-pos))))
#+allegro
(defun expose-three-lines-buf (stream)
  "exposes three lines around the current file position."
  (let ((this-pos (file-position stream)))
    (file-position stream
                   (previous-line-position stream))
    (prog1
        (cons (cl:read-line stream)
              (cons (cl:read-line stream)
                    (list (cl:read-line stream nil nil))))
      (file-position stream this-pos))))
#+allegro
(defun read-line (stream &optional (eof-error-p t) eof-value recursive-p)
  (declare (inline update-line-count))
  (prog1 (cl:read-line stream eof-error-p eof-value recursive-p)
    (update-line-count stream)))
#|
Above code is effective, too, in SBCL, but optional external-format :utf-8 causes an error.
No external-format sets default value ISO-8859-1 (8 bit code for Latin-1).
To escape this error, the following codes are developed.
|#
#+sbcl
(let ((line-position-table (make-hash-table))
      (line-count 0)
      (line-position 0))
  (setf (gethash 0 line-position-table) 0)
  (defun line-count (stream)
    (declare (ignore stream))
    line-count)
  (defun (setf line-count) (newval stream)
    (declare (ignore stream))
    (setq line-count newval))
  (defun line-position (stream)
    (declare (ignore stream))
    line-position)
  (defun (setf line-position) (newval stream)
    (declare (ignore stream))
    (setq line-position newval))
  (defun update-line-count (stream)
    "Note that this function should be called after reading a NEWLINE"
    (setf (gethash line-count line-position-table) line-position)
    (setq line-position (file-position stream))
    (incf line-count))
  (defun expose-one-line-buf (stream)
    "exposes one line of the current file position."
    (let ((this-pos (file-position stream)))
      (file-position stream line-position)
      (prog1 (cl:read-line stream)
        (file-position stream this-pos))))
  (defun expose-three-lines-buf (stream)
    "exposes three lines around the current file position."
    (let ((this-pos (file-position stream)))
      (file-position stream (gethash (1- line-count) line-position-table)) ; previous line position
      (prog1
          (cons (cl:read-line stream)
                (cons (cl:read-line stream)
                      (list (cl:read-line stream nil nil))))
        (file-position stream this-pos))))
  (defun read-line (stream &optional (eof-error-p t) eof-value recursive-p)
    (prog1 (string-right-trim '(#\Return #\Linefeed)
                              (cl:read-line stream eof-error-p eof-value recursive-p))
      (update-line-count stream)))
  (defmacro with-open-file (varargs &rest body)
    `(cl:with-open-file (,(car varargs) ,(cadr varargs) ,@(cddr varargs)) ; :utf-8 as default
       ,@body))
  )

#+allegro
(defun simple-stream-read-line (stream &optional (eof-error-p t) eof-value line)
  (multiple-value-bind (result stopped end)
      (excl:simple-stream-read-line stream eof-error-p eof-value line)
    (update-line-count stream)
    (when (and stopped (eq result eof-value)) 
      (return-from simple-stream-read-line (values eof-value 0)))
    (values result (or end (length result)))))
#+allegro
(defmacro with-open-file (varargs &rest body)
  "calling sequence: `with-open-file (stream filespec {options}*) {declaration}* {form}*'
   this macro adds class option for <line:stream>."
  `(cl:with-open-file (,(car varargs) ,(cadr varargs) ,@(cddr varargs)
                      ':class 'stream)
     ,@body))

#|
(in-package :line)
(with-open-file (stream "C:\\allegro-projects\\swclos2\\linetest.txt")
  (loop for line = (read-line stream nil nil)
      while line
      do (format t "~2D;~3D: ~A ~A~%" (line-count stream) (line-position stream) line (expose-one-line-buf stream))))

|#
;;;
;;; Token Reader
;;;
;;; A token is delimited white characters, that is, spaces and tabs in N-Triples,
;;; furthermore, newlines and returns in Turtles and RDF/XML.
;;; As well, those characters satisfy predicate <delimiter-p>.

(defmacro white-char-p (char)
  "Space, Tab, Newline, Return, or Page ?"
  `(or (char= ,char #\Space)
       (char= ,char #\Tab)
       (char= ,char #\Newline)
       (char= ,char #\Return)
       (char= ,char #\Page)))

(defun skipbl (stream)
  "In this version, this function must be, excepting comment, used to eat up every newline."
  (declare (optimize (speed 3) (safety 0)))
  (let ((char (read-char stream nil :EOF)))
    (cond ((eq char :EOF) :EOF)            ; eof then exit
          ((char= char #\Newline)          ; newline
           (update-line-count stream)
           (skipbl stream))                ; loop by tail recursive
          ((or (char= char #\Space)
               (char= char #\Tab)
               (char= char #\Return)
               (char= char #\Page))
           (skipbl stream))                ; loop by tail recursive
          (t (unread-char char stream))))) ; else unread and exit

(defun next-token (stream)
  "reads and returns a next token. A token is a string delimited by one or more white characters,
   and one or more semicolons and commas, and closing angle bracket and colon for XML.
   Note that white chars and semicolons and commas. succeeding a token is not eaten up."
;;;  (declare (optimize (speed 3) (safety 0)))
  (skipbl stream)
  (coerce
   (loop for char character = (read-char stream nil nil)
       until (or (eq char nil)   ; EOF
                 (when (or (char= char #\Space)
                           (char= char #\Tab)
                           (char= char #\Newline)
                           (char= char #\Return)
                           (char= char #\Page)
                           (char= char #\;)
                           (char= char #\,)
                           (char= char #\>)
                           (char= char #\:)
                           (char= char #\=))
                   (unread-char char stream)
                   t))
       collect char)
   'string))
#+allegro
(defun read-string (stream)
  (line:skipbl stream)
  (assert (char= #\" (read-char stream nil nil)))
  (excl::read-string stream #\"))
#+sbcl
(defun read-string (stream)
  (line:skipbl stream)
  (assert (char= #\" (read-char stream nil nil)))
  (%read-string stream #\"))
#+sbcl
(defun %read-string (stream closech)
  (let ((stringbuf nil))
    (loop for ch = (cl:read-char stream nil nil)
         while ch
        do (cond ((char= ch closech)
                  (return-from %read-string (coerce (nreverse stringbuf) 'cl:string)))
                 ((char= ch #\\)
                  (push ch stringbuf)
                  (push (read-char stream) stringbuf))
                 (t (push ch stringbuf))))))
     
;;;(defun read-string (stream)
;;;  (unless (boundp sb-impl::*read-buffer*) (setq sb-impl::*read-buffer* nil))
;;;  (line:skipbl stream)
;;;  (assert (char= #\" (read-char stream nil nil)))
;;;  (sb-impl::read-string stream #\"))

;; This counts newline.
(defmacro peeknext-char (stream)
  `(progn (skipbl ,stream) ; this is for newline
     (peek-char nil ,stream nil ,stream)))

(defmacro putback-char (c stream)
  `(unread-char ,c ,stream))

(defmacro getnext-char (stream)
  `(read-char ,stream))

;;;
;;;
;;;

(defun null-line-p (line pos &optional (len (length line)))
;;;  (declare (optimize (speed 3) (safety 0)))
  (declare (type fixnum pos len))
  (>= pos len))

;; End of module
;; --------------------------------------------------------------------
