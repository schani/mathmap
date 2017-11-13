;; utils.lisp

;; clickr

;; Copyright (C) 2002-2005 Mark Probst

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 675 Massachusetts Avenue; Cambridge, MA 02139, USA.

(defpackage :utils
  (:use :cl)
  (:export
   #:map-times #:integers-upto #:mappend #:dcs #:ucs #:make-tmp-name #:partition #:slet* #:random-select
   #-clisp #:string-concat))

(in-package :utils)

(defun map-times (n f)
  (labels ((mapper (i)
	     (if (>= i n)
		 '()
		 (cons (funcall f i) (mapper (1+ i))))))
    (mapper 0)))

(defun integers-upto (n)
  (map-times n #'(lambda (i) i)))

(defun mappend (func &rest lists)
  (reduce #'append (apply #'mapcar func lists)))

;; down-case-symbol
(defun dcs (x)
  (substitute #\d #\. (substitute #\p #\+ (substitute #\_ #\- (string-downcase (symbol-name x))))))

;; up-case-symbol
(defun ucs (x)
  (substitute #\d #\. (substitute #\p #\+ (substitute #\_ #\- (string-upcase (symbol-name x))))))

(defvar *tmp-num* 0)
(defun make-tmp-name ()
  (let ((name (format nil "tmp_~A" *tmp-num*)))
    (incf *tmp-num*)
    name))

(defun partition (pred list)
  (let ((in nil)
	(out nil))
    (dolist (item list)
      (if (funcall pred item)
	  (push item in)
	  (push item out)))
    (values (reverse in) (reverse out))))

(defmacro slet* (bindings &body body)
  (labels ((destructure (lhs names body)
	     (if (null lhs)
		 body
		 (let ((rest (destructure (cdr lhs) (cdr names) body)))
		   (if (symbolp (car lhs))
		       rest
		       `(destructuring-bind ,(car lhs)
			 ,(car names)
			 ,rest)))))
	   (expand (bindings)
	     (if (null bindings)
		 (cons 'progn body)
		 (let* ((binding (car bindings))
			(lhs (subseq binding 0 (1- (length binding))))
			(rhs (car (last binding)))
			(rest (expand (cdr bindings)))
			(names (mapcar #'(lambda (n)
					   (if (symbolp n) n (gensym)))
				       lhs))
			(destructured-rest (destructure lhs names rest)))
		   (case (length lhs)
		     (0 `(progn ,rhs ,destructured-rest))
		     (1 `(let ((,(car names) ,rhs))
			  ,destructured-rest))
		     (t `(multiple-value-bind ,names
			  ,rhs
			  ,destructured-rest)))))))
    (expand bindings)))

;; returns a random subset of the elements in list of length (min num
;; (length list))
(defun random-select (num list)
  (let* ((length (length list))
	 (num (min num length)))
    (labels ((indexes (num rest)
	       (if (zerop num)
		   rest
		   (let ((n (random length)))
		     (if (member n rest)
			 (indexes num rest)
			 (indexes (1- num) (cons n rest)))))))
      (let ((indexes (indexes num nil)))
	(mapcar #'(lambda (n) (nth n list)) indexes)))))

#-clisp
(defun string-concat (&rest strings)
  (apply #'concatenate 'string strings))
