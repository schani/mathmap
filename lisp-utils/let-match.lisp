;; let-match.lisp

;; MathMap

;; Copyright (C) 1999-2002 Mark Probst

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

(defpackage :let-match
  (:use :cl)
  (:export
   #:let-match #:case-match #:matchp #:var-symbol-p))

(in-package :let-match)

(defun var-symbol-p (sym)
  (and (symbolp sym)
       (> (length (symbol-name sym)) 1)
       (eq (elt (symbol-name sym) 0) #\?)))

(defun symbol-for-var-symbol (sym)
  (intern (subseq (symbol-name sym) 1)))

(defun dont-care-symbol-p (sym)
  (and (symbolp sym)
       (string-equal (symbol-name sym) "?")))

(defun quoted-p (val)
  (and (consp val) (eq (car val) 'quote)))

(defun compile-let-match (pattern val succ-cont fail-cont bound-vars)
  (cond ((null pattern)
	 (if (quoted-p val)
	     (if (null (cadr val))
		 (funcall succ-cont bound-vars)
	       (funcall fail-cont))
	   `(if (null ,val)
		,(funcall succ-cont bound-vars)
	      ,(funcall fail-cont))))
	((dont-care-symbol-p pattern)
	 (funcall succ-cont bound-vars))
	((var-symbol-p pattern)
	 (let ((bound (assoc pattern bound-vars)))
	   (if bound
	       (let ((static (second bound))
		     (value (third bound)))
		 (if static
		     (if (quoted-p val)
			 (if (equal value val)
			     (funcall succ-cont bound-vars)
			   (funcall fail-cont))
		       `(if (equal ,value ,val)
			    ,(funcall succ-cont bound-vars)
			  ,(funcall fail-cont)))
		   `(if (equal ,value ,val)
			,(funcall succ-cont bound-vars)
		      ,(funcall fail-cont))))
	     (funcall succ-cont (cons (list pattern (quoted-p val) val) bound-vars)))))
	((consp pattern)
	 (if (quoted-p val)
	     (let ((val (cadr val)))
	       (if (consp val)
		   (compile-let-match (car pattern) `(quote ,(car val))
				      #'(lambda (bound-vars)
					  (compile-let-match (cdr pattern) `(quote ,(cdr val))
							     #'(lambda (bound-vars)
								 (funcall succ-cont bound-vars))
							     #'(lambda ()
								 (funcall fail-cont))
							     bound-vars))
				      #'(lambda ()
					  (funcall fail-cont))
				      bound-vars)
		 (funcall fail-cont)))
	   `(if (consp ,val)
		,(let ((val-car (gensym))
		       (val-cdr (gensym)))
		   `(let ((,val-car (car ,val))
			  (,val-cdr (cdr ,val)))
		   ,(compile-let-match (car pattern) val-car
				       #'(lambda (bound-vars)
					   (compile-let-match (cdr pattern) val-cdr
							      #'(lambda (bound-vars)
								  (funcall succ-cont bound-vars))
							      #'(lambda ()
								  (funcall fail-cont))
							      bound-vars))
				       #'(lambda ()
					   (funcall fail-cont))
				       bound-vars)))
	      ,(funcall fail-cont))))
	(t
	 (if (quoted-p val)
	     (if (equal pattern (cadr val))
		 (funcall succ-cont bound-vars)
	       (funcall fail-cont))
	   `(if (equal ',pattern ,val)
		,(funcall succ-cont bound-vars)
	      ,(funcall fail-cont))))))

(defmacro let-match (bindings body &optional (fail nil))
  (labels ((compiler (bindings fail-expr bound-vars)
		     (if (null bindings)
			 (if (null bound-vars)
			     body
			   `(let ,(mapcar #'(lambda (binding)
					      (list (symbol-for-var-symbol (first binding)) (third binding)))
					  bound-vars)
			      ,body))
		       (compile-let-match (caar bindings) (cadar bindings)
					  #'(lambda (bound-vars)
					      (compiler (cdr bindings) fail-expr bound-vars))
					  #'(lambda ()
					      fail-expr)
					  bound-vars))))
    (let* ((fail-name (if (null fail) nil (gensym)))
	   (fail-expr (if (null fail) nil `(,fail-name)))
	   (matcher (compiler bindings fail-expr nil)))
      (if (null fail)
	  matcher
	`(flet ((,fail-name () ,fail))
	   ,matcher)))))

(defmacro case-match (value &rest cases)
  (let ((value-name (gensym))
	(cases (mapcar #'(lambda (case) (cons (gensym) case)) cases)))
    (labels ((compiler (cases)
		       (if (null cases)
			   nil
			 (let* ((case (car cases))
				(label-name (car case))
				(pattern (cadr case))
				(body-forms (cddr case)))
			   `((,label-name ()
					  ,@(if (member pattern '(t otherwise))
						body-forms
					      (list (compile-let-match pattern value-name
								       #'(lambda (bound-vars)
									   `(let ,(mapcar #'(lambda (binding)
											      (list (symbol-for-var-symbol (first binding)) (third binding)))
											  bound-vars)
									      ,@body-forms))
								       #'(lambda ()
									   (if (null (cdr cases))
									       'nil
									     `(,(caadr cases))))
								       nil))))
			     ,@(compiler (cdr cases)))))))
      `(let ((,value-name ,value))
	 (labels ,(compiler cases)
	   ,(if (caar cases)
		`(,(caar cases))
	      nil))))))

(defmacro matchp (value pattern)
  `(case-match ,value
     (,pattern t)
     (? nil)))
