;; metamathmap.lisp

;; MathMap

;; Copyright (C) 2004 Mark Probst

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

(load "utils.lisp")
(load "let-match.lisp")

;; symbol mathmap-name is-infix (optional)
(defparameter *functions*
  '((orig-val "origVal")
    (= "==" t)
    (< "<" t)
    (> ">" t)
    (<= "<=" t)
    (>= ">=" t)
    (+ "+" t)
    (- "-" t)
    (* "*" t)
    (/ "/" t)
    (% "%" t)
    (neg "-")
    (not "!")
    (pow "^" t)
    (and "&&" t)
    (or "||" t)
    ))

(defparameter *constants*
  '(xy x y ra r a pi))

(defun lookup (bindings name)
  (let ((binding (assoc name bindings)))
    (if (null binding)
	nil
      (values (third binding) (second binding)))))

(defun update-recursion-alist (name alist)
  (if (assoc name alist)
      (mapcar #'(lambda (rec) (if (equalp (first rec) name)
				  (cons name (1+ (cdr rec)))
				rec))
	      alist)
      (cons (cons name 1) alist)))

(defun metamap (function-list)
  (labels ((find-toplevel (type name)
			  (find-if #'(lambda (toplevel)
				       (and (equalp (first toplevel) type)
					    (equalp (first (second toplevel)) name)))
				   function-list))
	   (store-result-code (result-names)
	     (if (null result-names)
		 ""
		 (progn
		   (assert (= (length result-names) 1))
		   (format nil "~A=" (first result-names)))))
	   (compile-function (function arg-names result-names recursion-alist)
			     (let* ((result-types (cdr (second function)))
				    (args (third function))
				    (default-value (fourth function))
				    (body (cddddr function))
				    (name (first (second function)))
				    (recursion (assoc name recursion-alist)))
			       (if (and (not (null recursion))
					(>= (cdr recursion) 1)) ;recursion limit
				   (compile-expr default-value '() result-names recursion-alist)
				 (let ((recursion-alist (update-recursion-alist name recursion-alist))
				       (bindings (mapcar #'(lambda (arg arg-name)
							     (list (first arg) (second arg) arg-name))
							 args arg-names)))
				   (compile-exprs body bindings result-names recursion-alist)))))
	   (compile-exprs (exprs bindings result-names recursion-alist)
			  (if (null (cdr exprs))
			      (compile-expr (car exprs) bindings result-names recursion-alist)
			    (format nil "(~A;~%~A)"
				    (compile-expr (car exprs) bindings '() recursion-alist)
				    (compile-exprs (cdr exprs) bindings result-names recursion-alist))))
	   ;; bindings is a list of lists of the form (name type mathmap-name)
	   (compile-expr (expr bindings result-names recursion-alist)
	     (format t "compiling ~A with results in ~A (recursion list ~A)~%" expr result-names recursion-alist)
	     (case-match expr
	       ((variables ?vars . ?body)
		(let ((new-bindings (mapcar #'(lambda (var)
						(let ((name (first var))
						      (type (second var)))
						  (list name type (make-tmp-name))))
					    vars)))
		  (compile-exprs body (append new-bindings bindings) result-names recursion-alist)))
	       ((progn . ?body)
		(compile-exprs body bindings result-names recursion-alist))
	       ((if ?condition ?consequent)
		(assert (null result-names))
		(format nil "if ~A then~%~A~%end"
			(compile-expr condition bindings '() recursion-alist)
			(compile-expr consequent bindings '() recursion-alist)))
	       ((if ?condition ?consequent ?alternative)
		(format nil "if ~A then~%~A~%else~%~A~%end"
			(compile-expr condition bindings '() recursion-alist)
			(compile-expr consequent bindings result-names recursion-alist)
			(compile-expr alternative bindings result-names recursion-alist)))
	       ((while ?invariant . ?body)
		(format nil "while ~A do~%~A~%end"
			(compile-expr invariant bindings '() recursion-alist)
			(compile-exprs body bindings result-names recursion-alist)))
	       ((setq (aref ?name ?index) ?value)
		(error "arrays not yet supported"))
	       ((setq (values . ?names) ?values)
		(assert (null result-names))
		(let ((mathmap-names (mapcar #'(lambda (name)
						 (lookup bindings name))
					     names)))
		  (compile-expr values bindings mathmap-names recursion-alist)))
	       ((setq ?name ?value)
		(assert (null result-names))
		(let ((mathmap-name (lookup bindings name)))
		  (assert (not (null mathmap-name)))
		  (format nil "(~A = ~A)"
			  mathmap-name
			  (compile-expr value bindings '() recursion-alist))))
	       ((multiple-value-bind ?names ?values . ?body)
		(let ((mathmap-names (mapcar #'(lambda (name)
						 (let ((mathmap-name (lookup bindings name)))
						   (assert (not (null mathmap-name)))
						   mathmap-name))
					     names)))
		  (format nil "(~A;~%~A)"
			  (compile-expr values bindings mathmap-names recursion-alist)
			  (compile-exprs body bindings result-names recursion-alist))))
	       ((values . ?values)
		(assert (= (length result-names) (length values)))
		(format nil "(~{~A~^;~})"
			(mapcar #'(lambda (result-name value)
				    (compile-expr value bindings (list result-name) recursion-alist))
				result-names values)))
	       ((tuple ?type . ?values)
		(format nil "(~A~A:[~{~A~^,~}])"
			(store-result-code result-names)
			(dcs (first type))
			(mapcar #'(lambda (value)
				    (compile-expr value bindings '() recursion-alist))
				values)))
	       ((nth ?value ?index)
		(format nil "(~A(~A)[~A])"
			(store-result-code result-names)
			(compile-expr value bindings '() recursion-alist)
			(compile-expr index bindings '() recursion-alist)))
	       ((asm ?string)
		(format nil "~A~A"
			(store-result-code result-names)
			string))
	       ((?func-name . ?args)
		(if (find-toplevel 'function func-name)
		    (let ((arg-names (mapcar #'(lambda (arg) (make-tmp-name)) args)))
		      (format nil "(~{~A;~%~}~A)"
			      (mapcar #'(lambda (arg name)
					  (compile-expr arg bindings (list name) recursion-alist))
				      args arg-names)
			      (compile-function (find-toplevel 'function func-name)
						arg-names
						result-names
						recursion-alist)))
		    (let* ((function-params (assoc func-name *functions*))
			   (mathmap-name (if function-params
					     (second function-params)
					     (dcs func-name)))
			   (is-infix (if function-params
					 (third function-params)
					 nil)))
		      (if is-infix
			  (progn
			    (assert (= (length args) 2))
			    (format nil "(~A(~A ~A ~A))"
				    (store-result-code result-names)
				    (compile-expr (first args) bindings '() recursion-alist)
				    mathmap-name
				    (compile-expr (second args) bindings '() recursion-alist)))
			  (format nil "(~A~A(~{~A~^,~}))"
				  (store-result-code result-names)
				  mathmap-name
				  (mapcar #'(lambda (arg)
					      (compile-expr arg bindings '() recursion-alist))
					  args))))))
	       (t
		(cond ((numberp expr)
		       (format nil "(~A~A)"
			       (store-result-code result-names)
			       expr))
		      ((member expr *constants*)
		       (format nil "(~A~A)"
			       (store-result-code result-names)
			       (dcs expr)))
		      ((lookup bindings expr)
		       (let ((name (lookup bindings expr)))
			 (assert (not (null name)))
			 (format nil "(~A~A)"
				 (store-result-code result-names)
				 name)))
		      ((find-toplevel 'global expr)
		       (format nil "(~Aglobal_~A)"
			       (store-result-code result-names)
			       (dcs expr)))
		      (t
		       (assert nil)))))))
    (format nil "(~{~A;~%~}~A)"
	    (mappend #'(lambda (toplevel)
			 (if (equalp (first toplevel) 'global)
			     (list (compile-expr (third toplevel)
						 '()
						 (list (format nil "global_~A" (dcs (first (second toplevel)))))
						 '()))
			     '()))
		     function-list)
	    (compile-function (find-toplevel 'function 'main) '() '() '()))))

;; heusl is a function returning an rgba:4, taking two arguments, namely an
;; rgba:4 an xy:2.  on recursion overflow, it returns rgba:[0,0,0,1].
'(function (heusl (rgba 4)) ((bla (rgba 4))
			     (blu (xy 2)))
	   (tuple (rgba 4) 0 0 0 1)
	   (variables ((muh (rgba 4))
		       (kuh (array 5 (ri 2))))
		      (set (aref kuh 2) (tuple (rgba 4) 1 2 3 4))))

'(function (ident (rgba 4)) () (tuple (rgba 4) 0 0 0 1) (orig-val xy))

(defparameter **darts**
  '((global (heusl (nil 1)) (asm "user_float(\"heusl\", 0, 1)"))
    (function (main (rgba 4)) ()
	      (tuple (rgba 4) 0 0 0 1)
	      (variables ((ang (nil 1))
			  (p (rgba 4))
			  (q (rgba 4))
			  (dist (nil 1))
			  (width (nil 1)))
			 (setq ang (/ (* 2 pi) 10))
			 (setq dist 50)
			 (setq width 10)
			 (setq p (orig-val xy))
			 (setq q (if (inintv (% (- a (/ ang 4)) ang)
					     0
					     (/ ang 2))
				     p
				   (neg-pixel p)))
			 (setq q (if (inintv (% r dist)
					     (- dist width)
					     dist)
				     q
				   (neg-pixel q)))
			 (tuple (rgba 4) (nth q 0) (nth q 1) (nth q 2) (nth p 3))))
    (function (neg-pixel (rgba 4)) ((pixel (rgba 4)))
	      (tuple (rgba 4) 0 0 0 1)
	      (+ (neg pixel) 1))))

(defparameter **trace**
  '((function (perp (v3 3))
     ((v1 (v3 3))			;arguments
      (v2 (v3 3)))
     (tuple (v3 3) 0 0 0)		;default value
     (* v2 (dotp v2 v1)))

    (function (refl (v3 3))
     ((v1 (v3 3))
      (v2 (v3 3)))
     (tuple (v3 3) 0 0 0)
     (+ v1 (* (perp v1 (normalize v2)) -2)))

    (function (mag (nil 1))
     ((v (v3 3)))
     (tuple (nil 1) 0)
     (sqrt (dotp v v)))

    (function (hit-sphere (v3 3) (v3 3) (nil 1) (rgba 4) (v3 3))
     ((o (v3 3))			;arguments
      (v (v3 3)))
     0					;default value
     (variables ((er (nil 1))
		 (so (v3 3))
		 (oo (v3 3))
		 (ah (nil 1))
		 (b (nil 1))
		 (c (nil 1))
		 (q (nil 1)))
      (setq er 10)
      (setq so (tuple (v3 3) 0 0 0))
      (setq oo (- o so))
      (setq ah (dotp v v))
      (setq b (* 2 (dotp oo v)))
      (setq c (- (dotp oo oo) (* er er)))
      (setq q (- (* b b) (* (* 4 ah) c)))
      (if (> q 0)
	  (variables ((qq (nil 1))
		      (l1 (nil 1))
		      (ho (v3 3))
		      (xv (v3 3))
		      (out-o (v3 3))
		      (out-v (v3 3)))
	   (setq qq (sqrt q))
	   (setq l1 (/ (- (neg b) qq) (* 2 ah)))
	   (setq xv (* v l1))
	   (setq ho (+ oo xv))
	   (setq out-o (+ ho so))
	   (setq out-v (normalize (refl v ho)))
	   (values out-o ho l1 (tuple (rgba 4) 0 0 0.5 0.5) out-v))
	  (values (- o v) (tuple (v3 3) 0 0 0) -1 (tuple (rgba 4) 0 0 0 1) (tuple (v3 3) 0 0 0)))))

    (function (hit-plane (v3 3) (v3 3) (nil 1) (rgba 4) (v3 3))
     ((o (v3 3))			;arguments
      (v (v3 3)))
     0					;default value
     (variables ((po (v3 3))
		 (pn (v3 3))
		 (oo (v3 3))
		 (ah (nil 1))
		 (b (nil 1))
		 (u (nil 1))
		 (xv (nil 1))
		 (ho (v3 3))
		 (out-o (v3 3))
		 (out-v (v3 3))
		 (rv (v3 3))
		 (pv (v3 3))
		 (pw (v3 3))
		 (cv (v3 3))
		 (cw (v3 3)))
      (setq po (tuple (v3 3) 0 0 5))
      (setq pn (tuple (v3 3) 1 1 1))
      (setq oo (- o po))
      
      (setq ah (dotp pn v))
      (setq b (dotp pn oo))

      (setq u (/ b ah))

      (setq xv (* v (neg u)))
      (setq ho (+ oo xv))
      (setq out-o (+ ho po))

      (setq rv (refl v pn))
      (setq out-v (normalize rv))

      (setq pv (normalize (tuple (v3 3) 1 (/ (- (nth pn 2) (nth pn 0)) (nth pn 1)) -1)))
      (setq pw (normalize (crossp pv pn)))

      (setq cv (* (dotp ho pv) 20))
      (setq cw (* (dotp ho pw) 20))

      (values out-o pn (neg u) (orig-val (tuple (xy 2) cv cw)) out-v)))
     
    (global (vx (nil 1)) (asm "user_float(\"vx\", 0, 5)"))
    (global (vy (nil 1)) (asm "user_float(\"vy\", 0, 5)"))
    (global (vz (nil 1)) (asm "user_float(\"vz\", 0, 5)"))

    (global (lx (nil 1)) (asm "user_float(\"lx\", -5, 5)"))
    (global (ly (nil 1)) (asm "user_float(\"ly\", -5, 5)"))
    (global (lz (nil 1)) (asm "user_float(\"lz\", -5, 5)"))

    (function (raytrace (rgba 4))
     ((start-point (v3 3))		;arguments
      (eye-vector (v3 3)))
     (tuple (rgba 4) 0 0 0 1)		;default value
     (variables ((sphere-hit-point (v3 3))
		 (sphere-normal (v3 3))
		 (sphere-color (rgba 4))
		 (sphere-reflect (v3 3))
		 (sphere-dist (nil 1))
		 (plane-hit-point (v3 3))
		 (plane-normal (v3 3))
		 (plane-color (rgba 4))
		 (plane-reflect (v3 3))
		 (plane-dist (nil 1))
		 (obj-hit-point (v3 3))
		 (obj-normal (v3 3))
		 (obj-color (rgba 4))
		 (obj-reflect (v3 3))
		 (reflect-color (rgba 4)))
      (multiple-value-bind (sphere-hit-point sphere-normal sphere-dist sphere-color sphere-reflect)
	  (hit-sphere start-point eye-vector)
	(multiple-value-bind (plane-hit-point plane-normal plane-dist plane-color plane-reflect)
	    (hit-plane start-point eye-vector)
	  (if (and (< sphere-dist 0) (< plane-dist 0))
	      (tuple (rgba 4) 0 0 0 1)	;no hit
	      (progn
		(if (and (> sphere-dist 0)
			 (or (< plane-dist 0)
			     (< sphere-dist plane-dist)))
;		    (tuple (rgba 4) 1 0 0 1)
;		    (tuple (rgba 4) 0 1 0 0))))))))
    
;		    sphere-color
;		    plane-color)))))))

		    (progn		;sphere hit
		      (setq obj-hit-point sphere-hit-point)
		      (setq obj-normal sphere-normal)
		      (setq obj-color sphere-color)
		      (setq obj-reflect sphere-reflect))
		    (progn		;plane hit
		      (setq obj-hit-point plane-hit-point)
		      (setq obj-normal plane-normal)
		      (setq obj-color plane-color)
		      (setq obj-reflect plane-reflect)))
		(merd obj-hit-point)
		(merd obj-normal)
		(merd obj-color)
		(merd obj-reflect)
		(if (< (nth obj-color 3) 1)
		    (setq reflect-color (raytrace obj-hit-point obj-reflect))
		    (setq reflect-color (tuple (rgba 4) 0 0 0 1)))
		(+ obj-color (* reflect-color (nth obj-color 3)))))))))

    (function (main (rgba 4))
     ()					;arguments
     (tuple (rgba 4) (0 0 0 1))		;default value
     (variables ((o (v3 3))
		 (v (v3 3))
		 (ah (nil 1))
		 (b (nil 1))
		 (l (v3 3)))
      (setq o (tuple (v3 3) (* x 0.1) (* y 0.1) -20))
      (setq v (tuple (v3 3) vx vy vz))
      (raytrace o v)))))
#|
      (multiple-value-bind (ah o v)
	  (do-hit-sphere o v)
	(if (< ah 0)
	    (tuple (rgba 4) 0 0 0 1)
	    (progn
	      (setq l (normalize (tuple (v3 3) lx ly lz)))
	      (setq b (- 1 (dotp l (normalize v))))
	      (tuple (rgba 4) b b b 1))))))))
|#
