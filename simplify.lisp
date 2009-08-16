;; simplify.lisp

;; MathMap

;; Copyright (C) 2009 Mark Probst

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

(in-package :mathmap)

(defstruct simplify
  name pattern replacement)

(defparameter *simplifies* nil)

(defmacro defsimplify (name pattern replacement)
  `(push (make-simplify :name ',name :pattern ',pattern :replacement ',replacement)
	 *simplifies*))

(defsimplify closure-pixel-width
    (image-pixel-width (closure :as c) :as s)
  (c-fun "simplify_closure_pixel_size" s c))

(defsimplify closure-pixel-height
    (image-pixel-height (closure :as c) :as s)
  (c-fun "simplify_closure_pixel_size" s c))

(defun binding-var-name (name)
  (format nil "binding_~A" (dcs name)))

(defun print-matcher (pattern c-expr)
  (let ((done-label (dcs (gensym "done"))))
    (labels ((split-stmt-pattern (pattern)
	       (let* ((pos (position :as pattern))
		      (name (if pos (nth (1+ pos) pattern) (gensym "stmt")))
		      (pattern (if pos (subseq pattern 0 pos) pattern)))
		 (values pattern name)))
	     (find-op (name)
	       (find name *operators* :key #'op-name))
	     (process-pattern (pattern c-expr)
	       (multiple-value-bind (pattern name)
		   (split-stmt-pattern pattern)
		 (let ((c-name (binding-var-name name)))
		   (format t "statement_t *~A = ~A;~%" c-name c-expr)
		   (format t "g_assert(~A->kind == STMT_ASSIGN);~%" c-name)
		   (case-match pattern
		     ((closure)
		      (format t "if (~A->v.assign.rhs->kind != RHS_CLOSURE) goto ~A;~%" c-name done-label))
		     ((?op-name . ?args)
		      (let ((op (find-op op-name)))
			(assert (and op (= (op-arity op) (length args))))
			(format t "if (~A->v.assign.rhs->kind != RHS_OP) goto ~A;~%" c-name done-label)
			(format t "if (compiler_op_index(~A->v.assign.rhs->v.op.op) != ~A) goto ~A;~%"
				c-name (op-c-define op) done-label)
			(dolist (index (integers-upto (length args)))
			  (format t "if (~A->v.assign.rhs->v.op.args[~A].kind != PRIMARY_VALUE) goto ~A;~%"
				  c-name index done-label)
			  (process-pattern (nth index args)
					   (format nil "~A->v.assign.rhs->v.op.args[~A].v.value->def" c-name index)))))
		     (?val
		      (error "illegal pattern ~A" val)))))))
      (process-pattern pattern c-expr)
      (format t "does_match = TRUE;~%~A:~%" done-label))))

(defun print-replacer (replacement)
  (case-match replacement
    ((c-fun ?name . ?args)
     (format t "did_replace = ~A(filter, ~{~A~^, ~});~%" name (mapcar #'binding-var-name args)))
    (?val
     (error "illegal replacement ~A" val))))

(with-open-file (out "compopt/simplify_func.c" :direction :output :if-exists :supersede)
  (let ((*standard-output* out))
    (format t "static void recur (filter_t *filter, statement_t *stmt, gboolean *changed) {~%")
    (format t "while (stmt != NULL) {~%again: switch (stmt->kind) {~%case STMT_NIL: break;~%")
    (format t "case STMT_ASSIGN :~%")
    (dolist (simplify *simplifies*)
      (format t "{ /* ~A */~%gboolean does_match = FALSE;~%" (simplify-name simplify))
      (print-matcher (simplify-pattern simplify) "stmt")
      (format t "if (does_match) {~%gboolean did_replace;~%")
      (print-replacer (simplify-replacement simplify))
      (format t "if (did_replace) { *changed = TRUE; goto again; }~%}~%}"))
    (format t "break;~%")
    (format t "case STMT_IF_COND :~%recur(filter, stmt->v.if_cond.consequent, changed); recur(filter, stmt->v.if_cond.alternative, changed); break;~%")
    (format t "case STMT_WHILE_LOOP :~%recur(filter, stmt->v.while_loop.body, changed); break;~%")
    (format t "default : g_assert_not_reached();~%")
    (format t "}~% stmt = stmt->next;~%}~%}~%")))
