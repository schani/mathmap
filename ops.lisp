;; ops.lisp

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

(defparameter *operators* nil)

(defmacro defop (name arity c-define c-name &key (type-prop 'const) (type 'float) (pure t) (foldable t))
  `(push '(,name ,arity ,c-define ,c-name ,type-prop ,type ,pure ,(if pure foldable nil)) *operators*))

(defop nop 0 "OP_NOP" "NOP" :type int)

(defop + 2 "OP_ADD" "ADD" :type-prop max :type nil)
(defop - 2 "OP_SUB" "SUB" :type-prop max :type nil)
(defop - 1 "OP_NEG" "NEG" :type-prop max :type nil)
(defop * 2 "OP_MUL" "MUL" :type-prop max :type nil)
(defop / 2 "OP_DIV" "DIV")
(defop % 2 "OP_MOD" "MOD")

(defop abs 1 "OP_ABS" "fabs" :type-prop max :type nil)
(defop min 2 "OP_MIN" "MIN" :type-prop max :type nil)
(defop max 2 "OP_MAX" "MAX" :type-prop max :type nil)

(defop sqrt 1 "OP_SQRT" "sqrt")
(defop hypot 2 "OP_HYPOT" "hypot")
(defop sin 1 "OP_SIN" "sin")
(defop cos 1 "OP_COS" "cos")
(defop tan 1 "OP_TAN" "tan")
(defop asin 1 "OP_ASIN" "asin")
(defop acos 1 "OP_ACOS" "acos")
(defop atan 1 "OP_ATAN" "atan")
(defop atan 2 "OP_ATAN2" "atan2")
(defop pow 2 "OP_POW" "pow")
(defop exp 1 "OP_EXP" "exp")
(defop log 1 "OP_LOG" "log")
(defop sinh 1 "OP_SINH" "sinh")
(defop cosh 1 "OP_COSH" "cosh")
(defop tanh 1 "OP_TANH" "tanh")
(defop asinh 1 "OP_ASINH" "asinh")
(defop acosh 1 "OP_ACOSH" "acosh")
(defop atanh 1 "OP_ATANH" "atanh")
(defop gamma 1 "OP_GAMMA" "GAMMA")

(defop floor 1 "OP_FLOOR" "floor" :type int)
(defop = 2 "OP_EQ" "EQ" :type int)
(defop < 2 "OP_LESS" "LESS" :type int)
(defop <= 2 "OP_LEQ" "LEQ" :type int)
(defop not 1 "OP_NOT" "NOT" :type int)

(defop print-dummy 1 "OP_PRINT" "PRINT_FLOAT" :type int :pure nil)
(defop newline-dummy 0 "OP_NEWLINE" "NEWLINE" :type int :pure nil)

(defop start-debug-tuple 1 "OP_START_DEBUG_TUPLE" "START_DEBUG_TUPLE" :type int :pure nil)
(defop set-debug-tuple-data 2 "OP_SET_DEBUG_TUPLE_DATA" "SET_DEBUG_TUPLE_DATA" :type int :pure nil)

(defop orig-val 4 "OP_ORIG_VAL" "ORIG_VAL" :type color :foldable nil)
(defop red 1 "OP_RED" "RED_FLOAT")
(defop green 1 "OP_GREEN" "GREEN_FLOAT")
(defop blue 1 "OP_BLUE" "BLUE_FLOAT")
(defop alpha 1 "OP_ALPHA" "ALPHA_FLOAT")

(defop complex 2 "OP_COMPLEX" "COMPLEX" :type complex)
(defop c-real 1 "OP_C_REAL" "crealf")
(defop c-imag 1 "OP_C_IMAG" "cimagf")
(defop c-sqrt 1 "OP_C_SQRT" "csqrtf" :type complex)
(defop c-sin 1 "OP_C_SIN" "csinf" :type complex)
(defop c-cos 1 "OP_C_COS" "ccosf" :type complex)
(defop c-tan 1 "OP_C_TAN" "ctanf" :type complex)
(defop c-asin 1 "OP_C_ASIN" "casinf" :type complex)
(defop c-acos 1 "OP_C_ACOS" "cacosf" :type complex)
(defop c-atan 1 "OP_C_ATAN" "catanf" :type complex)
(defop c-pow 2 "OP_C_POW" "cpowf" :type complex)
(defop c-exp 1 "OP_C_EXP" "cexpf" :type complex)
(defop c-log 1 "OP_C_LOG" "clogf" :type complex)
(defop c-arg 1 "OP_C_ARG" "cargf")
(defop c-sinh 1 "OP_C_SINH" "csinhf" :type complex)
(defop c-cosh 1 "OP_C_COSH" "ccoshf" :type complex)
(defop c-tanh 1 "OP_C_TANH" "ctanhf" :type complex)
(defop c-asinh 1 "OP_C_ASINH" "casinhf" :type complex)
(defop c-acosh 1 "OP_C_ACOSH" "cacoshf" :type complex)
(defop c-atanh 1 "OP_C_ATANH" "catanhf" :type complex)
(defop c-gamma 1 "OP_C_GAMMA" "cgamma" :type complex)

(defop make-m2x2 4 "OP_MAKE_M2X2" "MAKE_M2X2" :type m2x2 :pure nil)
(defop make-m3x3 9 "OP_MAKE_M3X3" "MAKE_M3X3" :type m3x3 :pure nil)
(defop free-matrix 1 "OP_FREE_MATRIX" "FREE_MATRIX" :type int :pure nil)

(defop make-v2 2 "OP_MAKE_V2" "MAKE_V2" :type v2 :pure nil)
(defop make-v3 3 "OP_MAKE_V3" "MAKE_V3" :type v3 :pure nil)
(defop free-vector 1 "OP_FREE_VECTOR" "FREE_VECTOR" :type int :pure nil)
(defop vector-nth 2 "OP_VECTOR_NTH" "VECTOR_NTH" :pure nil)

(defop solve-linear-2 2 "OP_SOLVE_LINEAR_2" "SOLVE_LINEAR_2" :type v2 :pure nil)
(defop solve-linear-3 2 "OP_SOLVE_LINEAR_3" "SOLVE_LINEAR_3" :type v3 :pure nil)

(defop solve-poly-2 3 "OP_SOLVE_POLY_2" "SOLVE_POLY_2" :type v2 :pure nil)
(defop solve-poly-3 4 "OP_SOLVE_POLY_3" "SOLVE_POLY_3" :type v3 :pure nil)

(defop noise 3 "OP_NOISE" "noise")
(defop rand 2 "OP_RAND" "RAND" :pure nil)

(defop userval-int 1 "OP_USERVAL_INT" "USERVAL_INT_ACCESS" :type int :foldable nil)
(defop userval-float 1 "OP_USERVAL_FLOAT" "USERVAL_FLOAT_ACCESS" :type float :foldable nil)
(defop userval-bool 1 "OP_USERVAL_BOOL" "USERVAL_BOOL_ACCESS" :type int :foldable nil)
(defop userval-curve 2 "OP_USERVAL_CURVE" "USERVAL_CURVE_ACCESS" :type float :foldable nil)
(defop userval-color 1 "OP_USERVAL_COLOR" "USERVAL_COLOR_ACCESS" :type color :foldable nil)
(defop userval-gradient 2 "OP_USERVAL_GRADIENT" "USERVAL_GRADIENT_ACCESS" :type color :foldable nil)

(defop make-color 4 "OP_MAKE_COLOR" "MAKE_COLOR" :type color)
(defop output-color 1 "OP_OUTPUT_COLOR" "OUTPUT_COLOR" :type int :pure nil)

(defun make-op-defines ()
  (apply #'string-concat
	 (mapcar #'(lambda (op index)
		     (format nil "#define ~A ~A~%" (third op) index))
		 (reverse *operators*) (integers-upto (length *operators*)))))

(defun make-init-ops ()
  (apply #'string-concat
	 (mapcar #'(lambda (op)
		     (destructuring-bind (name arity c-define c-name type-prop type pure foldable)
			 op
		       (format nil "    init_op(~A, \"~A\", ~A, TYPE_PROP_~A, ~A, ~:[0~;1~], ~:[0~;1~]);~%"
			       c-define c-name arity (string-upcase (symbol-name type-prop))
			       (cdr (assoc type '((nil . "0")
						  (int . "TYPE_INT") (float . "TYPE_FLOAT")
						  (complex . "TYPE_COMPLEX") (color . "TYPE_COLOR")
						  (m2x2 . "TYPE_MATRIX") (m3x3 . "TYPE_MATRIX")
						  (v2 . "TYPE_VECTOR") (v3 . "TYPE_VECTOR"))))
			       pure foldable)))
		 (reverse *operators*))))

(defun make-ops-file ()
  (with-open-file (out "opdefs.h" :direction :output :if-exists :supersede)
    (let ((*standard-output* out))
      (format t "~A~%#define NUM_OPS ~A~%~%" (make-op-defines) (length *operators*))
      (format t "static void~%init_ops (void)~%{~%~A}~%~%" (make-init-ops)))))
