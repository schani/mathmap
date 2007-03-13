;; builtins.lisp

;; MathMap

;; Copyright (C) 2002-2007 Mark Probst

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

(load "ops.lisp")

(defparameter *only-ansi* nil)

(defun my-macroexpand (sexp macros)
  (labels ((expand (sexp)
	     (if (consp sexp)
		 (if (symbolp (car sexp))
		     (let ((macro (cdr (assoc (car sexp) macros))))
		       (if macro
			   (mapcar #'expand (apply macro (cdr sexp)))
			   (cons (car sexp) (mapcar #'expand (cdr sexp)))))
		     (mapcar #'expand sexp))
		 sexp)))
    (expand sexp)))

(defparameter *ops* '((+v 2 "OP_ADD" "ADD")
		      (-v 2 "OP_SUB" "SUB")
		      (-v 1 "OP_NEG" "NEG")
		      (*v 2 "OP_MUL" "MUL")
		      (/v 2 "OP_DIV" "DIV")
		      (%v 2 "OP_MOD" "MOD")
		      (abs-v 1 "OP_ABS" "fabs")))

(defparameter *primops* (mapcar #'(lambda (op)
				    (destructuring-bind (name arity c-define c-name type-prop type pure foldable)
					op
				      (list name arity c-define c-name type)))
				*operators*))

(defparameter *condops* '((= 2 "OP_EQ" "EQ")
			  (< 2 "OP_LESS" "LESS")
			  (<= 2 "OP_LEQ" "LEQ")))

(defparameter *builtins* nil)

(defmacro defbuiltin (overloaded-name name type args &rest body)
  `(setq *builtins* (cons (list ',overloaded-name ',name ',type ',args ',body) *builtins*)))

(defmacro def-simple-builtin (overloaded-name name op-name num-args)
  (let* ((arg-names (map-times num-args #'(lambda (i) (gensym))))
	 (args-decl (mapcar #'(lambda (arg-name) `(,arg-name (?T 1))) arg-names))
	 (actual-args (mapcar #'(lambda (arg-name) `(nth 0 ,arg-name)) arg-names)))
  `(defbuiltin ,overloaded-name ,name (?T 1) ,args-decl
    (set result (make (?T 1) (,op-name ,@actual-args))))))

(defun c-type (type)
  (second (assoc type '((nil "float") (float "float") (int "int")
			(color "color_t") (complex "complex float") (m2x2 "mm_m2x2_t")
			(m3x3 "gsl_matrix *") (v2 "mm_v2_t") (v3 "mm_v3_t")))))

(defun gen-builtin (overloaded-name name type args body for-compiler-p)
  (labels ((length-of-arg-pos (pos)
	     (if for-compiler-p
		 (format nil "arglengths[~A]" pos)
		 (format nil "STK[STKP - ~A].v.tuple.length" (- (length args) pos))))
	   (number-of-arg-pos (pos)
	     (if for-compiler-p
		 (format nil "argnumbers[~A]" pos)
	       (format nil "STK[STKP - ~A].v.tuple.number" (- (length args) pos))))
	   (lookup-length (l)
	     (if (var-symbol-p l)
		 (let ((position (position-if #'(lambda (a) (eq l (cadadr a))) args)))
		   (length-of-arg-pos position))
		 l))
	   (arg-pos (name)
	     (position-if #'(lambda (a) (eq name (car a))) args))
	   (lookup-arg (name)
	     (let ((pos (arg-pos name)))
	       (if (null pos)
		   nil
		   (values #'(lambda (n)
			       (if for-compiler-p
				   (format nil "args[~A][~A]" pos n)
				   (format nil "STK[STKP - ~A].v.tuple.data[~A]" (- (length args) pos) n)))
			   'tuple
			   (lookup-length (cadadr (nth pos args)))))))
	   (lookup-op (op num-args table)
	     (find-if #'(lambda (o) (and (eq op (first o)) (= num-args (second o)))) table)))
    (let ((body (my-macroexpand body `((eval . eval)
				       (+ . ,#'(lambda (&rest args) (reduce #'(lambda (a b) (list '+ a b)) args)))
				       (* . ,#'(lambda (&rest args) (reduce #'(lambda (a b) (list '* a b)) args))))))
	  (result-length (lookup-length (cadr type))))
      ;; bindings are of the form (?name ?c-name primary nil ?c-type) or
      ;; (?name ?func tuple ?length ?c-type) or (?name ?c-name counter nil ?c-type)
      (labels ((gen (stmt bindings)
		 (labels ((lookup-var (name)
			    (let ((var (assoc name bindings)))
			      (if (null var)
				  (lookup-arg name)
				  (values (second var) (third var) (fourth var) (fifth var)))))
			  (make-allocated (name allocatedp)
			    (if (or allocatedp (not for-compiler-p))
				""
				(format nil "~A = make_temporary();~%" name)))
			  ;; returns type, length, c-type
			  (expr-type (expr)
			    (case-match expr
			      ((nth ?n ?expr)
			       (multiple-value-bind (type length c-type)
				   (expr-type expr)
				 (values 'primary nil c-type)))
			      ((make ?type . ?args)
			       (values 'tuple (format nil "~A" (length args))))
			      ((splat ?type ?primary)
			       (multiple-value-bind (prim-type prim-length c-type)
				   (expr-type primary)
				 (values 'tuple (lookup-length (cadr type)) c-type)))
			      ((sum ?expr)
			       'primary)
			      ((?op . ?args)
			       (cond ((member op '(and or not))
				      'primary)
				     ((not (null (lookup-op op (length args) *ops*)))
				      (expr-type (first args)))
				     ((not (null (lookup-op op (length args) *primops*)))
				      (let ((op-entry (lookup-op op (length args) *primops*)))
					(assert (not (null op-entry)))
					(values 'primary nil (fifth op-entry))))
				     (t
				      (error "cannot determine type of expr ~A" expr))))
			      (?val
			       (cond ((integerp val)
				      'primary)
				     ((floatp val)
				      'primary)
				     ((symbolp val)
				      (multiple-value-bind (name type length c-type)
					  (lookup-var val)
					(if (null name)
					    (error "variable ~A not bound" val)
					    (values type length c-type))))
				     (t
				      (error "cannot determine type of expr ~A" expr))))))
			  (expr-decl (expr name)
			    (multiple-value-bind (type length c-type)
				(expr-type expr)
			      (case type
				(primary
				 (if for-compiler-p
				     (format nil "compvar_t *~A;" name)
				     (format nil "~A ~A;" (c-type c-type) name)))
				(tuple
				 (if for-compiler-p
				     (format nil "compvar_t *~A[~A];" name length)
				     (format nil "~A ~A[~A];" (c-type c-type) name length)))
				(t
				 (error "cannot declare expr ~A of type ~A" expr type)))))
			  (gen-op (op-name args lval allocatedp gen-sub)
			    (let ((arg-names (mapcar #'(lambda (x) (make-tmp-name)) args)))
			      (if for-compiler-p
				  (format nil "~A{~%compvar_t ~{*~A~^, ~};~%~{~A~}emit_assign(make_lhs(~A), make_op_rhs(~A~{, make_compvar_primary(~A)~}));~%}~%"
					  (make-allocated lval allocatedp)
					  arg-names
					  (mapcar #'(lambda (arg name) (funcall gen-sub arg name nil)) args arg-names)
					  lval op-name arg-names)
				  (let ((subs (mappend #'(lambda (arg name)
							   (multiple-value-bind (code type)
							       (funcall gen-sub arg name nil)
							     (list (c-type type) name code)))
						       args arg-names)))
				    (format nil "{~%~{~A ~A;~%~*~}~{~2*~A~}~A = ~A(~{~A~^, ~});~%}~%"
					    subs
					    subs
					    lval op-name arg-names)))))
			  (gen-primary (expr lval allocatedp)
			    (case-match expr
			      ((nth ?n ?expr)
			       (if (symbolp n)
				   (multiple-value-bind (name type length)
				       (lookup-var n)
				     (assert (eq type 'counter))
				     (gen-expr-nth expr lval allocatedp name))
				   (progn
				     (assert (integerp n))
				     (gen-expr-nth expr lval allocatedp n))))
			      ((sum ?expr)
			       (multiple-value-bind (type length c-type)
				   (expr-type expr)
				 (assert (eq type 'tuple))
				 (if for-compiler-p
				     (let ((t1 (make-tmp-name))
					   (t2 (make-tmp-name))
					   (ctr (make-tmp-name)))
				       (format nil "if (~A == 1)~%{~%~A~%}~%else~%{~%compvar_t *~A, *~A;~%int ~A;~%~A~A~Aemit_assign(make_lhs(~A), make_op_rhs(OP_ADD, make_compvar_primary(~A), make_compvar_primary(~A)));~%for (~A = 2; ~A < ~A; ++~A)~%{~Aemit_assign(make_lhs(~A), make_op_rhs(OP_ADD, make_compvar_primary(~A), make_compvar_primary(~A)));~%}~%}~%"
					       length (gen-expr-nth expr lval allocatedp "0")
					       t1 t2 ctr ;declare t1, t2, ctr
					       (make-allocated lval allocatedp)
					       (gen-expr-nth expr t1 nil "0") ;t1 = expr(0)
					       (gen-expr-nth expr t2 nil "1") ;t2 = expr(1)
					       lval t1 t2 ;lval = t1 + t2
					       ctr ctr length ctr ;for (ctr = 0; ctr < length; ++ctr)
					       (gen-expr-nth expr t1 nil ctr) ;t1 = expr(ctr)
					       lval lval t1)) ;lval = lval + t1
				     (let ((sum (make-tmp-name))
					   (tmp (make-tmp-name)))
				       (if (numberp length)
					   (format nil "{~%float ~A = 0.0, ~A;~%~{~A~A += ~A;~%~}~A = ~A;~%}~%"
						   sum tmp
						   (mappend #'(lambda (n) (list (gen-expr-nth expr tmp nil n) sum tmp)) (integers-upto length))
						   lval sum)
					   (let ((ctr (make-tmp-name)))
					     (format nil "{~%int ~A;~%float ~A = 0.0, ~A;~%for (~A = 0; ~A < ~A; ++~A)~%{~%~A~%~A += ~A;~%}~%~A = ~A;~%}~%"
						     ctr sum tmp
						     ctr ctr length ctr
						     (gen-expr-nth expr tmp nil ctr)
						     sum tmp
						     lval sum)))))))
			      ((argtag ?val)
			       (let ((number (number-of-arg-pos (arg-pos val))))
				 (if for-compiler-p
				     (format nil "~Aemit_assign(make_lhs(~A), make_int_const_rhs(~A));~%"
					     (make-allocated lval allocatedp) lval number)
				   (format nil "~A = ~A;~%" lval number))))
			      ((?op . ?args)
			       (let ((op-entry (lookup-op op (length args) *primops*)))
				 (if (not (null op-entry))
				     (values (gen-op (if for-compiler-p (third op-entry) (fourth op-entry))
						     args lval allocatedp #'gen-primary)
					     (fifth op-entry))
				     (error "unknown primop ~A/~A" op (length args)))))
			      (pi
			       (if for-compiler-p
				   (format nil "~Aemit_assign(make_lhs(~A), make_float_const_rhs(M_PI));~%"
					   (make-allocated lval allocatedp) lval)
				   (format nil "~A = M_PI;~%" lval)))
			      (?val
			       (cond ((integerp val)
				      (if for-compiler-p
					  (format nil "~Aemit_assign(make_lhs(~A), make_int_const_rhs(~A));~%"
						  (make-allocated lval allocatedp)
						  lval val)
					  (format nil "~A = ~A;~%" lval val)))
				     ((floatp val)
				      (if for-compiler-p
					  (format nil "~Aemit_assign(make_lhs(~A), make_float_const_rhs(~A));~%"
						  (make-allocated lval allocatedp)
						  lval val)
					  (format nil "~A = ~A;~%" lval val)))
				     ((symbolp val)
				      (multiple-value-bind (name type length c-type)
					  (lookup-var val)
					(case type
					  (primary
					   (values
					    (if for-compiler-p
						(if allocatedp
						    (format nil "emit_assign(make_lhs(~A), make_compvar_rhs(~A));~%"
							    lval name)
						  (format nil "~A = ~A;~%"
							  lval name))
					      (format nil "~A = ~A;~%" lval name))
					    c-type))
					  (counter
					   (values
					    (if for-compiler-p
						(format nil "~Aemit_assign(make_lhs(~A), make_int_const_rhs(~A));~%"
							(make-allocated lval allocatedp)
							lval name)
					      (format nil "~A = ~A;~%" lval name))
					    c-type))
					  (t
					   (error "expr ~A is not a primary but a ~A" expr type)))))
				     (t
				      (error "unknown primary ~A" expr))))
			      (t
			       (error "unknown primary ~A" expr))))
			  (gen-expr-nth (expr lval allocatedp n)
			    (case-match expr
			      ((make ?type . ?args)
			       (if (or for-compiler-p (not (numberp n)))
				   (format nil "switch (~A)~%{~%~{case ~A :~%~Abreak;~%~}default :~%assert(0);~%}~%"
					   n
					   (mappend #'(lambda (i p) (list i (gen-primary p lval allocatedp))) (integers-upto (length args)) args))
				 (gen-primary (nth n args) lval allocatedp)))
			      ((splat ?type ?primary)
			       (gen-primary primary lval allocatedp))
			      ((?op . ?args)
			       (let ((op-entry (lookup-op op (length args) *ops*)))
				 (if (not (null op-entry))
				     (gen-op (if for-compiler-p (third op-entry) (fourth op-entry))
					     args lval allocatedp #'(lambda (arg name allocatedp) (gen-expr-nth arg name allocatedp n)))
				     (error "unknown op ~A" op))))
			      (?var
			       (if (symbolp var)
				   (multiple-value-bind (c-name-or-func type length c-type)
				       (lookup-var var)
				     (if (not (null c-name-or-func))
					 (let ((code (if (stringp c-name-or-func)
							 c-name-or-func
							 (funcall c-name-or-func n))))
					   (values
					    (if (and allocatedp for-compiler-p)
						(format nil "emit_assign(make_lhs(~A), make_compvar_rhs(~A));~%" lval code)
					      (format nil "~A = ~A;~%" lval code))
					    c-type))
				       (error "unknown var ~A" var)))
				 (error "unknown expr ~A" expr)))
			      (t
			       (error "unknown expr ~A" expr))))
			  (gen-condition (expr lval allocatedp)
			    (case-match expr
			      ((and ?a ?b)
			       (if for-compiler-p
				   (format nil "~A~Astart_if_cond(make_compvar_rhs(~A));~%~Aswitch_if_branch();~%end_if_cond();~%"
					   (make-allocated lval allocatedp)
					   (gen-condition a lval t)
					   lval
					   (gen-condition b lval t))
				   (format nil "~Aif (~A)~%{~%~A}~%" (gen-condition a lval t) lval (gen-condition b lval t))))
			      ((or ?a ?b)
			       (if for-compiler-p
				   (format nil "~A~Astart_if_cond(make_compvar_rhs(~A));~%switch_if_branch();~%~Aend_if_cond();~%"
					   (make-allocated lval allocatedp)
					   (gen-condition a lval t)
					   lval
					   (gen-condition b lval t))
				   (format nil "~Aif (!~A)~%{~%~A}~%" (gen-condition a lval t) lval (gen-condition b lval t))))
			      ((not ?x)
			       (if for-compiler-p
				   (format nil "~Aemit_assign(make_lhs(~A), make_op_rhs(OP_NOT, make_compvar_primary(~A)));~%"
					   (gen-condition x lval allocatedp)
					   lval lval)
				   (format nil "~A~A = !~A;~%" (gen-condition x lval allocatedp) lval lval)))
			      ((?op . ?args)
			       (let ((op-entry (lookup-op op (length args) *condops*)))
				 (if (not (null op-entry))
				     (gen-op (if for-compiler-p (third op-entry) (fourth op-entry))
					     args lval allocatedp #'gen-primary)
				     (error "unknown condition op ~A" op))))
			      (t
			       (error "unknown condition ~A"))))
			  (gen-expr (expr lval allocatedp length)
			    (if (or for-compiler-p (not (numberp length)))
				(let ((ctr (make-tmp-name)))
				  (format nil "{~%int ~A;~%for (~A = 0; ~A < ~A; ++~A)~%{~%~A}~%}~%"
					  ctr ctr ctr length ctr
					  (gen-expr-nth expr (funcall lval ctr) allocatedp ctr)))
				(format nil "~{~A~}"
					(mapcar #'(lambda (i) (gen-expr-nth expr (funcall lval i) allocatedp i)) (integers-upto length)))))
			  ;; returns a list which for each let
			  ;; contains: (?decl ?code ?c-name primary
			  ;; nil ?c-type) for primaries and (?decl
			  ;; ?code ?func tuple ?length ?c-type) for
			  ;; tuples
			  (gen-let (name val)
			    (let ((c-name (make-tmp-name)))
			      (multiple-value-bind (type length c-type)
				  (expr-type val)
				(if (eq type 'primary)
				    (list (expr-decl val c-name)
					  (gen-primary val c-name nil)
					  c-name
					  'primary
					  nil
					  c-type)
				    (let ((func #'(lambda (n) (format nil "~A[~A]" c-name n))))
				      (list (expr-decl val c-name)
					    (gen-expr val func nil length)
					    func
					    'tuple
					    length
					    c-type)))))))
		   (case-match stmt
		     ((set result ?rhs)
		      (gen-expr rhs #'(lambda (n) (format nil "result[~A]" n)) t result-length))
		     ((set (nth ?n result) ?rhs)
		      (if (symbolp n)
			  (multiple-value-bind (name type length c-type)
			      (lookup-var n)
			    (assert (eq type 'counter))
			    (gen-primary rhs (format nil "result[~A]" name) t))
			  (progn
			    (assert (integerp n))
			    (gen-primary rhs (format nil "result[~A]" n) t))))
		     ((set ?var ?rhs)
		      (multiple-value-bind (name type length c-type)
			  (lookup-var var)
			(assert (eq type 'primary))
			(gen-primary rhs name t)))
		     ((forarglength ?val ?ctr . ?body)
		      (let ((c-ctr (make-tmp-name)))
			(assert (not (null (arg-pos val))))
			(format nil "{~%int ~A;~%for (~A = 0; ~A < ~A; ++~A)~%{~%~{~A~}}~%}~%"
				c-ctr
				c-ctr c-ctr (length-of-arg-pos (arg-pos val)) c-ctr
				(let ((bindings (cons (list ctr c-ctr 'counter nil) bindings)))
				  (mapcar #'(lambda (s) (gen s bindings)) body)))))
		     ((forget ?primary)
		      (let ((dummy (make-tmp-name)))
			(format nil "{~%~A~%~A}~%"
				(expr-decl primary dummy)
				(gen-primary primary dummy nil))))
		     ((print ?arg)
		      (let ((name (make-tmp-name)))
			(if for-compiler-p
			    (let ((dummy (make-tmp-name)))
			      (format nil "{~%compvar_t *~A, *~A = make_temporary();~%~Aemit_assign(make_lhs(~A), make_op_rhs(OP_PRINT, make_compvar_primary(~A)));~%}~%"
				      name dummy
				      (gen-primary arg name nil)
				      dummy name))
			    (format nil "{~%~A~%~APRINT_FLOAT(~A);~%}~%"
				    (expr-decl arg name)
				    (gen-primary arg name nil)
				    name))))
		     ((newline)
		      (if for-compiler-p
			  (let ((dummy (make-tmp-name)))
			    (format nil "{~%compvar_t *~A = make_temporary();~%emit_assign(make_lhs(~A), make_op_rhs(OP_NEWLINE));~%}~%"
				    dummy
				    dummy))
			  (format nil "NEWLINE();~%")))
		     ((progn . ?body)
		      (reduce #'string-concat (mapcar #'(lambda (s) (gen s bindings)) body)))
		     ((if ?condition ?consequent ?alternative)
		      (let ((condition-name (make-tmp-name)))
			(if for-compiler-p
			    (format nil "{~%~A~%~Astart_if_cond(make_compvar_rhs(~A));~%~Aswitch_if_branch();~Aend_if_cond();~%}~%"
				    (expr-decl condition condition-name) (gen-condition condition condition-name nil)
				    condition-name
				    (gen consequent bindings)
				    (gen alternative bindings))
			    (format nil "{~%~A~%~Aif (~A)~%{~%~A}~%else~%{~%~A}~%}~%"
				    (expr-decl condition condition-name)
				    (gen-condition condition condition-name nil)
				    condition-name
				    (gen consequent bindings)
				    (gen alternative bindings)))))
		     ((let ?lets . ?body)
		      (let ((lets (mapcar #'(lambda (l) (cons (car l) (gen-let (car l) (cadr l)))) lets)))
			(format nil "{~%~{~A~%~}~%~{~A~%~}~{~A~}}~%"
				(mapcar #'second lets)
				(mapcar #'third lets)
				(let ((bindings (append (mapcar #'(lambda (l) (list (first l) (fourth l) (fifth l) (sixth l) (seventh l))) lets)
							bindings)))
				  (mapcar #'(lambda (s) (gen s bindings)) body)))))))))
	(if for-compiler-p
	    (format t "static void~%gen_~A (compvar_t ***args, int *arglengths, int *argnumbers, compvar_t **result)~%{~%" (dcs name))
	    (format t "static void~%builtin_~A (mathmap_invocation_t *invocation, postfix_arg_t *arg)~%{~%float result[MAX_TUPLE_LENGTH];~%" (dcs name)))
	(dolist (stmt body)
	  (princ (gen stmt nil)))
	(if for-compiler-p
	    (format t "}~%~%")
	    (format t "{~%int i;~%for (i = 0; i < ~A; ++i)~%STK[STKP - ~A].v.tuple.data[i] = result[i];~%}~%STK[STKP - ~A].v.tuple.length = ~A;~%STKP -= ~A;~%}~%~%"
		    result-length (length args)
		    (length args) result-length
		    (1- (length args))))))))

(defbuiltin "merd" merd (?T ?L) ((val (?T ?L)))
  (let ((dummy1 (start-debug-tuple (argtag val))))
    (forarglength val i
      (let ((dummy2 (set-debug-tuple-data i (nth i val))))
	(set (nth i result) (nth i val))))))

(defbuiltin "print" print (nil 1) ((val (_ _)))
  (forarglength val i
    (print (nth i val)))
  (newline)
  (set result (make (nil 1) 0)))

(defbuiltin "__add" add_ri (ri 2) ((a (ri 2)) (b (ri 2)))
  (set result (+v a b)))

(defbuiltin "__add" add_ri_1 (ri 2) ((a (ri 2)) (b (_ 1)))
  (set result (+v a (make (ri 2) (nth 0 b) 0))))

(defbuiltin "__add" add_1_ri (ri 2) ((a (_ 1)) (b (ri 2)))
  (set result (+v b (make (ri 2) (nth 0 a) 0))))

(defbuiltin "__add" add_1 (?T 1) ((a (?T 1)) (b (?T 1)))
  (set result (+v a b)))

(defbuiltin "__add" add_s (?T ?L) ((a (?T ?L)) (b (? 1)))
  (set result (+v a (splat (T L) (nth 0 b)))))

(defbuiltin "__add" add_n (?T ?L) ((a (?T ?L)) (b (?T ?L)))
  (set result (+v a b)))

(defbuiltin "__sub" sub_ri (ri 2) ((a (ri 2)) (b (ri 2)))
  (set result (-v a b)))

(defbuiltin "__sub" sub_ri_1 (ri 2) ((a (ri 2)) (b (? 1)))
  (set result (-v a (make (ri 2) (nth 0 b) 0))))

(defbuiltin "__sub" sub_1_ri (ri 2) ((a (? 1)) (b (ri 2)))
  (set result (-v (make (ri 2) (nth 0 a) 0) b)))

(defbuiltin "__sub" sub_1 (?T 1) ((a (?T 1)) (b (?T 1)))
  (set result (-v a b)))

(defbuiltin "__sub" sub_s (?T ?L) ((a (?T ?L)) (b (? 1)))
  (set result (-v a (splat (?T ?L) (nth 0 b)))))

(defbuiltin "__sub" sub_n (?T ?L) ((a (?T ?L)) (b (?T ?L)))
  (set result (-v a b)))

(defbuiltin "__neg" neg (?T ?L) ((x (?T ?L)))
  (set result (-v x)))

(defbuiltin "__mul" mul_ri (ri 2) ((a (ri 2)) (b (ri 2)))
  (set result (make (ri 2)
		    (- (* (nth 0 a) (nth 0 b)) (* (nth 1 a) (nth 1 b)))
		    (+ (* (nth 0 a) (nth 1 b)) (* (nth 0 b) (nth 1 a))))))

(defbuiltin "__mul" mul_1_ri (ri 2) ((a (? 1)) (b (ri 2)))
  (set result (*v (splat (ri 2) (nth 0 a)) b)))

(defun matmul (n)
  (mappend #'(lambda (i)
	       (map-times n #'(lambda (j)
				`(+ ,@(map-times n #'(lambda (k)
						       `(* (nth ,(+ (* i n) k) a) (nth ,(+ (* k n) j) b))))))))
	   (integers-upto n)))

(defbuiltin "__mul" mul_m2x2 (m2x2 4) ((a (m2x2 4)) (b (m2x2 4)))
  (set result (eval `(make (m2x2 4) ,@(matmul 2)))))

(defbuiltin "__mul" mul_m3x3 (m3x3 9) ((a (m3x3 9)) (b (m3x3 9)))
  (set result (eval `(make (m3x3 9) ,@(matmul 3)))))

(defun vecmatmul (n)
  (map-times n #'(lambda (i)
		   `(+ ,@(map-times n #'(lambda (j)
					  `(* (nth ,j a) (nth ,(+ i (* n j)) b))))))))

(defbuiltin "__mul" mul_v2m2x2 (v2 2) ((a (v2 2)) (b (m2x2 4)))
  (set result (eval `(make (v2 2) ,@(vecmatmul 2)))))

(defbuiltin "__mul" mul_v3m3x3 (v3 3) ((a (v3 3)) (b (m3x3 9)))
  (set result (eval `(make (v3 3) ,@(vecmatmul 3)))))

(defun matvecmul (n)
  (map-times n #'(lambda (i)
		   `(+ ,@(map-times n #'(lambda (j)
					  `(* (nth ,(+ j (* n i)) a) (nth ,j b))))))))

(defbuiltin "__mul" mul_m2x2v2 (v2 2) ((a (m2x2 4)) (b (v2 2)))
  (set result (eval `(make (v2 2) ,@(matvecmul 2)))))

(defbuiltin "__mul" mul_m3x3v3 (v3 3) ((a (m3x3 9)) (b (v3 3)))
  (set result (eval `(make (v3 3) ,@(matvecmul 3)))))

(defbuiltin "__mul" mul_1 (?T 1) ((a (?T 1)) (b (?T 1)))
  (set result (make (?T 1) (* (nth 0 a) (nth 0 b)))))

(defbuiltin "__mul" mul_s (?T ?L) ((a (?T ?L)) (b (? 1)))
  (set result (*v a (splat (?T ?L) (nth 0 b)))))

(defbuiltin "__mul" mul_n (?T ?L) ((a (?T ?L)) (b (?T ?L)))
  (set result (*v a b)))

(defbuiltin "__div" div_ri (ri 2) ((a (ri 2)) (b (ri 2)))
  (if (and (= (nth 0 b) 0) (= (nth 1 b) 0))
      (set result (make (ri 2) 0 0))
      (let ((c (sum (*v b b))))
	(set result (make (ri 2)
			  (/ (sum (*v a b)) c)
			  (/ (+ (* (- (nth 0 a)) (nth 1 b))
				(* (nth 0 b) (nth 1 a)))
			     c))))))

(defbuiltin "__div" div_1_ri (ri 2) ((a (?T 1)) (b (ri 2)))
  (let ((tmp (sum (*v b b))))
    (if (= tmp 0)
	(set result (make (ri 2) 0 0))
	(set result (make (ri 2)
			  (/ (* (nth 0 a) (nth 0 b)) tmp)
			  (- (/ (* (nth 0 a) (nth 1 b)) tmp)))))))

(defbuiltin "__div" div_v2m2x2 (v2 2) ((a (? 2)) (b (m2x2 4)))
  (let ((m (make-m2x2 (nth 0 b) (nth 1 b) (nth 2 b) (nth 3 b)))
	(v (make-v2 (nth 0 a) (nth 1 a))))
    (let ((r (solve-linear-2 m v)))
      (set result (make (v2 2) (vector-nth 0 r) (vector-nth 1 r))))))

(defbuiltin "__div" div_v3m3x3 (v3 3) ((a (? 3)) (b (m3x3 9)))
  (let ((m (make-m3x3 (nth 0 b) (nth 1 b) (nth 2 b)
		      (nth 3 b) (nth 4 b) (nth 5 b)
		      (nth 6 b) (nth 7 b) (nth 8 b)))
	(v (make-v3 (nth 0 a) (nth 1 a) (nth 2 a))))
    (let ((r (solve-linear-3 m v)))
      (set result (make (v3 3) (vector-nth 0 r) (vector-nth 1 r) (vector-nth 2 r)))
      (forget (free-matrix m)))))

(defbuiltin "__div" div_1 (?T 1) ((a (?T 1)) (b (?T 1)))
  (if (= (nth 0 b) 0)
      (set result (make (?T 1) 0))
      (set result (/v a b))))

(defbuiltin "__div" div_s (?T ?L) ((a (?T ?L)) (b (? 1)))
  (if (= (nth 0 b) 0)
      (set result (splat (?T ?L) 0))
      (set result (/v a (splat (?T ?L) (nth 0 b))))))

(defbuiltin "__div" div_n (?T ?L) ((a (?T ?L)) (b (?T ?L)))
  (forarglength b i
    (if (= (nth i b) 0)
	(set (nth i result) 0)
	(set (nth i result) (/ (nth i a) (nth i b))))))

(defbuiltin "__mod" mod_1 (?T 1) ((a (?T 1)) (b (?T 1)))
  (if (= (nth 0 b) 0)
      (set result (make (?T 1) 0))
      (set result (%v a b))))

(defbuiltin "__mod" mod_s (?T ?L) ((a (?T ?L)) (b (? 1)))
  (if (= (nth 0 b) 0)
      (set result (splat (?T ?L) 0))
      (set result (%v a (splat (?T ?L) (nth 0 b))))))

(defbuiltin "__mod" mod_n (?T ?L) ((a (?T ?L)) (b (?T ?L)))
  (forarglength b i
    (if (= (nth i b) 0)
	(set (nth i result) 0)
	(set (nth i result) (% (nth i a) (nth i b))))))

(defbuiltin "pmod" pmod (?T 1) ((a (?T 1)) (b (?T 1)))
  (let ((mod (% (nth 0 a) (nth 0 b))))
    (if (< (nth 0 a) 0)
	(set result (make (?T 1) (+ mod (nth 0 b))))
	(set result (make (?T 1) mod)))))

(defbuiltin "sqrt" sqrt_ri (ri 2) ((a (ri 2)))
  (let ((c (c-sqrt (complex (nth 0 a) (nth 1 a)))))
    (set result (make (ri 2) (c-real c) (c-imag c)))))

(defbuiltin "sqrt" sqrt_1 (?T 1) ((a (?T 1)))
  (set result (make (?T 1) (sqrt (nth 0 a)))))

(defbuiltin "sum" sum (?T 1) ((a (?T ?L)))
  (set result (make (?T 1) (sum a))))

;;; vector functions

(defbuiltin "dotp" dotp (nil 1) ((a (?T ?L)) (b (?T ?L)))
  (set result (make (nil 1) (sum (*v a b)))))

(defbuiltin "crossp" crossp (?T 3) ((a (?T 3)) (b (?T 3)))
  (set result (make (?T 3)
		    (- (* (nth 1 a) (nth 2 b))
		       (* (nth 2 a) (nth 1 b)))
		    (- (* (nth 2 a) (nth 0 b))
		       (* (nth 0 a) (nth 2 b)))
		    (- (* (nth 0 a) (nth 1 b))
		       (* (nth 1 a) (nth 0 b))))))

(defbuiltin "det" det_m2x2 (nil 1) ((a (m2x2 4)))
  (set result (make (nil 1) (- (* (nth 0 a) (nth 3 a)) (* (nth 1 a) (nth 2 a))))))

(defbuiltin "det" det_m3x3 (nil 1) ((a (m3x3 9)))
  (set result (make (nil 1) (- (+ (* (nth 0 a) (nth 4 a) (nth 8 a))
				  (* (nth 1 a) (nth 5 a) (nth 6 a))
				  (* (nth 2 a) (nth 3 a) (nth 7 a)))
			       (+ (* (nth 2 a) (nth 4 a) (nth 6 a))
				  (* (nth 0 a) (nth 5 a) (nth 7 a))
				  (* (nth 1 a) (nth 3 a) (nth 8 a)))))))

(defbuiltin "normalize" normalize (?T ?L) ((a (?T ?L)))
  (let ((l (sum (*v a a))))
    (if (= l 0)
	(set result (splat (?T ?L) 0))
	(set result (/v a (splat (?T ?L) (sqrt l)))))))

(defbuiltin "abs" abs_ri (nil 1) ((a (ri 2)))
  (set result (make (nil 1) (hypot (nth 0 a) (nth 1 a)))))

(defbuiltin "abs" abs_1 (?T 1) ((a (?T 1)))
  (set result (abs-v a)))

(defbuiltin "abs" abs_n (?T ?L) ((a (?T ?L)))
  (set result (abs-v a)))

;;; trigonometry

(defbuiltin "deg2rad" deg2rad (nil 1) ((a (? 1)))
  (set result (make (nil 1) (* (nth 0 a) 0.017453292519943295722))))
                           
(defbuiltin "rad2deg" rad2deg (deg 1) ((a (? 1)))
  (set result (make (deg 1) (* (nth 0 a) 57.2957795130823208768))))

(defbuiltin "sin" sin_ri (ri 2) ((a (ri 2)))
  (let ((c (c-sin (complex (nth 0 a) (nth 1 a)))))
    (set result (make (ri 2) (c-real c) (c-imag c)))))

(defbuiltin "sin" sin (?T 1) ((a (?T 1)))
  (set result (make (?T 1) (sin (nth 0 a)))))

(defbuiltin "cos" cos_ri (ri 2) ((a (ri 2)))
  (let ((c (c-cos (complex (nth 0 a) (nth 1 a)))))
    (set result (make (ri 2) (c-real c) (c-imag c)))))

(defbuiltin "cos" cos (?T 1) ((a (?T 1)))
  (set result (make (?T 1) (cos (nth 0 a)))))

(defbuiltin "tan" tan_ri (ri 2) ((a (ri 2)))
  (let ((c (c-tan (complex (nth 0 a) (nth 1 a)))))
    (set result (make (ri 2) (c-real c) (c-imag c)))))

(defbuiltin "tan" tan (?T 1) ((a (?T 1)))
  (set result (make (?T 1) (tan (nth 0 a)))))

(defbuiltin "asin" asin_ri (ri 2) ((a (ri 2)))
  (let ((c (c-asin (complex (nth 0 a) (nth 1 a)))))
    (set result (make (ri 2) (c-real c) (c-imag c)))))

(defbuiltin "asin" asin (?T 1) ((a (?T 1)))
  (if (or (< (nth 0 a) -1) (< 1 (nth 0 a)))
      (set result (make (?T 1) 0))
      (set result (make (?T 1) (asin (nth 0 a))))))

(defbuiltin "acos" acos_ri (ri 2) ((a (ri 2)))
  (let ((c (c-acos (complex (nth 0 a) (nth 1 a)))))
    (set result (make (ri 2) (c-real c) (c-imag c)))))

(defbuiltin "acos" acos (?T 1) ((a (?T 1)))
  (if (or (< (nth 0 a) -1) (< 1 (nth 0 a)))
      (set result (make (?T 1) 0))
      (set result (make (?T 1) (acos (nth 0 a))))))

(defbuiltin "atan" atan_ri (ri 2) ((a (ri 2)))
  (let ((c (c-atan (complex (nth 0 a) (nth 1 a)))))
    (set result (make (ri 2) (c-real c) (c-imag c)))))

(defbuiltin "atan" atan (?T 1) ((a (?T 1)))
  (set result (make (?T 1) (atan (nth 0 a)))))

(defbuiltin "atan" atan2 (?T 1) ((a (?T 1)) (b (?T 1)))
  (set result (make (?T 1) (atan (nth 0 a) (nth 0 b)))))

;;; exp and friends

(defbuiltin "__pow" pow_ri_1 (ri 2) ((a (ri 2)) (b (?T 1)))
  (let ((c (c-pow (complex (nth 0 a) (nth 1 a)) (complex (nth 0 b) 0.0))))
    (set result (make (ri 2) (c-real c) (c-imag c)))))

(defbuiltin "__pow" pow_ri (ri 2) ((a (ri 2)) (b (ri 2)))
  (let ((c (c-pow (complex (nth 0 a) (nth 1 a)) (complex (nth 0 b) (nth 1 b)))))
    (set result (make (ri 2) (c-real c) (c-imag c)))))

(defbuiltin "__pow" pow_1_ri (ri 2) ((a (?T 1)) (b (ri 2)))
  (let ((c (c-pow (complex (nth 0 a) 0.0) (complex (nth 0 b) (nth 1 b)))))
    (set result (make (ri 2) (c-real c) (c-imag c)))))

(defbuiltin "__pow" pow_1 (?T 1) ((a (?T 1)) (b (?T 1)))
  (if (and (<= (nth 0 b) 0) (= (nth 0 a) 0))
      (set result (make (?T 1) 0))
      (set result (make (?T 1) (pow (nth 0 a) (nth 0 b))))))

(defbuiltin "__pow" pow_s (?T ?L) ((a (?T ?L)) (b (? 1)))
  (forarglength a i
    (if (and (<= (nth 0 b) 0) (= (nth i a) 0))
	(set (nth i result) 0)
	(set (nth i result) (pow (nth i a) (nth 0 b))))))

(defbuiltin "exp" exp_ri (ri 2) ((a (ri 2)))
  (let ((c (c-exp (complex (nth 0 a) (nth 1 a)))))
    (set result (make (ri 2) (c-real c) (c-imag c)))))

(defbuiltin "exp" exp_1 (?T 1) ((a (?T 1)))
  (set result (make (?T 1) (exp (nth 0 a)))))

(defbuiltin "log" log_ri (ri 2) ((a (ri 2)))
  (let ((c (c-log (complex (nth 0 a) (nth 1 a)))))
    (set result (make (ri 2) (c-real c) (c-imag c)))))

(defbuiltin "log" log_1 (?T 1) ((a (?T 1)))
  (if (<= (nth 0 a) 0)
      (set result (make (?T 1) 0))
      (set result (make (?T 1) (log (nth 0 a))))))

;;; complex

(defbuiltin "arg" arg_ri (nil 1) ((a (ri 2)))
  (set result (make (nil 1) (c-arg (complex (nth 0 a) (nth 1 a))))))

(defbuiltin "conj" conj_ri (ri 2) ((a (ri 2)))
  (set result (make (ri 2) (nth 0 a) (- (nth 1 a)))))

;;; hyperbolic

(defbuiltin "sinh" sinh_ri (ri 2) ((a (ri 2)))
  (let ((c (c-sinh (complex (nth 0 a) (nth 1 a)))))
    (set result (make (ri 2) (c-real c) (c-imag c)))))

(defbuiltin "sinh" sinh_1 (?T 1) ((a (?T 1)))
  (set result (make (?T 1) (sinh (nth 0 a)))))

(defbuiltin "cosh" cosh_ri (ri 2) ((a (ri 2)))
  (let ((c (c-cosh (complex (nth 0 a) (nth 1 a)))))
    (set result (make (ri 2) (c-real c) (c-imag c)))))

(defbuiltin "cosh" cosh_1 (?T 1) ((a (?T 1)))
  (set result (make (?T 1) (cosh (nth 0 a)))))

(defbuiltin "tanh" tanh_ri (ri 2) ((a (ri 2)))
  (let ((c (c-tanh (complex (nth 0 a) (nth 1 a)))))
    (set result (make (ri 2) (c-real c) (c-imag c)))))

(defbuiltin "tanh" tanh_1 (?T 1) ((a (?T 1)))
  (set result (make (?T 1) (tanh (nth 0 a)))))

(defbuiltin "asinh" asinh_ri (ri 2) ((a (ri 2)))
  (let ((c (c-asinh (complex (nth 0 a) (nth 1 a)))))
    (set result (make (ri 2) (c-real c) (c-imag c)))))

(defbuiltin "asinh" asinh_1 (?T 1) ((a (?T 1)))
  (set result (make (?T 1) (asinh (nth 0 a)))))

(defbuiltin "acosh" acosh_ri (ri 2) ((a (ri 2)))
  (let ((c (c-acosh (complex (nth 0 a) (nth 1 a)))))
    (set result (make (ri 2) (c-real c) (c-imag c)))))

(defbuiltin "acosh" acosh_1 (?T 1) ((a (?T 1)))
  (set result (make (?T 1) (acosh (nth 0 a)))))

(defbuiltin "atanh" atanh_ri (ri 2) ((a (ri 2)))
  (let ((c (c-atanh (complex (nth 0 a) (nth 1 a)))))
    (set result (make (ri 2) (c-real c) (c-imag c)))))

(defbuiltin "atanh" atanh_1 (?T 1) ((a (?T 1)))
  (set result (make (?T 1) (atanh (nth 0 a)))))

(defbuiltin "gamma" gamma_ri (ri 2) ((a (ri 2)))
  (let ((c (c-gamma (complex (nth 0 a) (nth 1 a)))))
    (set result (make (ri 2) (c-real c) (c-imag c)))))

(defbuiltin "gamma" gamma_1 (?T 1) ((a (?T 1)))
  (if (< (nth 0 a) 0)
      (set result (make (?T 1) 0))
      (set result (make (?T 1) (gamma (nth 0 a))))))

;;; elliptic

(def-simple-builtin "ell_int_Kcomp" ell_int_Kcomp ell-int-k-comp 1)
(def-simple-builtin "ell_int_Ecomp" ell_int_Ecomp ell-int-e-comp 1)

(def-simple-builtin "ell_int_F" ell_int_F ell-int-f 2)
(def-simple-builtin "ell_int_E" ell_int_E ell-int-e 2)
(def-simple-builtin "ell_int_P" ell_int_P ell-int-p 3)
(def-simple-builtin "ell_int_D" ell_int_D ell-int-d 3)

(def-simple-builtin "ell_int_RC" ell_int_RC ell-int-rc 2)
(def-simple-builtin "ell_int_RD" ell_int_RD ell-int-rd 3)
(def-simple-builtin "ell_int_RF" ell_int_RF ell-int-rf 3)
(def-simple-builtin "ell_int_RJ" ell_int_RJ ell-int-rj 4)

(defbuiltin "ell_jac" ell_jac (?T 3) ((u (?T 1)) (m (?T 1)))
  (let ((v (ell-jac (nth 0 u) (nth 0 m))))
    (set result (make (?T 3) (vector-nth 0 v) (vector-nth 1 v) (vector-nth 2 v)))))

;;; floor and friends

(defbuiltin "floor" floor (?T 1) ((a (?T 1)))
  (set result (make (?T 1) (floor (nth 0 a)))))

(defbuiltin "sign" sign_n (?T ?L) ((a (?T ?L)))
  (forarglength a i
    (if (< (nth i a) 0)
	(set (nth i result) -1)
	(if (< 0 (nth i a))
	    (set (nth i result) 1)
	    (set (nth i result) 0)))))

(defbuiltin "min" min_n (?T ?L) ((a (?T ?L)) (b (?T ?L)))
  (forarglength a i
    (if (< (nth i a) (nth i b))
	(set (nth i result) (nth i a))
	(set (nth i result) (nth i b)))))

(defbuiltin "max" max_n (?T ?L) ((a (?T ?L)) (b (?T ?L)))
  (forarglength a i
    (if (< (nth i a) (nth i b))
	(set (nth i result) (nth i b))
	(set (nth i result) (nth i a)))))

(defbuiltin "clamp" clamp (?T ?L) ((a (?T ?L)) (l (?T ?L)) (u (?T ?L)))
  (forarglength a i
    (if (< (nth i a) (nth i l))
	(set (nth i result) (nth i l))
	(if (< (nth i u) (nth i a))
	    (set (nth i result) (nth i u))
	    (set (nth i result) (nth i a))))))

(defbuiltin "lerp" lerp_1 (?T ?L) ((p (? 1)) (a (?T ?L)) (b (?T ?L)))
  (let ((l (- 1 (nth 0 p))))
    (forarglength a i
      (set (nth i result) (+ (* l (nth i a))
			     (* (nth 0 p) (nth i b)))))))

(defbuiltin "lerp" lerp_n (?T ?L) ((p (?T ?L)) (a (?T ?L)) (b (?T ?L)))
  (forarglength a i
    (set (nth i result) (+ (* (- 1 (nth i p)) (nth i a))
			   (* (nth i p) (nth i b))))))

(defbuiltin "scale" scale (?T ?L) ((a (?T ?L)) (fl (?T ?L)) (fu (?T ?L)) (tl (?T ?L)) (tu (?T ?L)))
  (forarglength a i
    (let ((div (- (nth i fu) (nth i fl))))
      (if (= div 0)
	(set (nth i result) 0)
	(set (nth i result) (+ (* (/ (- (nth i a) (nth i fl)) div)
				  (- (nth i tu) (nth i tl)))
			       (nth i tl)))))))

;;; polynomials

(defbuiltin "solve" solve-poly-2 (nil 2) ((p (poly 3)))
  (let ((v (solve-poly-2 (nth 0 p) (nth 1 p) (nth 2 p))))
    (set result (make (nil 2) (vector-nth 0 v) (vector-nth 1 v)))))

(defbuiltin "solve" solve-poly-3 (nil 3) ((p (poly 4)))
  (let ((v (solve-poly-3 (nth 0 p) (nth 1 p) (nth 2 p) (nth 3 p))))
    (set result (make (nil 3) (vector-nth 0 v) (vector-nth 1 v) (vector-nth 2 v)))))

;;; logic

(defbuiltin "__not" not (?T 1) ((a (?T 1)))
  (if (= (nth 0 a) 0)
      (set result (make (?T 1) 1))
      (set result (make (?T 1) 0))))

(defbuiltin "__or" or (?T 1) ((a (?T 1)) (b (?T 1)))
  (if (and (= (nth 0 a) 0) (= (nth 0 b) 0))
      (set result (make (?T 1) 0))
      (set result (make (?T 1) 1))))

(defbuiltin "__and" and (?T 1) ((a (?T 1)) (b (?T 1)))
  (if (or (= (nth 0 a) 0) (= (nth 0 b) 0))
      (set result (make (?T 1) 0))
      (set result (make (?T 1) 1))))

(defbuiltin "__xor" xor (?T 1) ((a (?T 1)) (b (?T 1)))
  (if (or (and (not (= (nth 0 a) 0)) (= (nth 0 b) 0))
	  (and (not (= (nth 0 b) 0)) (= (nth 0 a) 0)))
      (set result (make (?T 1) 1))
      (set result (make (?T 1) 0))))

;;; comparison

(defbuiltin "__equal" equal (?T 1) ((a (?T 1)) (b (?T 1)))
  (set result (make (?T 1) (= (nth 0 a) (nth 0 b)))))

(defbuiltin "__less" less (?T 1) ((a (?T 1)) (b (?T 1)))
  (set result (make (?T 1) (< (nth 0 a) (nth 0 b)))))

(defbuiltin "__greater" greater (?T 1) ((a (?T 1)) (b (?T 1)))
  (set result (make (?T 1) (< (nth 0 b) (nth 0 a)))))

(defbuiltin "__lessequal" lessequal (?T 1) ((a (?T 1)) (b (?T 1)))
  (set result (make (?T 1) (<= (nth 0 a) (nth 0 b)))))

(defbuiltin "__greaterequal" greaterequal (?T 1) ((a (?T 1)) (b (?T 1)))
  (set result (make (?T 1) (<= (nth 0 b) (nth 0 a)))))

(defbuiltin "__notequal" notequal (?T 1) ((a (?T 1)) (b (?T 1)))
  (set result (make (?T 1) (not (= (nth 0 a) (nth 0 b))))))

(defbuiltin "inintv" inintv (?T 1) ((a (?T 1)) (l (?T 1)) (u (?T 1)))
  (if (and (<= (nth 0 l) (nth 0 a))
	   (<= (nth 0 a) (nth 0 u)))
      (set result (make (?T 1) 1))
      (set result (make (?T 1) 0))))

(defbuiltin "origVal" origValXY (rgba 4) ((p (xy 2)) (frame (nil 1)) (drawable (image 1)))
  (let ((c (orig-val (nth 0 p) (nth 1 p) (nth 0 drawable) (nth 0 frame))))
    (set result (make (rgba 4) (red c) (green c) (blue c) (alpha c)))))

;;; colors

(defbuiltin "gray" gray (nil 1) ((c (rgba 4)))
  (set result (make (nil 1) (+ (* 0.299 (nth 0 c)) (* 0.587 (nth 1 c)) (* 0.114 (nth 2 c))))))

(defbuiltin "toHSVA" toHSVA (hsva 4) ((a (rgba 4)))
  (let ((r (max 0 (min 1 (nth 0 a))))
	(g (max 0 (min 1 (nth 1 a))))
	(b (max 0 (min 1 (nth 2 a)))))
    (set (nth 3 result) (max 0 (min 1 (nth 3 a))))
    (let ((max (max r (max g b)))
	  (min (min r (min g b))))
      (set (nth 2 result) max)
      (if (= max 0)
	  (progn
	    (set (nth 0 result) 0)	;actually undefined
	    (set (nth 1 result) 0))
	  (let ((delta (- max min))
		(h 0))
	    (set (nth 1 result) (/ delta max))
	    (if (= r max)
		(set h (/ (- g b) delta))
		(if (= g max)
		    (set h (+ 2 (/ (- b r) delta)))
		    (set h (+ 4 (/ (- r g) delta)))))
	    (set h (/ h 6.0))
	    (if (< h 0)
		(set (nth 0 result) (+ h 1))
		(set (nth 0 result) h)))))))

(defbuiltin "toRGBA" toRGBA (rgba 4) ((a (hsva 4)))
  (let ((s (max 0 (min 1 (nth 1 a))))
	(v (max 0 (min 1 (nth 2 a)))))
    (set (nth 3 result) (max 0 (min 1 (nth 3 a))))
    (if (= s 0)
	(progn
	  (set (nth 0 result) v)
	  (set (nth 1 result) v)
	  (set (nth 2 result) v))
	(let ((h (max 0 (nth 0 a))))
	  (if (<= 1 h)
	      (set h 0)
	      (set h (* h 6)))
	  (let ((i (floor h)))
	    (let ((f (- h i)))
	      (let ((p (* v (- 1 s)))
		    (q (* v (- 1 (* s f))))
		    (t (* v (- 1 (* s (- 1 f))))))
		(if (= i 0)
		    (progn
		      (set (nth 0 result) v)
		      (set (nth 1 result) t)
		      (set (nth 2 result) p))
		    (if (= i 1)
			(progn
			  (set (nth 0 result) q)
			  (set (nth 1 result) v)
			  (set (nth 2 result) p))
			(if (= i 2)
			    (progn
			      (set (nth 0 result) p)
			      (set (nth 1 result) v)
			      (set (nth 2 result) t))
			    (if (= i 3)
				(progn
				  (set (nth 0 result) p)
				  (set (nth 1 result) q)
				  (set (nth 2 result) v))
				(if (= i 4)
				    (progn
				      (set (nth 0 result) t)
				      (set (nth 1 result) p)
				      (set (nth 2 result) v))
				    (progn
				      (set (nth 0 result) v)
				      (set (nth 1 result) p)
				      (set (nth 2 result) q))))))))))))))

(defbuiltin "toXY" toXY (xy 2) ((a (ra 2)))
  (set result (make (xy 2)
		    (* (cos (nth 1 a)) (nth 0 a))
		    (* (sin (nth 1 a)) (nth 0 a)))))

(defbuiltin "toRA" toRA (ra 2) ((arg (xy 2)))
  (let ((r (hypot (nth 0 arg) (nth 1 arg))))
    (if (= r 0)
	(set result (make (ra 2) 0 0))
	(let ((a (acos (/ (nth 0 arg) r))))
	  (set (nth 0 result) r)
	  (if (< (nth 1 arg) 0)
	      (set (nth 1 result) (- (* 2 pi) a))
	      (set (nth 1 result) a))))))

(defbuiltin "rand" rand (?T 1) ((a (?T 1)) (b (?T 1)))
  (set result (make (?T 1) (rand (nth 0 a) (nth 0 b)))))

(defbuiltin "noise" noise (nil 1) ((a (? 3)))
  (set result (make (nil 1) (noise (nth 0 a) (nth 1 a) (nth 2 a)))))

;;; FIXME: rand

(defun type-string (type args)
  (labels ((str (s)
	     (cond ((eq s '?)
		    "_")
		   ((var-symbol-p s)
		    (format nil "~A" (subseq (symbol-name s) 1)))
		   ((symbolp s)
		    (dcs s))
		   (t
		    (format nil "~A" s)))))
    (format nil "((~A ~A)~{ (~A ~A)~})"
	    (str (car type)) (str (cadr type))
	    (mappend #'(lambda (a) (list (str (caadr a)) (str (cadadr a)))) args))))

(with-open-file (out "new_builtins.c" :direction :output :if-exists :supersede)
  (let ((*standard-output* out))
    (dolist (b (reverse *builtins*))
      (gen-builtin (first b) (second b) (third b) (fourth b) (fifth b) nil)
      (gen-builtin (first b) (second b) (third b) (fourth b) (fifth b) t))
    (format t "~%void~%init_builtins (void)~%{~%")
    (dolist (b (reverse *builtins*))
      (format t "register_overloaded_builtin(\"~A\", \"~A\", builtin_~A, gen_~A);~%"
	      (first b) (type-string (third b) (fourth b)) (dcs (second b)) (dcs (second b))))
    (format t "}~%")))

(make-ops-file)
