(defsimplify
    (make-color (red ?x) (green ?x) (blue ?x) (alpha ?x))
    x)

(defsimplify
    (+ ?x 0)
    x)

(defsimplify
    (- ?x 0)
    x)

(defsimplify
    (- ?x ?x)
    0)

(defsimplify
    (* ?x 1)
    x)

(defsimplify
    (/ ?x 1)
    x)
