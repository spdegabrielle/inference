#lang info
(define name "Inference")
(define blurb
  (list "Implements a rule-based inference engine that supports both "
        "forward (data-driven) and backward (goal-driven) chaining."))
(define release-notes
  (list "Added query*-values and query*-values* as query* versions of "
        "query-values and query-values*. Also changed the printing of "
        "assertions to use the error print limits."))
(define categories '(scientific))
(define scribblings '(("scribblings/inference.scrbl" (multi-page))))
(define primary-file "inference.ss")
(define repositories (list "4.x"))
