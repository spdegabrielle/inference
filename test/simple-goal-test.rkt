#lang racket
;;; PLT Scheme Inference Collection
;;; simple-goal-test.rkt

(require "../inference.rkt")

(define-ruleset simple-goal-test-rules)

(define-rule (rule-1 simple-goal-test-rules)
    (overworked ?x)
  <==
    (lecturing ?x)
    (marking-practicals ?x))

(define-rule (rule-2 simple-goal-test-rules)
    (lecturing alison)
  <==
    (month february))

(define-rule (rule-3 simple-goal-test-rules)
    (marking-practicals ?x)
  <==
    (month february))

(define-rule (rule-4 simple-goal-test-rules)
    (bad-mood ?x)
  <==
    (overworked ?x))

(define-rule (rule-5 simple-goal-test-rules)
    (bad-mood ?x)
  <==
    (slept-badly ?x))

(define-rule (rule-6 simple-goal-test-rules)
    (weather cold)
  <==
    (month february))

(define-rule (rule-7 simple-goal-test-rules)
    (economy bad)
  <==
    (year 1993))

(define (solve-simple-goal-test)
  (with-new-inference-environment
   (activate simple-goal-test-rules)
   (current-inference-trace #t)
   (assert '(month february))
   (assert '(year 1993))
   (let ((result (check '(bad-mood alison))))
     (printf "(check '(bad-mood alison)) = ~a ~n" result)
     (if (not (null? result))
         (printf "Alison is in a bad mood.~n")
         (printf "Alison is not (known to be) in a bad mood.~n")))))

(solve-simple-goal-test)
