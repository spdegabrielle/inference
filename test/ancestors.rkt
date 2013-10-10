#lang scheme/base
;;; PLT Scheme Inference Collection
;;; ancestors.ss
;;;
;;; Ancestors is a trivial knowledge-based system that determines
;;; a given person's ancestors based on asserted parent relationships.

(require "../inference.rkt")

(define-ruleset ancestors-ruleset)

(define-rule (initialize ancestors-ruleset)
    (?start <- (start))
  ==>
    (retract ?start)
    (printf "Please enter the first name of a~n")
    (printf "person whose ancestors you would~n")
    (printf "like to find:~n")
    (assert `(request ,(read))))
  
(define-rule (print-ancestors ancestors-ruleset)
    (?request <- (request ?name))
    (parents ?name ?mother ?father)
  ==>
    (retract ?request)
    (when ?mother
      (printf "~a is an ancestor via ~a~n" ?mother ?name)
      (assert `(request ,?mother)))
    (when ?father
      (printf "~a is an ancestor via ~a~n" ?father ?name)
      (assert `(request ,?father))))

(define-rule (remove-request ancestors-ruleset #:priority -100)
    (?request <- (request ?))
  ==>
    (retract ?request))

(define (find-ancestors)
  (with-new-inference-environment
   (activate ancestors-ruleset)
   (current-inference-trace #f)
   (assert '(parents penelope jessica jeremy))
   (assert '(parents jessica mary-elizabeth homer))
   (assert '(parents jeremy jenny steven))
   (assert '(parents steven loree john))
   (assert '(parents loree #f jason))
   (assert '(parents homer stephanie #f))
   (start-inference)))

(find-ancestors)
