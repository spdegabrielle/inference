#lang scheme/base
;;; PLT Scheme Inference Collection
;;; test-strategy.ss

(require (planet williams/inference/inference))

;;; Example Inference Model

(define-ruleset ancestors-ruleset)

(define-rule (initialize ancestors-ruleset)
    (?start <- (start))
  ==>
    (retract ?start)
    (assert '(request penelope)))

(define-rule (print-maternal-ancestors ancestors-ruleset)
    (?request <- (request ?name))
    (parents ?name (?mother ?mother) ?)
  ==>
    (printf "~a is an ancestor via ~a~n" ?mother ?name)
    (assert `(request ,?mother)))

(define-rule (print-paternal-ancestors ancestors-ruleset)
    (?request <- (request ?name))
    (parents ?name ? (?father ?father))
  ==>
    (printf "~a is an ancestor via ~a~n" ?father ?name)
    (assert `(request ,?father)))

(define-rule (remove-request ancestors-ruleset #:priority -100)
    (?request <- (request ?))
  ==>
    (retract ?request))

(define (find-ancestors strategy)
  (with-new-inference-environment
   (current-inference-strategy strategy)
   (activate ancestors-ruleset)
   (assert '(parents penelope jessica jeremy))
   (assert '(parents jessica mary-elizabeth homer))
   (assert '(parents jeremy jenny steven))
   (assert '(parents steven loree john))
   (assert '(parents loree #f jason))
   (assert '(parents homer stephanie #f))
   (printf "Conflict resolution strategy = ~a~n" strategy)
   (start-inference)
   (printf "~n~n")))

(find-ancestors 'depth)
(find-ancestors 'breadth)
(find-ancestors 'order)
(find-ancestors 'simplicity)
(find-ancestors 'complexity)
(find-ancestors 'order)
(find-ancestors 'random)
