#lang scheme/base
;;; PLT Scheme Inference Collection
;;; ancestors-alist.ss
;;;
;;; Ancestors is a trivial knowledge-based system that determines
;;; a given person's ancestors based on asserted parent relationships.

(require (planet williams/inference/inference))

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
    (parents ?name (mother . ?mother) (father . ?father))
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
   (assert '(parents penelope (mother . jessica) (father . jeremy)))
   (assert '(parents jessica (mother . mary-elizabeth) (father . homer)))
   (assert '(parents jeremy (mother . jenny) (father . steven)))
   (assert '(parents steven (mother . loree) (father . john)))
   (assert '(parents loree (mother . #f) (father . jason)))
   (assert '(parents homer (mother . stephanie) (father . #f)))
   (start-inference)))

(find-ancestors)
