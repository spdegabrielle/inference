#lang racket
;;; PLT Scheme Inference Collection
;;; patterns.ss
;;; Copyright (c) 2006-2010 M. Douglas Williams
;;;
;;; This library is free software; you can redistribute it and/or 
;;; modify it under the terms of the GNU Lesser General Public 
;;; License as published by the Free Software Foundation; either
;;; version 2.1 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful, 
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free
;;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
;;; 02111-1307 USA.
;;;
;;; Version  Date      Comments
;;; 1.0.1    07/17/06  Added association list matching.
;;; 2.0.0    06/26/08  Changes for V4.0.  (Doug Williams)

(require "bindings.rkt"
         "facts.rkt")

;;; -----------------------------------------------------------------------------
;;;                           Wildcard and Variables
;;; -----------------------------------------------------------------------------

;;; (wildcard? x) -> boolean?
;;;   x : any/c
;;; Returns true if its argument is a wildcard.
(define (wildcard? x)
  (eq? x '?))

;;; (variable? x) -> boolean?
;;;   x : any/c
;;; Returns true if its argument is a pattern variable.
(define (variable? x)
  (and (symbol? x)
       (not (wildcard? x))
       (string=? (substring (symbol->string x) 0 1) "?")))

;;; -----------------------------------------------------------------------------
;;;                                  Patterns
;;; -----------------------------------------------------------------------------

;;; (pattern? x) -> boolean?
;;; Returns true if its argument is a pattern.  Patterns are either a list or a
;;; vector, with at least one element, the first element is a symbol, and the
;;; symbol is not a wildcard or variable symbol.
(define (pattern? x)
  (or (and (pair? x)
           (symbol? (car x))
           (not (wildcard? (car x)))
           (not (variable? (car x))))
      (and (vector? x)
           (> (vector-length x) 0)
           (symbol? (vector-ref x 0))
           (not (wildcard? (vector-ref x 0)))
           (not (variable? (vector-ref x 0))))))

;;; (pattern-first pattern) -> symbol?
;;;   pattern : pattern?
;;; Returns the first element of a pattern, which must be a symbol.
(define (pattern-first pattern)
  (cond ((pair? pattern)
         (car pattern))
        ((vector? pattern)
         (vector-ref pattern 0))))

;;; (pattern-for-each pattern proc) -> void?
;;;   pattern : pattern?
;;;   proc : procedure?
;;; Iterates over the elements of a pattern passing each element in turn to proc.
;;; Note that the tail of an improper list is considered an element.
(define (pattern-for-each pattern proc)
  (cond ((pair? pattern)
         (let loop ((pattern-tail pattern))
           (if (pair? pattern-tail)
               (begin
                 (proc (car pattern-tail))
                 (loop (cdr pattern-tail)))
               (when (not (null? pattern-tail))
                 (proc pattern-tail))))
         (void))
        ((vector? pattern)
         (do ((i 0 (+ i 1)))
             ((= i (vector-length pattern)) (void))
           (proc (vector-ref pattern i))))))

;;; (pattern-variables pattern) -> (listof variable?)
;;;   pattern : pattern?
;;; Returns a list of the variables in a pattern.  This does not include
;;; variables only referenced in constraints.
(define (pattern-variables pattern)
  (let ((variables '()))
    (pattern-for-each
     pattern
     (lambda (element)
       (cond ((variable? element)
              (set! variables (cons element variables)))
             ((and (pair? element)
                   (variable? (car element)))
              (set! variables (cons (car element) variables)))
             ((and (pair? element)
                   (or (symbol? (car element))
                       (keyword? (car element)))
                   (variable? (cdr element)))
              (set! variables (cons (cdr element) variables)))
             ((and (pair? element)
                   (or (symbol? (car element))
                       (keyword? (car element)))
                   (pair? (cdr element))
                   (variable? (cadr element)))
              (set! variables (cons (cadr element) variables))))))
    (reverse variables)))

;;; -----------------------------------------------------------------------------
;;;                            Pattern Constraints
;;; -----------------------------------------------------------------------------

;;; (classify-constraint constraint variables) -> (one-of/c 0 1 2)
;;;   constraints : list?
;;;   variables : (listof symbol?)
;;; Recursively examines a constraint and classifies it based on the locality of
;;; its variable references.  The variables argument is a list of local (to a
;;; pattern) variables.  The returned value is:
;;;   0 - no variables
;;;   1 - local variables only
;;;   2 - non-local variables
(define NO-VARIABLES 0)
(define LOCAL-VARIABLES 1)
(define GLOBAL-VARIABLES 2)
(define (classify-constraint constraint variables)
  (let ((classification NO-VARIABLES))
    (for-each
     (lambda (element)
       (cond ((variable? element)
              (set! classification
                    (max classification
                         (if (memq element variables)
                             LOCAL-VARIABLES
                             GLOBAL-VARIABLES))))
             ((pair? element)
              (set! classification
                    (max classification
                         (classify-constraint element variables))))))
     ;; Quick fix to allow atomic conditions.  For example,
     ;; (?x ?x) matches anything that isn't false.
     (if (pair? constraint) constraint (list constraint)))
    classification))

;;; (pattern-match-constraints pattern variables) -> pattern?
;;;   constraints : list?
;;;   variables : (listof symbol?)
;;; Returns a list of the match constaints for a pattern.  A match constraint
;;; does not contain any non-local (to the pattern) variables.
(define (pattern-match-constraints pattern variables)
  (let ((match-constraints '()))
    (pattern-for-each
     pattern
     (lambda (element)
       (cond ((and (pair? element)
                   (variable? (car element)))
              (when (< (classify-constraint
                        (cadr element) variables)
                       GLOBAL-VARIABLES)
                (set! match-constraints
                      (cons (cadr element) match-constraints))))
             ((and (pair? element)
                   (or (symbol? (car element))
                       (keyword? (car element)))
                   (pair? (cdr element))
                   (variable? (cadr element)))
              (when (< (classify-constraint
                        (caddr element) variables)
                       GLOBAL-VARIABLES)
                (set! match-constraints
                      (cons (caddr element) match-constraints))))
             )))
    (reverse match-constraints)))

;;; (pattern-join-constraints pattern variables) -> pattern?
;;;   constraints : list?
;;;   variables : (listof symbol?)
;;; Returns a list of the join constraints for a pattern.  A join constraint
;;; contains non-local (to the pattern) variables.
(define (pattern-join-constraints pattern variables)
  (let ((join-constraints '()))
    (pattern-for-each
     pattern
     (lambda (element)
       (cond ((and (pair? element)
                   (variable? (car element)))
              (when (= (classify-constraint
                        (cadr element) variables)
                       GLOBAL-VARIABLES)
                (set! join-constraints
                      (cons (cadr element) join-constraints))))
             ((and (pair? element)
                   (or (symbol? (car element))
                       (keyword? (car element)))
                   (pair? (cdr element))
                   (variable? (cadr element)))
              (when (= (classify-constraint
                        (caddr element) variables)
                       GLOBAL-VARIABLES)
                (set! join-constraints
                      (cons (caddr element) join-constraints))))
             )))
    (reverse join-constraints)))

;;; (pattern-base-pattern pattern) -> pattern?
;;;   pattern : pattern?
;;; Returns the base pattern for a pattern.  The base pattern is the pattern with
;;; all of the constraints removed.  It is used by the matching (unify)
;;; algorithm.
(define (pattern-base-pattern pattern)
  (cond ((pair? pattern)
         (pattern-base-pattern-list pattern))
        ((vector? pattern)
         (let ((base-pattern (make-vector (vector-length pattern))))
           (do ((i 0 (+ i 1)))
               ((= i (vector-length pattern)) base-pattern)
             (let ((element (vector-ref pattern i)))
               (cond ((and (pair? element)
                           (variable? (car element)))
                      (vector-set! base-pattern i (car element)))
                     ((and (pair? element)
                           (or (symbol? (car element))
                               (keyword? (car element)))
                           (pair? (cdr element))
                           (variable? (cadr element)))
                      (vector-set! base-pattern i
                                   (cons (car element)
                                         (cadr element))))
                     (else
                      (vector-set! base-pattern i element)))))))))

;;; (pattern-base-pattern-list pattern) -> pattern?
;;;   pattern : pattern?
;;; Returns the base pattern for a list pattern.  This was easier to write
;;; recursively.  (Internal)
(define (pattern-base-pattern-list pattern)
  (cond ((null? pattern)
         '())
        ((pair? pattern)
         (let ((element (car pattern)))
           (cond ((and (pair? element)
                       (variable? (car element)))
                  (cons (car element)
                        (pattern-base-pattern-list (cdr pattern))))
                 ((and (pair? element)
                       (or (symbol? (car element))
                           (keyword? (car element)))
                       (pair? (cdr element))
                       (variable? (cadr element)))
                  (cons (cons (car element) (cadr element))
                        (pattern-base-pattern-list (cdr pattern))))
                 (else
                  (cons element
                        (pattern-base-pattern-list (cdr pattern)))))))
        (else
         pattern)))

;;; (pattern-substitute pattern bindings) -> pattern?
;;;   pattern : pattern?
;;;   bindings : bindings?
;;; Returns a new pattern with variable bindings substituted.  This
;;; should probably only be applied to base patterns.
(define (pattern-substitute pattern bindings)
  (cond ((pair? pattern)
         (pattern-substitute-list pattern bindings))
        ((vector? pattern)
         (let ((new-pattern (make-vector (vector-length pattern))))
           (do ((i 0 (+ i 1)))
               ((= i (vector-length pattern)) new-pattern)
             (vector-set! new-pattern i
                          (pattern-substitute-list
                           (vector-ref pattern i) bindings)))))))

;;; (pattern-substitute-list pattern bindings) -> pattern?
;;;   pattern : pattern?
;;;   bindings : bindings?
;;; Returns a new list pattern with variable bindings substituted.  This was also
;;; easier to write recursively.  (Internal)
(define (pattern-substitute-list pattern bindings)
  (cond ((null? pattern)
         '())
        ((pair? pattern)
         (cons (pattern-substitute-list (car pattern) bindings)
               (pattern-substitute-list (cdr pattern) bindings)))
        ((and (variable? pattern)
              (bindings-bound? bindings pattern))
         (bindings-ref bindings pattern))
        (else
         pattern)))

;;; -----------------------------------------------------------------------------
;;;                       Pattern Unification (Matching)
;;; -----------------------------------------------------------------------------

;;; (pattern-unify fact pattern bindings) -> (or/c bindings? false/c)
;;;   fact : fact?
;;;   pattern : pattern?
;;;   bindings : bindings?
;;; Matches the fact against the patterns using the given bindings.  If there is
;;; a match, a new set of bindings is returned, otherwise #f is returned.
;;; MDW - changed to ignore initial element
(define (pattern-unify fact pattern bindings)
  (cond ((and (pair? pattern)
              (pair? fact))
         (pattern-unify-list (cdr fact) (cdr pattern) bindings #f))
        ((and (vector? pattern)
              (vector? fact))
         (pattern-unify-vector fact pattern bindings))
        ((and (vector? pattern)
              (struct? fact))
         (pattern-unify-vector (struct->vector fact) pattern bindings))
        (else
         #f)))

;;; (pattern-unify-list fact pattern bindings alist?) -> (or/c bindings? false/c)
;;;   fact : fact?
;;;   pattern : pattern?
;;;   bindings : bindings?
;;;   alist? : boolean?
;;; Unifies a fact (list) against a pattern (list) with a set of bindings and
;;; returns either #f, if there is no match, or a list of bindings for the match.
;;; (Internal)
(define (pattern-unify-list fact pattern bindings alist?)
;  (printf "(pattern-unify-list ~s ~s ~s ~s)~n"
;          fact pattern bindings alist?)
  (cond ((null? pattern)
         (if (or (null? fact)
                 alist?)
             bindings
             #f))
        ((pair? pattern)
         (let ((element (car pattern)))
           (cond ((not (pair? fact))
                  #f)
                 ((wildcard? element)
                  (pattern-unify-list
                   (cdr fact) (cdr pattern) bindings #f))
                 ((variable? element)
                  (if (bindings-bound? bindings element)
                      (if (equal? (bindings-ref bindings element)
                                (car fact))
                          (pattern-unify-list
                           (cdr fact) (cdr pattern) bindings #f)
                          #f)
                        (pattern-unify-list
                         (cdr fact) (cdr pattern)
                         (append bindings
                                 (list
                                  (cons element (car fact))))
                         #f)))
                 ((and (pair? element)
                       (or (symbol? (car element))
                           (keyword? (car element)))
                       ;(variable? (cdr element)) ???
                       )
                  (let* ((key (car element))
                         (association (assq key fact)))
                    (if association
                        (let ((new-bindings (pattern-unify-list
                                             (cdr association) (cdr element)
                                             bindings #f)))
                          (if new-bindings
                              (pattern-unify-list
                               fact (cdr pattern) new-bindings #t)
                              #f))
                        #f)))
                 ((and (regexp? (car pattern))
                       (string? (car fact))
                       (regexp-match-exact? (car pattern) (car fact)))
                  (pattern-unify-list
                   (cdr fact) (cdr pattern) bindings #f))
                 ((equal? (car pattern) (car fact))
                  (pattern-unify-list
                   (cdr fact) (cdr pattern) bindings #f))
                 (else
                  #f))))
        ((wildcard? pattern)
         bindings)
        ((variable? pattern)
         (if (bindings-bound? bindings pattern)
             (if (equal? (bindings-ref bindings pattern)
                       fact)
                 bindings
                 #f)
             (append bindings (list (cons pattern fact)))))
;        ((and (regexp? pattern)
;              (string? fact)
;              (regexp-match-exact? pattern fact))
;         bindings)
        ((equal? pattern fact)
         bindings)
        ((null? fact)
         #f)
        (else #f)))

;;; (pattern-unify-vector fact pattern bindings) -> (or/c bindings? false/c)
;;;   fact : fact?
;;;   pattern : pattern?
;;;   bindings : bindings?
;;; Unifies a fact (vector) against a pattern (vector) with a set of bindings and
;;; returns either #f, if there is no match, or a list of bindings for the match.
;;; This uses pattern-unify-list to match the individual vector elements.
;;; (Internal)
;;; MDW - modified to ignore first element of the vectors.
(define (pattern-unify-vector fact pattern bindings)
  (if (>= (vector-length fact)
          (vector-length pattern))
      (let/ec return
        (do ((i 1 (+ i 1)))
            ((= i (vector-length pattern)) bindings)
          (let ((unified-element (pattern-unify-list
                                  (vector-ref fact i)
                                  (vector-ref pattern i)
                                  bindings #f)))
            (if unified-element
                (set! bindings unified-element)
                (return #f)))))
      #f))

;;; -----------------------------------------------------------------------------
;;;                             Module Constraints
;;; -----------------------------------------------------------------------------

;(provide
; NO-VARIABLES
; LOCAL-VARIABLES
; GLOBAL-VARIABLES)
;
;(provide/contract
; (wildcard?
;  (-> any/c boolean?))
; (variable?
;  (-> any/c boolean?))
; (pattern?
;  (-> any/c boolean?))
; (pattern-first
;  (-> pattern? symbol?))
; (pattern-for-each
;  (-> pattern? procedure? void?))
; (pattern-variables
;  (-> pattern? (listof variable?)))
; (classify-constraint
;  (-> list? (listof variable?) (one-of/c NO-VARIABLES LOCAL-VARIABLES GLOBAL-VARIABLES)))
; (pattern-match-constraints
;  (-> pattern? (listof variable?) list?))
; (pattern-join-constraints
;  (-> pattern? (listof variable?) list?))
; (pattern-base-pattern
;  (-> pattern? pattern?))
; (pattern-substitute
;  (-> pattern? bindings? pattern?))
; (pattern-unify
;  (-> fact? pattern? bindings? (or/c bindings? false/c))))

(provide (all-defined-out))
