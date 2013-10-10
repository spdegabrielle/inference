#lang scheme
;;; PLT Scheme Inference Collection
;;; inference.ss
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
;;; Version Date     Comment
;;; 1.0.1   07/16/06 Added fields for next-assertion-id and
;;;                  assertion-index (Doug Williams)
;;; 1.0.2   07/19/06 Added trace field.  (Doug Williams)
;;; 1.0.3   07/22/06 Added agenda and rule fields.  (Doug Williams)
;;; 1.0.4   07/23/06 Added strategy field.  (Doug Williams)
;;; 1.0.5   07/24/06 Added hierarchical inference environments.
;;;                  (Doug Williams)
;;; 2.0.0   09/22/09 Added ontology. (MDW)
;;; 2.0.1   03/21/10 Added assertion hooks. (MDW)
;;;
;;; To do:
;;; 1) Replace simple list form of agenda with a double linked form.

(require "ontology.ss")

;;; inverence-environment: struct
;;;   0 data-index         hasheq?
;;;   1 goal-index         hasheq?
;;;   2 rule-nodes         list?
;;;   3 exit               (or/c continuation? false/c)
;;;   4 next-assertion-id  exact-nonnegative-integer?
;;;   5 assertion-index    hasheq?
;;;   6 trace              boolean?
;;;   7 agenda             list?
;;;   8 rule               rule?
;;;   9 strategy           symbol?
;;;  10 parent             (inference-environment? or false/c)
;;;  11 rules-fired        exact-nonnegative-integer?
;;;  12 ontology           (or/c ontology? false/c)
;;;  13 assertion-hooks    (or/c hasheq? false/c)
(define-values (struct:inference-environment
                inference-environment-constructor
                inference-environment?
                inference-environment-field-ref
                set-inference-environment-field!)
  (make-struct-type 'inference-environment #f 14 0))

;;; data-index field
(define inference-environment-data-index
  (make-struct-field-accessor
   inference-environment-field-ref 0 'data-index))

(define set-inference-environment-data-index!
  (make-struct-field-mutator
   set-inference-environment-field! 0 'data-index))

;;; goal-index field
(define inference-environment-goal-index
  (make-struct-field-accessor
   inference-environment-field-ref 1 'goal-index))

(define set-inference-environment-goal-index!
  (make-struct-field-mutator
   set-inference-environment-field! 1 'goal-index))

;;; rule-nodes field
(define inference-environment-rule-nodes
  (make-struct-field-accessor
   inference-environment-field-ref 2 'rule-nodes))

(define set-inference-environment-rule-nodes!
  (make-struct-field-mutator
   set-inference-environment-field! 2 'rule-nodes))

;;; exit field
(define inference-environment-exit
  (make-struct-field-accessor
   inference-environment-field-ref 3 'exit))

(define set-inference-environment-exit!
  (make-struct-field-mutator
   set-inference-environment-field! 3 'exit))

;;; next-assertion-id field
(define inference-environment-next-assertion-id
  (make-struct-field-accessor
   inference-environment-field-ref 4 'next-assertion-id))

(define set-inference-environment-next-assertion-id!
  (make-struct-field-mutator
   set-inference-environment-field! 4 'next-assertion-id))

;;; assertion-index field
(define inference-environment-assertion-index
  (make-struct-field-accessor
   inference-environment-field-ref 5 'assertion-index))

(define set-inference-environment-assertion-index!
  (make-struct-field-mutator
   set-inference-environment-field! 5 'assertion-index))

;;; trace field
(define inference-environment-trace
  (make-struct-field-accessor
   inference-environment-field-ref 6 'trace))

(define set-inference-environment-trace!
  (make-struct-field-mutator
   set-inference-environment-field! 6 'trace))

;;; agenda field
(define inference-environment-agenda
  (make-struct-field-accessor
   inference-environment-field-ref 7 'agenda))

(define set-inference-environment-agenda!
  (make-struct-field-mutator
   set-inference-environment-field! 7 'agenda))

;;; rule field
(define inference-environment-rule
  (make-struct-field-accessor
   inference-environment-field-ref 8 'rule))

(define set-inference-environment-rule!
  (make-struct-field-mutator
   set-inference-environment-field! 8 'rule))

;;; strategy field
(define inference-environment-strategy
  (make-struct-field-accessor
   inference-environment-field-ref 9 'strategy))

(define set-inference-environment-strategy!
  (make-struct-field-mutator
   set-inference-environment-field! 9 'strategy))

;;; parent field
(define inference-environment-parent
  (make-struct-field-accessor
   inference-environment-field-ref 10 'parent))

(define set-inference-environment-parent!
  (make-struct-field-mutator
   set-inference-environment-field! 10 'parent))

;;; rules-fired field
(define inference-environment-rules-fired
  (make-struct-field-accessor
   inference-environment-field-ref 11 'rules-fired))

(define set-inference-environment-rules-fired!
  (make-struct-field-mutator
   set-inference-environment-field! 11 'rules-fired))

;;; ontology field
(define inference-environment-ontology
  (make-struct-field-accessor
   inference-environment-field-ref 12 'ontology))

(define set-inference-environment-ontology!
  (make-struct-field-mutator
   set-inference-environment-field! 12 'ontology))

;;; assertion-hooks field
(define inference-environment-assertion-hooks
  (make-struct-field-accessor
   inference-environment-field-ref 13 'assertion-hooks))

(define set-inference-environment-assertion-hooks!
  (make-struct-field-mutator
   set-inference-environment-field! 13 'assertion-hooks))

;;; make-inference-environment: inference-environment? ->
;;;   inference-environment?
;;; make-inference-environment: -> inference-environment?
(define (make-inference-environment (parent #f))
  (inference-environment-constructor
   (make-hasheq)                      ; data-index
   (make-hasheq)                      ; goal-index
   '()                                ; rule-nodes
   #f                                 ; exit
   1                                  ; next-assertion-id
   (make-hasheq)                      ; assertion-index
   #f                                 ; trace
   '()                                ; agenda
   #f                                 ; rule
   'depth                             ; strategy
   parent                             ; parent
   0                                  ; rules-fired
   #f                                 ; ontology
   #f                                 ; assertion-hooks
   ))

;;; default-inference-environment variable
(define default-inference-environment
  (make-inference-environment))

;;; current-inference-enironment parameter
(define current-inference-environment
  (make-parameter
   default-inference-environment
   (lambda (x)
     (when (not (inference-environment? x))
       (raise-type-error 'current-inference-environment
                         "inference-environment" x))
     x)))

;;; current-inference-data-index: -> hash-table?
;;; current-inference-data-index: hash-table?
(define current-inference-data-index
  (case-lambda
    (()
     (inference-environment-data-index
      (current-inference-environment)))
    ((data-index)
     (set-inference-environment-data-index!
      (current-inference-environment) data-index))))

;;; current-inference-goal-index: -> hash-table?
;;; current-inference-goal-index: hash-table?
(define current-inference-goal-index
  (case-lambda
    (()
     (inference-environment-goal-index
      (current-inference-environment)))
    ((goal-index)
     (set-inference-environment-goal-index!
      (current-inference-environment) goal-index))))

;;; current-inference-rule-nodes: -> list?
;;; current-inference-rule-nodes: list?
(define current-inference-rule-nodes
  (case-lambda
    (()
     (inference-environment-rule-nodes
      (current-inference-environment)))
    ((rule-nodes)
     (set-inference-environment-rule-nodes!
      (current-inference-environment) rule-nodes))))

;;; current-inference-exit: -> continuation? or #f
;;; current-inference-exit: continuation? or #f
(define current-inference-exit
  (case-lambda
    (()
     (inference-environment-exit
      (current-inference-environment)))
    ((exit)
     (set-inference-environment-exit!
      (current-inference-environment) exit))))

;;; current-inference-next-assertion-id: -> natural-number
;;; current-inference-next-assertion-id: natural-number
(define current-inference-next-assertion-id
  (case-lambda
    (()
     (inference-environment-next-assertion-id
      (current-inference-environment)))
    ((next-assertion-id)
     (set-inference-environment-next-assertion-id!
      (current-inference-environment) next-assertion-id))))

;;; current-inference-assertion-index: -> hash-table?
;;; current-inference-assertion-index: hash-table?
(define current-inference-assertion-index
  (case-lambda
    (()
     (inference-environment-assertion-index
      (current-inference-environment)))
    ((assertion-index)
     (set-inference-environment-assertion-index!
      (current-inference-environment) assertion-index))))

;;; current-inference-trace: -> boolean?
;;; current-inference-trace: boolean?
(define current-inference-trace
  (case-lambda
    (()
     (inference-environment-trace
      (current-inference-environment)))
    ((trace)
     (set-inference-environment-trace!
      (current-inference-environment) trace))))

;;; current-inference-agenda: -> list?
;;; current-inference-agenda: list?
(define current-inference-agenda
  (case-lambda
    (()
     (inference-environment-agenda
      (current-inference-environment)))
    ((agenda)
     (set-inference-environment-agenda!
      (current-inference-environment) agenda))))

;;; current-inference-rule: -> rule? or #f
;;; current-inference-rule: rule? or #f
(define current-inference-rule
  (case-lambda
    (()
     (inference-environment-rule
      (current-inference-environment)))
    ((rule)
     (set-inference-environment-rule!
      (current-inference-environment) rule))))

;;; current-inference-strategy: -> symbol?
;;; current-inference-strategy: symbol?
(define current-inference-strategy
  (case-lambda
    (()
     (inference-environment-strategy
      (current-inference-environment)))
    ((strategy)
     (set-inference-environment-strategy!
      (current-inference-environment) strategy))))

;;; current-inference-parent: -> inference-environment? or #f
;;; current-inference-parent: inference-environment? or #f
(define current-inference-parent
  (case-lambda
    (()
     (inference-environment-parent
      (current-inference-environment)))
    ((parent)
     (set-inference-environment-parent!
      (current-inference-environment) parent))))

;;; current-inference-rules-fired: -> integer?
;;; current-inference-rules-fired: integer?
(define current-inference-rules-fired
  (case-lambda
    (()
     (inference-environment-rules-fired
      (current-inference-environment)))
    ((rules-fired)
     (set-inference-environment-rules-fired!
      (current-inference-environment) rules-fired))))

;;; current-inference-ontology: -> hasheq?
;;; current-inference-ontology: hasheq?
(define current-inference-ontology
  (case-lambda
    (()
     (inference-environment-ontology
      (current-inference-environment)))
    ((ontology)
     (set-inference-environment-ontology!
      (current-inference-environment) ontology))))

;;; current-inference-assertion-hooks: -> (or/c hasheq? false/c)
;;; current-inference-assertion-hooks: (or/c hasheq? false/c)
(define current-inference-assertion-hooks
  (case-lambda
    (()
     (inference-environment-assertion-hooks
      (current-inference-environment)))
    ((assertion-hooks)
     (set-inference-environment-assertion-hooks!
      (current-inference-environment) assertion-hooks))))

;;; (with-inference-environment inference-environment
;;;   body ..)
(define-syntax with-inference-environment
  (syntax-rules ()
    ((with-inference-environment inference-environment
                                 body ...)
     (parameterize ((current-inference-environment
                     inference-environment))
       body ...))))

;;; (with-new-inference-environment
;;;   body ...)
(define-syntax with-new-inference-environment
  (syntax-rules ()
    ((with-new-inference-environment
      body ...)
     (parameterize ((current-inference-environment
                     (make-inference-environment)))
       body ...))))

;;; (with-new-child-inference-environment
;;;   body ...)
(define-syntax with-new-child-inference-environment
  (syntax-rules ()
    ((with-new-child-inference-environment
      body ...)
     (parameterize ((current-inference-environment
                     (make-inference-environment
                      (current-inference-environment))))
       body ...))))

;(provide
; default-inference-environment
; current-inference-environment
; with-inference-environment
; with-new-inference-environment
; with-new-child-inference-environment)
;
;(provide/contract
; (inference-environment?
;  (-> any/c boolean?))
; (make-inference-environment
;  (->* () (inference-environment?) inference-environment?))
; (inference-environment-data-index
;  (-> inference-environment? hash?))
; (inference-environment-goal-index
;  (-> inference-environment? hash?))
; (inference-environment-rule-nodes
;  (-> inference-environment? list?))
; (inference-environment-exit
;  (-> inference-environment? (or/c procedure? false/c)))
; (set-inference-environment-exit!
;  (-> inference-environment? (or/c procedure? false/c) void?))
; (inference-environment-next-assertion-id
;  (-> inference-environment? exact-positive-integer?))
; (set-inference-environment-next-assertion-id!
;  (-> inference-environment? exact-positive-integer? void?))
; (inference-environment-assertion-index
;  (-> inference-environment? hash?))
; (inference-environment-trace
;  (-> inference-environment? boolean?))
; (inference-environment-agenda
;  (-> inference-environment? any))
; (set-inference-environment-agenda!
;  (-> inference-environment? any/c void?))
; (inference-environment-rule
;  (-> inference-environment? any))
; (set-inference-environment-rule!
;  (-> inference-environment? any/c void?))
; (inference-environment-strategy
;  (-> inference-environment? symbol?))
; (set-inference-environment-strategy!
;  (-> inference-environment? symbol? void?))
; (inference-environment-parent
;  (-> inference-environment? (or/c inference-environment? false/c)))
; (set-inference-environment-parent!
;  (-> inference-environment? (or/c inference-environment? false/c) void?))
; (inference-environment-rules-fired
;  (-> inference-environment? exact-nonnegative-integer?))
; (inference-environment-ontology
;  (-> inference-environment? ontology?))
; (current-inference-data-index
;  (case->
;   (-> hash?)
;   (-> hash? void?)))
; (current-inference-goal-index
;  (case->
;   (-> hash?)
;   (-> hash? void?)))
; (current-inference-rule-nodes
;  (case->
;   (-> list?)
;   (-> list? void?)))
; (current-inference-exit
;  (case->
;   (-> (or/c procedure? false/c))
;   (-> (or/c procedure? false/c) void?)))
; (current-inference-next-assertion-id
;  (case->
;   (-> exact-positive-integer?)
;   (-> exact-positive-integer? void?)))
; (current-inference-assertion-index
;  (case->
;   (-> hash?)
;   (-> hash? void?)))
; (current-inference-trace
;  (case->
;   (-> boolean?)
;   (-> boolean? void?)))
; (current-inference-agenda
;  (case->
;   (-> any)
;   (-> any/c void?)))
; (current-inference-rule
;  (case->
;   (-> any)
;   (-> any/c void?)))
; (current-inference-strategy
;  (case->
;   (-> symbol?)
;   (-> symbol? void?)))
; (current-inference-parent
;  (case->
;   (-> (or/c inference-environment? false/c))
;   (-> (or/c inference-environment? false/c) void?)))
; (current-inference-rules-fired
;  (case->
;   (-> exact-nonnegative-integer?)
;   (-> exact-nonnegative-integer? void?)))
; (current-inference-ontology
;  (case->
;   (-> ontology?)
;   (-> ontology? void?)))
; )

(provide (all-defined-out))
