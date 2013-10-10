#lang scheme
;;; PLT Scheme Inference Collection
;;; rulesets.ss
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
;;; Version  Date       Comments
;;; 1.0.1    07/22/06   Added fields for conflict resolution.
;;; 1.0.2    07/23/06   Added calculations for conflict resolution fields.
;;;                    (Doug Williams)
;;; 2.0.0    06/26/08  Changes for V4.0.  (Doug Williams)

(require "patterns.ss")
(require "argument-lists.ss")

;;; Ruleset and Rule Definition

;;; ruleset structure
;;;   name  - a symbol naming the ruleset
;;;   rules - a list of the rules in the ruleset
(define-struct ruleset
  (name
   (rules #:mutable)
   namespace)
  #:inspector (make-inspector))

;;; rule-write: rule? x port? x boolean
;;; Custom writer for rules.
(define (rule-print rule port write?)
  (when write? (write-string "<" port))
  (fprintf port "(~a ~a)"
           (rule-name rule)
           (ruleset-name (rule-ruleset rule)))
  (when write? (write-string ">" port)))

;;; rule structure
;;;   name          - a symbol naming the rule
;;;   ruleset       - the ruleset to which the rule belongs
;;;   goals         - a list of goals
;;;   preconditions - a list of preconditions
;;;   actions       - a list of rule actions
;;;   priority      - user defined rule priority
;;;   order         - order of the rule in the ruleset
;;;   specificity   - specificity of the rule
(define-values (struct:rule
                make-rule
                rule?
                rule-field-ref
                set-rule-field!)
  (make-struct-type 'rule #f 8 0 #f
                    (list (cons prop:custom-write rule-print))
                    (make-inspector)))

;;; name field
(define rule-name
  (make-struct-field-accessor rule-field-ref 0 'name))

(define set-rule-name!
  (make-struct-field-mutator set-rule-field! 0 'name))

;;; ruleset field
(define rule-ruleset
  (make-struct-field-accessor rule-field-ref 1 'ruleset))

(define set-rule-ruleset!
  (make-struct-field-mutator set-rule-field! 1 'ruleset))

;;; goals field
(define rule-goals
  (make-struct-field-accessor rule-field-ref 2 'goals))

(define set-rule-goals!
  (make-struct-field-mutator set-rule-field! 2 'goals))

;;; preconditions field
(define rule-preconditions
  (make-struct-field-accessor rule-field-ref 3 'preconditions))

(define set-rule-preconditions!
  (make-struct-field-mutator set-rule-field! 3 'preconditions))

;;; actions field
(define rule-actions
  (make-struct-field-accessor rule-field-ref 4 'actions))

(define set-rule-actions!
  (make-struct-field-mutator set-rule-field! 4 'actions))

;;; priority field
(define rule-priority
  (make-struct-field-accessor rule-field-ref 5 'priority))

(define set-rule-priority!
  (make-struct-field-mutator set-rule-field! 5 'priority))

;;; order field
(define rule-order
  (make-struct-field-accessor rule-field-ref 6 'order))

(define set-rule-order!
  (make-struct-field-mutator set-rule-field! 6 'order))

;;; specificity field
(define rule-specificity
  (make-struct-field-accessor rule-field-ref 7 'specificity))

(define set-rule-specificity!
  (make-struct-field-mutator set-rule-field! 7 'specificity))

;;; add-rule: symbol x ruleset x list x procedure -> void
;;; Adds a rule to a ruleset.  Will be called by the define-rule
;;; macro in the inference collection.
(define (add-rule name ruleset goals preconditions actions priority)
  (let ((rule (make-rule name ruleset
                         goals preconditions actions
                         priority
                         (+ (length (ruleset-rules ruleset)) 1)
                         (length preconditions))))
    (set-ruleset-rules! ruleset
                        (append (ruleset-rules ruleset)
                                (list rule)))))

;;; (define-ruleset <name>)
(define-syntax define-ruleset
  (syntax-rules ()
    ((define-ruleset name)
     (define name
       (make-ruleset 'name 
                     '()
                     (current-namespace))))))

;;; (define-rule (<name> <ruleset> [#:priority priority])
;;;     [<pattern> ...                  ; goal pattern(s)
;;;   <==]
;;;     [<pattern> ...                  ; data pattern(s)
;;;   ==>]
;;;     [action ...])                   ; rule action(s)
(define-syntax define-rule
  (syntax-rules (<== ==>)
    (;; no priority specified
     (define-rule (name ruleset)
       item ...)
     (define-rule (name ruleset #:priority 0)
       item ...))
    (;; special case - goal with no data or actions
     (define-rule (name ruleset #:priority priority)
       item)
     (add-rule 'name
               ruleset
               '(item)
               '()
               #f
               priority))
    (;; standard initial call - start gathering goals or data
     (define-rule (name ruleset #:priority priority)
       item-1 item-2 ...)
     (define-rule
       "gather goals or data"
       (name ruleset #:priority priority)
       (item-1)
       item-2 ...))
    (;; only found goals (unusual case)
     (define-rule
       "gather goals or data"
       (name ruleset #:priority priority)
       (goal ...))
     (add-rule 'name
               ruleset
               '(goal ...)
               '()
               #f
               priority))
    (;; found end of goals and a single precondition
     (define-rule
       "gather goals or data"
       (name ruleset #:priority priority)
       (goal ...)
       <== item)
     (add-rule 'name
               ruleset
               '(goal ...)
               '(item)
               #f
               priority))
    (;; found end of goals - start gathering data or actions
     (define-rule
       "gather goals or data"
       (name ruleset #:priority priority)
       (goal ...)
       <== item-1 item-2 ...)
     (define-rule
       "gather data or actions"
       (name ruleset #:priority priority)
       (goal ...)
       (item-1)
       item-2 ...))
    (;; found data (and no goals)
     (define-rule
       "gather goals or data"
       (name ruleset #:priority priority)
       (data ...)
       ==> item ...)
     (add-rule 'name
               ruleset
               '()
               '(data ...)
               #'(item ...)
               priority))
    (;; still looking for goals or data
     (define-rule
       "gather goals or data"
       (name ruleset #:priority priority)
       (goal-or-data ...)
       item-1 item-2 ...)
     (define-rule
       "gather goals or data"
       (name ruleset #:priority priority)
       (goal-or-data ... item-1)
       item-2 ...))
    (;; found data (no actions)
     (define-rule
       "gather data or actions"
       (name ruleset #:priority priority)
       (goal ...)
       (data ...))
     (add-rule 'name
               ruleset
               '(goal ...)
               '(data ...)
               #f
               priority))
    (;; found the end of the data
     (define-rule
       "gather-data-or-actions"
       (name ruleset #:priority priority)
       (goal ...)
       (data ...)
       ==> action ...)
     (add-rule 'name
               ruleset
               '(goal ...)
               '(data ...)
               #'(action ...)
               priority))
    (;; still looking for data or actions
     (define-rule
       "gather data or actions"
       (name ruleset #:priority priority)
       (goal ...)
       (data-or-action ...)
       item-1 item-2 ...)
     (define-rule
       "gather data or actions"
       (name ruleset #:priority priority)
       (goal ...)
       (data-or-action ... item-1)
       item-2 ...))))

;(provide
; define-ruleset
; define-rule)
;
;(provide/contract
; (ruleset?
;  (-> any/c boolean?))
; (make-ruleset
;  (-> symbol? (listof rule?) any/c ruleset?))
; (ruleset-name
;  (-> ruleset? symbol?))
; (ruleset-rules
;  (-> ruleset? (listof rule?)))
; (set-ruleset-rules!
;  (-> ruleset? (listof rule?) void?))
; (rule?
;  (-> any/c boolean?))
; (make-rule
;  (-> symbol?
;      ruleset?
;      list?
;      list?
;      (or/c syntax? false/c)
;      real?
;      real?
;      real?
;      rule?))
; (rule-name
;  (-> rule? symbol?))
; (rule-ruleset
;  (-> rule? ruleset?))
; (rule-goals
;  (-> rule? list?))
; (rule-preconditions
;  (-> rule? list?))
; (rule-actions
;  (-> rule? (or/c syntax? false/c)))
; (rule-priority
;  (-> rule? real?))
; (rule-order
;  (-> rule? real?))
; (rule-specificity
;  (-> rule? real?))
; (add-rule
;  (-> symbol?
;      ruleset?
;      list?
;      list?
;      (or/c syntax? false/c)
;      real?
;      void?)))

(provide (all-defined-out))