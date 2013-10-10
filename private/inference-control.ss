#lang scheme
;;; PLT Scheme Inference Collection
;;; inference-control.ss
;;; Copyright (c) 2006-2008 M. Douglas Williams
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
;;; -----------------------------------------------------------------------------
;;;
;;; This module contains the main inference engine routines.
;;;
;;; Pretty much any mutable lists will be visible here. With V2.0.1 I am trying
;;; to limit mutable lists to just those that (I think) greatly affect
;;; performance. These are:
;;;  - the agenda
;;;  - matches
;;;
;;; Version Date      Comments
;;; 1.0.1   07/16/06  Modified assert, retract, and modify to maintain the
;;;                   assertion index. Eventually need to revisit the code for
;;;                   efficiency. (Doug Williams)
;;; 1.0.2   07/19/06  Added stop-simulation, succeed, and fail. Fixed notall and
;;;                   all existential processing. Fixed check to return the
;;;                   correct assertion. (Doug Williams)
;;; 1.0.3   07/22/06  Cleaned up the code (e.g. merged code to activate data and
;;;                   goal rules). Made trace more readable. (Doug Williams)
;;; 1.0.4   07/24/06  Added import and export. (Doug Williams)
;;; 1.0.5   07/30/06  Added initial support for shared nodes in the rule network.
;;;                   (Doug Williams)
;;; 1.0.6   08/18/06  Store the value of n for each match node rather than re-
;;;                   computing it. (Doug Williams)
;;; 1.0.7   08/18/06  Made agenda use depth first insertion within other
;;;                   strategies. (Doug Williams)
;;; 1.0.8   08/25/06  Added assumption processing. (Doug Williams)
;;; 1.0.9   09/23/06  Changed (original) modify to replace and added reuse of the
;;;                   assertion object. (Doug Williams)
;;; 1.0.10  03/07/07  Added match count indexing. (Doug Williams)
;;; 2.0.0   06/26/08  Changes for V4.0. (Doug Williams)
;;; 2.0.1   07/02/08  Getting mutable lists straightened out.  Changing bindings
;;;                   back to immutable lists. (Doug Williams)
;;; 2.0.2   12/25/08  Added module contracts and cleaned up the code. (Doug
;;;                   Williams
;;; 2.0.3   12/26/08  Fixed a bug with mutable lists for backward chaining.
;;;                   (Doug Williams)
;;; 2.0.4   09/14/09  Fixed errors in node copying. (Doug Williams)
;;; 2.0.5   03/21/10  Added assertion hooks. (Doug Williams)
;;; 2.0.6   06/18/10  Added query*. (MDW)

(require "ontology.ss")
(require "inference-environments.ss")
(require "bindings.ss")
(require "patterns.ss")
(require "facts.ss")
(require "rulesets.ss")
(require "matches.ss")
(require "assertions.ss")
(require "counts.ss")
(require "truth-maintenance.ss")
(require scheme/mpair)
(require (only-in srfi/1 delete! append! list-copy))

(define (mlist-copy mlst)
  (if (null? mlst)
      '()
      (mcons (mcar mlst) (mlist-copy (cdr mlst)))))

(define-namespace-anchor anchor)

;;; -----------------------------------------------------------------------------
;;;                                  Indexing
;;; -----------------------------------------------------------------------------
;;; These routines implement the indexing strategies for the inference engine.
;;; The level-1 indices are references via the corresponding fields in the
;;; current inference environment. The indices and keys are:
;;;
;;;   index            level-1 key  level-2 key
;;;   assertion-index  fact-first   fact
;;;   data-index       fact-first
;;;   goal-index       fact-first

;;; -----------------------------------------------------------------------------
;;;                              Assertion Index
;;; -----------------------------------------------------------------------------

;;; (assertion-level-2-index fact) -> hash?
;;;   fact : fact?
;;; Returns the level-2 assertion index for fact.  A new level-2 index is
;;; created if needed.
(define (assertion-level-2-index fact)
  (let ((level-1-index (current-inference-assertion-index))
        (level-1-key (fact-first fact)))
    (hash-ref level-1-index
              level-1-key
              (lambda ()
                (let ((level-2-index (make-hash)))
                  (hash-set! level-1-index
                             level-1-key
                             level-2-index)
                  level-2-index)))))

;;; (assertion-index-find fact) -> (or/c assertion? false/c)
;;;   fact : fact?
;;; Returns the existing assertion for fact, or #f if the fact is not (currently)
;;; asserted.
(define (assertion-index-find fact)
  (hash-ref (assertion-level-2-index fact) fact #f))

;;; (assertion-index-add assertion) -> void?
;;;   assertion : assertion?
;;; Adds the assertion to the assertion index using fact.
(define (assertion-index-add assertion)
  (let ((fact (assertion-fact assertion)))
    (hash-set! (assertion-level-2-index fact) fact assertion)))

;;; (assertion-index-remove assertion) -> void?
;;;    assertion : assertion?
;;; Removes the assertion from the assertion index.
(define (assertion-index-remove assertion)
  (let ((fact (assertion-fact assertion)))
    (hash-remove! (assertion-level-2-index fact) fact)))

;;; -----------------------------------------------------------------------------
;;;                                 Data Index
;;; -----------------------------------------------------------------------------

;;; (data-index-find assertion) -> (listof match-node?)
;;;   assertion : assertion?
;;; Returns a list of (non-goal) match nodes whose pattern have initial symbols
;;; matching the initial symbol in the assertion fact.
(define (data-index-find assertion)
  (let ((fact (assertion-fact assertion)))
    (hash-ref (current-inference-data-index) (fact-first fact) '())))

(define (data-index-find-name name)
  (hash-ref (current-inference-data-index) name '()))

;;; -----------------------------------------------------------------------------
;;;                                 Goal Index
;;; -----------------------------------------------------------------------------

;;; (goal-index-find assertion) -> (listof match-node?)
;;;   assertion : assertion?
;;; Returns a list of (goal) match nodes whose pattern have initial symbols
;;; matching the initial symbol in the assertion fact.
(define (goal-index-find assertion)
  (let ((fact (assertion-fact assertion)))
    (hash-ref (current-inference-goal-index) (fact-first fact) '())))

;;; -----------------------------------------------------------------------------
;;;                        Assert, Retract, and Replace
;;; -----------------------------------------------------------------------------

(define (pragma? fact)
  (and (list? fact)
       (keyword? (car fact))))

;;; (assert fact [reason]) -> assertion?
;;;   fact : fact?
;;;   reason : any/c = (current-inference-rule)
;;; Assert a fact. This function returns a new assertion from the given fact.  It
;;; does not check to see if the fact has already been asserted. The assertion is
;;; passed into the rule network for inferencing.
;;; Note that this code is reused in replace.  Any changes here need to
;;; be made there too.
;;; MDW - I'm not sure that the reason makes sense as an argument here. It might
;;; be better to let assert determine the reason based on the current state (e.g.
;;; rule).
(define (assert fact (reason (current-inference-rule)))
  (if (pragma? fact)
      (assert-pragma fact)
      (assert-fact fact reason)))

(define (assert-assertion assertion)
  ;; Add to assertion index.
  (assertion-index-add assertion)
  ;; Trace the assertion.
  (when (current-inference-trace)
    (printf ">>> ~s~n" assertion))
  ;; Match and propagate assertion through rule network.
  (let ((fact (assertion-fact assertion)))
    (for* ((name 
            (if (current-inference-ontology)
                (in-list (class-ancestor-names 
                          (ontology-find-class
                           (current-inference-ontology)
                           (fact-first fact))))
                (list (fact-first fact))))
           (match-node (in-list (data-index-find-name name))))
      (match-node-assert match-node assertion)))
  ;; Return the assertion.
  assertion)

(define (assert-fact fact reason)
  (define (default-assertion-hook fact)
    (let ((assertion (assertion-index-find fact)))
      (if assertion
          (values assertion #t)
          (values #f #f))))
  (let ((assertion-hook
         (and (current-inference-assertion-hooks)
              (hash-ref (current-inference-assertion-hooks)
                        (fact-first fact) #f))))
    (let-values (((assertion flag?)
                  (if assertion-hook
                      ((assertion-hook-assert-hook assertion-hook) fact)
                      (default-assertion-hook fact))))
      (if assertion
          (if flag?
              ;; New assertion, set reason and propogate.
              (begin
                (unless (assertion-reason assertion)
                  (set-assertion-reason! assertion reason))
                (assert-assertion assertion))
              (if (equal? fact (assertion-fact assertion))
                  ;; Exact match, update the reason and return the assertion.
                  (begin
                    (set-assertion-reason! assertion reason)
                    (when (current-inference-trace)
                      (printf ">*> ~s~n" assertion))
                    assertion)
                  ;; Not an exact match, retract the assertion and assert the fact.
                  (begin
                    (retract assertion)
                    (set-assertion-fact! assertion fact)
                    (set-assertion-reason! assertion reason)
                    (assert-assertion assertion))))
          (begin
            (assert-assertion (make-assertion fact reason)))))))

;;; (assert-pragma pragma)
;;;   pragma : pragma?
(define (assert-pragma pragma)
  (case (car pragma)
    ((#:class)
     (unless (current-inference-ontology)
       (current-inference-ontology (new-ontology)))
     (ontology-assert-class (current-inference-ontology) pragma)
     #f)
    ((#:assertion-hook)
     (unless (current-inference-assertion-hooks)
       (current-inference-assertion-hooks (make-hasheq)))
     (tm-assert-assertion-hook (current-inference-assertion-hooks) pragma)
     #f)
    ))

;;; retract assertion) -> void?
;;;   assertion : assertion?
;;; Retract an assertion. The retraction is passed into the rule network for
;;; inferencing
(define (retract assertion)
  ;; Remove the assertion for the assertion index.
  (assertion-index-remove assertion)
  ;; Trace the retraction.
  (when (current-inference-trace)
    (printf "<<< ~s~n" assertion))
  ;; Match and unpropagate through the rule network.
  (for-each 
   (lambda (match-node)
     (match-node-retract match-node assertion))
   (data-index-find assertion)))

;;; (replace assertion fact [reason]) -> assertion?
;;;   assertion : assertion?
;;;   fact : fact?
;;;   reason : any/c = (current-inference-rule)
;;; Replace an assertion. In essence, retract the assertion and then reassert it
;;; using the given fact.
(define (replace assertion fact (reason (current-inference-rule)))
  ;; Retract the assertion.
  (retract assertion)
  ;; Re-assert with the new fact and reason.
  ;; Copies from assert
  ;(assert fact reason)
  (set-assertion-fact! assertion fact)
  (set-assertion-reason! assertion reason)
  ;;
  (assert-assertion assertion))

;;; -----------------------------------------------------------------------------
;;;                               Check and Query
;;; -----------------------------------------------------------------------------

;;; (check fact) -> match?
;;;   fact : fact?
;;; Check a fact using backtracking. Can be called directly for a pure backward
;;; chaining strategy. Also called to initiate backward chaining for match nodes.
(define (check fact)
  (let/cc return
    (let ((assertion (make-assertion fact (current-inference-rule))))
      (when (current-inference-trace)
        (printf "??? ~s~n" assertion))
      (for-each
       (lambda (match-node)
         (let ((match (match-node-check match-node assertion return)))
           (when (not (null? match))
             (return match))))
       (hash-ref (current-inference-goal-index)
                       (fact-first fact) (lambda () '()))) '())))

;;; (query pattern) -> (listof match?)
;;;   pattern : pattern?
;;; Returns a list of assertions matching a pattern. Not currently used
;;; internally.  May be useful for dynamic bindings within a rule.
(define (query pattern)
  (let ((matches '()))
    (hash-for-each
     (assertion-level-2-index (list (pattern-first pattern)))
     (lambda (fact assertion)
       (let* (;(fact (assertion-fact assertion))
              (bindings (pattern-unify fact pattern '())))
         (when bindings
           (set! matches
                 (mcons (cons assertion bindings)
                       matches))))))
    matches))

;;; (query* pattern) -> (listof match?)
;;;   pattern : pattern?
;;; Like query, but also matches asserted facts in subclasses.
(define (query* pattern)
  (let ((matches '()))
    (for ((class-name (in-list
                       (class-descendant-names
                        (ontology-find-class
                         (current-inference-ontology)(pattern-first pattern))))))
      (hash-for-each
       (assertion-level-2-index (list class-name))
       (lambda (fact assertion)
         (let* ((bindings (pattern-unify fact pattern '())))
           (when bindings
             (set! matches
                   (mcons (cons assertion bindings)
                          matches)))))))
    matches))

;;; -----------------------------------------------------------------------------
;;;                                   Assume
;;; -----------------------------------------------------------------------------

;;; (assume fact) -> any
;;; Initiate a new inference using the assumed fact as a basis.
;;; Note that is experimental. Also, it should allow a list of facts to be
;;; assummed.
(define (assume fact)
  (let ((saved (copy-inference-environment (current-inference-environment))))
    (assert fact)
    (let ((result (start-inference)))
      (if result
          (current-inference-exit (inference-environment-exit saved))
          (current-inference-environment saved))
      result)))

;;; -----------------------------------------------------------------------------
;;;                                Rule Network
;;; -----------------------------------------------------------------------------

;;; (struct node (successors matches))
;;;   successors : (listof node?)
;;;   matches : (listof match?)
;;; An anstract structure used to define the nodes in a rule network. There are
;;; concrete structures defined on top of this: match-node, join-node, and rule-
;;; node.  The fields are:
;;;   successors           - list of successor nodes
;;;   matches              - list of matches for the node
;;;                        -   #f for goal nodes
(define-struct node
  (successors
   matches)
  #:inspector (make-inspector)
  #:mutable)

;;; (struct (match-node node) (assertion-variable
;;;                            pattern
;;;                            match-constraint-variables
;;;                            match-constraints
;;;                            match-constraint-predicate
;;;                            n))
;;;   assertion-variable : (or/c variable? false/c)
;;;   pattern : pattern?
;;;   match-constraint-variables : (listof variable?)
;;;   match-constraints : (listof list?)
;;;   match-constraint-predicate : (or/c procedure? false/c)
;;;   n : exact-nonnegative-integer?
;;; The fields are:
;;;   assertion-variable   - assertion variable or #f
;;;   pattern              - pattern to be matched
;;;   match-constraint-variables
;;;                        - a list of the variables in the match constraints
;;;   match-constraints    - a list of the match constraints
;;;   match-constraint-predicate
;;;                        - match constraint predicate
;;;   n                    - number of assertions for initial symbol
(define-struct (match-node node)
  (assertion-variable
   pattern
   match-constraint-variables
   match-constraints
   match-constraint-predicate
   (n #:mutable))
  #:inspector (make-inspector))

;;; (struct (join-node node) (left
;;;                           right
;;;                           join-constraint-variables
;;;                           join-constraints
;;;                           join-constraint-predicate
;;;                           existential?
;;;                           match-counts))
;;;   left : (or/c node? false/c)
;;;   right : node?
;;;   join-constraint-variables : (listof variable?)
;;;   join-constraints : (listof list?)
;;;   join-constraint-predicate : (or/c procedure? false/c)
;;;   existential? : (one-of/c #f 'no 'notany 'any 'notall 'all)
;;;   match-counts : counts?
;;; The fields are:
;;;   left                 - left node (#f or another join node)
;;;   right                - right node (match node)
;;;   join-constraint-variables
;;;                        - a list of the variables in the join constraints
;;;   join-constraints     - a list of the join constraints
;;;   join-constraint-predicate
;;;                        - join constraint predicate 
;;;   existential?         - existential nature of the associated match node
;;;                          (right)
;;;                            #f         - not an existential match
;;;                            no, notany - no assertions match 
;;;                            any        - at least one match
;;;                            notall     - at least one doesn't match
;;;                            all        - all assertions match
;;;   match-counts         - alist of count for joined matches
(define-struct (join-node node)
  (left
   right
   join-constraint-variables
   join-constraints
   join-constraint-predicate
   existential?
   match-counts)
  #:inspector (make-inspector))

;;; (struct (rule-node node) (rule join action))
;;;   rule : rule?
;;;   join : node?
;;;   action: (or/c procedure false/c)
;;; The fields are:
;;;   rule                 - rule structure
;;;   join                 - join node (predecessor)
;;;   action               - procedure to execute (or #f)
(define-struct (rule-node node)
  (rule
   join
   action)
  #:inspector (make-inspector))

;;; (get-match-node successors
;;;                 matches
;;;                 assertion-variable
;;;                 pattern
;;;                 match-constraint-variables
;;;                 match-constraints
;;;                 match-constraint-predicate
;;;                 n) -> match-node?
;;;   successors : (listof node?)
;;;   matches : (listof match?)
;;;   assertion-variable : (or/c variable? false/c)
;;;   pattern : pattern?
;;;   match-constraint-variables : (listof variable?)
;;;   match-constraints : (listof list?)
;;;   match-constraint-predicate : (or/c procedure? false/c)
;;;   n : exact-nonnegative-integer?
;;; Returns a match node for the given (parsed) pattern. This facilitates reuse
;;; within the rule network for efficiency.
(define (get-match-node successors
                        matches
                        assertion-variable
                        pattern
                        match-constraint-variables
                        match-constraints
                        match-constraint-predicate
                        n)
  (let/ec return
    ;; Search for an equivalent match node and return it if found.
    (for-each
     (lambda (match-node)
       (when (and (eq? assertion-variable
                       (match-node-assertion-variable match-node))
                  (equal? pattern
                          (match-node-pattern match-node))
                  (equal? match-constraint-variables
                          (match-node-match-constraint-variables match-node))
                  (equal? match-constraints
                          (match-node-match-constraints match-node)))
         (return match-node)))
     (hash-ref (current-inference-data-index)
               (pattern-first pattern)
               (lambda () '())))
    ;; No equivalent match mode found, so create a new one.
    (let ((match-node
           (make-match-node
            successors                        ; successors
            matches                           ; matches
            assertion-variable                ; assertion-variable
            pattern                           ; pattern
            match-constraint-variables        ; match-constraint-variables
            match-constraints                 ; match-contraints
            match-constraint-predicate        ; match-constraint-predicate
            n)))                              ; n
      (hash-set! (current-inference-data-index)
                       (pattern-first pattern)
                       (cons match-node
                             (hash-ref (current-inference-data-index)
                                       (pattern-first pattern)
                                       (lambda () '()))))
      match-node)))

;;; (get-match-node successors
;;;                 matches
;;;                 left
;;;                 right
;;;                 join-constraint-variables
;;;                 join-constraints
;;;                 join-constraint-predicate
;;;                 existential?
;;;                 match-counts) -> join-node?
;;;   successors : (listof node?)
;;;   matches : (listof match?)
;;;   left : (or/c node? false/c)
;;;   right : node?
;;;   join-constraint-variables : (listof variable?)
;;;   join-constraints : (listof list?)
;;;   join-constraint-predicate : (or/c procedure? false/c)
;;;   existential? : (one-of/c #f 'no 'notany 'any 'notall 'all)
;;;   match-counts : counts?
;;; Returns a join node for the given join operation. This facilitates reuse
;;; within the rule network for efficiency.
(define (get-join-node successors
                       matches
                       left
                       right
                       join-constraint-variables
                       join-constraints
                       join-constraint-predicate
                       existential?
                       match-counts)
  (let/ec return
    ;; Search for an equivalent join node and return it if found.
    (for-each
     (lambda (join-node)
       (when (and (join-node? join-node)
                  (eq? left
                       (join-node-left join-node))
                  (eq? right
                       (join-node-right join-node))
                  (equal? join-constraint-variables
                          (join-node-join-constraint-variables join-node))
                  (equal? join-constraints
                          (join-node-join-constraints join-node))
                  (eq? existential?
                       (join-node-existential? join-node)))
         (return join-node)))
     (node-successors left))
    ;; No equivalent join mode found, so create a new one.
    (make-join-node
     successors
     matches
     left
     right
     join-constraint-variables
     join-constraints
     join-constraint-predicate
     existential?
     match-counts)))

;;; (link-nodes predecessor successor) -> void?
;;;   predecessor : node?
;;;   successor : node?
;;; Link nodes in a predecessor -> successor relationship.
(define (link-nodes predecessor successor)
  (when (not (memq successor (node-successors predecessor)))
    (set-node-successors!
     predecessor
     (cons successor (node-successors predecessor)))))

;;; (add-match-to-node-matches match node) -> void?
;;;   match : match?
;;;   node : node?
;;; Add a match to the list of matches for a node.
(define (add-match-to-node-matches match node)
  (when (node-matches node)
    (set-node-matches!
     node (mcons match (node-matches node)))))

;;; (remove-match-from-node-matches match node) -> void?
;;;   match : match?
;;;   node : node?
;;; Remove a match from the list of matches for a node. For now, assume we have
;;; the actual match and can use eq?.
(define (remove-match-from-node-matches match node)
  (let/cc exit
    (let loop ((previous #f)
               (matches (node-matches node)))
      (when (not (null? matches))
        (when (eq? match (mcar matches))
          (if previous
              (set-mcdr! previous (mcdr matches))
              (set-node-matches! node (mcdr matches)))
          (exit (void)))
        (loop matches (mcdr matches))))))

;;; (get-node-matches node bindings) -> (listof match?)
;;;   node : node?
;;;   bindings : bindings?
;;; Returns the matches for a node. If the node is a backward chaining match
;;; node, then initiate backward chaining.
(define (get-node-matches node bindings)
  (let ((matches (node-matches node)))
    (if matches
        matches
        (if (match-node? node)
            (check (pattern-substitute
                    (match-node-pattern node) bindings))
            #f))))

;;; -----------------------------------------------------------------------------
;;;                             Ruleset Activation
;;; -----------------------------------------------------------------------------

;;; (activate ruleset) -> void?
;;;   ruleset : ruleset?
;;; Activate a ruleset. Builds the rule network for a ruleset by activating all
;;; of its rules.
(define (activate ruleset)
  ;; The initial join nodes is used for all data nodes.
  (let ((initial-join-node
         (make-join-node
          '()                          ; successors
          (mlist '(()))                     ; matches
          #f                           ; left
          #f                           ; right
          '()                          ; join-constraint-variables
          '()                          ; join-constraints
          #f                           ; join-constraint-predicate
          #f                           ; existential?
          (make-counts))))             ; match-counts
    ;; Activate all of the ruleset rules.
    (for-each
     (lambda (rule)
       (activate-rule rule initial-join-node))
     (ruleset-rules ruleset)))
  (fix-goal-matches))

;;; (activate-rule rule initial-node) -> void
;;;   rule : rule?
;;;   initial-node : join-node?
;;; Activate a data rule.  Build the rule network for a rule by creating and
;;; linking nodes.  There is one match node and one join node per precondition
;;; clause and one rule node per rule.
(define (activate-rule rule initial-node)
  (let ((match-node #f)
        (join-node #f)
        (rule-node #f)
        (previous-node initial-node)
        (previous-variable-list '()))
    ;; Create goal match node (goal nodes only).
    (when (not (null? (rule-goals rule)))
      (let ((goal-pattern (car (rule-goals rule))))
        (set! previous-node
              (make-match-node
               '()                     ; successors
               #f                      ; matches
               #f                      ; assertion-variable
               goal-pattern            ; pattern
               '()                     ; match-constraint-variables
               '()                     ; match-constraints
               #f                      ; match-constraint-predicate
               0                       ; n
               ))
        (hash-set!
         (current-inference-goal-index)
         (pattern-first goal-pattern)
         (cons previous-node
               (hash-ref
                (current-inference-goal-index)
                (pattern-first goal-pattern)
                (lambda () '()))))
        (set! previous-variable-list
              (pattern-variables goal-pattern))))
    ;; Process precondition clauses
    (for-each
     (lambda (clause)
       (let ((existential? #f)
             (assertion-variable #f)
             (pattern #f)
             (variable-list '()))
         ;; Parse clause
         (cond ((and (pair? clause)
                     (variable? (car clause)))
                (set! assertion-variable (car clause))
                (set! pattern (caddr clause)))
               ((and (pair? clause)
                     (memq (car clause) '(no notany any notall all)))
                (set! existential? (car clause))
                (set! pattern (cadr clause)))
               (else
                (set! pattern clause)))
         ;; Add pattern variables to the variable list
         (set! variable-list
               (merge-variable-lists
                previous-variable-list
                (if assertion-variable
                    (cons assertion-variable (pattern-variables pattern))
                    (pattern-variables pattern))))
         ;; Get match constraints
         (let ((match-constraints
                (pattern-match-constraints
                 pattern (pattern-variables pattern))))
           ;; Make match node and add to index
           (set! match-node
                 (get-match-node
                  '()                      ; successors
                  '()                      ; matches
                  assertion-variable       ; assertion-variable
                  (pattern-base-pattern pattern) ; pattern
                  (if assertion-variable   ; match-constraint-variables
                      (cons assertion-variable (pattern-variables pattern))
                      (pattern-variables pattern))
                  match-constraints        ; match-contraints
                  (if (null? match-constraints) ; match-constraint-predicate
                      #f
                      (eval
                       `(lambda ,(if assertion-variable
                                     (cons assertion-variable
                                           (pattern-variables pattern))
                                     (pattern-variables pattern))
                          (and ,@match-constraints))
                       (namespace-anchor->namespace anchor)))
                  0)))                      ; n
         ;; Make join node and link from match-node
         (let ((join-constraints
                (pattern-join-constraints
                 pattern (pattern-variables pattern))))
           (set! join-node
                 (get-join-node
                  '()                    ; successors
                  (if (null? (rule-goals rule))
                      (if (or (memq existential? '(no notany))
                              (eq? existential? 'all))
                          (node-matches previous-node)
                          '())       
                      #f)                ; matches
                  previous-node          ; left
                  match-node             ; right
                  variable-list          ; join-constraint-variables
                  join-constraints       ; join-constraints
                  (if (null? join-constraints); constraint-predicate
                      #f
                      (eval `(lambda ,variable-list
                               (and ,@join-constraints))
                            (namespace-anchor->namespace anchor)))
                  existential?          ; existential?
                  (make-counts))))   ; match-counts
         (link-nodes match-node join-node)
         ;; Link from previous join-node, if any
         (link-nodes previous-node join-node)
         (set! previous-node join-node)
         ;; Update previous-variable-list for non existentials
         (when (not existential?)
           (set! previous-variable-list variable-list))))
     (rule-preconditions rule))
    ;; Create rule node, link to it from the last join node, and add it
    (set! rule-node
          (make-rule-node
           '()                           ; successors
           (if (null? (rule-goals rule))
               (node-matches previous-node)
               #f)                       ; matches
           rule                          ; rule
           previous-node                 ; join
           (if (not (rule-actions rule))
               #f
               (eval `(lambda ,previous-variable-list
                         (begin ,@(rule-actions rule)))
                     (namespace-anchor->namespace anchor)))
           ; action
           ))
    (link-nodes previous-node rule-node)
    (current-inference-rule-nodes
     (append! (current-inference-rule-nodes)
              (list rule-node))))
  (void))

;;; (fix-goal-matches) -> void?
;;; Locate goal match nodes (i.e. any match node where the first symbol of its
;;; pattern matches a goal-directed rule) and mark them as such by setting their
;;; matches field to #f.
(define (fix-goal-matches)
  (hash-for-each
   (current-inference-data-index)
   (lambda (key value)
     (when (hash-ref (current-inference-goal-index) key
                     (lambda () #f))
       (for-each
        (lambda (match-node)
          (set-node-matches! match-node #f))
        value)
       (hash-remove! (current-inference-data-index) key)))))

;;; (merge-variable-lists list1 list2) -> (listof variable?)
;;;   list1 : (listof variable?)
;;;   list2 : (listof variable?)
;;; Merge two lists of variables maintaining the order.
(define (merge-variable-lists list1 list2)
  (cond ((null? list2)
         list1)
        ((memq (car list2) list1)
         (merge-variable-lists list1 (cdr list2)))
        (else
         (merge-variable-lists
          (append list1 (list (car list2))) (cdr list2)))))

;;; -----------------------------------------------------------------------------
;;;                           Match Node Processing
;;; -----------------------------------------------------------------------------

;;; (match-node-assert match-node assertion) -> void?
;;;   match-node : match-node?
;;;   assertion : assertion?
;;; Match an assertion against the pattern in a match node. If there is a match,
;;; update the matches for the node and propagate the match to the join node(s).
(define (match-node-assert match-node assertion)
  (set-match-node-n! match-node (+ (match-node-n match-node) 1))
  (let ((bindings (pattern-unify
                   (assertion-fact assertion)
                   (match-node-pattern match-node) (make-bindings))))
    (when bindings
      ;; Add assertion variable, if any.
      (when (match-node-assertion-variable match-node)
        (set! bindings
              (cons
               (cons (match-node-assertion-variable match-node)
                     assertion)
               bindings)))
      ;; Check match constraints
      (when (and (match-node-match-constraint-predicate match-node)
                 (not (apply (match-node-match-constraint-predicate match-node)
                             (bindings-values bindings))))
        (set! bindings #f)))
    (if bindings
        ;; Build and propagate match to successors
        (let ((match (cons (list assertion) bindings)))
          (add-match-to-node-matches match match-node)
          (for-each
           (lambda (successor)
             (propagate-match-from-match-node match successor))
           (node-successors match-node)))
        (for-each
         (lambda (successor)
           (propagate-nonmatch-from-match-node successor))
         (node-successors match-node)))))

;;; (match-node-retract match-node assertion) -> void?
;;;   match-node : match-node?
;;;   assertion : assertion?
;;; Retract an assertion from a match node and propagate the retraction through
;;; the rule network.
(define (match-node-retract match-node assertion)
  (set-match-node-n! match-node (- (match-node-n match-node) 1))
  (let/cc exit
    (let loop ((previous #f)
               (matches (node-matches match-node)))
      (when (not (null? matches))
        (let ((match (mcar matches)))
          (when (eq? assertion (caar match))
            ;; Remove the match
            (if previous
                (set-mcdr! previous (mcdr matches))
                (set-node-matches! match-node (mcdr matches)))
            (for-each
             (lambda (successor)
               (unpropagate-match-from-match-node match successor))
             (node-successors match-node))
            (exit (void))))
        (loop matches (mcdr matches))))
    ;; Match not found
    (for-each
     (lambda (successor)
       (unpropagate-nonmatch-from-match-node successor))
     (node-successors match-node))))

;;; (match-node-check match-node assertion continuation) -> void?
;;;   match-node : match-node?
;;;   assertion : assertion?
;;;   continuation : procedure?
;;; Perform backward chaining from a match node.
(define (match-node-check match-node assertion continuation)
  (let/ec exit
    (let ((bindings (pattern-unify
                     (assertion-fact assertion)
                     (match-node-pattern match-node) '())))
      (when bindings
        ;; Add assertion variable, if any.
        (when (match-node-assertion-variable match-node)
          (set! bindings
                (cons
                 (cons (match-node-assertion-variable match-node)
                       assertion)
                 bindings)))
        ;; Check match constraints
        (when (and (match-node-match-constraint-predicate match-node)
                   (not (apply (match-node-match-constraint-predicate match-node)
                               (bindings-values bindings))))
          (exit (void)))
        ;; Build and propagate match to successors
        (let ((match (cons (list assertion) bindings)))
          (add-match-to-node-matches match match-node)
          (for-each
           (lambda (successor)
             (propagate-match-from-join-node match successor continuation))
           (node-successors match-node)))))
    '()))

;;; -----------------------------------------------------------------------------
;;;                     Match Propagation and Unpropagation
;;; -----------------------------------------------------------------------------

;;; -----------------------------------------------------------------------------
;;;                       Propagate Match (or Non-Match)
;;; -----------------------------------------------------------------------------

;;; (propagate-match-from-match-node match join-node) -> void?
;;;   match : match?
;;;   join-node : join-node?
;;; Propagate a match from a match node. This is called when a match node propagates
;;; a match in response to an assertion.
(define (propagate-match-from-match-node match join-node)
  (when (node-matches join-node)
    (mfor-each
     (lambda (left-match)
       (let ((joined-match (join left-match match join-node)))
         (if (join-node-existential? join-node)
             ;; Existential join node
             (let ((count (counts-table-value
                           (join-node-match-counts join-node) left-match)))
               ;; Update the count (either stay the same or go up by 1)
               (when joined-match
                 (set! count (+ count 1))
                 (counts-table-increment!
                  (join-node-match-counts join-node) left-match))
               (case (join-node-existential? join-node)
                 ((no notany)
                  (when (and joined-match
                             (= count 1)) ; count went from 0 to 1
                    (unpropagate-match-to-successors left-match join-node)))
                 ((any)
                  (when (and joined-match
                             (= count 1)) ; count went from 0 to 1
                    (propagate-match-to-successors left-match join-node #f)))
                 ((notall)
                  (let ((n (match-node-n (join-node-right join-node))))
                    (when (and (not joined-match)
                               (= count (- n 1)))
                      ;; count went from n to n-1
                      ;;(because n went up by 1, but the count didn't)
                      (propagate-match-to-successors left-match join-node #f))))
                 ((all)
                  (let ((n (match-node-n (join-node-right join-node))))
                    (when (and (not joined-match)
                               (= count (- n 1)))
                      ;; count went from n to n-1
                      ;;(because n went up by 1, but the count didn't)
                      (unpropagate-match-to-successors left-match join-node))))))
             ;; Binding join node
             (when joined-match
               (propagate-match-to-successors joined-match join-node #f)))))
     (get-node-matches (join-node-left join-node) (car match)))))

;;; (propagate-nonmatch-from-match-node join-node) -> void?
;;;   join-node : join-node?
;;; Propagate a nonmatch from a match node. This is used to update notall and all
;;; existential matches.
(define (propagate-nonmatch-from-match-node join-node)
  (when (node-matches join-node)
    (mfor-each
     (lambda (left-match)
       (when (join-node-existential? join-node)
         (let ((count (counts-table-value
                       (join-node-match-counts join-node) left-match)))
           (case (join-node-existential? join-node)
             ((notall)
              (let ((n (match-node-n (join-node-right join-node))))
                (when (= count (- n 1))
                  (propagate-match-to-successors left-match join-node #f))))
             ((all)
              (let ((n (match-node-n (join-node-right join-node))))
                (when (= count (- n 1))
                  (unpropagate-match-to-successors left-match join-node))))))))
     (get-node-matches (join-node-left join-node) '()))))

;;; (propagate-match-from-join-node match join-node continuation) -> void
;;;   match : match?
;;;   join-node : join-node?
;;;   continuation : procedure?
;;; Propagate a match from one join node to a successor join node.
(define (propagate-match-from-join-node match join-node continuation)
  (if (join-node-existential? join-node)
      ;; Existential join node
      (let ((count 0))
        (mfor-each
         (lambda (right-match)
           (let ((joined-match (join match right-match join-node)))
             (when joined-match
               (set! count (+ count 1)))))
         (node-matches (join-node-right join-node)))
        ;; Add match to match counts
        (set-counts-table-value!
         (join-node-match-counts join-node) match count)
        ;; Propagate match based on existential type
        (case (join-node-existential? join-node)
          ((no notany)
           (when (= count 0)
             (propagate-match-to-successors match join-node continuation)))
          ((any)
           (when (> count 1)
             (propagate-match-to-successors match join-node continuation)))
          ((notall)
           (let ((n (match-node-n (join-node-right join-node))))
             (when (< count n)
               (propagate-match-to-successors match join-node continuation))))
          ((all)
           (let ((n (match-node-n (join-node-right join-node))))
             (when (= count n)
               (propagate-match-to-successors match join-node continuation))))))
      ;; Binding join node
      (mfor-each
       (lambda (right-match)
         (let ((joined-match (join match right-match join-node)))
           (when joined-match
             (propagate-match-to-successors joined-match join-node continuation))))
       (get-node-matches (join-node-right join-node) (cdr match)))))

;;; (propagate-match-to-successors match join-node continuation) -> void?
;;;   match : match?
;;;   join-node : join-node?
;;;   continuation : procedure?
;;; Propagate a match from a join node to its successor nodes.
(define (propagate-match-to-successors match join-node continuation)
  ;; Add match to node matches
  (add-match-to-node-matches match join-node)
  ;; Propagate match to successor nodes
  (for-each
   (lambda (successor)
     (if (join-node? successor)
         (propagate-match-from-join-node match successor continuation)
         (propagate-match-to-rule match successor continuation)))
   (node-successors join-node)))

;;; (propagate-match-to-rule match rule-node continuation) -> void?
;;;   match : match?
;;;   rule-node : rule-node?
;;;   continuation : procedure?
;;; Propagate a match from a join node to a successor rule node.
(define (propagate-match-to-rule match rule-node continuation)
  ;(add-match-to-node-matches match rule-node) ???
  (if (node-matches rule-node)
      (let ((rule-instance
             (make-rule-instance rule-node match 0)))
        (agenda-add! rule-instance))
      (begin
        (when (current-inference-trace)
          (printf "<== ~s: ~s~n"
                  (rule-node-rule rule-node)
                  (cdr match)))
        (when (rule-node-action rule-node)
          (current-inference-rule (rule-node-rule))
          (apply (rule-node-action rule-node)
                 (bindings-values match))
          (current-inference-rule #f)
          (current-inference-rules-fired
           (+ (current-inference-rules-fired) 1)))
        (continuation (mlist (cons (list (caar match)) (cdr match)))))))

;;; -----------------------------------------------------------------------------
;;;                            Unpropagate Matches
;;; -----------------------------------------------------------------------------

;;; (unpropagate-match-from-match-node match join-node) -> void?
;;;   match : match?
;;;   join-node : join-node?
;;; Unpropagate a match from a match node. This is called when a match node
;;; unpropagates a match in response to a retraction.
(define (unpropagate-match-from-match-node match join-node)
  (if (join-node-existential? join-node)
      ;; Existential node
      (mfor-each
       (lambda (left-match)
         (let ((count (counts-table-value
                       (join-node-match-counts join-node) left-match))
               (joined-match (join left-match match join-node)))
           (when joined-match
             (set! count (- count 1))
             (set-counts-table-value!
              (join-node-match-counts join-node) left-match count))
           (case (join-node-existential? join-node)
             ((no notany)
              (when (and joined-match
                         (= count 0))   ; count went from 1 to 0
                (propagate-match-to-successors left-match join-node #f)))
             ((any)
              (when (and join-node
                         (= count 0))   ; count went from 1 to 0
                (unpropagate-match-to-successors left-match join-node)))
             ((notall)
              (let ((n (match-node-n (join-node-right join-node))))
                (when (and (not joined-match)
                           (= count n))
                  ;  count went from n-1 to n
                  (unpropagate-match-to-successors left-match join-node))))
             ((all)
              (let ((n (match-node-n (join-node-right join-node))))
                (when (and (not joined-match)
                           (= count n))
                  ;  count went from n-1 to n
                  (propagate-match-to-successors left-match join-node #f)))))))
       (node-matches (join-node-left join-node)))
      ;; Binding join node
      (let ((assertion (caar match)))
        (mfor-each
         (lambda (match)
           (when (eq? assertion
                      (last (car match)))
             (unpropagate-match-to-successors match join-node)))
         (node-matches join-node)))))

;;; (unpropagate-nonmatch-from-match-node join-node) -> void?
;;;   join-node : join-node?
;;; Unpropagate a nonmatch (from a retraction). This is needed to update notall
;;; and all existential matches.
(define (unpropagate-nonmatch-from-match-node join-node)
  (when (join-node-existential? join-node)
    (mfor-each
     (lambda (left-match)
       (let ((count (counts-table-value
                     (join-node-match-counts join-node) left-match)))
         (case (join-node-existential? join-node)
           ((notall)
            (let ((n (match-node-n (join-node-right join-node))))
              (when (= count n)
                (unpropagate-match-to-successors left-match join-node))))
           ((all)
            (let ((n (match-node-n (join-node-right join-node))))
              (when (= count n)
                (propagate-match-to-successors left-match join-node #f)))))))
     (node-matches (join-node-left join-node)))))

;;; (unpropagate-match-from-join-node match join-node) -> void?
;;;   match : match?
;;;   join-node : join-node?
;;; Unpropagate a match from a join node.
;;; For each of the matches for the node:
;;;   When the given match is a subset of the node match:
;;;     Remove the node match and unpropagate it to its successors.
(define (unpropagate-match-from-join-node match join-node)
  ;; Remove the count
  (hash-remove!
   (counts-table (join-node-match-counts join-node)) match)
  ;; Unpropagate match to successors
  (mfor-each 
   (lambda (node-match)
     (when (match-subset? match node-match)
       (unpropagate-match-to-successors node-match join-node)))
   (node-matches join-node)))

;;; (unpropagate-match-to-successors match join-node) -> void?
;;;   match : match?
;;;   join-node : join-node?
;;; Unpropagate a match from a join node to its successors.
(define (unpropagate-match-to-successors match join-node)
  ;; Remove the match from the node matches.
  (remove-match-from-node-matches match join-node)
  ;; Unpropagate match to successor nodes.
  (for-each
   (lambda (successor)
     (if (join-node? successor)
         (unpropagate-match-from-join-node match successor)
         (unpropagate-match-to-rule match successor)))
   (node-successors join-node)))

;;; (unpropagate-match-to-rule match rule-node) -> void?
;;;   match : match?
;;;   rule-node : rule-node?
;;; Unpropagate a match from a join node to a successor rule node.
(define (unpropagate-match-to-rule match rule-node)
  (agenda-remove! rule-node match))

;;; -----------------------------------------------------------------------------
;;;                                    Join
;;; -----------------------------------------------------------------------------

;;; (join left-match right-match join-node) -> (or/c (listof any/c) false/c)
;;;   left-match : match?
;;;   right-match : match?
;;;   join-node : join-node?
;;; Join two (left and right) matches. If the bindings are consistant and the
;;; constraints are met, join the matches and propagate the joined match to any
;;; successor (join or rule) nodes.
(define (join left-match right-match join-node)
  (let ((left-assertions (car left-match))
        (left-bindings (cdr left-match))
        (right-assertions (car right-match))
        (right-bindings (cdr right-match)))
    (let/cc return
      ;; Check that bindings match
      (for-each
       (lambda (right-binding)
         (when (assq (car right-binding) left-bindings)
           (when (not (equal? (cdr right-binding)
                              (cdr (assq (car right-binding) left-bindings))))
             (return #f))))
       right-bindings)
      ;; Add new bindings
      (let ((bindings left-bindings))
        (for-each
         (lambda (right-binding)
           (when (not (assq (car right-binding) left-bindings))
             (set! bindings (append bindings (list right-binding)))))
         right-bindings)
        ;; Check constraints
        (when (and (join-node-join-constraint-predicate join-node)
                   (not (apply (join-node-join-constraint-predicate join-node)
                               (bindings-values bindings))))
          (return #f))
        ;; Return match
        (cons (append left-assertions right-assertions) bindings)))))

;;; -----------------------------------------------------------------------------
;;;                               Rule Instances
;;; -----------------------------------------------------------------------------

;;; (struct rule-instance (rule-node match random))
;;;   rule-node : rule-node?
;;;   match : match?
;;;   random : real?
;;; A rule instance represents an instance of a rule for a given match.
(define-struct rule-instance (rule-node match random) #:mutable)

;;; (rule-instance-apply rule-instance) -> any
;;;   rule-instance : rule-instance?
;;; Executes the rule action for the rule instance - i.e. fire the rule.
(define (rule-instance-apply rule-instance)
  (let* ((rule-node (rule-instance-rule-node rule-instance))
         (rule (rule-node-rule rule-node))
         (match (rule-instance-match rule-instance))
         (arguments (bindings-values (cdr match))))
    (when (current-inference-trace)
      (printf "==> ~s: ~s~n"
              rule (cdr match)))
    ;; Apply the rule
    (current-inference-rule rule)
    (apply (rule-node-action rule-node) arguments)
    (current-inference-rule #f)
    (current-inference-rules-fired
     (+ (current-inference-rules-fired) 1))))

;;; -----------------------------------------------------------------------------
;;;                               Agenda Maintenance
;;; -----------------------------------------------------------------------------

;;; (agenda-add! rule-instance) -> void?
;;;   rule-instance : rule-instance?
;;; Add a rule instance to the agenda in accordance with the current conflict
;;; resolution strategy.
(define (agenda-add! rule-instance)
  (let* ((rule-node (rule-instance-rule-node rule-instance))
         (rule (rule-node-rule rule-node))
         (priority (rule-priority rule)))
    (when (eq? (current-inference-strategy) 'random)
      (set-rule-instance-random! rule-instance (random)))
    (let ((agenda-tail (current-inference-agenda))
          (previous #f))
      (let loop ()
        (when (not (null? agenda-tail))
          (let* ((item (mcar agenda-tail))
                 (item-rule-node (rule-instance-rule-node item))
                 (item-rule (rule-node-rule item-rule-node))
                 (item-priority (rule-priority item-rule)))
            (cond ((> item-priority priority)
                   (set! previous agenda-tail)
                   (set! agenda-tail (mcdr agenda-tail))
                   (loop))
                  ((and (= item-priority priority)
                        (or (eq? (current-inference-strategy) 'breadth)
                            (and (eq? (current-inference-strategy) 'order)
                                 (< (rule-order item-rule)
                                    (rule-order rule)))
                            (and (eq? (current-inference-strategy) 'simplicity)
                                 (< (rule-specificity item-rule)
                                    (rule-specificity rule)))
                            (and (eq? (current-inference-strategy) 'complexity)
                                 (> (rule-specificity item-rule)
                                    (rule-specificity rule)))
                            (and (eq? (current-inference-strategy) 'random)
                                 (< (rule-instance-random item)
                                    (rule-instance-random rule-instance)))))
                   (set! previous agenda-tail)
                   (set! agenda-tail (mcdr agenda-tail))
                   (loop))
                  (else
                   (void))))))
      (if previous
          (set-mcdr! previous (mcons rule-instance agenda-tail))
          (current-inference-agenda (mcons rule-instance agenda-tail))))))

;;; (agenda-remove! rule-node match) -> void?
;;;   rule-node : rule-node?
;;;   match : match?
;;; Remove the rule instance for the given rule-node and match from the agenda.
(define (agenda-remove! rule-node match)
  (let loop ((previous #f)
             (agenda-tail (current-inference-agenda)))
    (when (not (null? agenda-tail))
      (let ((item (mcar agenda-tail)))
        (if (and (eq? rule-node (rule-instance-rule-node item))
                 (eq? match (rule-instance-match item)))
            (if previous
                (set-mcdr! previous (mcdr agenda-tail))
                (current-inference-agenda (mcdr agenda-tail)))
            (loop agenda-tail (mcdr agenda-tail)))))))

;;; -----------------------------------------------------------------------------
;;;                     Hierarchical Inference Environments
;;; -----------------------------------------------------------------------------

;;; (import pattern) -> void?
;;;   pattern : pattern?
;;; Import assertions from the parent environment matching the given pattern.
(define (import pattern)
  (when (not (current-inference-parent))
    (error 'import
           "Current inference environment is not a child environment"))
  (let ((matches '()))
    (with-inference-environment (current-inference-parent)
      (set! matches (query pattern)))
    (mfor-each
     (lambda (match)
       (assert (assertion-fact (car match))))
     matches)))

;;; (export pattern) -> void?
;;;   pattern : pattern?
;;; Exports assertions matching the given pattern to the parent environment.
(define (export pattern)
  (when (not (current-inference-parent))
    (error 'import
           "Current inference environment is not a child environment"))
  (let ((matches (query pattern)))
    (with-inference-environment (current-inference-parent)
      (mfor-each
       (lambda (match)
         (assert (assertion-fact (car match))))
       matches))))

;;; (copy-inference-environment source) -> inference-environment?
;;;   source : inference-environment?
;;; Return a copy of the source inference environment. This is used in assumption
;;; processing.
(define (copy-inference-environment source)
  (let ((target (make-inference-environment))
        (hash-table (make-hash)))
    ;; Copy rule network nodes from the rule nodes.
    (for-each
     (lambda (rule-node)
       (copy-rule-node rule-node hash-table))
     (inference-environment-rule-nodes source))
    ;; Copy data index
    (let ((target-data-index (inference-environment-data-index target)))
      (hash-for-each
       (inference-environment-data-index source)
       (lambda (key value)
         (hash-set!
          target-data-index
          key (copy-match-node-list value hash-table)))))
    ;; Clone goal index
    (let ((target-goal-index (inference-environment-goal-index target)))
      (hash-for-each
       (inference-environment-goal-index source)
       (lambda (key value)
         (hash-set!
          target-goal-index
          key (copy-match-node-list value hash-table)))))
    ;; Rule nodes will be cloned via the rule network cloning
    ;; Copy exit continuation
    (set-inference-environment-exit!
     target (inference-environment-exit source))
    ;; Copy next assertion id
    (set-inference-environment-next-assertion-id!
     target (inference-environment-next-assertion-id source))
    ;; Copy the assertion index
    (let ((target-assertion-index
           (inference-environment-assertion-index target)))
      (hash-for-each
       (inference-environment-assertion-index source)
       (lambda (key value)
;         (hash-set!
;          target-assertion-index key (list-copy value))
         (let ((new-level-2-index (make-hasheq)))
           (hash-for-each
            value
            (lambda (key value)
              (hash-set!
               new-level-2-index key value)))
           (hash-set!
            target-assertion-index key new-level-2-index))
         )
       ))
    ;; Copy the agenda
    (set-inference-environment-agenda!
     target (mlist-copy (inference-environment-agenda source)))
    ;; Copy the rule
    (set-inference-environment-rule!
     target (inference-environment-rule source))
    ;; Copy the strategy
    (set-inference-environment-strategy!
     target (inference-environment-strategy source))
    ;; Copy the parent
    (set-inference-environment-parent!
     target (inference-environment-parent source))
    ;; Return the target
    target))

;;; (copy-node node hash-table) -> node?
;;;   node : node?
;;;   hash-table : hash?
;;; Copies a node by dispatching to the appropriate copier.
(define (copy-node node hash-table)
  (cond ((match-node? node)
         (copy-match-node node hash-table))
        ((join-node? node)
         (copy-join-node node hash-table))
        ((rule-node? node)
         (copy-rule-node node hash-table))
        (else
         (error 'copy-node
                "~s is not a type of rule network node" node))))

;;; (copy-match-node match-node hash-table) -> match-node?
;;;    match-node : match-node?
;;;    hash-table : hash?
;;; Returns a copy of the match node, or a reference to an existing copy.
(define (copy-match-node match-node hash-table)
  (let ((copied-node (hash-ref hash-table match-node #f)))
    (unless copied-node
      (set! copied-node
            (make-match-node
             '()
             (if (node-matches match-node)
                 (mlist-copy (node-matches match-node))
                 #f)
             (match-node-assertion-variable match-node)
             (match-node-pattern match-node)
             (match-node-match-constraint-variables match-node)
             (match-node-match-constraints match-node)
             (match-node-match-constraint-predicate match-node)
             (match-node-n match-node)))
      (hash-set! hash-table match-node copied-node))
    copied-node))

;;; (copy-join-node join-node hash-table) -> join-node?
;;;    join-node : join-node?
;;;    hash-table : hash?
;;; Returns a copy of the join node, or a reference to an existing copy.
(define (copy-join-node join-node hash-table)
  (let ((copied-node (hash-ref hash-table join-node #f)))
    (unless copied-node
      (set! copied-node
            (make-join-node
             '()
             (if (node-matches join-node)
                 (list-copy (node-matches join-node))
                 #f)
             (copy-node (join-node-left join-node) hash-table)
             (copy-node (join-node-right join-node) hash-table)
             (join-node-join-constraint-variables join-node)
             (join-node-join-constraints join-node)
             (join-node-join-constraint-predicate join-node)
             (join-node-existential? join-node)
             ;(alist-copy (join-node-match-counts join-node))
             (make-counts)
             ))
      ;;
      (set-counts-table!
       (join-node-match-counts copied-node)
       (hash-copy (counts-table join-node)))
      ;;
      (link-nodes (join-node-left copied-node) copied-node)
      (link-nodes (join-node-right copied-node) copied-node)
      (hash-set! hash-table join-node copied-node))
    copied-node))

;;; (copy-rule-node rule-node hash-table) -> rule-node?
;;;    rule-node : rule-node?
;;;    hash-table : hash?
;;; Returns a copy of the rule node, or a reference to an existing copy.
(define (copy-rule-node rule-node hash-table)
  (let ((copied-node (hash-ref hash-table rule-node #f)))
    (unless copied-node
      (set! copied-node
            (make-rule-node
             '()
             (if (node-matches rule-node)
                 (list-copy (node-matches rule-node))
                 #f)
             (rule-node-rule rule-node)
             (copy-node (rule-node-join rule-node) hash-table)
             (rule-node-action rule-node)))
      (link-nodes (rule-node-join rule-node) copied-node)
      (hash-set! hash-table rule-node copied-node))
    copied-node))

;;; (copy-match-node-list match-node-list hash-table) -> (listof match-node?)
;;;    match-node-list : (listof match-node?)
;;; Clone a list of match nodes using the given hash table.
;;; MDW - check what this is about.  I don't remember what it's for and the 
;;; implementation seems naive, if not plain wrong.
(define (copy-match-node-list match-node-list hash-table)
  match-node-list)

;;; -----------------------------------------------------------------------------
;;;                              Inference Control
;;; -----------------------------------------------------------------------------

;;; (start-inference) -> any
;;; The inference engine main loop.
(define (start-inference #:assert-start (assert-start? #t))
  (when assert-start?
    (assert '(start)))
  (let/cc exit
    ;; Set the global exit continuation.
    (current-inference-exit exit)
    ;; Simple selection - find the first rule instance
    (let loop ()
      (let ((agenda (current-inference-agenda)))
        (if (not (null? agenda))
            (let ((rule-instance (mcar agenda)))
              (current-inference-agenda (mcdr agenda))
              (rule-instance-apply rule-instance)
              (loop))
            #f)))))

;;; (stop-inference) -> any
;;; Stop the current inferencing and optionally return a value.
(define stop-inference
  (case-lambda
    ((return-value)
     ((current-inference-exit) return-value))
    (()
     ((current-inference-exit)))))

;;; (succeed) -> any
;;; Stop the current inferencing and return #t indicating success.
(define (succeed)
  (stop-inference #t))

;;; (fail) ->
;;; Stop the current inferencing and return #f indicating failure.
(define (fail)
  (stop-inference '#:fail))

;;; -----------------------------------------------------------------------------
;;;                        Helpful Debugging Functions
;;; -----------------------------------------------------------------------------

;;; (print-rule-network) -> any
;;; For debugging.
(define (print-rule-network)
  (for-each
   (lambda (rule-node)
     (printf "----------~n")
     (printf "Rule: ~a~n~n" (rule-name (rule-node-rule rule-node)))
     (print-join-node (rule-node-join rule-node)))
   (current-inference-rule-nodes)))

(define (print-join-node join-node)
  (when (join-node-left join-node)
    (if (join-node? (join-node-left join-node))
        (print-join-node (join-node-left join-node))
        (print-match-node (join-node-left join-node))))
  (when (join-node-right join-node)
    (print-match-node (join-node-right join-node)))
  (printf "join node: existential? = ~a~n" (join-node-existential? join-node))
  (printf "join-node: match-counts = ~a~n" (join-node-match-counts join-node))
  (printf "join node: matches = ~a~n~n" (node-matches join-node)))

(define (print-match-node match-node)
  (printf "match-node: pattern = ~a~n" (match-node-pattern match-node))
  (printf "match-node: matches = ~a~n~n" (node-matches match-node)))

;;;

(define the-node-dictionary #f)

(define (node-ref node)
  (let ((entry (hash-ref the-node-dictionary node #f)))
    (if entry
        entry
        (let ((ref 
               (cond ((match-node? node)
                      (symbol->string (gensym "match")))
                     ((join-node? node)
                      (symbol->string (gensym "join")))
                     ((rule-node? node)
                      (format "rule : ~a"
                              (rule-name (rule-node-rule node)))))))
          (hash-set! the-node-dictionary node ref)
          ref))))

(define (graph-rule-network)
  (set! the-node-dictionary (make-hasheq))
  (let ((port (open-output-file "rule-network.dot"
                                #:mode 'text
                                #:exists 'replace)))
    (fprintf port "digraph \"rule-network\" {~n")
    (fprintf port "  rankdir=LR;~n")
    (for ((rule-node (in-list (current-inference-rule-nodes))))
      (let ((rule-node-ref (node-ref rule-node)))
        (fprintf port "  \"~a\";~n" rule-node-ref)
        (graph-join-node port (rule-node-join rule-node) rule-node-ref)))
    (fprintf port "}~n")
    (close-output-port port)))

(define (graph-join-node port join-node successor-ref)
  (let ((join-node-ref (node-ref join-node)))
    (fprintf port "  \"~a\" [label=\"join~a\\l~a\"];~n"
             join-node-ref
             (if (join-node-existential? join-node)
                 (format " : ~a" (join-node-existential? join-node))
                 "")
             (for/fold ((constraints-string ""))
               ((constraint (in-list (join-node-join-constraints join-node))))
               (string-append
                constraints-string
                (format "~s\\l" constraint))))
    (fprintf port "  \"~a\" -> \"~a\";~n"
             join-node-ref successor-ref)
    (when (join-node-right join-node)
      (graph-match-node port (join-node-right join-node) join-node-ref))
    (when (join-node-left join-node)
      (if (join-node? (join-node-left join-node))
          (graph-join-node port (join-node-left join-node) join-node-ref)
          (graph-match-node port (join-node-left join-node) join-node-ref)))
    ))

(define (graph-match-node port match-node successor-ref)
  (let ((match-node-ref (node-ref match-node)))
    (fprintf port "  \"~a\" [shape=box,label=\"match : ~a\\l~a\"];~n"
             match-node-ref
             (match-node-pattern match-node)
             (for/fold ((constraints-string ""))
               ((constraint (in-list (match-node-match-constraints match-node))))
               (string-append
                constraints-string
                (format "~s\\l" constraint))))
    (fprintf port "  \"~a\" -> \"~a\";~n"
             match-node-ref successor-ref)))

;;; -----------------------------------------------------------------------------
;;;                               Module Contracts
;;; -----------------------------------------------------------------------------

;(provide/contract
; (assert
;  (->* (fact?) (any/c) (or/c assertion? false/c)))
; (retract
;  (-> assertion? void?))
; (replace
;  (->* (assertion? fact?) (any/c) assertion?))
; (check
;  (-> fact? (mlistof match?)))
;; (query
;;  (-> pattern? (mlistof match?)))
; (query
;  (-> pattern? any))
; (assume
;  (-> fact? void?))
; (activate
;  (-> ruleset? void?))
; (start-inference
;  (-> any))
; (stop-inference
;  (case->
;   (-> any/c any)
;   (-> any)))
; (succeed
;  (-> any))
; (fail
;  (-> any))
; (graph-rule-network
;  (-> any))
; )

(provide (all-defined-out))