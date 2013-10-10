#lang scheme
;;; PLT Scheme Inference Collection
;;; facts.ss
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
;;; -----------------------------------------------------------------------------
;;;
;;; Counts encapsulate a hash table that maintains the number of matches for an
;;; existential pattern.
;;;
;;; Counts does not use any list operations.
;;;
;;; Version  Date      Description
;;; 2.0.0    06/26/08  Changes for V4.0. In particular, changes to the hash table
;;;                    interface. (Doug Williams)
;;; 2.0.1    07/02/08  Updated header. (Doug Williams)
;;; 2.0.2    12/25/08  Added module contracts and cleaned up the code. (Doug
;;;                    Williams)

;;; -----------------------------------------------------------------------------
;;;                                Counts Table
;;; -----------------------------------------------------------------------------

;;; (struct counts (table))
;;;   table : hash?
;;; A structure to maintain match counts for existential patterns. The keys are
;;; the actual matches for existential pattern, so - for now - equal? hash tables
;;; are used.
;;; To do:
;;;  - Check if an eq? hash tables will work.  The will if the matches cannot
;;;    change for an existential pattern.
(define-values (struct:counts
                counts-constructor
                counts?
                counts-field-ref
                set-counts-field!)
  (make-struct-type 'counts #f 1 0))

;;; table field
(define counts-table
  (make-struct-field-accessor
   counts-field-ref 0 'table))

(define set-counts-table!
  (make-struct-field-mutator
   set-counts-field! 0 'table))

;;; (make-counts) -> counts?
;;; Returns a new, empty counts table.
(define (make-counts)
  (counts-constructor (make-hash)))

;;; (counts-table-value counts index) -> exact-nonnegative-integer?
;;; Returns the match count for an existential pattern.
(define (counts-table-value counts index)
  (hash-ref (counts-table counts) index 0))

;;; (set-counts-table-value! counts index value) -> void?
;;; Sets the match count for an existential pattern.
(define (set-counts-table-value! counts index value)
  (hash-set! (counts-table counts) index value))

;;; (counts-table-increment! counts index) -> void?
;;; Increments the match count for an existential pattern by one.
(define (counts-table-increment! counts index)
  (set-counts-table-value!
   counts index
   (+ (counts-table-value counts index) 1)))

;;; (counts-table-decrement! counts index) -> void?
;;; Decrements the match count for an existential pattern by one.
(define (counts-table-decrement! counts index)
  (set-counts-table-value!
   counts index
   (- (counts-table-value counts index) 1)))

;;; -----------------------------------------------------------------------------
;;;                               Module Contracts
;;; -----------------------------------------------------------------------------

;(provide/contract
;  (counts?
;   (-> any/c boolean?))
;  (make-counts
;   (-> counts?))
;  (counts-table
;   (-> counts? hash?))
;  (set-counts-table!
;   (-> counts? hash? void?))
;  (counts-table-value
;   (-> counts? any/c exact-integer?))
;  (set-counts-table-value!
;   (-> counts? any/c exact-integer? void?))
;  (counts-table-increment!
;   (-> counts? any/c void?))
;  (counts-table-decrement!
;   (-> counts? any/c void?)))

(provide (all-defined-out))