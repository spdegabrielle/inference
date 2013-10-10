#lang racket
;;; PLT Scheme Inference Collection
;;; assertions.rkt
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
;;; Assertions represent facts in the knowledge base.  Each has a uniqie id, the
;;; fact itself, and a reason (source would have been a better term).
;;;
;;; The only use of list operation is for assertions-subset? and are
;;; verified to be immutable.
;;;
;;; Version  Date      Comments
;;; 1.0.1    07/22/06  Added custom printing for assertion structs. (Doug
;;;                    Williams)
;;; 2.0.0    06/25/08  Changes for V4.0. (Doug Williams)
;;; 2.0.1    07/02/08  Verified immutability of lists. Updated header. (Doug
;;;                    Williams)
;;; 2.0.2    12/25/08  Added module contracts and other code cleanup.  (Doug
;;;                    Williams)
;;; 2.0.3    10/01/11  Changed assertion print to truncate to (error-print-width)
;;;                    characters. (MDW)

(require "inference-environments.ss"
         "facts.ss")

;;; -----------------------------------------------------------------------------
;;;                                 Assertions
;;; -----------------------------------------------------------------------------

;;; (assertion-print assertion port write?) -> any/c
;;; Prints a readable form of an assertion to the specified port. This is used as
;;; custom write procedure for assertions.
(define (assertion-print assertion port write?)
  (when write? (write-string "<" port))
  (if write?
      (fprintf port "assertion-~a: ~.s"
               (assertion-id assertion)
               (assertion-fact assertion))
      (fprintf port "assertion-~a: ~.s"
               (assertion-id assertion)
               (assertion-fact assertion)))
  (when (assertion-reason assertion)
    (fprintf port " : ~.s"
             (assertion-reason assertion)))
  (when write? (write-string ">" port)))

;;; (struct assertion (id fact reason))
;;;   id : exact-nonnegative-integer?
;;;   fact : fact?
;;;   reason : any/c
;;; Each instance represents an assertion in the knowledge base. Uses make-
;;; struct-type to create the structure type to allow a special make-assertion
;;; function to be written.
(define-values (struct:assertion
                assertion-constructor
                assertion?
                assertion-field-ref
                set-assertion-field!)
  (make-struct-type 'assertion #f 3 0 #f
                    (list (cons prop:custom-write assertion-print))
                    (make-inspector)))

;;; id field
(define assertion-id
  (make-struct-field-accessor assertion-field-ref 0 'id))

(define set-assertion-id!
  (make-struct-field-mutator set-assertion-field! 0 'id))

;;; fact field
(define assertion-fact
  (make-struct-field-accessor assertion-field-ref 1 'fact))

(define set-assertion-fact!
  (make-struct-field-mutator set-assertion-field! 1 'fact))

;;; reason field
(define assertion-reason
  (make-struct-field-accessor assertion-field-ref 2 'reason))

(define set-assertion-reason!
  (make-struct-field-mutator set-assertion-field! 2 'reason))

;;; (make-assertion fact reason) -> assertion?
;;;   fact : any/c
;;;   reason : any/c
;;; Returns a new assertion with the specified fact and reason. The next id is
;;; used.
(define (make-assertion fact reason)
  (let ((assertion (assertion-constructor
                    (current-inference-next-assertion-id)
                    fact reason)))
    (current-inference-next-assertion-id
     (+ (current-inference-next-assertion-id) 1))
    assertion))

;;; (assertions-subset? assertions-1 assertions-2) -> boolean?
;;;   assertions-1 : (listof assertion?)
;;;   assertions-2 : (listof assertion?)
;;; Returns #t if the first list of assertions is a subset of the second. Used
;;; to implement match-subset?.
(define (assertions-subset? assertions-1 assertions-2)
  (cond ((null? assertions-1) #t)
        ((null? assertions-2) #f)
        ((not (eq? (car assertions-1) (car assertions-2))) #f)
        (else
         (assertions-subset? (cdr assertions-1) (cdr assertions-2)))))

;;; -----------------------------------------------------------------------------
;;;                               Module Contracts
;;; -----------------------------------------------------------------------------

;(provide/contract
; (assertion?
;  (-> any/c boolean?))
; (make-assertion
;  (-> fact? any/c assertion?))
; (assertion-id
;  (-> assertion? exact-nonnegative-integer?))
; (assertion-fact
;  (-> assertion? fact?))
; (set-assertion-fact!
;  (-> assertion? fact? void?))
; (assertion-reason
;  (-> assertion? any))
; (set-assertion-reason!
;  (-> assertion? any/c void?))
; (assertions-subset?
;  (-> (listof assertion?) (listof assertion?)  boolean?)))

(provide (all-defined-out))
