#lang racket
;;; PLT Scheme Inference Collection
;;; utilities.ss
;;; Copyright (c) 2010, M. Douglas Williams
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
;;; This module implements some small utilities to simplify interacting with the
;;; inference collection. Eventually, these should be part of the inference
;;; collection itself.

(require scheme/mpair
         "assertions.rkt"
         "inference-control.rkt")

;;; (query? pattern) -> boolean?
;;;   pattern : pattern?
;;; Returns #t if there are assertions mattching pattern.
(define (query? pattern)
  (not (null? (query pattern))))

;;; (sorted-query pattern) -> (listof assertion?)
;;;   pattern : pattern?
;;; Returns the results of a knowledge base query as an immutable list that is
;;; sorted by assertion-id. Therefore, the returned list list is chronologically
;;; ordered.
(define (sorted-query pattern)
  (sort (mlist->list (query pattern))
        (lambda (m1 m2)
          (< (assertion-id (car m1))
             (assertion-id (car m2))))))

(define (sorted-query* pattern)
  (sort (mlist->list (query* pattern))
        (lambda (m1 m2)
          (< (assertion-id (car m1))
             (assertion-id (car m2))))))

;;; (query-values pattern . defaults) -> any
;;;   pattern : pattern?
;;;   defaults : (listof any/c)
;;; Returns the bindings for a query as multiple values. If the query returns 
;;; zero values and defaults are provided, the defaults are returned as multiple
;;; values. If the query returns zero values and no defaults are provided or if
;;; the query returns more that one value, an error is returned.
(define (query-values pattern . defaults)
  (let ((results (mlist->list (query pattern))))
    (cond ((and (= (length results) 0)
                (> (length defaults) 0))
           (apply values defaults))
          ((= (length results) 1)
           (apply
            values
            (for/list ((association (in-list (cdar results))))
              (cdr association))))
          (else
           (error 'query-values
                  "expected one result for ~.s, got ~a"
                  pattern (length results))))))

;;; (query*-values pattern . defaults) -> any
;;;   pattern : pattern?
;;;   defaults : (listof any/c)
;;; Returns the bindings for a query as multiple values. If the query returns 
;;; zero values and defaults are provided, the defaults are returned as multiple
;;; values. If the query returns zero values and no defaults are provided or if
;;; the query returns more that one value, an error is returned.
(define (query*-values pattern . defaults)
  (let ((results (mlist->list (query* pattern))))
    (cond ((and (= (length results) 0)
                (> (length defaults) 0))
           (apply values defaults))
          ((= (length results) 1)
           (apply
            values
            (for/list ((association (in-list (cdar results))))
              (cdr association))))
          (else
           (error 'query*-values
                  "expected one result for ~.s, got ~a"
                  pattern (length results))))))

;;; (query-values* pattern . defaults) -> any
;;;   pattern : pattern?
;;;   defaults : (listof any/c)
;;; Returns the assertion and bindings for a query as multiple values. If the
;;; query returns zero values and defaults are provided, the defaults are
;;; returned as multiple values. If the query returns zero values and no defaults
;;; are provided or if the query returns more that one value, an error is
;;; returned.
(define (query-values* pattern . defaults)
  (let ((results (mlist->list (query pattern))))
    (cond ((and (= (length results) 0)
                (> (length defaults) 0))
           (apply values (cons #f defaults)))
          ((= (length results) 1)
           (apply
            values
            (cons (caar results)
                  (for/list ((association (in-list (cdar results))))
                    (cdr association)))))
          (else
           (error 'query-values*
                  "expected one result for ~.s, got ~a"
                  pattern (length results))))))

;;; (query*-values* pattern . defaults) -> any
;;;   pattern : pattern?
;;;   defaults : (listof any/c)
;;; Returns the assertion and bindings for a query as multiple values. If the
;;; query returns zero values and defaults are provided, the defaults are
;;; returned as multiple values. If the query returns zero values and no defaults
;;; are provided or if the query returns more that one value, an error is
;;; returned.
(define (query*-values* pattern . defaults)
  (let ((results (mlist->list (query* pattern))))
    (cond ((and (= (length results) 0)
                (> (length defaults) 0))
           (apply values (cons #f defaults)))
          ((= (length results) 1)
           (apply
            values
            (cons (caar results)
                  (for/list ((association (in-list (cdar results))))
                    (cdr association)))))
          (else
           (error 'query*-values*
                  "expected one result for ~.s, got ~a"
                  pattern (length results))))))

;;; (in-query pattern) -> sequence?
;;; Returns a sequence of the bindings for a query.
(define (in-query pattern)
  (make-do-sequence
   (lambda ()
     (values
      (lambda (results)
        (apply
         values
         (for/list ((association (in-list (cdar results))))
           (cdr association))))
      cdr
      (sorted-query pattern)
      (lambda (results) (not (null? results)))
      (lambda v #t)
      (lambda (results . v) #t)))))

(define (in-query* pattern)
  (make-do-sequence
   (lambda ()
     (values
      (lambda (results)
        (apply
         values
         (for/list ((association (in-list (cdar results))))
           (cdr association))))
      cdr
      (sorted-query* pattern)
      (lambda (results) (not (null? results)))
      (lambda v #t)
      (lambda (results . v) #t)))))

;;; Module Contracts

(provide (all-defined-out))
