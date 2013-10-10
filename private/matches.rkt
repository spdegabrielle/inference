#lang racket
;;; PLT Scheme Inference Collection
;;; matches.ss
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
;;; ------------------------------------------------------------------
;;;
;;; A match represents a series of assertions and assertions that match
;;; a set of facts in the knowledge base.
;;;
;;; All list operations are immutable.  [Although lists of matches
;;;
;;; Version  Date      Description
;;; 2.0.1    07/02/08  Updated header.  (Doug Williams)

(require "bindings.ss")
(require "assertions.ss")

;;; match?: any -> boolean
(define (match? x)
  (and (pair? x)
       (list? (car x))
       (bindings? (cdr x))))

;;; match-assertions: match? -> list
(define (match-assertions match)
  (car match))

;;; match-bindings: match? -> bindings?
(define (match-bindings match)
  (cdr match))

;;; match-subset? list x list -> boolean
;;; A predicate function that returns true if the first argument is a
;;; subset of the second argument.  This is determined by checking the
;;; list of assertions.
(define (match-subset? match-1 match-2)
  (assertions-subset? (car match-1) (car match-2)))

;(provide/contract
; (match?
;  (-> any/c boolean?))
; (match-assertions
;  (-> match? (listof assertion?)))
; (match-bindings
;  (-> match? bindings?))
; (match-subset?
;  (-> match? match? boolean?)))

(provide (all-defined-out))
