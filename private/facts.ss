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
;;; Facts are statements with the domain. The may either be immutable lists
;;; (including association lists), vectors, or structures.
;;;
;;; All list operations are immutable.
;;;
;;; Todo:
;;;   - constantly converting structures to lists here is not a good idea. We
;;;     need some way to cache them.
;;;
;;; Version  Date      Description
;;; 2.0.0    06/26/08  Changes for V4.0. (Doug Williams)
;;; 2.0.1    07/02/08  Verified immutable lists. (Doug Williams)
;;; 2.0.2    12/25/08  Added module contracts and cleaned up the code. (Doug
;;;                    Williams)

;;; -----------------------------------------------------------------------------
;;;                                   Facts
;;; -----------------------------------------------------------------------------


;;; (fact? x) -> boolean?
;;;   x : any/c
;;; Returns #t if x represents a fact. Facts may be a non-empty list whose first
;;; element is a symbol, a non-empty vector whose first element is a symbol, or a
;;; structure.
(define (fact? x)
  (or (and (pair? x)
           (or (symbol? (car x))
               (keyword? (car x))))
      (and (vector? x)
           (> (vector-length x) 0)
           (symbol? (vector-ref x 0)))
      (struct? x)))

;;; (fact-first fact) -> symbol?
;;; Returns the first element of a fact, which must be a symbol. The first
;;; element of a structure will always be the symbol naming the structure.
(define (fact-first fact)
  (cond ((pair? fact)
         (car fact))
        ((vector? fact)
         (vector-ref fact 0))
        ((struct? fact)
         (vector-ref (struct->vector fact) 0))))

;;; -----------------------------------------------------------------------------
;;;                               Module Contracts
;;; -----------------------------------------------------------------------------

;(provide/contract
; (fact?
;  (-> any/c boolean?))
; (fact-first
;  (-> fact? symbol?)))

(provide (all-defined-out))