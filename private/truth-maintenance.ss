#lang scheme
;;; PLT Scheme Inference Collection
;;; truth-maintenance.ss
;;; Copyright (c) 2010 M. Douglas Williams
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
;;; Version  Date      Description
;;; 1.0.0    03/21/10  Initial release. (MDW)

(define-struct assertion-hook (assert-hook retract-hook))

(define (parse-assertion-hook-pragma assertion-hook-pragma)
  (let ((name (cadr assertion-hook-pragma))
        (assert-hook (caddr assertion-hook-pragma))
        (retract-hook (caddr assertion-hook-pragma)))
    (values name assert-hook retract-hook)))

(define (tm-assert-assertion-hook assertion-hooks assertion-hook-pragma)
  (let-values (((name assert-hook retract-hook)
                (parse-assertion-hook-pragma assertion-hook-pragma)))
    (let ((assertion-hook
           (make-assertion-hook assert-hook retract-hook)))
      (hash-set! assertion-hooks name assertion-hook)))
  (void))

(provide (all-defined-out))