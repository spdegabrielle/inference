#lang racket
;;; PLT Scheme Inference Collection
;;; argument-lists.ss
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
;;; This module provides operations on argument lists. Argument lists are normal
;;; (immutable) lists.
;;;
;;; Version  Date      Description
;;; 2.0.0    06/25/08  Changes for V4.0.  (Doug Williams)
;;; 2.0.1    07/02/08  Verified immultability and updated the header.  (Doug
;;;                    Williams)
;;; 2.0.2    12/25/08  Added module contracts.  Simplified the definition of
;;;                    argument-list-merge.  (Doug Williams)

;;; (argument-list-merge list1 list2) -> (listof symbol?)
;;;   list1 : (listof symbol?)
;;;   list2 : (listof symbol?)
;;; Merge two list of variables maintaining the order.
;;; Todo:
;;;  - Check that this is only used in building the rule network and not during
;;;    inferencing.
(define (argument-list-merge list1 list2)
  (remove-duplicates (append list1 list2)))

;;; -----------------------------------------------------------------------------
;;;                              Module Contracts
;;; -----------------------------------------------------------------------------

;(provide/contract
; (argument-list-merge
;  (-> (listof symbol?)
;      (listof symbol?)
;      (listof symbol?))))

(provide (all-defined-out))
