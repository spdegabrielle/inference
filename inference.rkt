#lang racket
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
;;; To do:
;;;  1) Break constraints into match constraints and join constraints.
;;;     Match constraints will be checked early (in match) and join
;;;     constraints late (in join).  Only store the base pattern with
;;;     the constraints removed - I may leave the match constraints in
;;;     so the matcher can check them on the fly (design decision).
;;;     Done - MDW 7/4/06
;;;  2) Add reuse of rule network nodes.
;;;     a) Add variable renaming
;;;     b) Add match node reuse - Done 7/30/06
;;;     c) Add join node reuse - Done 7/30/06
;;;  3) Add tail matching for lists.
;;;     Done - MDW 7/15/06
;;;  4) Add vector matching.
;;;     Done - MDW 7/15/06
;;;  5) Move it into the production framework:
;;;       Add inference environments - Done MDW 7/16/06
;;;       Break up the code into separate modules or includes - Done MDW 7/16/06
;;;       Add the syntax elements - Done MDW7/16/06
;;;     Done - MDW 7/16/06
;;;  6) Add backward chaining
;;;     Done - MDW 7/15/06
;;;  7) Add assumption processing
;;;  8) Add structure matching
;;;     Done MDW 7/16/06
;;;  9) Add query
;;;     Done MDW 7/17/06
;;; 10) Add forall
;;;     Done MDW 7/19/06 (all existential)
;;; 11) Add association list matching
;;;     Done MDW 7/17/06
;;; 12) Add hierarchical inference environment
;;;       Add import and export - Done MDW 7/24/06
;;;     Done MDW 7/24/06
;;; 13) Add conflict resolution strategy
;;;       Add agenda - Done MDW 7/22/06
;;;       Add priority - Done MDW 7/23/06
;;;       Add conflict resolution - Done MDW 7/23/06
;;;     Done MDW 7/23/06
;;; 14) Clean up trace
;;;     Done MDW 7/19/06

  (require "private/inference-environments.rkt")
  (require "private/rulesets.rkt")
  (require "private/inference-control.rkt")
  (require "private/assertions.rkt")
  (require "private/utilities.rkt")
  
  (provide (all-from-out "private/inference-environments.rkt")
           (all-from-out "private/rulesets.rkt")
           (all-from-out "private/inference-control.rkt")
           (all-from-out "private/assertions.rkt")
           (all-from-out "private/utilities.rkt"))
 
