#lang scheme
;;; PLT Scheme Inference Collection
;;; ontology.ss
;;; Copyright (c) 2009-2010 M. Douglas Williams
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
;;; 1.0.0    06/24/09  Initial release. (MDW)
;;; 1.0.1    06/18/10  Added routines to find descendants. (MDW)

;;; (parse-class-pragma class-pragma) -> symbol?
;;;                                      (list-of symbol?)
;;;                                      (or/c string? false/c)
;;;                                      (list-of symbol?)
;;;   class-pragma : list?
;;; Parse a class pragma. The format of a class pragma is:
;;;   (#:class name (parent ...) [description] field ...)
;;; where,
;;;   #:class is the keyword denoting a class pragma
;;;   name is a symbol that is the name of the class
;;;   parent is a symbol that is the name of a parent class
;;;   description is a string that is a description of the class
;;;   field is a symbol naming a field of the class
(define (parse-class-pragma class-pragma)
  (let ((name (cadr class-pragma))
        (parents (caddr class-pragma))
        (description
         (if (and (> (length class-pragma) 3)
                  (string? (cadddr class-pragma)))
             (cadddr class-pragma )
             #f))
        (fields
         (if (and (> (length class-pragma) 3)
                  (string? (cadddr class-pragma)))
             (cddddr class-pragma)
             (cdddr class-pragma))))
    (values name parents description fields)))

;;; (struct ontology (class-table))
;;;   class-table : hash-eq?
;;; Defines the structure that represents an ontology.
(define-struct ontology (class-table))

;;; (new-ontology) -> ontology?
;;; Returns a new, empty ontology.
(define (new-ontology)
  (make-ontology (make-hasheq)))

;;; (struct class (name description parents children))
;;;   name : symbol?
;;;   description : (or/c string? false/c)
;;;   parents : (list-of class?)
;;;   children : (list-of class?)
;;;   fields : (list-of symbol?)
;;; Defines a class in an ontology.
(define-struct class (name
                      (description #:mutable)
                      (parents #:mutable)
                      (children #:mutable)
                      (fields #:mutable)))

;;; (new-class name (description #f)) -> class?
;;;   name : symbol?
;;;   description : (or/c string? false/c) = #f
;;; Creates a new, empty class with the specified name and adds it to the class-
;;; table for ontology.
(define (new-class ontology name)
  (let ((class (make-class name #f '() '() '())))
    (hash-set! (ontology-class-table ontology) name class)
    class))

;;; (ontology-find-class ontology name) -> class?
;;;   ontology : ontology?
;;;   name : symbol?
;;; Returns the class with the specified name in ontology. The class is created
;;; if it doesn't exist.
(define (ontology-find-class ontology name)
  (let ((class (hash-ref (ontology-class-table ontology) name #f)))
    (if class
        class
        (new-class ontology name))))

;;; (ontology-assert-class ontology class-pragma) -> void?
;;;   ontology : ontology?
;;;   class-pragma : list?
;;; Processes the #:class pragma assertion.
(define (ontology-assert-class ontology class-pragma)
  (let-values (((name parents description fields)
                (parse-class-pragma class-pragma)))
    (let ((class (ontology-find-class ontology name)))
      ;; Replace the description.
      (set-class-description! class description)
      ;; Replace the fields.
      (set-class-fields! class fields)
      ;; Add the parents.
      (for ((parent (in-list parents)))
        (let ((parent-class (ontology-find-class ontology parent)))
          (unless (memq parent (class-parents class))
            (set-class-parents! class (append (class-parents class) (list parent-class)))
            (set-class-children! parent-class (append (class-children parent-class) (list class))))))
      (void))))

;;; (ontology-retract-class ontology class-pragma) -> void?
;;;   ontology : ontology?
;;;   class-pragma : list?
;;; Processes the #:class pragma retraction.
(define (ontology-retract-class ontology class-pragma)
  (let* ((name (cadr class-pragma))
         (class (hash-ref (ontology-class-table ontology) name #f)))
    (when class
      ;; Remove the class from the ontology.
      (hash-remove! (ontology-class-table ontology) class)
      ;; Remove this class as a child of its parents.
      (for ((parent-class (in-list (class-parents class))))
        (set-class-children! parent-class (remove class (class-children class))))
      ;; Remove this class as a parent of its children.
      (for ((child-class (in-list (class-children class))))
        (set-class-parents! child-class (remove class (class-parents class)))))
    (void)))

;;; (class-ancestors class) -> (list-of class?)
;;;   class : class?
;;; Returns a list of the ancestors of class. The list is produced by a breadth-
;;; first tracersal up the parent links. The first element will always be class
;;; itself.
(define (class-ancestors class)
  (let ((open (list class))
        (closed '()))
    (let loop ()
      (unless (null? open)
        (let ((class (car open)))
          (set! open (cdr open))
          (set! closed (append closed (list class)))
          (for ((parent (in-list (class-parents class))))
            (unless (or (memq parent closed)
                        (memq parent open))
              (set! open (append open (list parent))))))
        (loop)))
    closed))

;;; (class-ancestor-names class) -> (list-of symbol?)
;;;   class : class?
;;; Returns a list of the names of the ancestors of class.
(define (class-ancestor-names class)
  (for/list ((class (in-list (class-ancestors class))))
    (class-name class)))

;;; (class-descendants class) -> (list-of class?)
;;;   class : class?
;;; Returns a list of the descendants of class. The list is produced by a breadth-
;;; first tracersal down the children links. The first element will always be class
;;; itself.
(define (class-descendants class)
  (let ((open (list class))
        (closed '()))
    (let loop ()
      (unless (null? open)
        (let ((class (car open)))
          (set! open (cdr open))
          (set! closed (append closed (list class)))
          (for ((child (in-list (class-children class))))
            (unless (or (memq child closed)
                        (memq child open))
              (set! open (append open (list child))))))
        (loop)))
    closed))

;;; (class-descendant-names class) -> (list-of symbol?)
;;;   class : class?
;;; Returns a list of the names of the descendants of class.
(define (class-descendant-names class)
  (for/list ((class (in-list (class-descendants class))))
    (class-name class)))

;;; Module contracts

(provide (all-defined-out))
