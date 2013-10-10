#lang racket
;;; PLT Scheme Inference Collection
;;; bindings.rkt
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
;;; Bindings are represented by association lists. Originally, I had converted
;;; them mutable list, but have changed them back and removed bindings-update!
;;; and bindings-remove!, which were never used.
;;;
;;; Verified the use of immutable lists.
;;;
;;; Todo:
;;;  - Since we are once again using immutable lists, bindings must
;;;    be merged using append, which can be slow.  Check if this is
;;;    a significant issue.
;;;  - Would hash tables be a better data structure to use?
;;;
;;; Version  Date      Description
;;; 2.0.0    06/26/08  Changes for V4.0.  Changed implementation to use mutable
;;;                    lists. (Doug Williams)
;;; 2.0.1    07/02/08  Reverted to immutable lists. (Doug Williams)
;;; 2.0.2    12/24/08  Added contracts and cleaned up the code. (Doug Williams)

(require (only-in srfi/1 every))

;;; -----------------------------------------------------------------------------
;;;                                  Bindings
;;; -----------------------------------------------------------------------------

;;; (bindings? x) -> boolean?
;;;   x : any/c
;;; Returns #t if x is a bindings object.  This code makes sure the bindings are
;;; an association list with every key being a symbol?
(define (bindings? x)
  (and (list? x)
       (every
        (lambda (element)
          (and (pair? element)
               (symbol? (car element))))
        x)))

;;; (make-bindings) -> bindings?
;;; Returns an empty bindings object.
(define (make-bindings)
  '())

;;; (bindings-bound? bindings key) -> boolean?
;;;   bindings : bindings?
;;;   key : symbol?
;;; Returns #t if the key is bound in bindings.
(define (bindings-bound? bindings key)
  (let ((binding (assq key bindings)))
    (if binding #t #f)))

;;; (bindings-put! bindings key value) -> bindings?
;;;   bindings : bindings?
;;;   key : symbol?
;;;   value : any/c
;;; Binds key to datum in bindings.
(define (bindings-put! bindings key value)
  (cons (cons key value) bindings))

;;; (bindings-ref bindings key) -> any
;;;   bindings : bindings?
;;;   key : symbol?
;;; Returns the value of the key in bindings.  An error is signaled if the key is
;;; not bound in bindings.
(define (bindings-ref bindings key)
  (let ((binding (assq key bindings)))
    (if binding
        (cdr binding)
        (error 'bindings-ref
               "No binding for ~a in ~a" key bindings))))

;;; (bindings-map bindings proc) -> list?
;;;   bindings : bindings?
;;;   proc : (-> symbol? any/c any)
;;; Applies proc to all of the bindings and returns a list of the results.
(define (bindings-map bindings proc)
  (map
   (lambda (binding)
     (proc (car binding) (cdr binding)))
   bindings))

;;; (bindings-for-each bindings proc) -> void?
;;;   bindings : bindings?
;;;   proc : (-> symbol? any/c any)
;;; Applies proc to all of the bindings.
(define (bindings-for-each bindings proc)
  (for-each
   (lambda (binding)
     (proc (car binding) (cdr binding)))
   bindings))

;;; (bindings-keys: bindings) -> (listof symbol?)
;;;   bindings : bindings?
;;; Returns a list of the keys in the bindings.
(define (bindings-keys bindings)
  (bindings-map bindings
   (lambda (key datum)
      key)))

;;; bindings-values: bindings? -> list?
;;;   bindings : bindings?
;;; Returns a list of the values in the bindings.
(define (bindings-values bindings)
  (bindings-map bindings
   (lambda (key value)
     value)))

;;; bindings-keys-values: bindings? -> (listof symbol?) list?
;;;   bindings : bindings?
;;; Returns a list of the keys and a list of the values in the bindings.
(define (bindings-keys-values bindings)
  (let ((keys (bindings-keys bindings))
        (data (bindings-values bindings)))
    (values keys data)))

;;; -----------------------------------------------------------------------------
;;;                               Module Contracts
;;; -----------------------------------------------------------------------------

;(provide/contract
; (bindings?
;  (-> any/c boolean?))
; (make-bindings
;  (-> bindings?))
; (bindings-bound?
;  (-> bindings? symbol? boolean?))
; (bindings-put!
;  (-> bindings? symbol? any/c bindings?))
; (bindings-ref
;  (-> bindings? symbol? any))
; (bindings-map
;  (-> bindings? (-> symbol? any/c any) list?))
; (bindings-for-each
;  (-> bindings? (-> symbol? any/c any) void?))
; (bindings-keys
;  (-> bindings? (listof symbol?)))
; (bindings-values
;  (-> bindings? list?))
; (bindings-keys-values
;  (-> bindings? (values (listof symbol?) list))))

(provide (all-defined-out))
