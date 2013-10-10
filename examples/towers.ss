#lang scheme/base
;;; PLT Scheme Inference Collection
;;; towers.ss
;;;
;;; Towers of Hanoi from Artificial Intelligence: Tools, Techniques,
;;; and Applications, Tim O'Shea and Marc Eisenstadt, Harper & Rowe,
;;; 1984, pp.45

;;; The rules of the game are: (1) move one ring at a time and (2)
;;; never place a larger ring on top of a smaller ring.  The object
;;; is to transfer the entire pile of rings from its starting
;;; peg to either of the other pegs - the target peg.

(require (planet williams/inference/inference))

(define-ruleset towers-rules)

;;; If the target peg hold all the rings 1 to n, stop because according
;;; to game rule (2) they must be in their original order and so the
;;; problem is solved.
(define-rule (rule-1 towers-rules)
    (all (ring ? on right))
  ==>
    (printf "Problem solved!~n")
    (succeed))

;;; If there is no current goal - that is, if a ring has just been
;;; successfully moved, or if no rings have yet to be moved - generate
;;; a goal.  In this case the goal is to be that of moving to the 
;;; target peg the largest ring that is not yet on the target peg.
(define-rule (rule-2 towers-rules)
    (no (move . ?))
    (ring ?size on (?peg (not (eq? ?peg 'right))))
    (no (ring (?size-1 (> ?size-1 ?size))
              on (?peg-1 (not (eq? ?peg-1 'right)))))
  ==>
    (assert `(move ,?size from ,?peg to right)))

;;; If there is a current goal, it can be achieved at once of there is
;;; no small rings on top of the ring to be moved (i.e. if the latter
;;; is at the top of its pile), and there are no small rings on the
;;; peg to which it is to be moved (i.e. the ring to be moved is 
;;; smaller that the top ring on the peg we intend to move it to).  If
;;; this is the case, carry out the move and then delete the current
;;; goal so that rule 2 will apply next time.
(define-rule (rule-3 towers-rules)
    (?move <- (move ?size from ?from to ?to))
    (?ring <- (ring ?size on ?from))
    (no (ring (?size-1 (< ?size-1 ?size)) on ?from))
    (no (ring (?size-2 (< ?size-2 ?size)) on ?to))
  ==>
    (printf "Move ring ~a from ~a to ~a.~n" ?size ?from ?to)
    (replace ?ring `(ring ,?size on ,?to))
    (retract ?move))

;;; If there is a current goal but its disc cannot be moved as in rule
;;; 3, set up a new goal: that of moving the largest of the obstructing
;;; rings to the peg that is neither of those specified in the current
;;; goal (i.e. well out of the way of the current goal).  Delete the
;;; current goal, so that rule 2 will apply to the new goal next time.
(define-rule (rule-4 towers-rules)
    (?move <- (move ?size from ?from to ?to))
    (peg (?other (not (memq ?other (list ?from ?to)))))
    (ring (?size-1 (< ?size-1 ?size))
          on (?peg-1 (not (eq? ?peg-1 ?other))))
    (no (ring (?size-2 (< ?size-1 ?size-2 ?size))
              on (?peg-2 (not (eq? ?peg-2 ?other)))))
  ==>
    (replace ?move `(move ,?size-1 from ,?peg-1 to ,?other)))

(define (solve-towers n)
  (with-new-inference-environment
   (activate towers-rules)
   ;(current-inference-trace #t)
   ;; Create pegs.
   (assert '(peg left))
   (assert '(peg middle))
   (assert '(peg right))
   ;; Create rings.
   (for ((i (in-range 1 (+ n 1))))
     (assert `(ring ,i on left)))
   ;; Start inferencing.
   (start-inference)
   (printf "Rules fired = ~a~n" (current-inference-rules-fired))))

(solve-towers 6)
