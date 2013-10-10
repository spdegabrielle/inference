#lang scribble/doc
@(require scribble/manual
          scribblings/icons
          (for-label scheme/base
                     "../inference.ss"))

@title[#:tag "using"]{Using the Inference Collection}

@local-table-of-contents[]

This chapter describes how to use the PLT Scheme Inference Collection and introduces its conventions.

@section{An Example}

The following code demonstrates the use of the PLT Scheme Inference Collection by solving the Towers of Hanoi problem using a rule-based inference system.

@schememod[
scheme/base
(code:comment "PLT Scheme Inference Collection")
(code:comment "towers-alist.ss")
(code:comment "")
(code:comment "Towers of Hanoi from Artificial Intelligence: Tools, Techniques,")
(code:comment "and Applications, Tim O'Shea and Marc Eisenstadt, Harper & Rowe,")
(code:comment "1984, pp.45")
(code:comment "")
(code:comment "The rules of the game are: (1) move one ring at a time and (2)")
(code:comment "never place a larger ring on top of a smaller ring.  The object")
(code:comment "is to transfer the entire pile of rings from its starting")
(code:comment "peg to either of the other pegs - the target peg.")

(require (planet williams/inference/inference))

(define-ruleset towers-rules)

(code:comment "If the target peg holds all the rings 1 to n, stop because")
(code:comment "according to game rule (2) they must be in their original order")
(code:comment "and so the problem is solved.")
(define-rule (rule-1 towers-rules)
    (all (ring ? (on . right)))
  ==>
    (succeed))

(code:comment "If there is no current goal - that is, if a ring has just been")
(code:comment "successfully moved, or if no rings have yet to be moved -")
(code:comment "generate a goal. In this case the goal is to be that of moving")
(code:comment "to the target peg the largest ring that is not yet on the target")
(code:comment "peg.")
(define-rule (rule-2 towers-rules)
    (no (move . ?))
    (ring ?size (on ?peg (not (eq? ?peg 'right))))
    (no (ring (?size-1 (> ?size-1 ?size))
              (on ?peg-1 (not (eq? ?peg-1 'right)))))
  ==>
    (assert `(move (size . ,?size)
                   (from . ,?peg)
                   (to . right))))

(code:comment "If there is a current goal, it can be achieved at once of there")
(code:comment "is no small rings on top of the ring to be moved (i.e. if the")
(code:comment "latter is at the top of its pile), and there are no small rings")
(code:comment "on the peg to which it is to be moved (i.e. the ring to be moved")
(code:comment "is smaller that the top ring on the peg we intend to move it to).")
(code:comment "If this is the case, carry out the move and then delete the")
(code:comment "current goal so that rule 2 will apply next time.")
(define-rule (rule-3 towers-rules)
    (?move <- (move (size . ?size) (from . ?from) (to . ?to)))
    (?ring <- (ring ?size (on . ?from)))
    (no (ring (?size-1 (< ?size-1 ?size)) (on . ?from)))
    (no (ring (?size-2 (< ?size-2 ?size)) (on . ?to)))
  ==>
    (printf "Move ring ~a from ~a to ~a.~n" ?size ?from ?to)
    (replace ?ring `(ring ,?size (on . ,?to)))
    (retract ?move))

(code:comment "If there is a current goal but its disc cannot be moved as in")
(code:comment "rule 3, set up a new goal: that of moving the largest of the")
(code:comment "obstructing rings to the peg that is neither of those specified")
(code:comment "in the current goal (i.e. well out of the way of the current")
(code:comment "goal). Delete the current goal, so that rule 2 will apply to the")
(code:comment "new goal next time.")
(define-rule (rule-4 towers-rules)
    (?move <- (move (size . ?size) (from . ?from) (to . ?to)))
    (peg (?other (not (memq ?other (list ?from ?to)))))
    (ring (?size-1 (< ?size-1 ?size))
          (on ?peg-1 (not (eq? ?peg-1 ?other))))
    (no (ring (?size-2 (< ?size-1 ?size-2 ?size))
              (on ?peg-2 (not (eq? ?peg-2 ?other)))))
  ==>
    (replace ?move `(move (size . ,?size-1)
                          (from . ,?peg-1)
                          (to . ,?other))))

(code:comment "The main routine:")
(code:comment "In a new inference environment:")
(code:comment "  Activate the towers rule set.")
(code:comment "  Optionally, turn on tracing.")
(code:comment "  Create the three pegs - left, middle, and right.")
(code:comment "  Create the n rings.")
(code:comment "  Start the inference.")
(code:comment "The rules will print the solution to the problem.")
(define (solve-towers n)
  (with-new-inference-environment
   (activate towers-rules)
   (code:comment "(current-inference-trace #t)")
   (code:comment "Create pegs.")
   (assert '(peg left))
   (assert '(peg middle))
   (assert '(peg right))
   (code:comment "Create rings.")
   (for ((i (in-range 1 n)))
     (assert `(ring ,i (on . left))))
   (code:comment "Start inferencing.")
   (start-inference)))

(code:comment "Test with 6 disks.")
(solve-towers 6)
]

The following shows the resulting printed output.

@verbatim{
Move ring 1 from left to right.
Move ring 2 from left to middle.
Move ring 1 from right to middle.
Move ring 3 from left to right.
Move ring 1 from middle to left.
Move ring 2 from middle to right.
Move ring 1 from left to right.
Move ring 4 from left to middle.
Move ring 1 from right to middle.
Move ring 2 from right to left.
Move ring 1 from middle to left.
Move ring 3 from right to middle.
Move ring 1 from left to right.
Move ring 2 from left to middle.
Move ring 1 from right to middle.
Move ring 5 from left to right.
Move ring 1 from middle to left.
Move ring 2 from middle to right.
Move ring 1 from left to right.
Move ring 3 from middle to left.
Move ring 1 from right to middle.
Move ring 2 from right to left.
Move ring 1 from middle to left.
Move ring 4 from middle to right.
Move ring 1 from left to right.
Move ring 2 from left to middle.
Move ring 1 from right to middle.
Move ring 3 from left to right.
Move ring 1 from middle to left.
Move ring 2 from middle to right.
Move ring 1 from left to right.
#t
}

@section{Loading the Inference Collection}

The PLT Scheme Inference Collection is loaded using the following form:

@schemeblock[
(require (planet williams/inference/inference))
]

This will load the inference collection from the PLaneT server, if necessary, and install it on your machine.