#lang scheme/base

;(require (planet williams/inference/inference))
(require "../inference.ss")
(require (lib "list.ss" "srfi" "1"))
(require scheme/mpair)
(provide (all-defined-out))

(define-ruleset sudoku-rules)

;; If there is a board and no cells, initialize the system.
(define-rule (initialize sudoku-rules)
  (board ?board)
  (no (cell . ?))
  ==>
  (printf "initialize:~n")
  (print-board ?board)
  (do ((row 0 (+ row 1)))
    ((= row 9) (void))
    (assert `(digit ,row))
    (do ((column 0 (+ column 1)))
      ((= column 9) (void))
      (let ((value (vector-ref (vector-ref ?board row) column))
            (box (+ (* (quotient row 3) 3)
                    (quotient column 3))))
        (if (eqv? value '_)
            (assert `(cell ,row ,column ,box (1 2 3 4 5 6 7 8 9)))
            (assert `(cell ,row ,column ,box ,value)))))))

;; If all of the cells are numbered, we've succeeded.
(define-rule (rule-1 sudoku-rules)
  (all (cell ?row ?column ?box (?value (number? ?value))))
  (board ?board)
  ==>
  (stop-inference ?board))

;; If a cell has no possible values. we've failed.
(define-rule (rule-2 sudoku-rules)
  (cell ?row ?column ?box (?value (eq? ?value '())))
  ==>
  (fail))

;; If we have a single possible value in a cell,
;;   use it to number the cell.
(define-rule (rule-3 sudoku-rules)
  (?cell <- (cell ?row ?column ?box
                  (?value ;(and (list? ?value) (= (length ?value) 1))
                   (and (pair? ?value) (null? (cdr ?value)))
                   )))
  (board ?board)
  ==>
  (vector-set! (vector-ref ?board ?row) ?column (car ?value))
  (replace ?cell `(cell ,?row ,?column ,?box ,(car ?value))))

;; If a cell is numbered and it conflicts with another numbered cell,
;;   fail.
;(define-rule (rule-4 sudoku-rules)
;    (cell ?row ?column ?box (?value (number? ?value)))
;    (or (cell ?row (?column-1 (not (= ?column-1 ?column))) ?box-1
;              (?value-1 (and (number? ?value-1) (= ?value-1 ?value))))
;        (cell (?row-1 (not (= ?row-1 ?row))) ?column ?box-1
;              (?value-1 (and (number? ?value-1) (= ?value-1 ?value))))
;        (cell ?row-1 ?column-1 (?box (or (not (= ?row-1 ?row))
;                                         (not (= ?column-1 ?column))))
;              (?value-1 (and (number? ?value-1) (= ?value-1 ?value)))))
;  ==>
;    (fail))

(define-rule (rule-4a sudoku-rules)
  (cell ?row ?column ?box (?value (number? ?value)))
  (cell ?row (?column-1 (not (= ?column-1 ?column))) ?box-1
        (?value-1 (and (number? ?value-1) (= ?value-1 ?value))))
  ==>
  (fail))

(define-rule (rule-4b sudoku-rules)
  (cell ?row ?column ?box (?value (number? ?value)))
  (cell (?row-1 (not (= ?row-1 ?row))) ?column ?box-1
        (?value-1 (and (number? ?value-1) (= ?value-1 ?value))))
  ==>
  (fail))

(define-rule (rule-4c sudoku-rules)
  (cell ?row ?column ?box  (?value (number? ?value)))
  (cell ?row-1 ?column-1 (?box (or (not (= ?row-1 ?row))
                                   (not (= ?column-1 ?column))))
        (?value-1 (and (number? ?value-1) (= ?value-1 ?value))))
  ==>
  (fail))

;; If a cell is numbered, remove that number from other cell in the
;; same row, column, or box.
;(define-rule (rule-5 sudoku-rules)
;  (cell ?row ?column ?box
;        (?value (number? ?value)))
;  (or (?cell-1 <- (cell (?row-1 (= ?row-1 ?row))
;                        (?column-1 (not (= ?column-1 ?column)))
;                        ?box-1
;                        (?value-1 (and (pair? ?value-1)
;                                       (memv ?value ?value-1)))))
;      (?cell-1 <- (cell (?row-1 (not (= ?row-1 ?row)))
;                        (?column-1 (= ?column-1 ?column))
;                        ?box-1
;                        (?value-1 (and (pair? ?value-1)
;                                       (memv ?value ?value-1)))))
;      (?cell-1 <- (cell ?row-1
;                        ?column-1
;                        (?box-1 (and (= ?box-1 ?box))
;                                     (or (not (= ?row-1 ?row))
;                                         (not (= ?column-1 ?column)))))
;                        (?value-1 (and (pair? ?value-1)
;                                       (memv ?value ?value-1))))))
;  ==>
;  (replace ?cell-1 `(cell ,?row-1 ,?column-1 ,?box-1
;                          ,(delete ?value ?value-1))))
  
(define-rule (rule-5a sudoku-rules)
  (cell ?row ?column ?box
        (?value (number? ?value)))
  (?cell-1 <- (cell ?row
                    (?column-1 (not (= ?column-1 ?column)))
                    ?box-1
                    (?value-1 (and (pair? ?value-1)
                                   (memv ?value ?value-1)))))
  ==>
  (replace ?cell-1 `(cell ,?row ,?column-1 ,?box-1
                          ,(delete ?value ?value-1))))  

(define-rule (rule-5b sudoku-rules)
  (cell ?row ?column ?box
        (?value (number? ?value)))
  (?cell-1 <- (cell (?row-1 (not (= ?row-1 ?row)))
                    ?column
                    ?box-1
                    (?value-1 (and (pair? ?value-1)
                                   (memv ?value ?value-1)))))
  ==>
  (replace ?cell-1 `(cell ,?row-1 ,?column ,?box-1
                          ,(delete ?value ?value-1))))

(define-rule (rule-5c sudoku-rules)
  (cell ?row ?column ?box
        (?value (number? ?value)))
  (?cell-1 <- (cell ?row-1
                    ?column-1
                    (?box (or (not (= ?row-1 ?row))
                              (not (= ?column-1 ?column))))
                    (?value-1 (and (pair? ?value-1)
                                   (memv ?value ?value-1)))))
  ==>
  (replace ?cell-1 `(cell ,?row-1 ,?column-1 ,?box
                          ,(delete ?value ?value-1))))

;; If there is a value that only occurs once as a possibility  in any
;; row, column, or box, then make it the only possible value.
(define-rule (rule-6a sudoku-rules)
  (digit ?digit)
  (?cell <- (cell ?row ?column ?box (?value (and (pair? ?value)
                                                 (memv ?digit ?value)))))
  (no (cell ?row (?column-1 (not (= ?column-1 ?column))) ?
            (?value-1 (or (and (number? ?value-1)
                               (= ?value-1 ?digit))
                          (and (pair? ?value-1)
                               (memv ?digit ?value-1))))))
  ==>
  (replace ?cell `(cell ,?row ,?column ,?box ,(list ?digit))))

(define-rule (rule-6b sudoku-rules)
  (digit ?digit)
  (?cell <- (cell ?row ?column ?box (?value (and (pair? ?value)
                                                 (memv ?digit ?value)))))
  (no (cell (?row-1 (not (= ?row-1 ?row))) ?column ?
            (?value-1 (or (and (number? ?value-1)
                               (= ?value-1 ?digit))
                          (and (pair? ?value-1)
                               (memv ?digit ?value-1))))))
  ==>
  (replace ?cell `(cell ,?row ,?column ,?box ,(list ?digit))))

(define-rule (rule-6c sudoku-rules)
  (digit ?digit)
  (?cell <- (cell ?row ?column ?box (?value (and (pair? ?value)
                                                 (memv ?digit ?value)))))
  (no (cell ?row-1 ?column-1 (?box (or (not (= ?row-1 ?row))
                                       (not (= ?column-1 ?column))))
            (?value-1 (or (and (number? ?value-1)
                               (= ?value-1 ?digit))
                          (and (pair? ?value-1)
                               (memv ?digit ?value-1))))))
  ==>
  (replace ?cell `(cell ,?row ,?column ,?box ,(list ?digit))))

;; If the above rules don't find a solution (or fail), then create a
;; child inference to search using the shorted list of possibilities.
(define-rule (search sudoku-rules #:priority -100)
  (board ?board)
  (cell ?row ?column ?box (?value (and (pair? ?value)
                                       (> (length ?value) 1)))
        )
  (no (cell ? ? ? (?value-1 (and (pair? ?value-1)
                                 (< (length ?value-1)
                                    (length ?value))))))
  ==>
  (printf "search: row = ~a, column = ~a, box = ~a, values = ~a~n"
          ?row ?column ?box ?value)
  (for-each
   (lambda (value)
     (let ((new-board (copy-board ?board)))
       (vector-set! (vector-ref new-board ?row) ?column value)
       (let ((result
              (with-new-child-inference-environment
               (activate sudoku-rules)
               (assert `(board ,new-board))
               (let ((result (start-inference)))
                 (printf "Rules fired = ~a~n" (current-inference-rules-fired))
                 result))))
         (when (vector? result)
           (stop-inference result)))))
   ?value)
  (fail))

(define (copy-board board)
  (let ((new-board (make-vector 9)))
    (do ((row 0 (+ row 1)))
      ((= row 9) new-board)
      (let ((board-row (vector-ref board row))
            (new-row (make-vector 9)))
        (do ((column 0 (+ column 1)))
          ((= column 9) (void))
          (vector-set! new-row column
                       (vector-ref board-row column)))
        (vector-set! new-board row new-row)))))

(define (print-board board)
  (do ((row 0 (+ row 1)))
    ((= row 9) (void))
    (do ((column 0 (+ column 1)))
      ((= column 9) (void))
      (let ((value (vector-ref (vector-ref board row) column)))
        (printf "~a " value)))
    (printf "~n")))

(define (sudoku-solver board)
  (printf "Initial Board~n")
  (print-board board)
  (with-new-inference-environment
   (activate sudoku-rules)
   (graph-rule-network)
   ;(current-inference-strategy 'breadth)
   ;(current-inference-trace #t)
   (assert `(board ,board))
   (let ((result (start-inference)))
     (cond ((eq? result '#:fail)
            (printf "Problem cannot be solved!~n"))
           ((not result)
            (printf "No solution found!~n"))
           (else
            (printf "Solution found!~n")
            (print-board result))))
   (printf "Rules fired = ~a~n" (current-inference-rules-fired))
   ))

(define _ '_)

(printf "Trivial - Already Solved~n")
(sudoku-solver (vector (vector 1 2 3 4 5 6 7 8 9)
                       (vector 4 5 6 7 8 9 1 2 3)
                       (vector 7 8 9 1 2 3 4 5 6)
                       (vector 2 3 4 5 6 7 8 9 1)
                       (vector 5 6 7 8 9 1 2 3 4)
                       (vector 8 9 1 2 3 4 5 6 7)
                       (vector 3 4 5 6 7 8 9 1 2)
                       (vector 6 7 8 9 1 2 3 4 5)
                       (vector 9 1 2 3 4 5 6 7 8)))

(printf "~nTrivial - One open cell~n")
(sudoku-solver (vector (vector _ 2 3 4 5 6 7 8 9)
                       (vector 4 5 6 7 8 9 1 2 3)
                       (vector 7 8 9 1 2 3 4 5 6)
                       (vector 2 3 4 5 6 7 8 9 1)
                       (vector 5 6 7 8 9 1 2 3 4)
                       (vector 8 9 1 2 3 4 5 6 7)
                       (vector 3 4 5 6 7 8 9 1 2)
                       (vector 6 7 8 9 1 2 3 4 5)
                       (vector 9 1 2 3 4 5 6 7 8)))

(printf "~nEasy~n")
(sudoku-solver (vector (vector 7 8 1 6 _ 2 9 _ 5)
                       (vector 9 _ 2 7 1 _ _ _ _)
                       (vector _ _ 6 8 _ _ _ 1 2)
                       (vector 2 _ _ 3 _ _ 8 5 1)
                       (vector _ 7 3 5 _ _ _ _ 4)
                       (vector _ _ 8 _ _ 9 3 6 _)
                       (vector 1 9 _ _ _ 7 _ 8 _)
                       (vector 8 6 7 _ _ 3 4 _ 9)
                       (vector _ _ 5 _ _ _ 1 _ _)))

(printf "~nMedium~n")
(sudoku-solver (vector (vector _ 8 _ _ _ _ _ _ _)
                       (vector _ 4 7 8 _ 9 _ _ 1)
                       (vector _ _ 1 4 5 _ _ 2 _)
                       (vector 8 1 6 7 _ _ 5 _ _)
                       (vector 9 _ _ _ _ 1 _ _ _)
                       (vector _ _ _ 5 6 _ _ _ _)
                       (vector _ _ _ _ _ 8 _ 5 3)
                       (vector _ _ _ _ _ _ _ 8 _)
                       (vector _ _ _ 3 1 _ _ 4 6)))

(printf "~nHard~n")
(sudoku-solver (vector (vector _ 1 9 _ _ _ _ _ _)
                       (vector _ _ 8 _ _ 3 _ 5 _)
                       (vector _ 7 _ 6 _ _ _ 8 _)
                       (vector _ _ 1 _ _ 6 8 _ 9)
                       (vector 8 _ _ _ 4 _ _ _ 7)
                       (vector 9 4 _ _ _ _ _ 1 _)
                       (vector _ _ _ _ _ 2 _ _ _)
                       (vector _ _ _ _ 8 _ 5 6 1)
                       (vector _ _ 3 7 _ _ _ 9 _)))
 