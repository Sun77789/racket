;; Done by Malika Aubakirova
;; solution to the problem http://www.puzzles.com/puzzleplayground/KeepItEven/KeepItEven.htm.
;; Given a four by four square arrangement of coins 
;; remove any six coins such that each row and column contains an even number of coins.
(require racket/match)


;; this function was presented in class
;; so it is not mine
;; Ïƒ is a type variable, a mnemonic for "state."
;; search : Ïƒ (Ïƒ -> (or (listof Ïƒ) 'goal)) -> (or Ïƒ false)
;; generic backtrack search starting from the start state.  The next
;; function either returns 'goal, when its argument is a goal state,
;; '() when its argument is a dead end, or else a list of successor
;; states.
(define (search start next)
  (local
    {;; look : Ïƒ -> (or Ïƒ false)
     ;; starting from given state, return a goal state or false
     (define (look s)
       (match (next s)
         ['goal s]
         [next-states
          (local
            {;; lp : (listof Ïƒ) -> (or Ïƒ false)
             ;; look for a goal from list of states;
             ;; return first goal encountered, or false 
             (define (lp ss)
               (cond
                [(empty? ss) false]
                [else 
                 (match (look (first ss))
                   [#f (lp (rest ss))]
                   [g g])]))}
            (lp next-states))]))}
    (look start)))

;; MY SOLUTION

;; a cp-state is a (make-cp-state n row board), where n is the number of coins removed,
;; row is the current row we are starting traversing with
;; b is a board with rows 0..r-1 devoid of coins.
(define-struct cp-state (n row board))

;; a square on the board is defined by its row and column
(define-struct sq (row col))

;; a board is represented as a list of cells where squares
;; where COINS ARE REMOVED

;; cp-board-even?: (listof sq) -> bool
;; checks whether given board is even
;; i.e. all columns and rows have even number of removed coins
(define (cp-board-even? cs) 
  (local 
    {(define (check-row counter)
       (even? (length (filter (lambda (i) (= (sq-row i) counter)) cs))))
     (define (check-col counter)
       (even? (length (filter (lambda (i) (= (sq-col i) counter)) cs)))) 
     (define (check-rows counter)
       (cond
         [(> counter 4) true]
         [else (and (check-row counter) (check-rows (add1 counter)))]))
     (define (check-cols counter)
       (cond
         [(> counter 4) true]
         [else (and (check-col counter) (check-cols (add1 counter)))]))}
    (if (empty? cs) false (and (check-rows 0) (check-cols 0))))) 
;; tests    
(check-expect 
 (cp-board-even? 
  (list (make-sq 0 1) (make-sq 0 3)
        (make-sq 1 0) (make-sq 1 3)
        (make-sq 3 0) (make-sq 3 1))) 
 true)
(check-expect 
 (cp-board-even? 
  (list (make-sq 0 1) (make-sq 0 3)
        (make-sq 1 0) (make-sq 1 3)
        (make-sq 2 0) (make-sq 3 1))) 
 false)

;; cp-row-even?: (listof sq) num -> bool
;; checks whether row in a given board is even
(define (cp-row-even? cs row-num)
  (local {(define filt-ls (filter (lambda (i) (equal? (sq-row i) row-num)) cs))}
    (even? (length filt-ls))))
;; tests
(check-expect 
 (cp-row-even?
  (list (make-sq 0 1) (make-sq 0 3)
        (make-sq 1 0) (make-sq 1 3)
        (make-sq 1 0) (make-sq 3 1)) 0) 
 true)

(check-expect 
 (cp-row-even?
  (list (make-sq 0 1) (make-sq 0 3)
        (make-sq 1 0) (make-sq 1 3)
        (make-sq 1 0) (make-sq 3 1)) 1) 
 false)

;; cp-next : cp-state -> (or (listof cp-state) 'goal)
;; next states function for Coin-problem search
(define (cp-next state)
  (match state
    [(cp-state n r b)
     (if (and (= n 6) (cp-board-even? b))
         'goal
         (local
           {(define row-el (filter (lambda (i) (= (sq-row i) r)) b))
            (define row-num (length row-el))
            (define (gen c)
              (cond
                [(> c 3) '()]
                [(>= n 6) '()]
                [(and (= r 2)
                      (= 1 (length (filter (lambda (i) (= (sq-row i) 3)) b))))
                 (list (make-cp-state n (add1 r) b))]
                [(= row-num 1)
                 (cond
                   [(= c (sq-col (first row-el))) (gen (add1 c))]
                   [else
                    (cons
                     (make-cp-state (add1 n) (add1 r) (cons (make-sq r c) b))
                     (gen (add1 c)))])]
                [(and (cp-row-even? b r)
                      (> row-num 1))
                 (list (make-cp-state n (add1 r) b))]
                [else
                 (gen-even-row state)]))} 
           (gen 0)))]))  
;; gen-even-row: cp-state -> (listof cp-state)
;; Returns all possible combinations to remove two coins from the row with all 4 coins
;; note*: it is the responsobility of the caller to make sure row in need had all 4 coins initially
;; (in my code I check for it so it is correct)
(define (gen-even-row state)
  (match state
    [(cp-state n r b)
     (local
       {;; helper: num num -> (listof cp-state)
        ;; Returns states with one fixed removed coin and a second coin depending on column
        (define (helper f s)
          (cond 
            [(> s 3) '()]
            [else (cons (make-cp-state (+ 2 n) (add1 r) 
                                       (append (list (make-sq r f) (make-sq r s)) b))
                        (helper f (add1 s)))]))
        ;; helper2: num -> (listof state)
        ;; primal use is to use "helper" function above for all columns for the first coin
        (define (helper2 f)
          (cond
            [(<= f 2) (append (helper f (add1 f)) (helper2 (add1 f)))]
            [else '()]))}
       (helper2 0))]))    
;; tests
(check-expect 
 (gen-even-row (make-cp-state 0 0 (list)))
(list
 (make-cp-state 2 1 (list (make-sq 0 0) (make-sq 0 1)))
 (make-cp-state 2 1 (list (make-sq 0 0) (make-sq 0 2)))
 (make-cp-state 2 1 (list (make-sq 0 0) (make-sq 0 3)))
 (make-cp-state 2 1 (list (make-sq 0 1) (make-sq 0 2)))
 (make-cp-state 2 1 (list (make-sq 0 1) (make-sq 0 3)))
 (make-cp-state 2 1 (list (make-sq 0 2) (make-sq 0 3)))))

(check-expect 
 (gen-even-row (make-cp-state 2 1 (list (make-sq 0 1) (make-sq 0 2))))
 (shared ((-6- (list (make-sq 0 1) (make-sq 0 2))))
   (list
    (make-cp-state 4 2 (cons (make-sq 1 0) (cons (make-sq 1 1) -6-)))
    (make-cp-state 4 2 (cons (make-sq 1 0) (cons (make-sq 1 2) -6-)))
    (make-cp-state 4 2 (cons (make-sq 1 0) (cons (make-sq 1 3) -6-)))
    (make-cp-state 4 2 (cons (make-sq 1 1) (cons (make-sq 1 2) -6-)))
    (make-cp-state 4 2 (cons (make-sq 1 1) (cons (make-sq 1 3) -6-)))
    (make-cp-state 4 2 (cons (make-sq 1 2) (cons (make-sq 1 3) -6-))))))

(define cp-start (make-cp-state 0 0 '()))
(cp-state-board (search cp-start cp-next))
;; tests
(check-expect 
 (cp-state-board (search (make-cp-state 1 0 (list (make-sq 0 1))) cp-next))
 (list (make-sq 2 1) (make-sq 2 2) (make-sq 1 0) 
       (make-sq 1 2) (make-sq 0 0) (make-sq 0 1)))

(check-expect 
 (cp-state-board (search (make-cp-state 1 0 (list (make-sq 0 0))) cp-next))
 (list (make-sq 2 1) (make-sq 2 2) (make-sq 1 0)
       (make-sq 1 2) (make-sq 0 1) (make-sq 0 0)))

(check-expect
 (cp-state-board (search (make-cp-state 1 0 (list (make-sq 0 0))) cp-next))
 (list (make-sq 2 1) (make-sq 2 2) (make-sq 1 0) 
       (make-sq 1 2) (make-sq 0 1) (make-sq 0 0)))

(check-expect
 (cp-state-board (search 
                  (make-cp-state 2 0 (list (make-sq 0 0) (make-sq 0 1))) 
                  cp-next))
 (list (make-sq 2 1) (make-sq 2 2) (make-sq 1 0)
       (make-sq 1 2) (make-sq 0 0) (make-sq 0 1)))

(check-expect 
 (cp-state-board (search 
                  (make-cp-state 3 1 
                                 (list (make-sq 0 1) (make-sq 0 3) (make-sq 3 1))) 
                  cp-next))
 (list (make-sq 3 0) (make-sq 1 0) (make-sq 1 3) 
       (make-sq 0 1) (make-sq 0 3) (make-sq 3 1)))

(check-expect 
 (search 
  (make-cp-state 3 1 
                 (list (make-sq 0 1) (make-sq 0 3) (make-sq 0 2))) 
  cp-next)
 false)
