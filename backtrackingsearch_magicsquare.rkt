;; Done by Malika Aubakirova
;; 
(require racket/match)

;; this function was presented in class
;; so it is not mine
;; after that follows my code

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


;; an ms-state is a (make-ms-state placed to-place) where
;; - placed is a (listof (or num false)) of length 9, and
;; - to-place is a (listof num) of length 0 to 9
(define-struct ms-state (placed to-place))

;; MY CODE

;; sum-st: ms-state -> bool
;; check whether sum of all rows, columns and diagonals is equal
(define (sum-st state)
  (match state
    [(ms-state p t)
     (local
       {(define (sum3 a b c)
          (+ (list-ref p a)
             (list-ref p b) 
             (list-ref p c)))}
       (= (sum3 0 1 2) (sum3 3 4 5)
          (sum3 6 7 8) (sum3 0 3 6)
          (sum3 1 4 7) (sum3 2 5 8)
          (sum3 0 4 8) (sum3 2 4 6)))]))
;; tests
(check-expect 
 (sum-st (make-ms-state (list 4 3 8 9 5 1 2 7 6) empty))
 true)
(check-expect 
 (sum-st (make-ms-state (list 4 3 8 9 5 1 2 6 7) empty))
 false)

;; list-set!: (listof num) num num -> (list of num)
;; places a number "num "in position "c" of the list
;; similar to vec-set! for vectors
(define (list-set! nums c num)
  (local
    {(define (lp count ls)
       (cond
         [(= count c) (cons num (rest ls))]
         [else (cons (first ls) (lp (add1 count) (rest ls)))]))}
    (lp 0 nums)))
;; tests
(check-expect 
 (list-set! (list 1 2 3) 2 5)
(list 1 2 5))
(check-expect 
 (list-set! (list 1 2 3) 0 5)
(list 5 2 3))

;; gen-poss: state num -> (listof ms-state)
;; places all numbers from "inventory" of the state to the given "square"
;; in other words sets all possible numbers to the given place in the list of board
(define (gen-poss state c)
  (local
    {(define p (ms-state-placed state))
     (define t (ms-state-to-place state))
     (define (lp ls)
       (cond
         [(empty? ls) '()]
         [else 
          (cons
           (make-ms-state (list-set! p c (first ls)) (remove (first ls) t))
           (lp (rest ls)))]))}
    (lp t)))
;; test
(check-expect
 (gen-poss (make-ms-state (list 5 2 #f #f #f #f #f #f #f) (list 1 3 4 6 7 8 9)) 2)
 (shared ((-12- (cons 4 -13-))
          (-13- (cons 6 -14-))
          (-14- (cons 7 -15-))
          (-15- (cons 8 -16-))
          (-16- (list 9))
          (-5- (list false false false false false false)))
   (list
    (make-ms-state (cons 5 (cons 2 (cons 1 -5-))) (cons 3 -12-))
    (make-ms-state (cons 5 (cons 2 (cons 3 -5-))) (cons 1 -12-))
    (make-ms-state (cons 5 (cons 2 (cons 4 -5-))) (cons 1 (cons 3 -13-)))
    (make-ms-state (cons 5 (cons 2 (cons 6 -5-))) (cons 1 (cons 3 (cons 4 -14-))))
    (make-ms-state (cons 5 (cons 2 (cons 7 -5-))) 
                   (cons 1 (cons 3 (cons 4 (cons 6 -15-)))))
    (make-ms-state (cons 5 (cons 2 (cons 8 -5-))) 
                   (cons 1 (cons 3 (cons 4 (cons 6 (cons 7 -16-))))))
    (make-ms-state (cons 5 (cons 2 (cons 9 -5-))) 
                   (list 1 3 4 6 7 8)))))
;; smart-gen: state num -> (listof ms-state)
;; places eaxctly a number so that the row is equal to desirable sum
;; instead of strying all numbers
(define (smart-gen state c)
  (local
    {(define p (ms-state-placed state))
     (define t (ms-state-to-place state))
     (define sum (- 15 (list-ref p (- c 2)) (list-ref p (- c 1))))
     (define (lp ls)
       (cond
         [(empty? ls) '()]
         [(= sum (first ls)) 
          (cons
           (make-ms-state (list-set! p c (first ls)) (remove (first ls) t))
           (lp (rest ls)))]
         [else (lp (rest ls))]))}
    (lp t)))
;; tests
(check-expect
 (smart-gen (make-ms-state (list 5 2 #f #f #f #f #f #f #f) 
                           (list 1 3 4 6 7 8 9)) 2)
 (list (make-ms-state 
        (list 5 2 8 false false false false false false)
        (list 1 3 4 6 7 9))))

(check-expect
 (smart-gen (make-ms-state (list 7 2 #f #f #f #f #f #f #f) 
                           (list 1 3 4 6 5 8 9)) 2)
 (list (make-ms-state 
        (list 7 2 6 false false false false false false)
        (list 1 3 4 5 8 9))))

; ms-next : ms-state -> (or (listof q-state) 'goal)
; next states function for magic-square search
(define (ms-next state)
  (match state
    [(ms-state p t)
     (if (and (not (member? false p)) (sum-st state))
         'goal
         (local
           {(define (gen c)
              (cond
                [(> c 8) '()]
                [(not (equal? #f (list-ref p c))) 
                 (gen (add1 c))]
                [(or (= c 2) (= c 5) (= c 8))
                 (smart-gen state c)]
                [else (gen-poss state c)]))}
           (gen 0)))]))

;; eyeball tests
(define srt (make-ms-state (make-list 9 false) (make-list 9 1)))
(search srt ms-next)
(define st1-9 (make-ms-state (make-list 9 false) (build-list 9 (lambda (t) (add1 t)))))
(search st1-9 ms-next)
;; more tests
(check-expect
 (search (make-ms-state (list 4 3 #f #f #f #f #f 7 #f)
                        (list 1 2 5 6 8 9)) ms-next)
 (make-ms-state (list 4 3 8 9 5 1 2 7 6) empty))

(check-expect (search 
               (make-ms-state (list 8 #f #f 3 #f #f #f #f #f) 
                              (list 1 2 4 5 6 7 9))
               ms-next) 
              (make-ms-state (list 8 1 6 3 5 7 4 9 2) empty))
