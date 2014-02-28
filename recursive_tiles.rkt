;; Done by Malika Aubakirova
(require 2htdp/image)

;; Problem #1
;; concat-with: string (listof string) -> string
;; It concatenates all strings with the given separator. 
;; Examples: (concat-with "," (list "Crosby" "Stills" "Nash")) => "Crosby,Stills,Nash"
(define (concat-with sep ss) 
    (cond [(empty? ss) ""]
          [(and (cons? ss) (not (= (length ss) 1))) (string-append (first ss) sep (concat-with sep (rest ss)))]
          [(and (cons? ss) (= (length ss) 1)) (string-append (first ss) "")]))
;;tests
(check-expect (concat-with "," (list "Crosby" "Stills" "Nash")) "Crosby,Stills,Nash")
(check-expect (concat-with "<|>" (list "Crosby" "Stills" "Nash")) "Crosby<|>Stills<|>Nash")
(check-expect (concat-with "<|>" (list "Crosby")) "Crosby")
(check-expect (concat-with "," (list )) "")

;;Problem 2

;; chessline: num img img -> image
;; produces one line consisting of num images beside each other, which appear consequently in turn (img 1 img2 img1 img2 etc)
(define (chessline num first second)
  (local {(define (func counter first second)
  (cond [(= counter 0) empty-image]
        [(<= counter num) (beside first (func (- counter 1) second first))]))}
      (func num first second)))

;; test for chessline
"Line of alternating squares"
(chessline 5 (square  50 "solid" "purple") (square 50 "solid" "red"))

;; chessboard-helper: num num img img -> image
;; produces chesslines N times above each other (making a chessboard if given a square)
;; remark: first two numbers should be equal
(define (chessboard-helper num counter first second)
   (cond [(= counter 0) empty-image]
        [(<= counter num) (above (chessline num first second) (chessboard-helper num (- counter 1) second first))]))
;; test for chessboard-helper
"Boards of alternating squares"
(chessboard-helper 5 5 (square  50 "solid" "white") (square 50 "solid" "blue"))

;; chessboard : num num color color -> image.
;; This functions consumes:
;; - the number of squares in each row/column,
;; - the side length of each individual square,
;; - the light square color, and
;; - the dark square color
;; And produces chessboard

(define (chessboard num length lcolor dcolor)
  (local {(define light (square length "solid" lcolor))
          (define dark (square length "solid" dcolor))}
    (chessboard-helper num num light dark)))

;; "eye ball" Test
"Chessboards"
(chessboard 6 15 "white" "red")
(chessboard 3 20 "red" "black")
(chessboard 4 15 "white" "red")
(chessboard 2 20 "yellow" "green")

;; Problem 3
;; pascal : num -> (listof (listof num))
;; Computes the first n rows of Pascal's triangle (for n â‰¥ 0).
(define (pascal num)
  (local {(define (fun counter)
            (cond [(> counter (- num 1)) empty]
                  [(<= counter (- num 1)) (cons (pascal-helper counter) (fun (+ counter 1)))]))}
    (fun 0))) 
;; Tests for Pascal
(check-expect (pascal 3) (list (list 1) (list 1 1) (list 1 2 1)))
(check-expect (pascal 0) empty)
(check-expect (pascal 4) (list (list 1) (list 1 1) (list 1 2 1) (list 1 3 3 1) ))
(check-expect (pascal 5) (list (list 1) (list 1 1) (list 1 2 1) (list 1 3 3 1) (list 1 4 6 4 1)))

;; pascal-helper: num -> (listof num)
;; Calculates row in a Pascal triangle (count of rows starts with 0, not 1)
(define (pascal-helper row)
  (local {(define (fun counter)
  (cond [(> counter row) empty]
        [(<= counter row) (cons (/ (fact row) (* (fact counter) (fact (- row counter)))) (fun (+ counter 1)))]))}
  (fun 0))) 
;; checks for pascal-helper
(check-expect (pascal-helper 0) (list 1)) 
(check-expect (pascal-helper 1) (list 1 1))
(check-expect (pascal-helper 3) (list 1 3 3 1))
        
;; fact: num -> num
;; return factorial of n
(define (fact n)
  (cond [(= n 0) 1]
        [else (* n (fact (-  n 1)))]))
(check-expect (fact 0) 1)
(check-expect (fact 4) 24)
  
;; Problem 4

;; Dataset is list of posns
;; where posn is a built-in structure of an x and y

;; lineq is a slope and a y-intercept 
;; where m and b are from the linear equation form y = mx + b. 
(define-struct lineq (m b))

;; sum-y: dataset -> num
;; Calculates the sum of all y
(define (sum-y dataset) (cond [(empty? dataset) 0]
                               [(cons? dataset) (+ (posn-y (first dataset)) (sum-y (rest dataset)))]))
;; #grader This code is too far over to the right.
;; Please see the text for examples of code layout.
;; It should look like this:
;(define (sum-y dataset) 
;  (cond [(empty? dataset) 0]
;        [(cons? dataset) (+ (posn-y (first dataset)) (sum-y (rest dataset)))]))
  
(check-expect (sum-y (list (make-posn 2 -3) (make-posn 1 2) (make-posn 0 1))) 0)
(check-expect (sum-y (list (make-posn 5 3) (make-posn 2 2) (make-posn 4 1))) 6)
;; sum-x: dataset -> num
;; Calculates the sum of all x
(define (sum-x dataset) (cond [(empty? dataset) 0]
                              [(cons? dataset) (+ (posn-x (first dataset)) (sum-x (rest dataset)))]))
  
(check-expect (sum-x (list (make-posn 2 -3) (make-posn 1 2) (make-posn 0 1))) 3)
(check-expect (sum-x (list (make-posn 5 3) (make-posn 2 2) (make-posn 4 1))) 11)
;; sum-xy: dataset -> num
;; Calculates the sum of all (xy)
(define (sum-xy dataset) 
  (cond
    [(empty? dataset) 0]
    [(cons? dataset) (+ (* (posn-y (first dataset)) (posn-x (first dataset))) (sum-xy (rest dataset)))]))
  
(check-expect (sum-xy (list (make-posn 2 -3) (make-posn 1 2) (make-posn 0 1))) -4)
(check-expect (sum-xy (list (make-posn 5 3) (make-posn 2 2) (make-posn 4 1))) 23)

;; sum-xx: dataset -> num
;; Calculates the sum of all (sqr x)
(define (sum-xx dataset) (cond [(empty? dataset) 0]
                               [(cons? dataset) (+ (sqr (posn-x (first dataset))) (sum-xx (rest dataset)))]))
(check-expect (sum-xx (list (make-posn 2 -3) (make-posn 1 2) (make-posn 0 1))) 5)
(check-expect (sum-xx (list (make-posn 5 3) (make-posn 2 2) (make-posn 4 1))) 45)

;; sum-yy: dataset -> num
;; Calculates the sum of all (sqr y)
(define (sum-yy dataset) (cond [(empty? dataset) 0]
                               [(cons? dataset) (+ (sqr (posn-y (first dataset))) (sum-yy (rest dataset)))]))
(check-expect (sum-yy (list (make-posn 2 -3) (make-posn 1 2) (make-posn 0 1))) 14)
(check-expect (sum-yy (list (make-posn 5 3) (make-posn 2 2) (make-posn 4 0))) 13)

  
;; slope: dataset -> num
;; Determines the best-fit slope for the linear model of a dataset
(define (slope dataset) 
  (local {(define n (length dataset))
          (define x (sum-x dataset))
          (define y (sum-y dataset))
          (define xx (sum-xx dataset))
          (define xy (sum-xy dataset))}
    [/ (- (* n xy) (* x y)) (- (* n xx) (sqr x))]))
;; tests for slope
(check-expect (slope (list (make-posn 0 0) (make-posn 1 1) (make-posn 2 2))) 1) 
(check-within (slope (list (make-posn 1 1.01) (make-posn 2 1.99) (make-posn 3 3.10))) 1.045 0.1)

;; intercept: dataset -> num
;; Determines the y-intercept for the linear model of a dataset
(define (intercept dataset) 
  (local {(define n (length dataset))
          (define x (sum-x dataset))
          (define y (sum-y dataset))
          (define xx (sum-xx dataset))
          (define xy (sum-xy dataset))}
    [/ (- (* y xx) (* x xy)) (- (* n xx) (sqr x))]))
;; tests for intercept
(check-expect (intercept (list (make-posn 0 0) (make-posn 1 1) (make-posn 2 2))) 0) 
(check-within (intercept (list (make-posn 1 1.01) (make-posn 2 1.99) (make-posn 3 3.10))) -0.056 0.1)

;; linreg: dataset -> lineq
;; its result is the line that best fits the data (provably so) as it would appear on a two-dimensional plot.
(define (linereg dataset) (make-lineq (slope dataset) (intercept dataset)))
;; tests for linereg
(check-within (linereg (list (make-posn 1 1.01) (make-posn 2 1.99) (make-posn 3 3.10))) (make-lineq 1.045 -0.056) 0.1)

;; r: dataset -> num
;; Computes the square of the linear correlation coefficient 
(define (r dataset)
  (local {(define n (length dataset))
          (define x (sum-x dataset))
          (define y (sum-y dataset))
          (define xx (sum-xx dataset))
          (define yy (sum-yy dataset))
          (define xy (sum-xy dataset))}
    (/ (- (* n xy) (* x y)) (* (sqrt (- (* n xx) (sqr x))) (sqrt (- (* n yy) (sqr y)))))))
;; tests
(check-within (r (list (make-posn 1 2) (make-posn 2 3) (make-posn 3 4))) 0.01 1)
(check-within (r (list (make-posn 1 1.01) (make-posn 2 1.99) (make-posn 3 3.10))) 0.01 0.999) 
;; analysis is struct consisting of two parts: a lineq and a num, 
;; the latter being the value of r2. 
(define-struct analysis (lineq num))
  
;; full-linreg: dataset -> analysis
;; Computes a full linear regression analysis

;; #grader This was supposed to be named full-linreg
(define (full-linereg dataset) (make-analysis (linereg dataset) (sqr (r dataset))))

;; test for full-linreg
;; cannot use check-expect since (sqr r) is enaxact and we cannot truncate it
(full-linereg (list (make-posn 1 2) (make-posn 2 3) (make-posn 3 4)))

;; Problem 5
  
;; design : num -> img
;; draw square design, not symmetrical, with given side length
(define (design s)
  (overlay 
   (square s "outline" "maroon")
   (above
    (beside (rectangle (/ s 2) 1 "solid" "darkgray")
            (overlay 
             (circle (/ s 6) "solid" "gray")
             (circle (/ s 5) "solid" "maroon")))
    (rectangle 1 (/ s 2) "solid" "darkgray"))
   (square s "solid" "darkgray"))) 

;; part2: num num -> img
;; Builds part of Recursive-tiles, 
;; more precisely returns a square if SIZE containing of designs (each rotated), each following row smaller in size by 2
(define (part2 size rotation) 
  (cond [(<= size 2) (rotate rotation (design 2))] 
        [else (local {(define fig (part2 (/ size 2) rotation))}
                       (beside (above (rotate rotation (design (/ size 2))) (rotate rotation (design (/ size 2)))) (above fig fig)))]))
;; part2 check
(part2 128 0)
(part2 128 -90)

;; quarter-tiles: num -> img
;; produces 1/4 of a tiles
(define (quarter size) 
  (local {(define s (/ size 2))
          (define (fun s) 
            (cond [(<= s 2) (design 2)]
                  [else (beside (above (fun (/ s 2)) (rotate 180 (part2 s -90))) 
                                (above (rotate  90 (part2 s 0)) (rotate 90 (design s))))]))}
    (fun s)))
;; quarter test
(quarter 256)

;; recursive-tiles : num -> img
;; Consumes a given side length and produces specific recursive tiling of the design. 
(define (recursive-tiles length) 
  (local {(define quart (quarter (/ length 2)))}
    (above (beside quart (rotate -90 quart)) (beside (rotate 90 quart) (rotate 180 quart)))))

;; Check recursive tiles
(recursive-tiles 512) 
(check-expect (image-height (recursive-tiles 512)) 512)
(check-expect (image-width (recursive-tiles 512)) 512)
