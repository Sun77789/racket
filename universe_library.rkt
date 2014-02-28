(require 2htdp/image)
(require racket/match)
(require 2htdp/universe)

(define photo .) 
;; a world is a (make-world i cs r m show? done?) where 
;; - i is an image, 
;; - cs (the clicks) is a (listof posn),
;; - r is a num, 
;; - m is a string, 
;; - show? is a bool, and 
;; - done? is a bool
(define-struct world (i cs r m show? done?))

;; key handler:
;; z - erases the most recent click from the list.
;; c - clears all clicks from the current click list.
;; t - toggles the "show mode." 
;; + - increments the radius of the click circles by 1. 
;; - decrements the radius of the click circles by 1, but radius is never < 1
;; q -  quits.
(define options
  (local {(define s 16)
          (define col "black")}
    (above/align "left"
                 (text "Options:" s col)
                 (text "z - erases the most recent click from the list" s col)
                 (text "c - clears all clicks from the current click list." s col)
                 (text "t - toggles the show mode." s col)
                 (text "+ increments the radius of the click circles by 1. " s col)
                 (text " - decrements the radius of the click circles by 1" s col)
                 (text "q -  quits." s col))))
;; render: world -> image
;; Draws an image of the world
(define (render w)
  (match w
    [(world img cs r m show? done?) 
     (above (if show? 
                (foldl (lambda (f rt) 
                         (place-image 
                          (circle r "solid" (make-color 255 51 51 150)) (posn-x f) (posn-y f) rt))
                       img cs) img)
            (if show? (text (number->string (length cs)) 16 "purple") empty-image)
            (overlay (text m 16 "black") 
                     (rectangle (image-width img) (image-height (text m 16 "black")) "solid" (make-color 102 102 255 200)))
            options)]))

;; handle-key: world key -> world 
;; Handles keys for universe
(define (handle-key w k)
  (match w
    [(world img cs r m show? done?) 
     (cond
       [(key=? k "z") 
        (if (empty? cs) (make-world img '() r "Sorry, there is nothing to erase" show? done?) (make-world img (rest cs) r m show? done?))]
       [(key=? k "c") 
        (make-world img empty r "All clicks are cleared" show? done?)]
       [(key=? k "t") 
        (make-world img cs r "Show mode is switched" (not show?) done?)]
       [(key=? k "+") 
        (make-world img cs (add1 r) (string-append "Radius is now " (number->string r)) show? done?)]
       [(key=? k "-") 
        (if (>= r 2) 
            (make-world img cs (sub1 r) (string-append "Radius is now " (number->string r)) show? done?)
            w)]
       [(key=? k "q")
        (make-world img cs r "Bye!" show? true)]
       [else (make-world img cs r "Wrong key" show? done?)])])) 

;; handle-mouse: world num num mouse-event -> world 
;; Handles mouse events
(define (handle-mouse w x y e)
  (match w
    [(world img cs r m show? done?) 
     (cond 
       [(and (string=? e "button-up")
             show?
             (<= x (image-width img))
             (<= y (image-height img)))
        (make-world img (cons (make-posn x y) cs) r "You did it!" show? done?)]
       [else (make-world img cs r "Enjoy the game" show? done?)])]))
;; finish?: world -> bool
;; Checks whether to world should quit
(define (finish? w)
  (world-done? w))

;; big-bang
;; create universe
(big-bang 
 (make-world photo '() 10 "Welcome to the game" true false)
 [to-draw render]
 [on-key handle-key]
 [on-mouse handle-mouse]
 [stop-when finish?])
