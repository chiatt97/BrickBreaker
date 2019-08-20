;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw05) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;Assignment: HW05
;Name: Chandler Hiatt
;Date: 03-05-19
;URL: https://www.radford.edu/~itec380//2019spring-ibarland/Homeworks/lists/lists.html
;HW04: https://www.radford.edu/~itec380//2019spring-ibarland/Homeworks/hw04/hw04.html
;Overlap.rkt: https://www.radford.edu/~itec380//2019spring-ibarland/Homeworks/lists/overlap.rkt
;Some code derived from overlap.rkt, also used racket documentation.

(require 2htdp/image)
(require 2htdp/universe)


(check-expect (count-bigs 7 empty) 0)

(check-expect (count-bigs 7 (cons 5 empty)) 0)
(check-expect (count-bigs 5 (cons 5 empty)) 0)
(check-expect (count-bigs 3 (cons 5 empty)) 1)

(check-expect (count-bigs 7 (cons 2 (cons 5 empty))) 0)
(check-expect (count-bigs 3 (cons 2 (cons 5 empty))) 1)
(check-expect (count-bigs 2 (cons 2 (cons 5 empty))) 1)
(check-expect (count-bigs 1 (cons 2 (cons 5 empty))) 2)

(check-expect (count-bigs 7 (cons 10 (cons 2 (cons 5 empty)))) 1)
(check-expect (count-bigs 3 (cons 10 (cons 2 (cons 5 empty)))) 2)

;count-bigs : real, list-of-numbers -> real
;Returns the amount of numbers in a list that are bigger than the given threshold

(define (count-bigs thresh list)
  (cond [(empty? list) 0]
        [(cons? list) (cond [(< thresh (first list))
                               (+ 1 (count-bigs thresh (rest list)))]
                            [else (count-bigs thresh (rest list))])]))

(check-expect (map-sqr empty)  empty)
(check-expect (map-sqr (cons 7 empty))  (cons 49 empty))
(check-expect (map-sqr (cons 9 (cons 7 empty)))  (cons 81 (cons 49 empty)))

;map-sqr : list-of-numbers -> list-of-numbers
;Takes in a list of numbers, and squares every number in the list, and returns a new list of those numbers

(define (map-sqr list)
  (cond [(empty? list) '()]
        [(cons? list)
         (cons (* (first list) (first list)) ;Square first element of list
               (map-sqr (rest list)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; ## OLD CODE FROM HW04 ;;;;;;;;;;;;

;Data Definition
(define-struct team (name offense defense))
;make-team : string, natural, natural -> team

;Examples of team
(define t1 (make-team "Highlanders" 100 90))
(define t2 (make-team "Panthers" 60 60))
(define t3 (make-team "Steelers" 70 50))
(define t4 (make-team "Giants" 75 55))

;Function for a team
;func-for-team : team -> ???
(define (func-for-team a-team)
  (...(team-name a-team)
   ...(team-offense a-team)
   ...(team-defense a-team)))

;Test Cases for team>?

(check-expect (team>? t1 t2) #t)
(check-expect (team>? t2 t1) #f)
(check-expect (team>? t1 t3) #t)
(check-expect (team>? t3 t1) #f)
(check-expect (team>? t1 t4) #t)
(check-expect (team>? t4 t1) #f)
(check-expect (team>? t2 t3) #f)
(check-expect (team>? t3 t2) #f)
(check-expect (team>? t3 t4) #f)
(check-expect (team>? t4 t3) #f)

;team>? : team1, team2 -> false
;Return a boolean, #t if t1 is > than t2, #f if t1 is < than t2

(define (team>? t1 t2)
  (cond [(> (team-offense t1) (team-defense t2))
          (cond [(> (team-defense t1) (team-offense t2)) #t]
                [else #f])]
        [else #f]))

;Data Definition
(define-struct ball (x y xVel yVel))
;make-ball : int int int int -> ball

;Examples of Ball

(define b1 (make-ball 0 0 1 1))
(define b2 (make-ball 50 50 -1 1))
(define b3 (make-ball 225 469 1 -1))
(define b4 (make-ball 150 150 -1 -1))
(define b5 (make-ball 50 100 1 1))
(define b6 (make-ball 225 469 1 -1))

;Function for a ball
;func-for-ball : ball -> ???
(define (func-for-ball a-ball)
  (...(ball-x a-ball)
   ...(ball-y a-ball)
   ...(ball-xVel a-ball)
   ...(ball-yVel a-ball)))

;Data Definition
(define-struct paddle (x length))
;make-paddle : int int -> paddle

;Examples of paddle

(define paddle1 (make-paddle 225 20))

;Function for a paddle
;func-for-paddle : paddle -> ???
(define (func-for-paddle a-paddle)
  (...(paddle-x a-paddle)
   ...(paddle-length a-paddle)))

;Data Definition
(define-struct brick (x y))
;make-brick : int int -> brick

;examples of a brick
(define brick1 (make-brick 50 100))
(define brick2 (make-brick 140 100))
(define brick3 (make-brick 230 100))
(define brick4 (make-brick 320 100))
(define brick5 (make-brick 410 100))
(define brick6 (make-brick 500 100))

;Function for a brick
;func-for-brick : brick -> ???
(define (func-for-brick a-brick)
  (...(brick-x a-brick)
   ...(brick-y a-brick)))

;Test Cases for move-ball

(check-expect (move-ball b1) (make-ball 3 3 1 1))
(check-expect (move-ball b2) (make-ball 47 53 -1 1))
(check-expect (move-ball b6) (make-ball 228 466 1 -1)) 
(check-expect (move-ball b4) (make-ball 147 147 -1 -1))
(check-expect (move-ball (move-ball b1)) (make-ball 6 6 1 1))
(check-expect (move-ball (move-ball b2)) (make-ball 44 56 -1 1))
(check-expect (move-ball (move-ball b6)) (make-ball 231 463 1 -1))
(check-expect (move-ball (move-ball b4)) (make-ball 144 144 -1 -1))


;move-ball : ball -> ball
;Takes in a ball and returns a ball which moves once depending on the x/y velocity

(define (move-ball _ball)
  (cond [(> (ball-xVel _ball) 0)
            (cond [(> (ball-yVel _ball) 0)
                   (make-ball (+ 3 (ball-x _ball)) (+ 3 (ball-y _ball)) (ball-xVel _ball) (ball-yVel _ball))]
                  [else (make-ball (+ 3 (ball-x _ball)) (- (ball-y _ball) 3) (ball-xVel _ball) (ball-yVel _ball))])]
        [else (cond [(> (ball-yVel _ball) 0)
                   (make-ball (- (ball-x _ball) 3) (+ 3 (ball-y _ball)) (ball-xVel _ball) (ball-yVel _ball))]
                  [else (make-ball (- (ball-x _ball) 3) (- (ball-y _ball) 3) (ball-xVel _ball) (ball-yVel _ball))])]))

;bounce-side : ball -> ball
;Takes in a ball and returns a new ball with the x velocity reversed if it hit the side boundaries.

(define (bounce-side _ball)
  (cond [(<= (ball-x _ball) 0) (make-ball (ball-x _ball) (ball-y _ball) 1 (ball-yVel _ball))]
        [(>= (ball-x _ball) 800) (make-ball (ball-x _ball) (ball-y _ball) -1 (ball-yVel _ball))]))

;bounce-top-bottom : ball -> ball
;Takes in a ball and returns a new ball with the y velocity reversed if it hits the top/bottom boundaries.

(define (bounce-top-bottom _ball)
  (cond [(<= (ball-y _ball) 0) (make-ball (ball-x _ball) (ball-y _ball) (ball-xVel _ball) 1)]
        [(>= (ball-y _ball) 800) (make-ball (ball-x _ball) (ball-y _ball) (ball-xVel _ball) -1)]))

;paddle-handle-key : paddle, a-key-press -> paddle
;Takes in a paddle and a left or right arrow key press and returns a paddle moved into the desired direction.

(define (paddle-handle-key padd a-key)
  (cond [(key=? a-key "left") (make-paddle (- (paddle-x padd) 7) (paddle-length padd))]
        [(key=? a-key "right") (make-paddle (+ (paddle-x padd) 7) (paddle-length padd))]))

;Test cases for draw-brick

(check-expect (draw-brick (make-brick 0 0) (rectangle 500 500 "solid" "gray"))
              (place-image (rectangle 80 30 "solid" "black")
               0
               0
               (rectangle 500 500 "solid" "gray")))
(check-expect (draw-brick (make-brick 250 250) (rectangle 500 500 "solid" "gray"))
              (place-image (rectangle 80 30 "solid" "black")
               250
               250
               (rectangle 500 500 "solid" "gray")))

;draw-brick : brick, image -> image
;returns an image with a brick drawn onto the given image.

(define (draw-brick brick image)
  (place-image (rectangle 80 30 "solid" "black")
               (brick-x brick) 
               (brick-y brick)
               image))

;Test cases for draw-paddle

(check-expect (draw-paddle (make-paddle 200 20) (rectangle 500 500 "solid" "gray"))
              (place-image (rectangle 80 10 "solid" "red")
               200
               480
               (rectangle 500 500 "solid" "gray")))
(check-expect (draw-paddle (make-paddle 100 20) (rectangle 500 500 "solid" "gray"))
              (place-image (rectangle 80 10 "solid" "red")
               100
               480
               (rectangle 500 500 "solid" "gray")))

;draw-paddle : paddle, image -> image
;returns an image with a paddle drawn onto the given image. 

(define (draw-paddle paddle image)
  (place-image (rectangle 80 10 "solid" "red")
               (paddle-x paddle)
               480
               image))

;Test cases for draw-ball

(check-expect (draw-ball b1 (rectangle 500 500 "solid" "gray"))
              (place-image (circle 5 "solid" "blue")
               0
               0
               (rectangle 500 500 "solid" "gray")))
(check-expect (draw-ball b4 (rectangle 500 500 "solid" "gray"))
              (place-image (circle 5 "solid" "blue")
               150
               150
               (rectangle 500 500 "solid" "gray")))


;draw-ball : ball, image -> image
;returns an image with a ball drawn onto the given image.

(define (draw-ball ball image)
  (place-image (circle 5 "solid" "blue")
               (ball-x ball)
               (ball-y ball)
               image))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; ## CODE FOR REST OF HW05 ##
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Examples of data:

(define lob2 '())
(define lob3 (cons brick1 '()))
(define lob4 (cons brick1 (cons brick2 (cons brick3 '()))))
(define lob5 (cons brick1 (cons brick2 (cons brick3 (cons brick4 '())))))
(define lob1 (cons brick1 (cons brick2 (cons brick3 (cons brick4 (cons brick5 (cons brick6 '())))))))


#|         Data Definition:          |#
; a LoB ("list-of-bricks is:
;      '(), OR
;      (make-cons [brick] [LoB])

; template for processing a list of bricks:
; func-for-bricks : list-of-bricks -> ???
;
(define (func-for-bricks a-lob)
   (cond [(empty? a-lob)  ...]
         [(cons? a-lob)   (... (first a-lob) (func-for-list (rest a-lob)))]))

;test cases for draw-bricks
(check-expect (draw-bricks lob1 (rectangle 550 550 "solid" "gray"))
              (draw-brick (make-brick 500 100) 
              (draw-brick (make-brick 410 100)
              (draw-brick (make-brick 320 100)
              (draw-brick (make-brick 230 100)
              (draw-brick (make-brick 140 100)
                          (draw-brick (make-brick 50 100)(rectangle 550 550 "solid" "gray"))))))))

;draw-bricks : list-of-bricks, image -> image
;traverses a list drawing each brick onto the previous image

(define (draw-bricks l-o-b image)
  (cond [(empty? l-o-b) image]
        [(cons? l-o-b) (draw-bricks (rest l-o-b) (draw-brick (first l-o-b) image))]))

;data definition
;a world contains a paddle, list-of-bricks, and a ball
(define-struct world (paddle l-o-b ball))
;make-world : paddle, l-o-b, ball -> world

;Examples of world

(define world1 (make-world paddle1 lob1 b3))
(define world2 (make-world paddle1 lob1 b1))
(define world3 (make-world paddle1 lob1 b6))

;Function for a world
;func-for-world : world -> ???
(define (func-for-world a-world)
  (...(world-paddle a-world)
   ...(world-l-o-b a-world)
   ...(world-ball a-world)))

;Test cases for update-world
(check-expect (update-world world3)
              (make-world paddle1 lob1 (make-ball 228 466 1 -1)))
(check-expect (update-world world2) 
              (make-world paddle1 lob1 (make-ball -3 -3 -1 -1))) 

;update-world : world -> world
;Returns a new "world" one "tick" later ;;;;;;;;;;;;;;;;;;;;;;;

(define (update-world world)
  (make-world (world-paddle world)
              (bricks-remaining (world-l-o-b world) (world-ball world))
              (update-ball world)))

;world-handle-key : world, keypress -> world
;Handles keypresses and returns a new world depending on which key was pressed

(define (world-handle-key world keypress) 
  (cond [(key=? keypress "left")
         (make-world (paddle-handle-key (world-paddle world) keypress)
                     (world-l-o-b world)
                     (world-ball world))]
        [(key=? keypress "right")(make-world (paddle-handle-key (world-paddle world) keypress)
                     (world-l-o-b world)
                     (world-ball world))]))

;draw-world : world -> image
;Draws world with given inputs onto a blank gray screen

(define (draw-world world)
  (draw-bricks (world-l-o-b world)(draw-ball (world-ball world) (draw-paddle (world-paddle world) (rectangle 550 550 "solid" "gray")))))

#|-- CODE FROM OVERLAP.RKT --|#
(define (overlap? x1 y1 w1 h1  x2 y2 w2 h2)
  ; Check dist-btwn-centers, in each dimension:
  (and (overlap-1d? x1 w1 x2 w2)
       (overlap-1d? y1 h1 y2 h2)))

; overlap-1d? : real? real?  real? real?  -> boolean
; Given the centers (x1,x2) and widths (w1,w2) of two half open intervals,
; do they overlap?
(define (overlap-1d? x1 w1  x2 w2)
  (or (< (abs (- x1 x2)) (/ (+ w1 w2) 2))
      ; two intervals overlap if the centers are closer than the sum of the 2 radii.
      ; Use strictly-< to capture open intervals.
      ; For half-open, the case that the above formula doesn't work for is
      ; one (or both) of the rectangles being 0-width.  So check for the 0-width
      ; being on the left(closed) side:
      (= x1 (- x2 (/ w2 2))) ; r1=0 and x1 was on left edge of interval-2
      (= x2 (- x1 (/ w1 2))) ; r2=0 and x2 was on left edge of interval-1
      ))
#|---------------------------|#


;brick-collide-ball? : ball, brick -> bool
;Returns a boolean, 
;   #t, if a ball collides with a brick
;   #f, if a ball does not collide with a brick

(define (brick-collide-ball? ball brick)
  (cond [(overlap? (ball-x ball) (ball-y ball) 5 5 (brick-x brick) (brick-y brick) 80 30) #t]
        [else #f]))

#| Test Case for bricks-remaining  |#

(check-expect (bricks-remaining lob4 b5) (cons (make-brick -500 -500) (cons brick2 (cons brick3 '()))))

;bricks-remaining : list-of-bricks, ball -> list-of-bricks
;Returns a list with bricks not colliding with a ball
;Those that are colliding are put off the screen at x = -500 and y = -500

(define (bricks-remaining lob ball)
  (cond [(empty? lob) '()] 
        [(cons? lob) (cond [(boolean=? (brick-collide-ball? ball (first lob)) #f)
                            (cons (first lob) (bricks-remaining (rest lob) ball))]
                           [else (cons (make-brick -500 -500) (bricks-remaining (rest lob) ball))])])) 
 
;ball-collide-top-bottom : ball, int, int, int, int -> ball
;     r-x, is the rectangle you are checking collision for x value.
;     r-y, is the rectangle you are checking collision for x value.
;     r-w, is the rectangle you are checking collision for width.
;     r-h, is the rectangle you are checking collision for height.

(define (ball-collide-top-bottom ball r-x r-y r-w r-h)
  (cond [(overlap? (ball-x ball) (ball-y ball) 5 5 ;
                   r-x r-y r-w r-h)                ;IF BALL COLLIDES WITH TOP
         (make-ball (ball-x ball) (ball-y ball) (ball-xVel ball) (* (ball-yVel ball) -1))];THEN REVERSE Y VELOCITY
        [else (make-ball (ball-x ball) (ball-y ball) (ball-xVel ball) (ball-yVel ball))]))

;ball-collide-left-side : ball, int, int, int, int -> ball
;Checks to see if the
;     r-x, is the rectangle you are checking collision for x value.
;     r-y, is the rectangle you are checking collision for x value.
;     r-w, is the rectangle you are checking collision for width.
;     r-h, is the rectangle you are checking collision for height.

(define (ball-collide-left-side ball r-x r-y r-w r-h)
  (cond [(overlap? (ball-x ball) (ball-y ball) 5 5 ;
                   r-x r-y r-w r-h)                ;IF BALL COLLIDES WITH LEFT SIDE
         (make-ball (ball-x ball) (ball-y ball) (* (ball-xVel ball) -1) (ball-yVel ball))];THEN REVERSE X VELOCITY
        [else (make-ball (ball-x ball) (ball-y ball) (ball-xVel ball) (ball-yVel ball))]))

;ball-collide-right-side : ball, int, int, int, int -> ball
;Checks to see if the ball collides with the right side
;     r-x, is the rectangle you are checking collision for x value.
;     r-y, is the rectangle you are checking collision for x value.
;     r-w, is the rectangle you are checking collision for width.
;     r-h, is the rectangle you are checking collision for height.

(define (ball-collide-right-side ball r-x r-y r-w r-h)
  (cond [(overlap? (ball-x ball) (ball-y ball) 5 5 ;
                   r-x r-y r-w r-h)                ;IF BALL COLLIDES WITH RIGHT SIDE
         (make-ball (ball-x ball) (ball-y ball) (* (ball-xVel ball) -1) (ball-yVel ball))];THEN REVERSE X VELOCITY
        [else (make-ball (ball-x ball) (ball-y ball) (ball-xVel ball) (ball-yVel ball))]))

;ball-collide-sides : ball -> ball
;checks to see if the ball collides with either side of the frame.

(define (ball-collide-sides ball)
  (ball-collide-left-side (ball-collide-right-side ball 550 275 5 550) 0 275 5 550))

;reflect-ball-off-paddle : ball, paddle -> ball
;Inverses the y direction, once the ball hits a paddle

(define (reflect-ball-off-paddle ball paddle) 
  (cond [(overlap? (ball-x ball) (ball-y ball) 5 5 ;IF BALL
                   (paddle-x paddle) 480 80 10)    ;OVERLAPS
         (make-ball (ball-x ball) (ball-y ball) (ball-xVel ball) (* (ball-yVel ball) -1))] ;THEN SWITCH Y VELOCITY
        [else (make-ball (ball-x ball) (ball-y ball) (ball-xVel ball) (ball-yVel ball))]))

;reflect-ball-off-brick : ball, brick -> ball
;inverses the y direction once a ball hits a brick

(define (reflect-ball-off-brick ball brick)
  (cond [(overlap? (ball-x ball) (ball-y ball) 5 5
                   (brick-x brick) (brick-y brick) 80 30)
         (make-ball (ball-x ball) (ball-y ball) (ball-xVel ball) (* (ball-yVel ball) -1))]
        [else (make-ball (ball-x ball) (ball-y ball) (ball-xVel ball) (ball-yVel ball))]))

;reflect-ball-off-bricks : ball, list-of-bricks -> ball
;inverses the y direction, once a ball hits a brick.
;Checks to see if it hit any brick in the list

(define (reflect-ball-off-bricks ball lob)
  (cond [(empty? lob) (make-ball (ball-x ball) (ball-y ball) (ball-xVel ball) (ball-yVel ball))]
        [(cons? lob) (reflect-ball-off-bricks (reflect-ball-off-brick ball (first lob)) (rest lob))]))

#|   Test Cases for update-ball   |#

(check-expect (update-ball world3) (make-ball 228 466 1 -1))
(check-expect (update-ball world2) (make-ball -3 -3 -1 -1))
(check-expect (update-ball (make-world paddle1 lob1 b6)) (make-ball 228 466 1 -1))

;update-ball : world -> ball
;updates a ball, using the helper functions given previously
 
(define (update-ball world)
  (move-ball (reflect-ball-off-bricks
              (reflect-ball-off-paddle
              (ball-collide-sides  
               (ball-collide-top-bottom
                (world-ball world) 275 0 550 5)) (world-paddle world)) (world-l-o-b world))))

;ball-is-dead : world -> bool
;Returns #t if the ball has went below the paddle
;Returns #f if not
;This is used in the "stop-when" in the big-bang

(define (ball-is-dead world)
  (cond [(> (ball-y (world-ball world)) 550) #t]
        [else #f])) 

 
(big-bang world1
[on-key  world-handle-key]
[on-tick update-world]
[to-draw draw-world]
[stop-when ball-is-dead]
)
 














