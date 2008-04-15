;;TETRIS

;;MAIN

(load "graphics.scm")

(define (initialize)
  ;; Drawing canvas
  #t)

(define (update)
  #t)

(define window-width 200)
(define window-height 400)

(define piece-size 20)   ; pixels
(define board-width 10)  ; pieces
(define board-height 20) ; pieces

;(define I '((0 1) (0 2) (0 3) (0 4)))
;(define T '((0 0) (-1 0) (1 0) (0 1)))
;(define T '((0 0) (0 1) (-1 0) (0 -1)))
(define S '((0 0) (0 1) (1 1) (-1 0)))
(define L '((0 0) (0 -1) (0 1) (1 1)))
(define I '((0 0) (-1 0) (1 0) (2 0)))
(define activepiece I)

(define (move-piece piece delta-x delta-y)
  (if (and (not (null? piece)) (move-possible? piece delta-x delta-y))
      (let* ((block (car piece))
             (new-x (+ (car block) delta-x))
             (new-y (+ (cadr block) delta-y)))
        (begin
          (set-car! block new-x)
          (set-cdr! block (list new-y))
          (move-piece (cdr piece) delta-x delta-y)))))

(define (move-possible? piece delta-x delta-y)
  (if (null? piece)
      #t
      (let* ((block (car piece))
             (new-x (+ (car block) delta-x))
             (new-y (+ (cadr block) delta-y)))
        (if (and (>= new-x 0) (<= new-x (- board-width 1))
                 (>= new-y 0) (<= new-y (- board-height 1)))
            (move-possible? (cdr piece) delta-x delta-y)
            #f))))
; XXX
(define (move-possible? . args) #t)

; coordinate (1 2)
(define (delta-coordinate first second)
  (list (- (car second) (car first)) (- (cadr second) (cadr first))))

(define (move-block! block coordinates)
  (set-car! block (+ (car block) (car coordinates)))
  (set-cdr! block (list (+ (cadr block) (cadr coordinates)))))

; default
(define rotate-clockwise #t)

(define (rotate-piece! piece)
  (define (worker piece fix-block)
    (if (not (null? piece))
        (begin
          (let* ((block (car piece))
                 (placement (delta-coordinate fix-block block)))
            (if rotate-clockwise
                ; clockwise
                (cond
                  ((equal? '(0 1) placement)
                   (move-block! block '(-1 -1)))
                  ((equal? '(0 -1) placement)
                   (move-block! block '(1 1))) 
                  ((equal? '(1 0) placement)
                   (move-block! block '(-1 1)))
                  ((equal? '(1 1) placement)
                   (move-block! block '(-2 0))) 
                  ((equal? '(1 -1) placement)
                   (move-block! block '(0 2)))  
                  ((equal? '(-1 0) placement)
                   (move-block! block '(1 -1)))
                  ((equal? '(-1 1) placement)
                   (move-block! block '(0 -2)))  
                  ((equal? '(-1 -1) placement)
                   (move-block! block '(2 0)))
                  ;; Hack to handle the I-piece
                  ((equal? '(2 0) placement)
                   (move-block! block '(-2 2)))) ; (0 2)
                ; !clockvise
                (cond
                  ((equal? '(0 1) placement)
                   (move-block! block '(1 -1))) ; -> (1 0)
                  ((equal? '(0 -1) placement)
                   (move-block! block '(-1 1))) ; -> (-1 0)
                  ((equal? '(1 0) placement)
                   (move-block! block '(-1 -1))) ; -> (0 -1)
                  ((equal? '(1 1) placement)
                   (move-block! block '(0 -2))) ; -> (1 -1)
                  ((equal? '(1 -1) placement)
                   (move-block! block '(-2 0))) ; -> (-1 -1) 
                  ((equal? '(-1 0) placement)
                   (move-block! block '(1 1))) ; -> (0 1)
                  ((equal? '(-1 1) placement)
                   (move-block! block '(2 0)))  ; -> (1 1)
                  ((equal? '(-1 -1) placement)
                   (move-block! block '(0 2)))  ; -> (-1 1))
                  ; Hack to handle the I-piece
                  ((equal? '(0 2) placement)
                   (move-block! block '(2 -2))))) ; (2 0) 
            (display placement)
            (worker (cdr piece) fix-block)))))
  (worker piece (car piece)))

(define (draw-piece piece)
  (if (not (null? piece))
      (begin
        (draw-block (car piece))
        (draw-piece (cdr piece)))))

(define (draw-block block)
  (draw-rectangle (* (car block) piece-size) (* (cadr block) piece-size) piece-size piece-size *black-pen* *blue-brush*))

(define (draw)
  (clear)
  (draw-piece activepiece)
  (show))

(define (handle-key-event key)
  (cond
    ((eq? key #\space)
     (display "space")
     (set! rotate-clockwise (not rotate-clockwise)))
    ((eq? key 'up) 
     (display "up\n")
     (rotate-piece! activepiece))
    ((eq? key 'down) 
     (display "down\n")
     (move-piece activepiece 0 1))
    ((eq? key 'left)
     (display "left\n")
     (move-piece activepiece -1 0))
    ((eq? key 'right)
     (display "right\n")
     (move-piece activepiece 1 0))))

(start-loop)