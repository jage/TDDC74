;;TETRIS

;;MAIN


;; Load all files
(load "graphics.scm")
(load "objects/block.scm")
(load "objects/piece.scm")
(load "objects/board.scm")


;; Dummy
(define (update) #t)

(define (draw)
  (clear)
  (send *board* draw-board)
  (show))

(define window-width 200)
(define window-height 400)

(define piece-size 20)   ; pixels
(define board-width 10)  ; pieces
(define board-height 20) ; pieces

(define T '((0 1) (-1 0) (0 -1)))
(define S '((0 1) (1 1) (-1 0)))
(define L '((0 -1) (0 1) (1 1)))
(define I '((-1 0) (1 0) (2 0)))

(define *board* (make-object board% (cons 10 20)))
(send *board* add-piece-on-board (make-object piece% 'L L *yellow-brush*))

(define (draw-pieces list-of-pieces)
  (if (not (null? list-of-pieces))
      (begin
        (draw-piece (car list-of-pieces))
        (draw-pieces (cdr list-of-pieces)))))

(define (draw-piece piece)
  (if (not (null? piece))
      (begin
        (draw-block (car piece))
        (draw-piece (cdr piece)))))

(define (draw-block block)
  (draw-rectangle (* (car block) piece-size) (* (cadr block) piece-size) piece-size piece-size *black-pen* *blue-brush*))

(define (handle-key-event key)
  (let ((active-piece (send *board* get-active-piece)))
    (cond
      ((eq? key 'up) 
       (display "up\n")
       (send active-piece rotate))
      ((eq? key 'down) 
       (display "down\n")
       (send *board* move-piece active-piece 0 -1))
      ((eq? key 'left)
       (display "left\n")
       (send *board* move-piece active-piece -1 0))
      ((eq? key 'right)
       (display "right\n")
       (send *board* move-piece active-piece 1 0)))))

(start-loop)