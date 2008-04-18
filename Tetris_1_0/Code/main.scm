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
  (draw-pieces (send *board* get-pieces))
  (show))

(define window-width 200)
(define window-height 400)

(define piece-size 20)   ; pixels
(define board-width 10)  ; pieces
(define board-height 20) ; pieces

;; '(0 0) 
(define I '((0 -2) (0 -1) (0 0) (0 1))) ; Cyan
(define J '((-1 -1) (-1 0) (0 0) (1 0))) ; Blue
(define L '((-1 0) (0 0) (1 0) (1 1))) ; Orange
(define O '((0 0) (1 0) (1 1) (0 1))) ; Yellow
; S Green
; T Purple
; Z Red

(define T '((0 1) (-1 0) (0 -1)))
(define S '((0 1) (1 1) (-1 0)))
(define L '((-1 0) (1 0)))
(define I '((-1 0) (1 0) (2 0)))

(define *board* (make-object board% (cons 10 20) 20))
(send *board* add-piece-on-board-custom (make-object piece% 'S S *yellow-brush*) (cons 3 8) #f)

(send *board* add-piece-on-board-default (make-object piece% 'L L *green-brush*))

(define (draw-pieces list-of-pieces)
  (for-each
   (lambda (piece)
     (draw-piece piece))
   list-of-pieces))

(define (draw-piece piece)
  (for-each
   (lambda (block)
     (draw-block block (send piece get-brush)))
   (send piece get-blocks-coords)))

(define (draw-block block-coord brush)
  (draw-rectangle 
   (* (car block-coord) (send *board* get-pixels-per-unit)) 
   (* (- (send *board* get-height) (cdr block-coord) 1) (send *board* get-pixels-per-unit))
   (send *board* get-pixels-per-unit) (send *board* get-pixels-per-unit) *black-pen* brush))

(define (handle-key-event key)
  (let ((active-piece (send *board* get-active-piece)))
    (cond
      ((eq? key 'up) 
       (display "up\n")
       (send active-piece rotate))
      ((eq? key 'down) 
       (display "down\n")
       (send *board* move-piece active-piece (cons 0 -1)))
      ((eq? key 'left)
       (display "left\n")
       (send *board* move-piece active-piece (cons -1 0)))
      ((eq? key 'right)
       (display "right\n")
       (send *board* move-piece active-piece (cons 1 0))))))

(start-loop)