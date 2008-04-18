;;TETRIS

;;MAIN


;; Load all files
(load "graphics.scm")
(load "graphics_engine.scm")
(load "objects/block.scm")
(load "objects/piece.scm")
(load "objects/board.scm")


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

(define (handle-key-event key)
  (let ((active-piece (send *board* get-active-piece)))
    (cond
      ((eq? key "q")
       (display "q")
       (hide-gui *gui*))
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
