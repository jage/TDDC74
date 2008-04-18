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
(define I '((0 1) (0 0) (0 -1) (0 -2))) ; Cyan
(define J '((-1 -1) (0 -1) (0 0) (0 1))) ; Blue
(define L '((1 -1) (0 -1) (0 0) (0 1))) ; Orange
(define O '((0 0) (0 -1) (1 0) (1 -1))) ; Yellow
(define S '((-1 0) (0 0) (0 1) (1 1)))
(define Z '((-1 1) (0 1) (0 0) (1 0)))
(define T '((-1 0) (0 0) (1 0) (0 1)))
; S Green
; T Purple
; Z Red

(define *board* (make-object board% (cons 10 20) 20))
;(send *board* add-piece-on-board-default (make-object piece% 'J J *yellow-brush*))

(send *board* add-piece-on-board-custom (make-object piece% 'L T *green-brush*) (cons 5 10) #f)

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