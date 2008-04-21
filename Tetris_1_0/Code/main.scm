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

(define pieces (vector I-piece% J-piece% L-piece% O-piece% S-piece% Z-piece% T-piece%))

(define create-random-piece (lambda () (make-object (vector-ref pieces (random 7)))))
;(define create-random-piece (lambda () (make-object I-piece%)))

(define *board* (make-object board% (cons 10 20) 20))

(define (update)
  (if (= counter 24)
      (send *board* move-piece (send *board* get-active-piece) (cons 0 -1))
      (if (piece-on-bottom? (send *board* get-active-piece))
          (send *board* add-piece-on-board-default (create-random-piece)))))

(define (piece-on-bottom? piece)
  (define bottom false)
  (for-each
   (lambda (coord)
     (if (= 0 (cdr coord))
         (set! bottom true)))
   (send piece get-blocks-coords))
  bottom)

(send *board* add-piece-on-board-default (create-random-piece))

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