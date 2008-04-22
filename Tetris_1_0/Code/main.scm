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

; The new piece code isn't optimal, if one drops down a piece when the 
;  counter is at 23, the time to move it sideways will be 1/24 sec ...
(define (update)
  (if (= counter 24)
      (begin
        (if (or (piece-on-bottom? (send *board* get-active-piece))
                (not (send *board* move-piece (send *board* get-active-piece) (cons 0 -1))))
            (send *board* add-piece-on-board-default (create-random-piece))))))
            

(define (piece-on-bottom? piece)
  (define bottom #f)
  (for-each
   (lambda (block)
     (if (= 0 (send block get-abs-y))
         (set! bottom #t)))
   (send piece get-blocks))
  bottom)

(send *board* add-piece-on-board-default (create-random-piece))

(define (handle-key-event key)
  (let ((active-piece (send *board* get-active-piece)))
    (cond
      ((eq? key #\q)
       (hide-gui *gui*)
       (stop-loop))
      ((eq? key 'up) 
       (send active-piece rotate))
      ((eq? key 'down) 
       (send *board* move-piece active-piece (cons 0 -1)))
      ((eq? key 'left)
       (send *board* move-piece active-piece (cons -1 0)))
      ((eq? key 'right)
       (send *board* move-piece active-piece (cons 1 0))))))

(start-loop)