;;TETRIS

;;MAIN

(define *debug* #f)

;; Load all files

(load "graphics.scm")
(load "objects/player.scm")
(load "objects/block.scm")
(load "objects/piece.scm")
(load "objects/board.scm")

(define (initiate-graphics)
  (load "graphics_engine.scm"))

(define pieces (vector I-piece% J-piece% L-piece% O-piece% S-piece% Z-piece% T-piece%))

(define create-random-piece (lambda () (make-object (vector-ref pieces (random 7)))))
(define *next-piece* (create-random-piece))
(define (generate-next-piece)
  (set! *next-piece* (create-random-piece)))

(define *board* (make-object board% (cons 10 20) 20))
(initiate-graphics)

(define (initialize)
  (send *board* set-player! (make-object player% "Test"))
  (send *board* add-piece-on-board-default *next-piece*)
  (generate-next-piece))

; The new piece code isn't optimal, if one drops down a piece when the 
;  counter is at 23, the time to move it sideways will be 1/24 sec ...
(define (update)
  (if (= (remainder *counter* 6) 0)
      (begin
        (if (or (send *board* on-bottom? (send *board* get-active-piece))
                (not (send *board* move-piece (send *board* get-active-piece) (cons 0 -1))))
            (begin
              (send *board* clean-up-board)
              (if (not (send *board* add-piece-on-board-default *next-piece*))
                  (begin
                    (stop-loop)
                    (draw-game-over-text))
                  (generate-next-piece)))))))

(define (handle-key-event key)
  (let ((active-piece (send *board* get-active-piece)))
    (cond
      ((eq? key #\p)
       (if *should-run* (stop-loop) (start-loop))
       (draw))
      ((eq? key #\q)
       (hide-gui *gui*)
       (stop-loop))
      ((eq? key #\space)
       (send *board* drop-down-piece active-piece)
       (set! *counter* 1))
      ((eq? key 'up) 
       (send active-piece rotate))
      ((eq? key 'down) 
       (send *board* move-piece active-piece (cons 0 -1)))
      ((eq? key 'left)
       (send *board* move-piece active-piece (cons -1 0)))
      ((eq? key 'right)
       (send *board* move-piece active-piece (cons 1 0))))))

(start-loop)

(define (print-coords)
  (for-each 
   (lambda (piece) 
     (display (send piece get-coord)))
   (send *board* get-pieces)))