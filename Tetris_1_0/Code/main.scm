;;TETRIS

;;MAIN

;; Load all files

(load "utilities.scm")
(load "graphics.scm")
(load "objects/player.scm")
(load "objects/block.scm")
(load "objects/piece.scm")
(load "objects/board.scm")
(load "objects/supervisor.scm")

(define (initiate-graphics)
  (load "graphics_engine.scm"))

(define board-size (coords 10 20))
(define pixels-per-unit 20)
(define *supervisor* (make-object supervisor% board-size pixels-per-unit))
(define *board* (send *supervisor* get-board))

(define (update)
  (if (and (time-to-update?)
           (or (send *board* on-bottom? (send *board* get-active-piece))
               (not (send *board* move-piece (send *board* get-active-piece) 'down))))
      (begin
        (send *board* clean-up-board)
        (if (not (send *board* add-piece-on-board-default (send *supervisor* get-next-piece)))
            (send *supervisor* stop)
            (send *supervisor* generate-next-piece)))))

(define (handle-key-event key)
  ;; Block if game is paused, unless the key is for unpausing, reset or quit
  (if (or (not (send *supervisor* paused?))
          (eq? key #\p) (eq? key #\r) (eq? key #\q))
      (let ((active-piece (send *board* get-active-piece)))
        (cond
          ((eq? key #\i)
           (send *supervisor* increase-speed!))
          ((eq? key #\d)
           (send *supervisor* decrease-speed!))
          ((eq? key #\p)
           (send *supervisor* pause))
          ((eq? key #\q)
           (send *supervisor* quit))
          ((eq? key #\r)
           (send *supervisor* quit)
           (send *supervisor* start))
          ((eq? key #\space)
           (send *board* drop-down-piece active-piece)
           (set! *counter* 1))
          ((eq? key 'up)
           (send active-piece rotate))
          ((eq? key 'down)
           (send *board* move-piece active-piece 'down))
          ((eq? key 'left)
           (send *board* move-piece active-piece 'left))
          ((eq? key 'right)
           (send *board* move-piece active-piece 'right))))))

(send *supervisor* start)