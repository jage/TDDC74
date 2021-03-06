;; Main - main.scm
;;
;; Written by Johan Eckerström and Viktor Deleskog
;; For TDDC74 at LiU, 2008

;; -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
;; DEBUG-MODE
;; Type '(send *supervisor* enable(disable)-debug)' in the intepreter to
;; enable/disable debug-mode. Debug mode can be changed during the game.
;; The 'pixels-per-unit' variable should be set to >25 to see the block 
;; coordinates on the board. Must be set before the game is started!
;; debug: >25
;; default: 20
;; -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

;; -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
;; FOR VISUALLY HANDICAPPED PERSONS & WIDESCREEN NERDS
;; If you're visually handicapped or a 'must-have-a-widecreen-layout' person
;; increase the board size and pixels-per-unit if you want a bigger pieces or
;; bigger window layout. The variables to change are 'pixels-per-unit' and 
;; 'board-size' (width height).
;; Hope you enjoy this high-tech feature!
;; -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

;; Load all files

(load "utilities.scm")
(load "graphics.scm")
(load "objects/player.scm")
(load "objects/block.scm")
(load "objects/piece.scm")
(load "objects/board.scm")
(load "objects/supervisor.scm")

; Used by supervisor when it's time to show some graphics
(define (initiate-graphics)
  (load "graphics_engine.scm"))

(define board-size (coords 10 20)) ;; default 10 20
(define pixels-per-unit 25) ;; set to >25 when debugging (default 20)
(define *supervisor* (make-object supervisor% board-size pixels-per-unit))
(define *board* (send *supervisor* get-board))

; VOID Update the game
; This procedure runs in every frame, first it checks if it's time to do 
; anything, then checks if the active piece has hit the floor, it then 
; moves the active one step down.
;
; If the piece has hit the floor, or the move wasn't successful, the 
; board will be cleaned of filled rows, then if the next piece can't be
; placed, it's game over.

(define (update)
  (let ((active-piece (send *board* get-active-piece)) 
        (next-piece (send *supervisor* get-next-piece)))
    (if (and (time-to-update?)
             (or (send *board* on-bottom? active-piece)
                 (not (send *board* move-piece active-piece 'down))))
        (begin
          (send *board* clean-up)
          (if (not (send *board* add-piece-on-board-default next-piece))
              (send *supervisor* stop)
              (send *supervisor* generate-next-piece))))))

; VOID Handles the keyboard events
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

; Start the game
(send *supervisor* start)