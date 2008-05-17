; Height and width for the drawing canvas
; Space for the board and some information on the left
;  and a help feature at the bottom
(define window-height
  (+ 4 (send *board* units->pixels (send *board* get-board-height))))
(define window-width
  (+ (send *board* units->pixels 6) (send *board* units->pixels (send *board* get-board-width))))

; VOID Draw all the graphics, this procedure is called from the main loop
(define (draw)
  (clear)
  (draw-pieces (send *board* get-pieces))
  (draw-score)
  (draw-status)
  (draw-interval)
  (draw-design)
  (draw-shadow)
  (show))

; VOID Draw the game status
(define (draw-status)
  (draw-text
   (send *supervisor* get-status)
   (+ 10 (send *board* units->pixels (send *board* get-board-width))) 
   40
   *black-pen* *black-brush*))

; VOID Draw the speed interval
(define (draw-interval)
  (draw-text
   (string-append "Interval: " (number->string (send *supervisor* get-interval-time)) " ms")
   (+ 10 (send *board* units->pixels (send *board* get-board-width))) 
   25
   *black-pen* *black-brush*))

; VOID Should check that the x-values are unique
(define (draw-shadow)
  (let ((x-coords '()))
    (for-each 
     (lambda (block)
       (set! x-coords (append x-coords (list (get-x (send block get-abs-coords))))))
     (send (send *board* get-active-piece) get-blocks))
    (for-each
     (lambda (coord)
       (draw-line 
        (send *board* units->pixels coord)
        (- window-height 2)
        (send *board* get-pixels-per-unit)
        0
        *black-pen* *black-brush*))
     x-coords)))

; VOID
; Draws a line on the right of the board,
; on the right side of the line there's some game info
(define (draw-design)
  (draw-line 
   (send *board* units->pixels (send *board* get-board-width)) 
   10
   0
   window-height
   *black-pen* *black-brush*))

; VOID Draw the current score
(define (draw-score)
  (draw-text
   (string-append "Score: "(number->string (send (send *board* get-player) get-score)))
   (+ 10 (send *board* units->pixels (send *board* get-board-width))) 
   10
   *black-pen* *black-brush*))

; VOID draws pieces 
; <- list-of-pieces [list piece%]
(define (draw-pieces list-of-pieces)
  (for-each
   (lambda (piece)
     (draw-piece piece))
   list-of-pieces))

; VOID draw pieces
; <- piece [piece%]
(define (draw-piece piece)
  (for-each
   (lambda (block)
     (draw-block (send block get-abs-coords) (send piece get-brush)))
   (send piece get-blocks)))

; VOID
; <- block-coords [coords]
; <- brush [brush]
(define (draw-block block-coords brush)
  (draw-rectangle 
   (* (get-x block-coords) (send *board* get-pixels-per-unit)) 
   (* (- (send *board* get-board-height) (get-y block-coords) 1) (send *board* get-pixels-per-unit))
   (send *board* get-pixels-per-unit) (send *board* get-pixels-per-unit) *black-pen* brush)
  
; Will be used for the 3D-effect later ...  
;  (draw-line
;   (* (car block-coords) (send *board* get-pixels-per-unit))
;   (* (- (send *board* get-board-height) (get-y block-coords) 1) (send *board* get-pixels-per-unit))
;   20
;   0
;   *gray-pen* brush)
  
  ; DEBUG
  (if (send *supervisor* debug?)
      (draw-text
       (string-append
        (number->string (get-x block-coords))
        "x"
        (number->string (get-y block-coords)))
       (* (get-x block-coords) (send *board* get-pixels-per-unit)) 
       (* (- (send *board* get-board-height) (get-y block-coords) 1) (send *board* get-pixels-per-unit))
       *black-pen* brush)))

;; --------------------------------------------------------------------
;; The animation loop
;; --------------------------------------------------------------------
(define *should-run* #f)

(define (start-loop)
  (when (not *should-run*)
    (set! *should-run* #t)
    (thread loop)
    (show-gui *gui*)))

(define (stop-loop)
  (set! *should-run* #f))

(define (fps->seconds fps)
  (/ 1 fps))

(define *sleep-time* (fps->seconds 24))

(define *counter* 1)
(define (loop)
  (when *should-run*
    (set! *counter* (+ *counter* 1))
    (update)
    (draw)
    (if (= *counter* 24) (set! *counter* 1))
    (sleep *sleep-time*)
    (loop)))

(define (time-to-update?)
  (= (remainder *counter* (send *supervisor* get-counter-divisor)) 0))

;; --------------------------------------------------------------------
;; The GUI and its components (buttons, menus etc)
;; --------------------------------------------------------------------

(define *frame* (make-object frame% "GUI"))

(instantiate button% 
  ("Quit" *frame* (lambda (e b) (hide-gui *gui*)))
  (horiz-margin 2)
  (vert-margin 2)
  (stretchable-width #f))

(define *menu-bar* 
  (instantiate menu-bar%
    (*frame*)))

(define *menu* 
  (instantiate menu%
    ("Menu" *menu-bar*)))

(instantiate menu-item%
  ("Quit" *menu* (lambda (a b) (hide-gui *gui*))))

(define *canvas*
  (instantiate my-canvas% ()
    (parent *frame*)
    (paint-callback draw-canvas)
    (key-callback key-fn)
    (min-height window-height) ;;global
    (min-width window-width) ;global
    (stretchable-width #f) 
    (stretchable-height #f)))

(define *buffer* (make-object bitmap% window-width window-height #f))
(define *dc* (make-object bitmap-dc% *buffer*))

(define *gui* 
  (make-gui 
   *frame*
   *canvas*
   *buffer*
   *dc*))
