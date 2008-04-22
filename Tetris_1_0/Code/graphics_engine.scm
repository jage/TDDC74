; Lite småfult
(define window-height
  (send *board* units->pixels (send *board* get-height)))
(define window-width
  (send *board* units->pixels (send *board* get-width)))

(define (draw)
  (clear)
  (draw-pieces (send *board* get-pieces))
  (show))

(define (draw-pieces list-of-pieces)
  (for-each
   (lambda (piece)
     (draw-piece piece))
   list-of-pieces))

(define (draw-piece piece)
  (for-each
   (lambda (block)
     (draw-block (send block get-abs-coord) (send piece get-brush)))
   (send piece get-blocks)))

(define (draw-block block-coord brush)
  (draw-rectangle 
   (* (car block-coord) (send *board* get-pixels-per-unit)) 
   (* (- (send *board* get-height) (cdr block-coord) 1) (send *board* get-pixels-per-unit))
   (send *board* get-pixels-per-unit) (send *board* get-pixels-per-unit) *black-pen* brush))

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

(define counter 0)
(define (loop)
  (when *should-run*
    (set! counter (+ counter 1))
    (update)
    (draw)
    (if (= counter 24) (set! counter 1))
    (sleep *sleep-time*)
    (loop)))

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