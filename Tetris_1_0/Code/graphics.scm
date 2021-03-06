;; Tetris - graphics.scm
;;
;; Written by Annika Silvervarg at IDA
;; Modified by Johan Eckerström and Viktor Deleskog


;; ---------------------------------------------------------------------
;; GUI
;; ---------------------------------------------------------------------

;; CONSTRUCTOR

(define (make-gui frame canvas buffer dc)
  (send canvas focus)
  (list frame canvas buffer dc))

;; SELECTORS

(define (get-frame gui)
  (car gui))

(define (get-canvas gui)
  (cadr gui))

(define (get-buffer gui)
  (caddr gui))

(define (get-dc gui)
  (cadddr gui))

(define (show-gui gui)
  (send (get-frame gui) show #t))

(define (hide-gui gui)
  (send (get-frame gui) show #f))

;; ---------------------------------------------------------------------
;; Canvas
;; ---------------------------------------------------------------------

(define my-canvas%
  (class canvas%
    (override on-char)
    (init-field (key-callback #f))
    (define (on-char event)
      (when key-callback
        (key-callback event)))
    (super-instantiate ())))

(define (key-fn key-event)
  (let ((key (send key-event get-key-code)))
    (handle-key-event key)))

(define (draw-canvas canvas dc)
  (send dc draw-bitmap (get-buffer *gui*) 0 0))

(define (redraw)
  (send (get-canvas *gui*) on-paint))

;; ---------------------------------------------------------------------
;; Functions to draw
;; ---------------------------------------------------------------------

; A procedure that clears the GUI
(define (clear)
  (send *dc* clear))

; A procedure that sets the background color of the GUI
(define (background)
  (send *dc* set-background  (make-object color% (random 255) (random 255) (random 255))))

;; A procedures that draws an ellipse
(define (draw-circle x y size-x size-y pen brush)
  (send (get-dc *gui*) set-pen pen)
  (send (get-dc *gui*) set-brush brush)
  (send (get-dc *gui*) draw-ellipse x y size-x size-y))

;; A procedures that draws a rectangle
(define (draw-rectangle x y size-x size-y pen brush)
  (send (get-dc *gui*) set-pen pen)
  (send (get-dc *gui*) set-brush brush)
  (send (get-dc *gui*) draw-rectangle x y size-x size-y))

;; A procedures that draws a line
(define (draw-line x y size-x size-y pen brush)
  (send (get-dc *gui*) set-pen pen)
  (send (get-dc *gui*) set-brush brush)
  (send (get-dc *gui*) draw-line x y (+ x size-x) (+ y size-y)))

;; A procedures that draws text
(define (draw-text text x y pen brush)
  (send (get-dc *gui*) set-pen pen)
  (send (get-dc *gui*) set-brush brush)
  (send (get-dc *gui*) draw-text text x y))

;; A procedures that draws a picture from file
(define (draw-pic file x y)
  (send (get-dc *gui*) draw-bitmap (make-object bitmap% file 'unknown #f) x y))

; A procedure that shows the new GUI
(define (show)
  (redraw))

;; The colors to draw with:
(define *light-red-pen*
  (send the-pen-list find-or-create-pen "orangered" 1 'solid))
(define *red-pen*
  (send the-pen-list find-or-create-pen "red" 1 'solid))
(define *dark-red-pen*
  (send the-pen-list find-or-create-pen "darkred" 1 'solid))

(define *light-green-pen*
  (send the-pen-list find-or-create-pen "lawngreen" 1 'solid))
(define *green-pen*
  (send the-pen-list find-or-create-pen "green" 1 'solid))
(define *dark-green-pen*
  (send the-pen-list find-or-create-pen "darkgreen" 1 'solid))

(define *black-pen*
  (send the-pen-list find-or-create-pen "black" 1 'solid))

(define *light-blue-pen*
  (send the-pen-list find-or-create-pen "royalblue" 1 'solid))
(define *blue-pen*
  (send the-pen-list find-or-create-pen "blue" 1 'solid))
(define *dark-blue-pen*
  (send the-pen-list find-or-create-pen "darkblue" 1 'solid))

(define *light-yellow-pen*
  (send the-pen-list find-or-create-pen "yellow" 1 'solid))
(define *yellow-pen*
  (send the-pen-list find-or-create-pen "gold" 1 'solid))
(define *dark-yellow-pen*
  (send the-pen-list find-or-create-pen "goldenrod" 1 'solid))

(define *white-pen*
  (send the-pen-list find-or-create-pen "white" 1 'solid))

(define *light-cyan-pen*
  (send the-pen-list find-or-create-pen "lightcyan" 1 'solid))
(define *cyan-pen*
  (send the-pen-list find-or-create-pen "cyan" 1 'solid))
(define *dark-cyan-pen*
  (send the-pen-list find-or-create-pen "darkcyan" 1 'solid))

(define *light-magenta-pen*
  (send the-pen-list find-or-create-pen "mediumorchid" 1 'solid))
(define *magenta-pen*
  (send the-pen-list find-or-create-pen "magenta" 1 'solid))
(define *dark-magenta-pen*
  (send the-pen-list find-or-create-pen "darkmagenta" 1 'solid))

(define *light-orange-pen*
  (send the-pen-list find-or-create-pen "sandybrown" 1 'solid))
(define *orange-pen*
  (send the-pen-list find-or-create-pen "orange" 1 'solid))
(define *dark-orange-pen*
  (send the-pen-list find-or-create-pen "chocolate" 1 'solid))


(define *yellow-brush*
  (send the-brush-list find-or-create-brush "gold" 'solid))
(define *red-brush*
  (send the-brush-list find-or-create-brush "red" 'solid))
(define *blue-brush*
  (send the-brush-list find-or-create-brush "blue" 'solid))
(define *green-brush*
  (send the-brush-list find-or-create-brush "green" 'solid))
(define *white-brush*
  (send the-brush-list find-or-create-brush "white" 'solid))
(define *black-brush*
  (send the-brush-list find-or-create-brush "black" 'solid))
(define *cyan-brush*
  (send the-brush-list find-or-create-brush "cyan" 'solid))
(define *magenta-brush*
  (send the-brush-list find-or-create-brush "magenta" 'solid))
(define *orange-brush*
  (send the-brush-list find-or-create-brush "orange" 'solid))