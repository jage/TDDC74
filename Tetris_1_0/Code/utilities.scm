;; Tetris - objects/utilities.scm
;;
;; Written by Johan Eckerström and Viktor Deleskog
;; For TDDC74 at LiU, 2008


;;### COORDS ABSTRACTION ###

;;FUNC get coords object (abstracted over cons-pair)
;; <- x [num]
;; <- y [num]
;; -> [cons]
(define (coords x y)
  (cons x y))

;;FUNC get x coord
;; <- coords [coords]
;; -> [num]
(define (get-x coords)
  (car coords))

;;FUNC get y coord
;; <- coords [coords]
;; -> [num]
(define (get-y coords)
  (cdr coords))

;;VOID set x coord
;; <- coord [coords]
;; <- x [num]
(define (set-x! coords x)
  (set-car! coords x))

;;VOID set y coord
;; <- coords [coords]
;; <- y [num]
(define (set-y! coords y)
  (set-cdr! coords y))

;;VOID add dx to x coord
;; <- coords [coords]
;; <- dx [num]
(define (add-dx! coords dx)
  (set-car! coords (+ dx (get-x coords))))

;;VOID add dy to y coord
;; <- coords [coords]
;; <- dy [num]
(define (add-dy! coords dy)
  (set-cdr! coords (+ dy (get-y coords))))

;;VOID add dx, dy to coords
;; <- coords [coords]
;; <- dxdy-coord [coords]
(define (add-dxdy! coords dxdy-coords)
  (add-dx! coords (get-x dxdy-coords))
  (add-dy! coords (get-y dxdy-coords)))

;### SIZE ABSTRACTION ###

;;FUNC size object (abstracted over cons-pair
;; <- width [num]
;; <- height [num]
;; -> [size]
(define (size width height)
  (cons width height))

;;FUNC get width
;; <- size [size]
;; -> [num]
(define (get-width size)
  (car size))

;;FUNC get height
;; <- size [size]
;; -> [num]
(define (get-height size)
  (cdr size))

;;VOID set width
;; <- size [size]
;; <- width [num]
(define (set-width! size width)
  (set-car! size width))

;;VOID set height
;; <- size [size]
;; <- height [num]
(define (set-height! size height)
  (set-cdr! size height))

;### HELPER FUNCTIONS ###

;;FUNC convert direction to dxdy-coords
;; <- direction [symb] (up, down, left, right)
;; -> [coords]
(define (direction->dxdy direction)
  (cond
    ((eq? direction 'up) (coords 0 1))
    ((eq? direction 'down) (coords 0 -1))
    ((eq? direction 'left) (coords -1 0))
    ((eq? direction 'right) (coords 1 0))
    (else (coords 0 0))))

;;FUNC converts units -> pixels
;; <- units [num]
;; -> [num]
(define (units->pixels units)
  (* (send *board* get-pixels-per-unit) units))