;; Tetris - objects/piece.scm
;;
;; Written by Johan Eckerström and Viktor Deleskog
;; For TDDC74 at LiU, 2008

;;PIECE CLASS

(define piece%
  (class object%
    (super-new)

    ;;### FIELDS ###
    (init piece-structure brush light-pen dark-pen)

    (define _piece-structure piece-structure)
    (define _brush brush) ; [brush]
    (define _light-pen light-pen) ; [pen]
    (define _dark-pen dark-pen) ; [pen]
    (define _blocks '())
    (define _clockwise #t) ; [bool]

    ;;### CONSTRUCTOR ###

    (define/private (constructor)
      (create-piece _piece-structure))

    ;;VOID creates piece from block list
    ;;called by constructor proc
    ;; <- list [list cons]
    (define/private (create-piece list)
      (if (not (null? list))
          (begin
            (let ((pair (car list)))
              (send this add-block! (coords (car pair) (cadr pair)))
              (create-piece (cdr list))))))

    ;;calls the constructor
    (constructor)

    ;;### FIELD ACCESSORS ###

    ;;GET blocks
    ;; -> [list block%]
    (define/public (get-blocks)
      _blocks)

    ;;GET brush
    ;; -> [brush]
    (define/public (get-brush)
      _brush)

    ;;GET light pen
    ;; -> [pen]
    (define/public (get-light-pen)
      _light-pen)
    
    ;;GET dark pen
    ;; -> [pen]
    (define/public (get-dark-pen)
      _dark-pen)
    
    ;;GET rotateable (override in child classes)
    ;; -> [bool]
    (define/public (rotate?)
      #f)

    ;;GET toggable (override in child classes)
    ;; -> [bool]
    (define/public (toggle?)
      #f)

    ;;### METHODS ###

    ;;VOID add block
    ;; <- struct-coords [coords]
    (define/public (add-block! struct-coords)
      (set! _blocks 
            (append _blocks (list (make-object block% struct-coords this)))))

    ;;VOID move piece to
    ;; <- board-coords [coords]
    (define/public (move-to! board-coords)
      (for-each
       (lambda (block)
         (send block move-to! 
               (coords (+ (get-x (send block get-struct-coords)) 
                          (get-x board-coords))
                       (+ (get-y (send block get-struct-coords)) 
                          (get-y board-coords)))))
       _blocks))

    ;;VOID move blocks in xy-plane
    ;; <- direction [symb] (up, down, right, left)
    (define/public (move! direction)
      (for-each
       (lambda (block)
         (send block move! direction))
       _blocks))

    ;;VOID move blocks in xy-plane
    ;; <- dxdy-coords [coords]
    (define/public (move-dxdy! dxdy-coords)
      (for-each
       (lambda (block)
         (send block move-dx! (get-x dxdy-coords))
         (send block move-dy! (get-y dxdy-coords)))
       _blocks))

    ;;VOID revert rotation
    ;; -> [bool]
    ;;comment: use with care!
    (define/public (revert-rotation!)
      (rotate-worker (not _clockwise)))

    ;;VOID rotation worker
    ;; <- clockwise [bool]
    (define/private (rotate-worker clockwise)
      (for-each
       (lambda (block)
         (let ((new (rotate-block-struct-coords block clockwise))
               (old (send block get-struct-coords))
               (abs (send block get-abs-coords)))
           (send block set-struct-coords! new)
           (send block set-abs-coords!
                 (coords (+ (get-x abs) (- (get-x new) (get-x old)))
                         (+ (get-y abs) (- (get-y new) (get-y old)))))))
       _blocks))

    ;;VOID rotate block structure coords
    ;; <- block [block%]
    ;; <- clockwise [bool]

    ;; Counter clockwise rotation
    ;; Keep the pieces inside this shape ...
    ;; O = Origo (0 . 0)
    ;;  X X X
    ;;  X O X X
    ;;  X X X
    ;;    X
    (define/private (rotate-block-struct-coords block clockwise)
      (let ((struct-pos (send block get-struct-coords)))
        (if clockwise
            (cond
              ((equal? struct-pos (coords 0 1))   (coords -1 0))
              ((equal? struct-pos (coords 0 -1))  (coords 1 0))
              ((equal? struct-pos (coords 1 0 ))  (coords 0 1))
              ((equal? struct-pos (coords 1 1))   (coords -1 1))
              ((equal? struct-pos (coords 1 -1))  (coords 1 1))
              ((equal? struct-pos (coords -1 0))  (coords 0 -1))
              ((equal? struct-pos (coords -1 1))  (coords -1 -1))
              ((equal? struct-pos (coords -1 -1)) (coords 1 -1))
              ((equal? struct-pos (coords 2 0))   (coords 0 2))
              (else struct-pos))
            (cond
              ((equal? struct-pos (coords 0 1))   (coords 1 0))
              ((equal? struct-pos (coords 0 -1))  (coords -1 0))
              ((equal? struct-pos (coords 1 0 ))  (coords 0 -1))
              ((equal? struct-pos (coords 1 1))   (coords 1 -1))
              ((equal? struct-pos (coords 1 -1))  (coords -1 -1))
              ((equal? struct-pos (coords -1 0))  (coords 0 1))
              ((equal? struct-pos (coords -1 1))  (coords 1 1))
              ((equal? struct-pos (coords -1 -1)) (coords -1 1))
              ((equal? struct-pos (coords 0 2))   (coords 2 0))
              (else struct-pos)))))

    ;;VOID delete block from piece
    ;; <- block [block%]
    (define/public (delete-block block)
      (set! _blocks (remove block _blocks)))

    ;;### FUNCTIONS ###

    ;;FUNC rotate piece
    ;; -> [bool]
    (define/public (rotate)
      (if (rotate?)
          (begin
            (rotate-worker _clockwise)
            (if (revert-rotation?)
                (revert-rotation!)
                (if (toggle?) (set! _clockwise (not _clockwise))))))
      #f)

    ;;FUNC revert rotation possible?
    ;; Has it collided or moved out of canvas?
    ;; -> [bool]
    (define/private (revert-rotation?)
      (or (send *board* will-collide? this (coords 0 0)))
      (not (send *board* move-possible? this (coords 0 0))))))

;;TETRIS PIECES

;;The I-piece is broken ...
(define I '((-1 0) (0 0) (1 0) (2 0)))   ; Cyan
(define J '((-1 -1) (0 -1) (0 0) (0 1))) ; Blue
(define L '((1 -1) (0 -1) (0 0) (0 1)))  ; Orange
(define O '((0 0) (0 -1) (1 0) (1 -1)))  ; Yellow
(define S '((-1 0) (0 0) (0 1) (1 1)))   ; Green
(define Z '((-1 1) (0 1) (0 0) (1 0)))   ; Red
(define T '((-1 0) (0 0) (1 0) (0 1)))   ; Magenta

;;I
(define I-piece%
  (class piece%
    (override toggle? rotate?)
    (define (toggle?) #t)
    (define (rotate?) #t)
    (super-new (piece-structure I) 
               (brush *cyan-brush*) 
               (light-pen *light-cyan-pen*) 
               (dark-pen *dark-cyan-pen*))))

;;J
(define J-piece%
  (class piece%
    (override toggle? rotate?)
    (define (toggle?) #f)
    (define (rotate?) #t)
    (super-new (piece-structure J) 
               (brush *blue-brush*)
               (light-pen *light-blue-pen*)
               (dark-pen *dark-blue-pen*))))

;;L
(define L-piece%
  (class piece%
    (override toggle? rotate?)
    (define (toggle?) #f)
    (define (rotate?) #t)
    (super-new (piece-structure L) 
               (brush *orange-brush*)
               (light-pen *light-orange-pen*)
               (dark-pen *dark-orange-pen*))))

;;O
(define O-piece%
  (class piece%
    (override toggle? rotate?)
    (define (toggle?) #f)
    (define (rotate?) #f)
    (super-new (piece-structure O)
               (brush *yellow-brush*)
               (light-pen *light-yellow-pen*)
               (dark-pen *dark-yellow-pen*))))

;;S
(define S-piece%
  (class piece%
    (override toggle? rotate?)
    (define (toggle?) #t)
    (define (rotate?) #t)
    (super-new (piece-structure S)
               (brush *green-brush*)
               (light-pen *light-green-pen*)
               (dark-pen *dark-green-pen*))))

;;Z
(define Z-piece%
  (class piece%
    (override toggle? rotate?)
    (define (toggle?) #t)
    (define (rotate?) #t)
    (super-new (piece-structure Z) 
               (brush *red-brush*)
               (light-pen *light-red-pen*)
               (dark-pen *dark-red-pen*))))

;;T
(define T-piece%
  (class piece%
    (override toggle? rotate?)
    (define (toggle?) #f)
    (define (rotate?) #t)
    (super-new (piece-structure T)
               (brush *magenta-brush*)
               (light-pen *light-magenta-pen*)
               (dark-pen *dark-magenta-pen*))))

;; ### CONSTANTS ###

; Tetris pieces
(define *pieces* 
  (vector I-piece% J-piece% L-piece% O-piece% S-piece% Z-piece% T-piece%))