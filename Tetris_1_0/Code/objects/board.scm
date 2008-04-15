;;TETRIS

;;Board object

(load "piece.scm")

(define board%
  (class object%
   (super-new)
   
   (init-field size block-size)
   
   ;;variables
   (define _pieces '())
   (define _active-piece  #f)
   (define _player #f)
   (define _size size)
    (define _block-size block-size)
   
   ;;get all pieces on board
   (define/public (get-pieces)
     _pieces)
   
   ;;sets the active piece on the board
   (define/public (set-active-piece! piece)
     (set! _active-piece piece))
    
    ;;get the active piece on the board
    (define/public (get-active-piece)
      _active-piece)
   
   ;;sets player
   (define/public (set-player! player)
     (set! _player player))
   
   ;;get player
   (define/public (get-player)
     _player)
   
   ;;get board size
   (define/public (get-size)
     _size)
    
    ;;get board width
    (define/public (get-width)
      (car _size))
    
    ;;get board height
    (define/public (get-height)
      (cdr _size))
    
    ;;get block size
    (define/public (get-block-size)
      _block-size)
    
    ;;get units->pixels
    (define/private (units->pixels units)
      (* (get-block-size) units))
    
    ;;add block on board
    (define/public (add-piece-on-board piece)
      (send piece set-coord! (cons 100 10))
      (set! _pieces (append (list piece) _pieces))
      (set-active-piece! piece))

    ;;moves a piece on the board
    (define/public (move-piece piece dx dy)
      (if (and (not (null? piece)) (move-possible? piece dx dy))
          (let* ((block (car piece))
                 (new-x (+ (car block) dx))
                 (new-y (+ (cadr block) dy)))
            (begin
              (display new-x)
              (display new-y)
              (set-car! block new-x)
              (set-cdr! block (list new-y))
              (move-piece (cdr piece) dx dy)))))
    
    ;;is move possible?
    (define/public (move-possible? piece dx dy)
      (define (worker blocks)
        (if (null? blocks)
            #t
            (begin
              (let* ((block (car blocks))
                     (new-x (+ (units->pixels (send block get-x)) dx))
                     (new-y (+ (units->pixels (send block get-y)) dy)))
                (display (send block get-coord))
                (display new-x)
                (display new-y)
                (newline)
                (newline)
                (if (and (>= new-x 0) (<= new-x (- (get-width) 1))
                         (>= new-y 0) (<= new-y (- (get-height) 1)))
                    (worker (cdr blocks))
                    #f)))))
      (worker (send piece get-blocks)))
   ))

(define test-board (make-object board% (cons 200 400) 20))

(send test-board add-piece-on-board (make-object piece% 'test '((1 0) (-2 0)) *red-brush*))
