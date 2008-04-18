;;TETRIS

;;Board object

(define board%
  (class object%
    (super-new)
    
    (init-field board-size-units pixels-per-unit)
    
    ;;variables
    (define _pieces '())
    (define _active-piece  #f)
    (define _player #f)
    (define _size board-size-units)
    (define _pixels-per-unit pixels-per-unit)
    
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
    
    ;;get pixels per unit
    (define/public (get-pixels-per-unit)
      _pixels-per-unit)
    
    ;;get units->pixels
    (define/public (units->pixels units)
      (* (get-pixels-per-unit) units))
    
    ;;add block on board
    (define/public (add-piece-on-board piece)
      (send piece set-coord! (cons (/ (get-width) 2) 1))
      (set! _pieces (append (list piece) _pieces))
      (set-active-piece! piece))
    
    ;;moves a piece on the board
    (define/public (move-piece piece coord)
      (if (and (not (null? piece)) (move-possible? piece coord))
          ;;DEBUG: Check if the piece could be moved
          ;;(begin
          ;;(display "MOVE PIECE")
          ;;(newline)
          ;;(display "old coord: ")
          ;;(display (send piece get-coord))
          (send piece set-coord! (cons (+ (car coord) (send piece get-x-coord)) (+ (cdr coord) (send piece get-y-coord))))
          ;;(display " new coord: ")
          ;;(display (send piece get-coord)))
          #f))
    
    ;;is move possible?
    (define/public (move-possible? piece coord)
      (define (worker blocks)
        (if (null? blocks)
            #t
            (begin
              (let* ((block (car blocks))
                     (new-x (+ (+ (send piece get-x-coord) (car coord)) (send block get-x)))
                     (new-y (+ (+ (send piece get-y-coord) (cdr coord)) (send block get-y))))
                ;;DEBUG: Check coordinates
                ;;(display "MOVE POSSIBLE?"
                ;;(newline)
                ;;(display (send block get-coord))
                ;;(display new-x)
                ;;(display " ")
                ;;(display new-y)
                ;;(newline)
                ;;(newline)
                ;;------------------------
                (if (and (>= new-x 0) (<= new-x (get-width))
                         (>= new-y 0) (<= new-y (get-height)))
                    (worker (cdr blocks))
                    #f)))))
      (worker (send piece get-blocks)))
    ))

;;(define test-board (make-object board% (cons 10 20) 20))

;;(send test-board add-piece-on-board (make-object piece% 'test '((1 0) (-1 0)) *red-brush*))