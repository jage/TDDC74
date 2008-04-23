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
    
    ;;add block on board (default properties)
    (define/public (add-piece-on-board-default piece)
      (send piece set-coord! (cons (/ (get-width) 2) (- (get-height) 1)))
      (set! _pieces (append (list piece) _pieces))
      (set-active-piece! piece)
      (not (collide? (send this get-active-piece) (cons 0 0))))
    
    ;;add block on board (custom properties)
    (define/public (add-piece-on-board-custom piece coord active)
      (send piece set-coord! coord)
      (set! _pieces (append (list piece) _pieces))
      (if active
          (set-active-piece! piece)))
    
    ;;moves a piece on the board
    (define/public (move-piece piece delta-coord)
      (if (and (not (null? piece)) (move-possible? piece delta-coord))
          (begin
            (send piece set-coord! (cons (+ (car delta-coord) (send piece get-abs-x)) (+ (cdr delta-coord) (send piece get-abs-y))))
            #t)
          #f))
    
    ;;is move possible?
    (define/public (move-possible? piece delta-coord)
      (define (worker blocks)
        (if (null? blocks)
            #t
            (begin
              (let* ((block (car blocks))
                     (new-x (+ (car delta-coord) (send block get-abs-x)))
                     (new-y (+ (cdr delta-coord) (send block get-abs-y))))
                ;                DEBUG: Check coordinates
                ;                (display "MOVE POSSIBLE?")
                ;                (newline)
                ;                (display (send block get-coord))
                ;                (display new-x)
                ;                (display " ")
                ;                (display new-y)
                ;                (newline)
                ;                (newline)
                ;                ------------------------
                (if (and (>= new-x 0) (<= new-x (- (get-width) 1))
                         (>= new-y 0) (<= new-y (- (get-height) 1))
                         (not (collide? piece delta-coord)))
                    (worker (cdr blocks))
                    #f)))))
      (worker (send piece get-blocks)))
    
    ;;checks if piece collides with other pieces on the board
    (define/public (collide? piece delta-coord)
      (let ((collision #f))
        (for-each
         (lambda (block)
           (for-each
            (lambda (board-piece)
              (for-each
               (lambda (board-piece-block)
                 (if (not (eq? board-piece piece))
                     (if (and (= (+ (send block get-abs-x) (car delta-coord)) (send board-piece-block get-abs-x))
                              (= (+ (send block get-abs-y) (cdr delta-coord)) (send board-piece-block get-abs-y)))
                         (set! collision #t))))
               (send board-piece get-blocks)))
            (send this get-pieces)))
         (send piece get-blocks))
        collision))
    
    (define/public (get-blocks-on-filled-rows)
      (let* ((i (- (send this get-height) 1))
             (j 0)
             (rows '())
             (blocks '())
             (blocks-tmp '()))
        (define (row-loop)
          (for-each
           (lambda (piece)
             (for-each
              (lambda (block)
                (if (= i (send block get-abs-y))
                    (begin
                      (set! j (+ 1 j))
                      (set! blocks-tmp (append (list block) blocks-tmp)))))
              (send piece get-blocks)))
           (send this get-pieces))
          (if (= j (send this get-width)) ;;row i filled
              (set! blocks (append blocks-tmp blocks)))
          (set! i (- i 1)) ;;check next row
          (set! j 0) ;;set column counter to zero
          (set! blocks-tmp '()) ;;set tmp block list to null
          (if (< i 0) ;;all rows are checked
              blocks
              (row-loop)))
        (row-loop))) ;;start the loop
    ))

;(load "piece.scm")
;(load "block.scm")
;(load "../graphics.scm")
;
;(define test-board (make-object board% (cons 10 20) 20))
;
;(send test-board add-piece-on-board-custom (make-object piece% 'test '((0 0) (1 0) (2 0) (3 0) (4 0) (5 0) (6 0) (7 0) (8 0) (9 0)) *red-brush*) (cons 0 0) #f)
;(send test-board add-piece-on-board-custom (make-object piece% 'test '((0 0) (1 0) (2 0) (3 0) (4 0) (5 0) (6 0) (7 0) (8 0) (9 0)) *red-brush*) (cons 0 7) #f)