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
    
    (define/public (get-filled-rows)
      (let* ((i (- (send this get-height) 1))
             (j 0)
             (rows '()))
        (define (row-loop)
          (for-each
           (lambda (piece)
             (for-each
              (lambda (block)
                (if (= i (send block get-abs-y))
                    (set! j (+ 1 j))))
              (send piece get-blocks)))
           (send this get-pieces))
          (if (= j (send this get-width))
              (set! rows (append rows (list i))))
          (set! i (- i 1))
          (set! j 0)
          (if (< i 0)
              rows
              (row-loop)))
        (row-loop)))
             
    (define/private (shift-down-from-row start-row)
;      (display "SHIFT: ")
;      (display start-row)
;      (newline)
;      (newline)
      (for-each
       (lambda (piece)
         (if (<= start-row (send piece get-abs-y))
             (send piece set-coord! (cons (send piece get-abs-x) (- (send piece get-abs-y) 1)))))
       (send this get-pieces)))
    
    (define/private (delete-row row)
;      (display "DELETE-ROW ")
;      (display row)
;      (newline)
;      (newline)
      (for-each
       (lambda (piece)
         (for-each 
          (lambda (block)
            (if (= row (send block get-abs-y))
                (send piece remove-block block)))
          (send piece get-blocks)))
       (send this get-pieces)))
    
    (define/public (clean-up-board)
      (let ((filled-rows (get-filled-rows)))
        ;;delete all filled rows
        (for-each
         (lambda (row)
           (delete-row row))
         filled-rows)
        ;;shift down from top -> bottom
        (for-each
         (lambda (row)
           (shift-down-from-row row))
         filled-rows)
        
        ;;update player score based on deleted rows
        (send _player update-score (length filled-rows))))
    ))

;(load "piece.scm")
;(load "block.scm")
;(load "../graphics.scm")
;
;(define test-board (make-object board% (cons 10 20) 20))
;
;(send test-board add-piece-on-board-custom (make-object piece% 'test '((0 0) (1 0) (2 0) (3 0) (4 0) (5 0) (6 0) (7 0) (8 0) (9 0)) *red-brush*) (cons 0 18) #f)
;(send test-board add-piece-on-board-custom (make-object piece% 'test '((0 0) (1 0) (2 0) (3 0) (4 0) (5 0) (6 0) (7 0) (8 0) (9 0)) *red-brush*) (cons 0 15) #f)