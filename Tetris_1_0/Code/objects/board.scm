;;TETRIS
;;board.scm


;;BOARD CLASS

(define board%
  (class object%
    (super-new)
    
    ;;### FIELDS ###
    (init-field board-size-units pixels-per-unit)
    
    (define _pieces '()) ;;[list]
    (define _active-piece  #f) ;;[bool]
    (define _player #f) ;;[player%]
    (define _size board-size-units) ;;[cons]
    (define _pixels-per-unit pixels-per-unit) ;;[num]
    
    ;;### FIELD ACCESSORS
    
    ;;GET pieces
    ;; -> [list piece%]
    (define/public (get-pieces)
      _pieces)
    
    ;;SET active piece
    ;; <- [piece%]
    (define/public (set-active-piece! piece)
      (set! _active-piece piece))
    
    ;;GET active piece
    ;; <- [piece%]
    (define/public (get-active-piece)
      _active-piece)
    
    ;;SET player
    ;; <- [player%]
    (define/public (set-player! player)
      (set! _player player))
    
    ;;GET player
    ;; -> [player%]
    (define/public (get-player)
      _player)
    
    ;;GET board size
    ;; -> [cons]
    (define/public (get-size)
      _size)
    
    ;;GET board width
    ;; -> [num]
    (define/public (get-width)
      (car _size))
    
    ;;GET board height
    ;; -> [num]
    (define/public (get-height)
      (cdr _size))
    
    ;;GET pixels per unit
    ;; -> [num]
    (define/public (get-pixels-per-unit)
      _pixels-per-unit)
    
    ;;### METHODS ##
    
    ;;VOID add piece on board (default pos)
    ;; <- piece [piece%]
    ;; -> [bool]
    (define/public (add-piece-on-board-default piece)
      (send piece set-coord! (cons (/ (get-width) 2) (- (get-height) 1)))
      (set! _pieces (append (list piece) _pieces))
      (set-active-piece! piece)
      (not (collide? (send this get-active-piece) (cons 0 0))))
    
    ;;VOID add piece on board (custom pos)
    ;; <- piece [piece%]
    ;; <- coord [cons]
    ;; <- active [bool]
    ;; -> [bool]
    (define/public (add-piece-on-board-custom piece coord active)
      (send piece set-coord! coord)
      (set! _pieces (append (list piece) _pieces))
      (if active
          (set-active-piece! piece)))
    
    ;;VOID delete piece from board
    ;; <- piece [piece%]
    ;; -> [bool]
    (define/private (delete-piece! piece)
      (remove piece _pieces))
    
    ;;VOID move piece
    ;; <- piece [piece%]
    ;; <- delta-coord [cons]
    (define/public (move-piece piece delta-coord)
      (if (and (not (null? piece)) (move-possible? piece delta-coord))
          (begin
            (send piece set-coord! (cons (+ (car delta-coord) (send piece get-abs-x)) (+ (cdr delta-coord) (send piece get-abs-y))))
            #t)
          #f))
    
    ;;VOID shift down rows
    ;; <- start-row [num]
    (define/private (shift-down-from-row start-row)
      ;-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
      (if *debug*
          (begin
            (display "call to shift-down-from-row in board: ")
            (display start-row)
            (newline)
            (newline)))
      ;-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
      (for-each
       (lambda (piece)
         (let ((piece-shifted #f))
           (for-each
            (lambda (block)
              (if (not piece-shifted)
                  (if (>= (send block get-abs-y) start-row)
                      (begin
                        (send (send block get-parent-piece) set-coord!
                              (cons (send (send block get-parent-piece) get-abs-x)
                                    (- (send (send block get-parent-piece) get-abs-y) 1)))
                        (set! piece-shifted #t)))))
            (send piece get-blocks))))
       (send this get-pieces)))
    
    ;;VOID delete all blocks on row
    ;; <- row [num]
    (define/private (delete-row row)
      ;-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
      (if *debug*
          (begin
            (display "call to delete-row in board: ")
            (display row)
            (newline)
            (newline)))
      ;-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
      (for-each
       (lambda (piece)
         (for-each 
          (lambda (block)
            (if (= row (send block get-abs-y))
                (send piece delete-block block)))
          (send piece get-blocks)))
       (send this get-pieces)))
    
    ;;VOID clean up filled rows
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
        
        ;;deletes all 'garbage' pieces
        (delete-garbage-pieces) 
        
        ;;update player score based on deleted rows
        (send _player update-score (length filled-rows))))
    
    ;;VOID delete 'garbage'
    (define/private (delete-garbage-pieces)
      (for-each
       (lambda (piece)
         (if (or (null? (send piece get-blocks)) ;;null piece
                 (piece-below-bottom? piece)) ;;below the board bottom
             (delete-piece! piece)))
       _pieces))
    
    ;;VOID drops the piece on the bottom
    ;; <- piece [piece%]
    (define/public (drop-down-piece piece)
      (if (move-piece piece (cons 0 -1))
          (drop-down-piece piece)))
    
    
    ;;### FUNCTIONS
    
    ;;FUNC converts units -> pixels
    ;; <- units [num]
    ;; -> [num]
    (define/public (units->pixels units)
      (* (get-pixels-per-unit) units))
    
    ;;FUNC move piece possible
    ;; <- piece [piece%]
    ;; <- delta-coord [cons]
    ;; -> [bool]
    (define/public (move-possible? piece delta-coord)
      (define (worker blocks)
        (if (null? blocks)
            #t
            (begin
              (let* ((block (car blocks))
                     (new-x (+ (car delta-coord) (send block get-abs-x)))
                     (new-y (+ (cdr delta-coord) (send block get-abs-y))))
                ;-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
                (if *debug*
                    (begin
                      (display "call to move-possible in board")
                      (newline)
                      (display (send block get-coord))
                      (display new-x)
                      (display " ")
                      (display new-y)
                      (newline)
                      (newline)))
                ;-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
                (if (and (>= new-x 0) (<= new-x (- (get-width) 1))
                         (>= new-y 0) (<= new-y (- (get-height) 1))
                         (not (collide? piece delta-coord)))
                    (worker (cdr blocks))
                    #f)))))
      (worker (send piece get-blocks)))
    
    ;;FUNC collision?
    ;; <- piece [piece%]
    ;; <- delta-coord [cons]
    ;; -> [bool]
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
    
    ;;FUNC filled rows on board
    ;; -> [list num]
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
    
    ;;FUNC is piece below the bottom?
    ;; <- piece [piece%]
    ;; -> [bool]
    (define/private (piece-below-bottom? piece)
      (let ((below #t))
        (for-each
         (lambda (block)
           (if (>= (send block get-abs-y) 0)
               (set! below #f)))
         (send piece get-blocks))
        below))
    
    ;;FUNC is piece on bottom?
    ;; <- piece [piece%]
    ;; -> [bool]
    (define/public (on-bottom? piece)
      (define bottom #f)
      (for-each
       (lambda (block)
         (if (= 0 (send block get-abs-y))
             (set! bottom #t)))
       (send piece get-blocks))
      bottom)
    ))

;; ### DEBUG CODE ###

;(load "piece.scm")
;(load "block.scm")
;(load "../graphics.scm")
;
;(define test-board (make-object board% (cons 10 20) 20))
;
;(send test-board add-piece-on-board-custom (make-object piece% 'test '((0 0) (1 0) (2 0) (3 0) (4 0) (5 0) (6 0) (7 0) (8 0) (9 0)) *red-brush*) (cons 0 18) #f)
;(send test-board add-piece-on-board-custom (make-object piece% 'test '((0 0) (1 0) (2 0) (3 0) (4 0) (5 0) (6 0) (7 0) (8 0) (9 0)) *red-brush*) (cons 0 15) #f)