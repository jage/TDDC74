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
    (define _size board-size-units) ;;[size]
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
    ;; -> [size]
    (define/public (get-size)
      (size (get-width _size) (get-height _size)))
    
    ;;GET board width
    ;; -> [num]
    (define/public (get-board-width)
      (get-width _size))
    
    ;;GET board height
    ;; -> [num]
    (define/public (get-board-height)
      (get-height _size))
    
    ;;GET pixels per unit
    ;; -> [num]
    (define/public (get-pixels-per-unit)
      _pixels-per-unit)
    
    ;;### METHODS ##
    
    ;;VOID add piece on board (default pos)
    ;; <- piece [piece%]
    ;; -> [bool]
    (define/public (add-piece-on-board-default piece)
      (send piece move-to! (coords (/ (get-board-width) 2) (- (get-board-height) 1)))
      ;(set! _pieces (append (list piece) _pieces))
      (set-active-piece! piece)
      (not (will-collide? (send this get-active-piece) (coords 0 0))))
    
    ;;VOID add piece on board (custom pos)
    ;; <- piece [piece%]
    ;; <- coords [coords]
    ;; <- active [bool]
    ;; -> [bool]
    (define/public (add-piece-on-board-custom piece coords active)
      (send piece move-to! coords)
      (set! _pieces (append (list piece) _pieces))
      (if active
          (set-active-piece! piece)))
    
    ;;VOID delete piece from board
    ;; <- piece [piece%]
    ;; -> [bool]
    (define/private (delete-piece! piece)
      (set! _pieces (remove piece _pieces)))
    
    ;; VOID reset board
    (define/public (reset!)
      (for-each
       (lambda (piece) (delete-piece! piece))
       _pieces))
    
    ;;VOID move piece
    ;; <- piece [piece%]
    ;; <- direction [symb] (up, down, left, right)
    (define/public (move-piece piece direction)
      (if (and (not (null? piece)) (move-possible? piece (direction->dxdy direction)))
          (send piece move! direction)
          #f))
    
    ;;VOID shift down rows
    ;; <- start-row [num]
    (define/private (shift-down-from-row start-row)
      ;-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
      (if (send *supervisor* debug?)
          (begin
            (display "BOARD -> SHIFT-DOWN-FROM-ROW\n")
            (display "start-row: ")
            (display start-row)
            (newline)(newline)))
      ;-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
      (for-each
       (lambda (piece)
         (for-each
          (lambda (block)
            (if (>= (get-y (send block get-abs-coords)) start-row)
                (send block move! 'down)))
          (send piece get-blocks)))
      (send this get-pieces)))
    
    ;;VOID delete all blocks on row
    ;; <- row [num]
    (define/private (delete-row row)
      ;-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
      (if (send *supervisor* debug?)
          (begin
            (display "BOARD -> DELETE-ROW\n")
            (display "row: ")
            (display row)
            (newline)(newline)))
      ;-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
      (for-each
       (lambda (piece)
         (for-each 
          (lambda (block)
            (if (= row (get-y (send block get-abs-coords)))
                (send piece delete-block block)))
            (send piece get-blocks)))
         (send this get-pieces)))
    
    ;;VOID clean up filled rows
    (define/public (clean-up-board)
      (let ((filled-rows (get-filled-rows)))
        ;-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
        (if (send *supervisor* debug?)
            (begin
              (display "BOARD -> CLEAN-UP-BOARD\n")
              (display "filled-rows: ")
              (display filled-rows)
              (newline)(newline)))
        ;-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
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
      (if (move-piece piece 'down)
          (drop-down-piece piece)))
    
    
    ;;### FUNCTIONS
    
    ;;FUNC converts units -> pixels
    ;; <- units [num]
    ;; -> [num]
    (define/public (units->pixels units)
      (* (get-pixels-per-unit) units))
    
    ;;FUNC move piece possible
    ;; <- piece [piece%]
    ;; <- delta-coord [coord]
    ;; -> [bool]
    (define/public (move-possible? piece dxdy-coords)
      (define (worker blocks)
        (if (null? blocks)
            #t
            (begin
              (let* ((new-coords (send (car blocks) get-abs-coords)))
                (add-dxdy! new-coords dxdy-coords)
                ;-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
                (if (send *supervisor* debug?)
                    (begin
                      (display "BOARD -> MOVE-POSSIBLE?\n")
                      (display "current coords: ")
                      (display "x=")
                      (display (get-x (send (car blocks) get-abs-coords)))
                      (display " y=")
                      (display (get-y (send (car blocks) get-abs-coords)))
                      (display "\nnew coords: ")
                      (display "x=")
                      (display (get-x new-coords))
                      (display " y=")
                      (display (get-y new-coords))
                      (newline)(newline)))
                ;-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
                (if (and (>= (get-x new-coords) 0) (<= (get-x new-coords) (- (get-board-width) 1))
                         (>= (get-y new-coords) 0) (<= (get-y new-coords) (- (get-board-height) 1))
                         (not (will-collide? piece dxdy-coords)))
                    (worker (cdr blocks))
                    #f)))))
      (worker (send piece get-blocks)))
    
    ;;FUNC will piece collide with the new coordinates?
    ;; <- piece [piece%]
    ;; <- dxdy-coords [coords]
    ;; -> [bool]
    (define/public (will-collide? piece dxdy-coords)
      (let ((collision #f))
        (for-each
         (lambda (block)
           (let ((new-coords (send block get-abs-coords)))
             (add-dxdy! new-coords dxdy-coords)
             (for-each
              (lambda (board-piece)
                (for-each
                 (lambda (board-piece-block)
                   (if (not (eq? board-piece piece))
                       (if (equal? new-coords (send board-piece-block get-abs-coords))
                           (set! collision #t))))
                 (send board-piece get-blocks)))
              (send this get-pieces))))
         (send piece get-blocks))
        collision))
    
    ;;FUNC filled rows on board
    ;; -> [list num]
    (define/public (get-filled-rows)
      (let* ((i (- (send this get-board-height) 1))
             (j 0)
             (rows '()))
        (define (row-loop)
          (for-each
           (lambda (piece)
             (for-each
              (lambda (block)
                (if (= i (get-y (send block get-abs-coords)))
                    (set! j (+ 1 j))))
              (send piece get-blocks)))
           (send this get-pieces))
          (if (= j (send this get-board-width))
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
      (let ((below #f))
        (for-each
         (lambda (block)
           (if (< (get-y (send block get-abs-coords)) 0)
               (set! below #t)))
         (send piece get-blocks))
        below))
    
    ;;FUNC is piece on bottom?
    ;; <- piece [piece%]
    ;; -> [bool]
    (define/public (on-bottom? piece)
      (define bottom #f)
      (for-each
       (lambda (block)
         (if (= 0 (get-y (send block get-abs-coords)))
             (set! bottom #t)))
       (send piece get-blocks))
      bottom)))

;; ### DEBUG CODE ###

;(load "../utilities.scm")
;(load "piece.scm")
;(load "block.scm")
;(load "../graphics.scm")
;
;(define test-board (make-object board% (size 10 20) 20))
;(send test-board add-piece-on-board-custom (make-object piece% '((0 0) (1 0) (-1 0)) *red-brush*) (coords 2 3) #f)
;
;(define figur (car (send test-board get-pieces)))
;
;(define (print-coords piece)
;  (for-each
;   (lambda (block)
;     (display (send block get-abs-coords))
;     (newline))
;   (send piece get-blocks)))


;
;(send test-board add-piece-on-board-custom (make-object piece% 'test '((0 0) (1 0) (2 0) (3 0) (4 0) (5 0) (6 0) (7 0) (8 0) (9 0)) *red-brush*) (cons 0 18) #f)
;(send test-board add-piece-on-board-custom (make-object piece% 'test '((0 0) (1 0) (2 0) (3 0) (4 0) (5 0) (6 0) (7 0) (8 0) (9 0)) *red-brush*) (cons 0 15) #f)