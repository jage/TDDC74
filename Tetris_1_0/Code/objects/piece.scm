;;TETRIS
;;piece.scm


;;PIECE CLASS

(define piece%
  (class object%
    (super-new)
    
    ;;### FIELDS ###
    (init type block-list brush)
    
    (define _type type)
    (define _block-list block-list)
    (define _brush brush)
    (define _coord (cons 0 0))
    (define _blocks '())
    (define _clockwise #t)
    
    ;;### CONSTRUCTOR ###
    
    (define/private (constructor)
      (create-piece _block-list))
    
    ;;creates piece from block list
    ;;called by constructor proc
    ;; <- list of coords
    (define/private (create-piece list)
      (if (not (null? list))
          (begin
            (let ((pair (car list)))
              (send this add-block! (cons (car pair) (cadr pair)))
              (create-piece (cdr list))))))
    
    ;;calls the constructor
    (constructor)
    
    ;;### FIELD ACCESSORS ###
    
    ;;SET abs. coordinates
    ;; <- coord [cons]
    (define/public (set-coord! coord)
      (set! _coord coord))
    
    ;;GET abs. coordinates
    ;; -> [cons]
    (define/public (get-coord)
      _coord)
    
    ;;GET abs. x-coord
    ;; -> [num]
    (define/public (get-abs-x)
      (car (get-coord)))
    
    ;;GET abs. y-coord
    ;; -> [num]
    (define/public (get-abs-y)
      (cdr (get-coord)))
    
    ;;GET blocks
    ;; -> [list block%]
    (define/public (get-blocks)
      _blocks)
    
    ;;GET brush
    ;; -> [brush]
    (define/public (get-brush)
      _brush)
    
    ;;GET type of piece
    ;; -> [symb]
    (define/public (get-type)
      _type)
    
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
    ;; <- rel-coord [cons]
    (define/public (add-block! rel-coord)
      (set! _blocks (append _blocks (list (make-object block% rel-coord this)))))
    
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
         (send block set-rel-coord! (rotate-block-coord block clockwise)))
       _blocks))
    
    ;;VOID rotate block coords
    ;; <- block [block%]
    ;; <- clockwise [bool]

    ;; Counter clockwise rotation
    ;; Keep the pieces inside this shape ...
    ;; O = Origo (0 . 0)
    ;;  X X X
    ;;  X O X X
    ;;  X X X
    ;;    X
    (define/private (rotate-block-coord block clockwise)
      (let ((placement (send block get-rel-coord)))
        (if clockwise
            (cond
              ((equal? placement (cons 0 1))   (cons -1 0))
              ((equal? placement (cons 0 -1))  (cons 1 0))
              ((equal? placement (cons 1 0 ))  (cons 0 1))
              ((equal? placement (cons 1 1))   (cons -1 1))
              ((equal? placement (cons 1 -1))  (cons 1 1))
              ((equal? placement (cons -1 0))  (cons 0 -1))
              ((equal? placement (cons -1 1))  (cons -1 -1))
              ((equal? placement (cons -1 -1)) (cons 1 -1))
              ((equal? placement (cons 2 0))   (cons 0 2))
              (else placement))            
            (cond
              ((equal? placement (cons 0 1))   (cons 1 0))
              ((equal? placement (cons 0 -1))  (cons -1 0))
              ((equal? placement (cons 1 0 ))  (cons 0 -1))
              ((equal? placement (cons 1 1))   (cons 1 -1))
              ((equal? placement (cons 1 -1))  (cons -1 -1))
              ((equal? placement (cons -1 0))  (cons 0 1))
              ((equal? placement (cons -1 1))  (cons 1 1))
              ((equal? placement (cons -1 -1)) (cons -1 1))
              ((equal? placement (cons 0 2))   (cons 2 0))
              (else placement)))))
    
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
    ;; -> [bool]
    (define/private (revert-rotation?)
      (or (send *board* collide? this (cons 0 0))
          (not (send *board* move-possible? this (cons 0 0)))))
    ))

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
    (super-new (type 'I) (block-list I) (brush *cyan-brush*))))

;;J
(define J-piece%
  (class piece%
    (override toggle? rotate?)
    (define (toggle?) #f)
    (define (rotate?) #t)
    (super-new (type 'J) (block-list J) (brush *blue-brush*))))

;;L
(define L-piece%
  (class piece%
    (override toggle? rotate?)
    (define (toggle?) #f)
    (define (rotate?) #t)
    (super-new (type 'L) (block-list L) (brush *orange-brush*))))

;;O
(define O-piece%
  (class piece%
    (override toggle? rotate?)
    (define (toggle?) #f)
    (define (rotate?) #f)
    (super-new (type 'O) (block-list O) (brush *yellow-brush*))))

;;S
(define S-piece%
  (class piece%
    (override toggle? rotate?)
    (define (toggle?) #t)
    (define (rotate?) #t)
    (super-new (type 'S) (block-list S) (brush *green-brush*))))

;;Z
(define Z-piece%
  (class piece%
    (override toggle? rotate?)
    (define (toggle?) #t)
    (define (rotate?) #t)
    (super-new (type 'Z) (block-list Z) (brush *red-brush*))))

;;T
(define T-piece%
  (class piece%
    (override toggle? rotate?)
    (define (toggle?) #f)
    (define (rotate?) #t)
    (super-new (type 'T) (block-list T) (brush *magenta-brush*))))