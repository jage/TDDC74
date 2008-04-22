;;TETRIS

; XXX TMP
;(load "block.scm")
;(load "../graphics.scm")


;;Piece object

; Parent class for the pieces ...
(define piece%
  (class object%
    (super-new)
    
    ;; Fields
    (init type block-list brush)
    
    (define _type type)
    (define _block-list block-list)
    (define _brush brush)
    (define _coord (cons 0 0))
    (define _blocks '())
    (define _clockwise #t)
    
    ;;constructor
    (define (constructor)
      (create-piece _block-list))
    
    ;; creates piece from block list 
    (define (create-piece list)
      (if (not (null? list))
          (begin
            (let ((pair (car list)))
              (send this add-block! (cons (car pair) (cadr pair)))
              (create-piece (cdr list))))))
    
    ;;calls the constructor
    (constructor)
    
    ;;get piece absolute coordinates
    (define/public (get-coord)
      _coord)
    
    ;;get piece absolute x-coord
    (define/public (get-abs-x)
      (car (get-coord)))
    
    ;;get piece absolute y-coord
    (define/public (get-abs-y)
      (cdr (get-coord)))
    
    ;;set absolute coordinates
    (define/public (set-coord! coord)
      (set! _coord coord))
    
    ;;add block to piece
    (define/public (add-block! rel-coord)
      (set! _blocks (append _blocks (list (make-object block% rel-coord this)))))
    
    ;;get blocks
    (define/public (get-blocks)
      _blocks)
    
    ;    ;;get blocks absolute coordinates
    ;    (define/public (get-blocks-coords)
    ;      (let ((coords-list '()))
    ;        (for-each
    ;         (lambda (block)
    ;           (set! coords-list (append coords-list (list (get-block-coord block)))))
    ;         (get-blocks))
    ;        coords-list))
    
    ;;get piece brush
    (define/public (get-brush)
      _brush)
    
    ;;get piece type
    (define/public (get-type)
      _type)
    
    ;; get if piece is rotatable
    (define/public (rotate?)
      #f)
    
    (define/public (toggle?)
      #f)
    
    ; Watch out for loops of DEATH!
    (define/public (revert-rotation!)
      (rotate-worker (not _clockwise)))
    
    ;;rotate piece
    (define/public (rotate)
      (if (rotate?)
          (begin
            (rotate-worker _clockwise)
            (if (revert-rotation?) 
                (revert-rotation!)
                (if (toggle?) (set! _clockwise (not _clockwise))))))
      #f)
    
    (define/private (revert-rotation?)
      (or (send *board* collide? this (cons 0 0))
          (not (send *board* move-possible? this (cons 0 0)))))
    
    (define/private (rotate-worker clockwise)
      (for-each
       (lambda (block)
         (send block set-rel-coord! (rotate-coord block clockwise)))
       _blocks))
    
    ; Counter clockwise rotation
    ; Keep the pieces inside this shape ...
    ; O = Origo (0 . 0)
    ;  X X X
    ;  X O X X
    ;  X X X
    ;    X
    (define/private (rotate-coord block clockwise)
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
    ))

; The I-piece is broken ...
(define I '((-1 0) (0 0) (1 0) (2 0)))   ; Cyan
(define J '((-1 -1) (0 -1) (0 0) (0 1))) ; Blue
(define L '((1 -1) (0 -1) (0 0) (0 1)))  ; Orange
(define O '((0 0) (0 -1) (1 0) (1 -1)))  ; Yellow
(define S '((-1 0) (0 0) (0 1) (1 1)))   ; Green
(define Z '((-1 1) (0 1) (0 0) (1 0)))   ; Red
(define T '((-1 0) (0 0) (1 0) (0 1)))   ; Magenta

(define I-piece%
  (class piece%
    (override toggle? rotate?)
    (define (toggle?) #t)
    (define (rotate?) #t)
    (super-new (type 'I) (block-list I) (brush *cyan-brush*))))

(define J-piece%
  (class piece%
    (override toggle? rotate?)
    (define (toggle?) #f)
    (define (rotate?) #t)
    (super-new (type 'J) (block-list J) (brush *blue-brush*))))

(define L-piece%
  (class piece%
    (override toggle? rotate?)
    (define (toggle?) #f)
    (define (rotate?) #t)
    (super-new (type 'L) (block-list L) (brush *orange-brush*))))

(define O-piece%
  (class piece%
    (override toggle? rotate?)
    (define (toggle?) #f)
    (define (rotate?) #f)
    (super-new (type 'O) (block-list O) (brush *yellow-brush*))))

(define S-piece%
  (class piece%
    (override toggle? rotate?)
    (define (toggle?) #t)
    (define (rotate?) #t)
    (super-new (type 'S) (block-list S) (brush *green-brush*))))

(define Z-piece%
  (class piece%
    (override toggle? rotate?)
    (define (toggle?) #t)
    (define (rotate?) #t)
    (super-new (type 'Z) (block-list Z) (brush *red-brush*))))

(define T-piece%
  (class piece%
    (override toggle? rotate?)
    (define (toggle?) #f)
    (define (rotate?) #t)
    (super-new (type 'T) (block-list T) (brush *magenta-brush*))))