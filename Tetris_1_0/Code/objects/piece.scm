;;TETRIS

;;Piece object

(define piece%
  (class object%
    (super-new)
    
    ;;class fields
    (init type block-list brush)
    
    (define _type type)
    (define _block-list block-list)
    (define _brush brush)
    (define _coord (cons 0 0))
    (define _blocks '())
    (define _rotatable #t)
    (define _toggle #f)
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
    (define/public (get-x-coord)
      (car (get-coord)))
    
    ;;get piece absolute y-coord
    (define/public (get-y-coord)
      (cdr (get-coord)))
    
    ;;set absolute coordinates
    (define/public (set-coord! coord)
      (set! _coord coord))
    
    ;;add block to piece
    (define/public (add-block! rel-coord)
      (set! _blocks (append _blocks (list (make-object block% rel-coord)))))
    
    ;;get blocks
    (define/public (get-blocks)
      _blocks)
    
    ;;get real block coord
    (define/public (get-block-coord block)
      (cons (+ (get-x-coord) (send block get-x)) (+ (get-y-coord) (send block get-y))))
    
    ;;get blocks absolute coordinates
    (define/public (get-blocks-coords)
      (let ((coords-list '()))
        (for-each
         (lambda (block)
           (set! coords-list (append coords-list (list (get-block-coord block)))))
         _blocks)
        coords-list))
    
    ;;get piece brush
    (define/public (get-brush)
      _brush)
    
    ;;get piece type
    (define/public (get-type)
      _type)
    
    ;; get if piece is rotatable
    (define/public (rotatable?)
      _rotatable)
    
    ;;rotate piece
    (define/public (rotate)
      (if _toggle (set! _clockwise (not _clockwise)))
      (for-each
       (lambda (block)
         (send block set-coord! (rotate-coord block _clockwise)))
       _blocks))

    ; counter clockwise rotation
    (define/private (rotate-coord block clockwise)
      (let ((placement (send block get-coord)))
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
  

;;(define test-piece (make-object piece% 'test '((1 0) (-1 0)) *red-brush*))
