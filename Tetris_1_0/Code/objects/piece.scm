;;TETRIS

;;Piece object

(load "block.scm")
(load "../graphics.scm")

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
    (define _rotated #f)
    
    ;;constructor
    (define (constructor)
      (add-block! (cons 0 0)) ;;center block
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
    
    ;;set absolute coordinates
    (define/public (set-coord! coord)
      (set! _coord coord))
    
    ;;add block to piece
    (define/public (add-block! rel-coord)
      (set! _blocks (append _blocks (list (make-object block% rel-coord)))))
    
    ;;get blocks
    (define/public (get-blocks)
      _blocks)
    
    ;;get piece brush
    (define/public (get-brush)
      _brush)

    ;;get piece type
    (define/public (get-type)
      _type)
    
    ;;get if piece is rotated
    (define/public (rotated?)
      _rotated)
    
    ;;rotate piece
    (define/public (rotate)
      (if _rotated
          (set! _rotated #f)
          (set! _rotated #t)))    
    ))
   

;;(define test-piece (make-object piece% 'test '((1 0) (-1 0)) *red-brush*))