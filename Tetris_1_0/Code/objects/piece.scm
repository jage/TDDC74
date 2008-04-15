;;TETRIS

;;Piece object

(load "block.scm")

(define piece%
  (class object%
    (super-new)
    
    ;;class fields
    (init _type _block-list _color)
    (define _coord (cons 0 0))
    (define _blocks '())
    (deine _rotated #f)
    
    ;;constructor
    (define (constructor)
      (create-piece _block-list))
    
    ;; creates piece from block list 
    (define (create-piece list)
      (if (not (null? list))
          (send this add-block! (car list))
          (create-piece (cdr list))))
    
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
      (set! _blocks (append (list (make-object block% (car rel-coord) (cdr rel-coord))) _blocks)))
    
    ;;get blocks
    (define/public (get-blocks)
      _blocks)
    
    ;;get piece color
    (define/public (get-color)
      (_color))
    
    (define/public (get-type)
      (_type))
    
    ;;rotate piece
    (define/public (rotate)
      (if _rotated
          (set! _rotated #f)
          (set! _rotated #t)))    
    ))
   

;;(define test-piece (make-object piece% 'test '((1 0) (-1 0)) 'red))

