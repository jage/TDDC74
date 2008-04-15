;;TETRIS

;;Block object

(define block%
  (class object%
    (super-new)
    
    (init coord) ;;constructor
    
    ;;member variables
    (define _coord coord)
    
    ;;fields
    
    ;;get x coord
    (define/public (get-x)
      (car _coord))
    
    ;;get y coord
    (define/public (get-y)
      (cdr _coord))
    
    ;;set block rel. coord
    (define/public (set-coord! coord)
      (set! _coord coord))
    
    ;;get block rel. coord to piece
    (define/public (get-coord)
      _coord)
    ))
  
  
;;(define tja (make-object block% (cons 10 10)))