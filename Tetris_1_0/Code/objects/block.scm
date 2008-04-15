;;TETRIS

;;Block object

(define block%
  (class object%
    (super-new)
    
    (init x y) ;;constructor
    
    ;;member variables
    (define _x x)
    (define _y x)
    
    ;;fields
    
    ;;x coord
    (define/public (get-x)
      _x)
    
    ;;y coord
    (define/public (get-y)
      _y)))
  
  
  ;;(define tja (make-object block% 10 10))