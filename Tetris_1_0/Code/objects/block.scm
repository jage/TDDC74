;;TETRIS

;;Block object

(define block%
  (class object%
    (super-new)
    
    (init coord parent-piece) ;;constructor
    
    ;;member variables
    (define _coord coord)
    (define _parent-piece parent-piece)
    
    ;;fields
    
    ;;get rel x coord
    (define/public (get-rel-x)
      (car _coord))
    
    ;;get rel y coord
    (define/public (get-rel-y)
      (cdr _coord))
    
        ;;get abs x coord
    (define/public (get-abs-x)
      (+ (get-rel-x) (send (get-parent-piece) get-abs-x)))
    
    ;;get abs y coord
    (define/public (get-abs-y)
      (+ (get-rel-y) (send (get-parent-piece) get-abs-y)))
    
    ;;set block rel. coord
    (define/public (set-rel-coord! coord)
      (set! _coord coord))
    
    ;;get block rel. coord to piece
    (define/public (get-rel-coord)
      _coord)
    
    ;;get block rel. coord to board
    (define/public (get-abs-coord)
      (cons (+ (get-rel-x) (send (get-parent-piece) get-abs-x)) (+ (get-rel-y) (send (get-parent-piece) get-abs-y))))

    ;;get block parent piece
    (define/public (get-parent-piece)
      _parent-piece)
    ))

;;(define tja (make-object block% (cons 10 10)))