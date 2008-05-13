;;TETRIS
;;block.scm


;;BLOCK CLASS

(define block%
  (class object%
    (super-new)
    
    ;;### FIELDS ###
    
    (init coord parent-piece)
    
    (define _coord coord)
    (define _parent-piece parent-piece)
    
    ;; ### FIELD ACCESSORS ###
    
    ;;SET rel. coords
    ;; <- coord [cons]
    (define/public (set-rel-coord! coord)
      (set! _coord coord))
    
    ;;GET rel. coords
    ;; -> [cons]
    (define/public (get-rel-coord)
      _coord)
    
    ;;GET rel. x-coord
    ;; -> [num]
    (define/public (get-rel-x)
      (car _coord))
    
    ;;GET rel. y-coord
    ;; -> [num]
    (define/public (get-rel-y)
      (cdr _coord))
    
     ;;GET abs. coords
    ;; -> [cons]
    (define/public (get-abs-coord)
      (cons (+ (get-rel-x) (send (get-parent-piece) get-abs-x)) (+ (get-rel-y) (send (get-parent-piece) get-abs-y))))
   
    ;;GET abs. x-coord
    ;; -> [num]
    (define/public (get-abs-x)
      (+ (get-rel-x) (send (get-parent-piece) get-abs-x)))
    
    ;;GET abs. y-coord
    ;; -> [num]
    (define/public (get-abs-y)
      (+ (get-rel-y) (send (get-parent-piece) get-abs-y)))
       
    ;;GET parent piece
    ;; -> [piece%]
    (define/public (get-parent-piece)
      _parent-piece)
    ))