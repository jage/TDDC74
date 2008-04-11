;;TETRIS

;;Piece object

(load "block.scm")

;;interface for rotating a piece
(define rotate-interface<%> (interface () rotate))

(define piece%
  (class object% rotate-interface<%>
    (super-new)
    
    (public rotate)
    
    (init type color)
    
    (define _type type)
    (define _color color)
    (define _coord (cons 0 0))
    (define _blocks '())
    
    ;;to be overridden by child class
    (define (rotate)
      #f)
    
    (define/public (get-coord)
      _coord)
    
    (define/public (set-coord! coord)
      (set! _coord coord))
    
    (define/public (add-block! rel-coord)
      (set! _blocks (append (list (make-object block% (car rel-coord) (cdr rel-coord))) _blocks)))
    
    (define/public (get-blocks)
      _blocks)))
   

(define test-piece (make-object piece% 'test 'red))

(send test-piece add-block! (cons 0 0))
(send test-piece add-block! (cons 1 0))
(send test-piece add-block! (cons 2 0))
(send test-piece add-block! (cons -1 0))