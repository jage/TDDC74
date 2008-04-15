;;TETRIS

;;Board object

(define board%
  (class object%
   (super-new)
   
   (init-field size)
   
   ;;variables
   (define _pieces '())
   (define _active-piece  #f)
   (define _player #f)
   (define _size size)
   
   ;;get all pieces on board
   (define/public (get-pieces)
     _pieces)
   
   ;;sets the active piece on the board
   (define/public (set-active-piece! piece)
     (set! _active-piece piece))
   
   ;;sets player
   (define/public (set-player! player)
     (set! _player player))
   
   ;;get player
   (define/public (get-player)
     _player)
   
   ;;get board size
   (define/public (get-size)
     _size)
   ))