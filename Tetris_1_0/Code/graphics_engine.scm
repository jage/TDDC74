(define (draw-pieces list-of-pieces)
  (for-each
   (lambda (piece)
     (draw-piece piece))
   list-of-pieces))

(define (draw-piece piece)
  (for-each
   (lambda (block)
     (draw-block block (send piece get-brush)))
   (send piece get-blocks-coords)))

(define (draw-block block-coord brush)
  (draw-rectangle 
   (* (car block-coord) (send *board* get-pixels-per-unit)) 
   (* (- (send *board* get-height) (cdr block-coord) 1) (send *board* get-pixels-per-unit))
   (send *board* get-pixels-per-unit) (send *board* get-pixels-per-unit) *black-pen* brush))