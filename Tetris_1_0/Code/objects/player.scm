(define player%
  (class object%
    (super-new)
    (init name)
    
    (define _name name)
    (define _score 0)
    
    (define/public (get-name)
      _name)
    
    (define/public (increase-score value)
      (set! _score (+ _score value)))
    
    (define/public (get-score)
      _score)))