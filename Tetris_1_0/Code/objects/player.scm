;;TETRIS
;;player.scm


;;PLAYER CLASS

(define player%
  (class object%
    (super-new)
    
    ;;### FIELDS ###
    (init name)
    
    (define _name "You") ;; [string]
    (define _score 0) ;; [num]
    
    ;;### FIELD ACCESSORS ###
    
    ;;GET player name
    ;; -> [string]
    (define/public (get-name)
      _name)
    
    ;;GET player score
    ;; -> [num]
    (define/public (get-score)
      _score)
    
    ;;### METHODS ###
    
    ;;VOID increase player score with value
    ;; <- value [num]
    (define/public (increase-score value)
      (set! _score (+ _score value)))
    
    ;;VOID update player score based on rows
    ;; <- rows [num]
    (define/public (update-score rows)
      (set! _score (+ _score (* rows (* 10 rows)))))
    ))