;; Tetris - objects/block.scm
;;
;; Written by Johan Eckerström and Viktor Deleskog
;; For TDDC74 at LiU, 2008

;;BLOCK CLASS

(define block%
  (class object%
    (super-new)

    ;;### FIELDS ###

    (init struct-coords parent-piece)

    (define _struct-coords (coords (get-x struct-coords) (get-y struct-coords)))
    ;;comment: sets abs. coordninates to struct. coords when block is created
    (define _abs-coords (coords (get-x struct-coords) (get-y struct-coords)))
    (define _parent-piece parent-piece)

    ;; ### FIELD ACCESSORS ###

    ;;SET structure coords
    ;; <- coords [coords]
    (define/public (set-struct-coords! new-coords)
      (set! _struct-coords (coords (get-x new-coords) (get-y new-coords))))

    ;;SET abs. coords
    ;; <- coords [coords]
    (define/public (set-abs-coords! new-coords)
      (set! _abs-coords (coords (get-x new-coords) (get-y new-coords))))

    ;;GET struct. coords
    ;; -> [coords]
    (define/public (get-struct-coords)
      (coords (get-x _struct-coords) (get-y _struct-coords)))

    ;;GET abs. coords
    ;; -> [coords]
    (define/public (get-abs-coords)
      (coords (get-x _abs-coords) (get-y _abs-coords)))

    ;;GET parent piece
    ;; -> [piece%]
    (define/public (get-parent-piece)
      _parent-piece)

    ;; ### METHODS ###

    ;;VOID move block along x-axis
    ;; <- [num]
    (define/public (move-dx! dx)
      (add-dx! _abs-coords dx))

    ;;VOID move block along y-axis
    ;; <- [num]
    (define/public (move-dy! dy)
      (add-dy! _abs-coords dy))

    ;;VOID move block in xy-plane
    ;; <- coords [coords]
    (define/public (move-to! coords)
      (set-x! _abs-coords (get-x coords))
      (set-y! _abs-coords (get-y coords)))

    ;;VOID move block in xy-plane
    ;; <- direction [symb] (up, down, left, right)
    (define/public (move! direction)
      (cond
        ((eq? direction 'up) (move-dy! 1))
        ((eq? direction 'down) (move-dy! -1))
        ((eq? direction 'left) (move-dx! -1))
        ((eq? direction 'right) (move-dx! 1))))))