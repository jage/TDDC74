;;TETRIS

;;Block object

;;History
;;080409 Created by VD

(define block%
  (class* object% () ()
    (init-field x y)
    (public*
     (set-x! (lambda (arg) (set! x arg)))
     (set-y! (lambda (arg) (set! y arg))))
  (super-new)))

(define tja (make-object block% 10 10))