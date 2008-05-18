;; Tetris - objects/supervisor.scm
;;
;; Written by Johan Eckerström and Viktor Deleskog
;; For TDDC74 at LiU, 2008

;; ### CONSTANTS ###

; Speed divisors
(define speeds (vector 24 12 8 6 3 1))

;; SUPERVISOR CLASS

(define supervisor%
  (class object%
    (super-new)

    ;;### FIELDS ###

    (init-field board-size pixels-per-unit)

    (define _board-size board-size) ;;[cons]
    (define _pixels-per-unit pixels-per-unit) ;;[num]
    (define _board #f) ;;[board]
    (define _debug #f) ;;[bool]
    (define _speed 0) ;;[num]
    (define _status "Not started") ;;[string]
    (define _seed #f) ;; [num]
    (define _next-piece #f) ;; [piece]

    ;;### CONSTRUCTOR ###

    (define/private (constructor)
      ; Initiate board
      (set! _board (make-object board% _board-size _pixels-per-unit)))

    ; Call the constructor
    (constructor)


    ;; ### METHODS ###

    ; VOID - Start the game
    (define/public (start)
      ; The first piece
      (generate-next-piece)
      ; Generate and set random seed for the piece generation
      (set! _seed (random 99999999))
      (random-seed _seed)
      ; Set default speed
      (set! _speed 2)
      ; Add player
      (send _board set-player! (make-object player%))
      ; Add the first piece
      (send _board add-piece-on-board-default (get-next-piece))
      ; Generate the second piece
      (generate-next-piece)
      ; Time for graphics
      (initiate-graphics)
      ; And we're off!
      (start-loop)
      (set! _status "Running!"))

    ; VOID - Stop the game, but keep the window active
    (define/public (stop)
      (stop-loop)
      (set! _status "Game over!"))

    ; VOID - Quit everything
    (define/public (quit)
      (hide-gui *gui*)
      (send _board reset!)
      (stop-loop)
      (set! _status "Not Running"))

    ; VOID - Pause/Unpause
    (define/public (pause)
      (if *should-run*
          (begin
            (stop-loop)
            (set! _status "Paused!"))
          (begin
            (start-loop)
            (set! _status "Running!")))
      (draw))

    ; GET - Current status of the game
    (define/public (get-status)
      _status)

    (define/public (paused?)
      (not *should-run*))

    ;; GET - Get the current board
    ;; -> [board]
    (define/public (get-board)
      _board)

    ; GET - Next piece
    (define/public (get-next-piece)
      _next-piece)

    ; VOID - Generate the next piece and place it on the side for preview
    (define/public (generate-next-piece)
      (set! _next-piece (get-random-piece))
      (send _board add-piece-on-board-custom
            _next-piece
            (coords (+ (send _board get-board-width) 2)
                    (- (send _board get-board-height) 6))
            #f))

    ;; VOID - Set debug mode
    (define/public (enable-debug)
      (set! _debug #t))

    ;; VOID - Unset debug mode
    (define/public (disable-debug)
      (set! _debug #f))

    ;; GET - Are we in debug mode?
    ;; -> [bool]
    (define/public (debug?)
      _debug)

    ;; VOID - Increase the speed to the next level
    (define/public (increase-speed!)
      (let ((new-speed (+ _speed 1)))
        (if (< new-speed (vector-length speeds))
            (set! _speed (+ _speed 1)))))

    ;; VOID - Decrease the speed to the previous level
    (define/public (decrease-speed!)
      (let ((new-speed (- _speed 1)))
        (if (>= new-speed 0)
            (set! _speed (- _speed 1)))))

    ;; GET - Get the counter divisor
    ;; -> [num]
    (define/public (get-counter-divisor)
      (vector-ref speeds _speed))

    ;; GET - Return in ms
    ;; -> [num]
    (define/public (get-interval-time)
      (quotient 1000 (/ 24 (get-counter-divisor))))

    ;; ### FUNCTIONS ###
    
    ;; GET - Return a random piece
    ;; -> [piece]
    (define/public (get-random-piece)
      (make-object (vector-ref *pieces* (random (vector-length *pieces*)))))))