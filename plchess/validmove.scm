(define (find-piece chessboard file-num rank-num dx dy piece)
    (set! 'file-num (+ file-num dx))
    (set! 'rank-num (+ rank-num dy))

    (loop
        (if (or (>= file-num 8)
                (>= rank-num 8)
                (< file-num 0)
                (< rank-num 0))
            (break false))

        (if (= (chessboard-ref-num chessboard file-num rank-num) piece)
            (break true))

        (if (!= (chessboard-ref-num chessboard file-num rank-num) '())
            (break false))

        (set! 'file-num (+ file-num dx))
        (set! 'rank-num (+ rank-num dy))))

(define (find-piece-positions chessboard file-num rank-num positions piece)
    (define new-file-num '())
    (define new-rank-num '())

    (loop
        (if (null? positions) (break false))

        (set! 'new-file-num (+ file-num (caar positions)))
        (set! 'new-rank-num (+ rank-num (cadar positions)))
        (if (and (>= new-file-num 0)
                 (>= new-rank-num 0)
                 (< new-file-num 8)
                 (< new-rank-num 8)
                 (= (chessboard-ref-num chessboard new-file-num new-rank-num) piece))
            (break true))

        (set! 'positions (cdr positions))))

(define (under-attack-queen? chessboard file-num rank-num side)
    (define opponent (opponent-side side))
    (define opponent-queen (get-side-piece 'q opponent))

    (or (find-piece chessboard file-num rank-num 1 0 opponent-queen)
        (find-piece chessboard file-num rank-num -1 0 opponent-queen)
        (find-piece chessboard file-num rank-num 0 1 opponent-queen)
        (find-piece chessboard file-num rank-num 0 -1 opponent-queen)
        (find-piece chessboard file-num rank-num 1 1 opponent-queen)
        (find-piece chessboard file-num rank-num -1 -1 opponent-queen)
        (find-piece chessboard file-num rank-num 1 -1 opponent-queen)
        (find-piece chessboard file-num rank-num -1 1 opponent-queen)))

(define (under-attack-rook? chessboard file-num rank-num side)
    (define opponent (opponent-side side))
    (define opponent-king (get-side-piece 'r opponent))

    (or (find-piece chessboard file-num rank-num 1 0 opponent-king)
        (find-piece chessboard file-num rank-num -1 0 opponent-king)
        (find-piece chessboard file-num rank-num 0 1 opponent-king)
        (find-piece chessboard file-num rank-num 0 -1 opponent-king)))

(define (under-attack-bishop? chessboard file-num rank-num side)
    (define opponent (opponent-side side))
    (define opponent-king (get-side-piece 'b opponent))

    (or (find-piece chessboard file-num rank-num 1 1 opponent-king)
        (find-piece chessboard file-num rank-num -1 -1 opponent-king)
        (find-piece chessboard file-num rank-num 1 -1 opponent-king)
        (find-piece chessboard file-num rank-num -1 1 opponent-king)))

(define knight-positions (list '(2 1) '(2 -1) '(-2 1) '(-2 -1)
                               '(1 2) '(1 -2) '(-1 2) '(-1 -2)))

(define (under-attack-knight? chessboard file-num rank-num side)
    (define opponent (opponent-side side))
    (define opponent-knight (get-side-piece 'n opponent))

    (find-piece-positions chessboard file-num rank-num knight-positions opponent-knight))

(define king-positions (list '(1 0) '(-1 0) '(0 1) '(0 -1)
                             '(1 1) '(-1 -1) '(1 -1) '(-1 1)))

(define (under-attack-king? chessboard file-num rank-num side)
    (define opponent (opponent-side side))
    (define opponent-king (get-side-piece 'k opponent))

    (find-piece-positions chessboard file-num rank-num king-positions opponent-king))

(define white-pawn-attack-positions (list '(1 1) '(1 -1)))
(define black-pawn-attack-positions (list '(-1 1) '(-1 -1)))

(define (under-attack-pawn? chessboard file-num rank-num side)
    (define opponent (opponent-side side))
    (define opponent-pawn (get-side-piece 'p opponent))

    (if (= side 'white)
        (find-piece-positions chessboard file-num rank-num black-pawn-attack-positions opponent-pawn)
        (find-piece-positions chessboard file-num rank-num white-pawn-attack-positions opponent-pawn)))

(define (under-attack? chessboard file-num rank-num side)
    (or (under-attack-queen? chessboard file-num rank-num side)
        (under-attack-rook? chessboard file-num rank-num side)
        (under-attack-bishop? chessboard file-num rank-num side)
        (under-attack-knight? chessboard file-num rank-num side)
        (under-attack-king? chessboard file-num rank-num side)
        (under-attack-pawn? chessboard file-num rank-num side)))

(define (find-king chessboard side)
    (define king (get-side-piece 'k side))
    (define linear-index 0)

    (loop
        (if (> linear-index 63) (break false))
        (if (= (chessboard-ref-linear chessboard linear-index) king)
            (break linear-index))
        (set! 'linear-index (+ linear-index 1))))

(define (in-check? chessboard side)
    (define king-index (find-king chessboard side))
    (define king-file (% king-index 8))
    (define king-rank (/ king-index 8))

    (under-attack? chessboard king-file king-rank side))

(define (apply-move chessboard move)
    (define start-file (car move))
    (define start-rank (cadr move))
    (define end-file (caddr move))
    (define end-rank (cadddr move))

    (define piece (chessboard-ref-num chessboard start-file start-rank))
    (define new-chessboard (chessboard-dup chessboard))
    (chessboard-set-num! new-chessboard start-file start-rank '())
    (chessboard-set-num! new-chessboard end-file end-rank piece)
    new-chessboard)

(define (find-hv-moves! chessboard side file-num rank-num dx dy output)
    (define init-file-num file-num)
    (define init-rank-num rank-num)

    (set! 'file-num (+ file-num dx))
    (set! 'rank-num (+ rank-num dy))

    (loop
        (if (or (>= file-num 8)
                (>= rank-num 8)
                (< file-num 0)
                (< rank-num 0))
            (break))

        (if (or (= (chessboard-ref-num chessboard file-num rank-num) '())
                (= (get-piece-side (chessboard-ref-num chessboard file-num rank-num)) side))
            (break))

        (vector-push! output (list init-file-num init-rank-num file-num rank-num))

        ; If the piece is an enemy piece, we can capture it, but we can't move past it.
        (if (!= '() (chessboard-ref-num chessboard file-num rank-num))
            (break))

        (set! 'file-num (+ file-num dx))
        (set! 'rank-num (+ rank-num dy))))

(define (find-queen-moves! chessboard side file-num rank-num output)
    (find-hv-moves! chessboard side file-num rank-num 1 0 output)
    (find-hv-moves! chessboard side file-num rank-num -1 0 output)
    (find-hv-moves! chessboard side file-num rank-num 0 1 output)
    (find-hv-moves! chessboard side file-num rank-num 0 -1 output)
    (find-hv-moves! chessboard side file-num rank-num 1 1 output)
    (find-hv-moves! chessboard side file-num rank-num -1 -1 output)
    (find-hv-moves! chessboard side file-num rank-num 1 -1 output)
    (find-hv-moves! chessboard side file-num rank-num -1 1 output))

(define (find-rook-moves! chessboard side file-num rank-num output)
    (find-hv-moves! chessboard side file-num rank-num 1 0 output)
    (find-hv-moves! chessboard side file-num rank-num -1 0 output)
    (find-hv-moves! chessboard side file-num rank-num 0 1 output)
    (find-hv-moves! chessboard side file-num rank-num 0 -1 output))

(define (find-bishop-moves! chessboard side file-num rank-num output)
    (find-hv-moves! chessboard side file-num rank-num 1 1 output)
    (find-hv-moves! chessboard side file-num rank-num -1 -1 output)
    (find-hv-moves! chessboard side file-num rank-num 1 -1 output)
    (find-hv-moves! chessboard side file-num rank-num -1 1 output))

(define (find-position-moves! chessboard side file-num rank-num positions output)
    (define new-file-num '())
    (define new-rank-num '())

    (loop
        (if (null? positions) (break))

        (set! 'new-file-num (+ file-num (caar positions)))
        (set! 'new-rank-num (+ rank-num (cadar positions)))
        (if (and (>= new-file-num 0)
                 (>= new-rank-num 0)
                 (< new-file-num 8)
                 (< new-rank-num 8)
                 (or (= (chessboard-ref-num chessboard new-file-num new-rank-num) '())
                     (!= (get-piece-side (chessboard-ref-num chessboard new-file-num new-rank-num)) side)))
            (vector-push! output (list file-num rank-num new-file-num new-rank-num)))

        (set! 'positions (cdr positions))))

(define (find-knight-moves! chessboard side file-num rank-num output)
    (find-position-moves! chessboard side file-num rank-num knight-positions output))

(define (find-king-moves! chessboard side file-num rank-num output)
    (find-position-moves! chessboard side file-num rank-num king-positions output))

(define second-rank-white-pawn-positions (list '(0 1) '(0 2)))
(define white-pawn-moves (list '(0 1)))
(define white-pawn-captures (list '(1 1) '(-1 1)))
(define seventh-rank-black-pawn-positions (list '(0 -1) '(0 -2)))
(define black-pawn-moves (list '(0 -1)))
(define black-pawn-captures (list '(1 -1) '(-1 -1)))

(define (find-capture-moves! chessboard side file-num rank-num positions output)
    (define new-file-num '())
    (define new-rank-num '())

    (loop
        (if (null? positions) (break))

        (set! 'new-file-num (+ file-num (caar positions)))
        (set! 'new-rank-num (+ rank-num (cadar positions)))
        (if (and (>= new-file-num 0)
                 (>= new-rank-num 0)
                 (< new-file-num 8)
                 (< new-rank-num 8)
                 (not (= (chessboard-ref-num chessboard new-file-num new-rank-num) '()))
                 (not (= (get-piece-side (chessboard-ref-num chessboard new-file-num new-rank-num)) side)))
            (vector-push! output (list file-num rank-num new-file-num new-rank-num)))
        (set! 'positions (cdr positions))))

(define (find-noncapture-moves! chessboard side file-num rank-num positions output)
    (define new-file-num '())
    (define new-rank-num '())

    (loop
        (if (null? positions) (break))

        (set! 'new-file-num (+ file-num (caar positions)))
        (set! 'new-rank-num (+ rank-num (cadar positions)))

        (if (and (>= new-file-num 0)
                 (>= new-rank-num 0)
                 (< new-file-num 8)
                 (< new-rank-num 8)
                 (= (chessboard-ref-num chessboard new-file-num new-rank-num) '()))
            (vector-push! output (list file-num rank-num new-file-num new-rank-num)))
        (set! 'positions (cdr positions))))

(define (find-pawn-moves! chessboard side file-num rank-num output)
    (if (= side 'white)
        (begin
            (if (= rank-num 1)
                (find-noncapture-moves! chessboard side file-num rank-num second-rank-white-pawn-positions output)
                (find-noncapture-moves! chessboard side file-num rank-num white-pawn-moves output))
            (find-capture-moves! chessboard side file-num rank-num white-pawn-captures output))
        (begin
            (if (= rank-num 6)
                (find-noncapture-moves! chessboard side file-num rank-num seventh-rank-black-pawn-positions output)
                (find-noncapture-moves! chessboard side file-num rank-num black-pawn-moves output))
            (find-capture-moves! chessboard side file-num rank-num black-pawn-captures output))))

; TODO: castling
(define white-king-side-castle-empty-squares (list '(5 0) '(6 0)))
(define white-king-side-castle-safe-squares (list '(4 0) '(5 0) '(6 0)))
(define white-queen-side-castle-empty-squares (list '(1 0) '(2 0) '(3 0)))
(define white-queen-side-castle-safe-squares (list '(2 0) '(3 0) '(4 0)))

(define black-king-side-castle-empty-squares (list '(5 7) '(6 7)))
(define black-king-side-castle-safe-squares (list '(4 7) '(5 7) '(6 7)))
(define black-queen-side-castle-empty-squares (list '(1 7) '(2 7) '(3 7)))
(define black-queen-side-castle-safe-squares (list '(2 7) '(3 7) '(4 7)))

(define (find-all-moves! chessboard side output)
    (define linear-idx 0)

    (define file-num '())
    (define rank-num '())
    (define piece '())

    (loop
        (if (> linear-idx 63) (break))

        (set! 'file-num (% linear-idx 8))
        (set! 'rank-num (/ linear-idx 8))
        (set! 'piece (chessboard-ref-num chessboard file-num rank-num))

        (if (and (not (= piece '()))
                 (= (get-piece-side piece) side))
            (cond [(= piece (get-side-piece 'q side))
                   (find-queen-moves! chessboard side file-num rank-num output)]
                  [(= piece (get-side-piece 'r side))
                   (find-rook-moves! chessboard side file-num rank-num output)]
                  [(= piece (get-side-piece 'b side))
                   (find-bishop-moves! chessboard side file-num rank-num output)]
                  [(= piece (get-side-piece 'n side))
                   (find-knight-moves! chessboard side file-num rank-num output)]
                  [(= piece (get-side-piece 'k side))
                   (find-king-moves! chessboard side file-num rank-num output)]
                  [(= piece (get-side-piece 'p side))
                   (find-pawn-moves! chessboard side file-num rank-num output)]
                  [else (error "Unknown piece type")]))

        (set! 'linear-idx (+ linear-idx 1))))
