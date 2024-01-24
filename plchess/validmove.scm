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
    (define opponent-queen (get-piece 'q opponent))

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
    (define opponent-king (get-piece 'r opponent))

    (or (find-piece chessboard file-num rank-num 1 0 opponent-king)
        (find-piece chessboard file-num rank-num -1 0 opponent-king)
        (find-piece chessboard file-num rank-num 0 1 opponent-king)
        (find-piece chessboard file-num rank-num 0 -1 opponent-king)))

(define (under-attack-bishop? chessboard file-num rank-num side)
    (define opponent (opponent-side side))
    (define opponent-king (get-piece 'b opponent))

    (or (find-piece chessboard file-num rank-num 1 1 opponent-king)
        (find-piece chessboard file-num rank-num -1 -1 opponent-king)
        (find-piece chessboard file-num rank-num 1 -1 opponent-king)
        (find-piece chessboard file-num rank-num -1 1 opponent-king)))

(define knight-positions (list '(2 1) '(2 -1) '(-2 1) '(-2 -1)
                               '(1 2) '(1 -2) '(-1 2) '(-1 -2)))

(define (under-attack-knight? chessboard file-num rank-num side)
    (define opponent (opponent-side side))
    (define opponent-knight (get-piece 'n opponent))

    (find-piece-positions chessboard file-num rank-num knight-positions opponent-knight))

(define king-positions (list '(1 0) '(-1 0) '(0 1) '(0 -1)
                             '(1 1) '(-1 -1) '(1 -1) '(-1 1)))

(define (under-attack-king? chessboard file-num rank-num side)
    (define opponent (opponent-side side))
    (define opponent-king (get-piece 'k opponent))

    (find-piece-positions chessboard file-num rank-num king-positions opponent-king))

(define white-pawn-attack-positions (list '(1 1) '(1 -1)))
(define black-pawn-attack-positions (list '(-1 1) '(-1 -1)))

(define (under-attack-pawn? chessboard file-num rank-num side)
    (define opponent (opponent-side side))
    (define opponent-pawn (get-piece 'p opponent))

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
    (define king (get-piece 'k side))
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
