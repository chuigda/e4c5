(define (find-piece chessboard file rank dx dy piece)
    (define file-num (- (file->index file) 1))
    (define rank-num (- (rank->index rank) 1))

    (loop
        (if (or (>= file-num 8)
                (>= rank-num 8)
                (< file-num 0)
                (< rank-num 0))
            (break false))

        (if (eq? (chessboard-ref-num chessboard file rank) piece)
            (break true))

        (set! 'file-num (+ file-num dx))
        (set! 'rank-num (+ rank-num dy))))

(define (find-piece-positions chessboard file rank positions piece)
    (define file-num (- (file->index file) 1))
    (define rank-num (- (rank->index rank) 1))

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
                 (eq? (chessboard-ref-num chessboard file rank) piece))
            (break true))

        (set! 'positions (cdr positions))))

(define (under-attack-queen? chessboard file rank side)
    (define opponent (opponent-side side))
    (define opponent-queen (get-piece 'q opponent))

    (or (find-piece chessboard file rank 1 0 opponent-queen)
        (find-piece chessboard file rank -1 0 opponent-queen)
        (find-piece chessboard file rank 0 1 opponent-queen)
        (find-piece chessboard file rank 0 -1 opponent-queen)
        (find-piece chessboard file rank 1 1 opponent-queen)
        (find-piece chessboard file rank -1 -1 opponent-queen)
        (find-piece chessboard file rank 1 -1 opponent-queen)
        (find-piece chessboard file rank -1 1 opponent-queen)))

(define (under-attack-rook? chessboard file rank side)
    (define opponent (opponent-side side))
    (define opponent-king (get-piece 'r opponent))

    (or (find-piece chessboard file rank 1 0 opponent-king)
        (find-piece chessboard file rank -1 0 opponent-king)
        (find-piece chessboard file rank 0 1 opponent-king)
        (find-piece chessboard file rank 0 -1 opponent-king)))

(define (under-attack-bishop? chessboard file rank side)
    (define opponent (opponent-side side))
    (define opponent-king (get-piece 'b opponent))

    (or (find-piece chessboard file rank 1 1 opponent-king)
        (find-piece chessboard file rank -1 -1 opponent-king)
        (find-piece chessboard file rank 1 -1 opponent-king)
        (find-piece chessboard file rank -1 1 opponent-king)))

(define knight-positions (list '(2 1) '(2 -1) '(-2 1) '(-2 -1)
                               '(1 2) '(1 -2) '(-1 2) '(-1 -2)))

(define (under-attack-knight? chessbaord file rank side)
    (define opponent (opponent-side side))
    (define opponent-knight (get-piece 'n opponent))

    (find-piece-positions chessboard file rank knight-positions opponent-knight))

(define king-positions (list '(1 0) '(-1 0) '(0 1) '(0 -1)
                             '(1 1) '(-1 -1) '(1 -1) '(-1 1)))

(define (under-attack-king? chessboard file rank side)
    (define opponent (opponent-side side))
    (define opponent-king (get-piece 'k opponent))

    (find-piece-positions chessboard file rank king-positions opponent-king))

(define white-pawn-attack-positions (list '(1 1) '(1 -1)))
(define black-pawn-attack-positions (list '(-1 1) '(-1 -1)))

(define (under-attack-pawn? chessboard file rank side)
    (define opponent (opponent-side side))
    (define opponent-pawn (get-piece 'p opponent))

    (if (eq? side 'white)
        (find-piece-positions chessboard file rank black-pawn-attack-positions opponent-pawn)
        (find-piece-positions chessboard file rank white-pawn-attack-positions opponent-pawn)))

(define (under-attack? chessboard file rank side)
    (or (under-attack-queen? chessboard file rank side)
        (under-attack-rook? chessboard file rank side)
        (under-attack-bishop? chessboard file rank side)
        (under-attack-knight? chessboard file rank side)
        (under-attack-king? chessboard file rank side)
        (under-attack-pawn? chessboard file rank side)))
