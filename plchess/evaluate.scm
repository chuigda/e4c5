(define (piece-value piece)
  (cond [(or (= piece 'K) (= piece 'k)) 999]
        [(or (= piece 'Q) (= piece 'q)) 9]
        [(or (= piece 'R) (= piece 'r)) 5]
        [(or (= piece 'B) (= piece 'b)) 3]
        [(or (= piece 'N) (= piece 'n)) 3]
        [(or (= piece 'P) (= piece 'p)) 1]))

; knights worth more in the center, and most times it's less
; valuable than a bishop
(define position-value-knight
    (vector 0.3 0.5 0.5 0.5 0.5 0.5 0.5 0.3
            0.5 0.7 0.9 0.9 0.9 0.9 0.7 0.5
            0.5 0.9 1.0 1.1 1.1 1.0 0.9 0.5
            0.5 0.9 1.1 1.1 1.1 1.1 0.9 0.5
            0.5 0.9 1.1 1.1 1.1 1.1 0.9 0.5
            0.5 0.9 1.0 1.1 1.1 1.0 0.9 0.5
            0.5 0.7 0.9 0.9 0.9 0.9 0.7 0.5
            0.3 0.5 0.5 0.5 0.5 0.5 0.5 0.3))

; pawns worth more as they advance, and this also encourages
; them to move forward, also encourages king's pawn and queen's
; pawn openings
(define position-value-pawn-white
    (vector 0   0   0 0 0 0 0   0
            0.5 0.5 1 1 1 1 0.5 0.5
            1   1   1 1 1 1 1   1
            1   1   1 2 2 1 1   1
            1.5 1.5 2 2 2 2 1.5 1.5
            2   2   2 2 2 2 2   2
            3   3   3 3 3 3 3   3
            9   9   9 9 9 9 9   9))

; reversed for black
(define position-value-pawn-black
    (vector 9   9   9 9 9 9 9   9
            3   3   3 3 3 3 3   3
            2   2   2 2 2 2 2   2
            1.5 1.5 2 2 2 2 1.5 1.5
            1   1   1 2 2 1 1   1
            1   1   1 1 1 1 1   1
            0.5 0.5 1 1 1 1 0.5 0.5
            0   0   0 0 0 0 0   0))

; rooks worth more on open files, but we don't want to implement. However,
; usually you want to move your rooks over the 4th rank, so we'll give
; them a bonus for that
(define position-value-rook-white
    (vector 1   1   1   1   1   1   1   1
            1   1   1   1   1   1   1   1
            1   1   1   1   1   1   1   1
            1.2 1.2 1.2 1.2 1.2 1.2 1.2 1.2
            1.2 1.2 1.2 1.2 1.2 1.2 1.2 1.2
            1.2 1.2 1.2 1.2 1.2 1.2 1.2 1.2
            1.2 1.2 1.2 1.2 1.2 1.2 1.2 1.2
            ; rooks on last rank usually execute some tactical idea, but
            ; they are actually less active so we make them not that
            ; valuable
            1   1   1   1   1   1   1   1))

; reversed for black
(define position-value-rook-black
    (vector 1   1   1   1   1   1   1   1
            1.2 1.2 1.2 1.2 1.2 1.2 1.2 1.2
            1.2 1.2 1.2 1.2 1.2 1.2 1.2 1.2
            1.2 1.2 1.2 1.2 1.2 1.2 1.2 1.2
            1.2 1.2 1.2 1.2 1.2 1.2 1.2 1.2
            1   1   1   1   1   1   1   1
            1   1   1   1   1   1   1   1
            1   1   1   1   1   1   1   1))
