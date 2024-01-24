(define (make-chessboard)
    (struct 'k 'chessboard
            'position (vector 'R  'N  'B  'Q  'K  'B  'N  'R
                              'P  'P  'P  'P  'P  'P  'P  'P
                              '() '() '() '() '() '() '() '()
                              '() '() '() '() '() '() '() '()
                              '() '() '() '() '() '() '() '()
                              '() '() '() '() '() '() '() '()
                              'p  'p  'p  'p  'p  'p  'p  'p
                              'r  'n  'b  'q  'k  'b  'n  'r)
            'side-to-move 'white
            'castle-rights (vector 'K 'Q 'k 'q)
            'en-passant '()))

(define (chessboard-dup chessboard)
    (struct 'k 'chessboard
            'position (vector-dup (get-field chessboard 'position))
            'side-to-move (get-field chessboard 'side-to-move)
            'castle-rights (vector-dup (get-field chessboard 'castle-rights))
            'en-passant (get-field chessboard 'en-passant)))

(define (file->index file)
    (cond [(= file 'a) 1]
          [(= file 'b) 2]
          [(= file 'c) 3]
          [(= file 'd) 4]
          [(= file 'e) 5]
          [(= file 'f) 6]
          [(= file 'g) 7]
          [(= file 'h) 8]
          [else (error "unknown chessboard file: " file)]))

(define (index->file index)
    (cond [(= index 1) 'a]
          [(= index 2) 'b]
          [(= index 3) 'c]
          [(= index 4) 'd]
          [(= index 5) 'e]
          [(= index 6) 'f]
          [(= index 7) 'g]
          [(= index 8) 'h]
          [else (error "unknown chessboard file index: " index)]))

(define (cartesian->linear file rank)
    (define file-num (- (file->index file) 1))
    (define rank-num (- rank 1))
    (+ (* rank-num 8) file-num))

(define (chessboard-ref-num chessboard file-num rank-num)
    (define linear-index (+ (* rank-num 8) file-num))
    (vector-ref (get-field chessboard 'position) linear-index))

(define (chessboard-ref chessboard file rank)
    (define linear-index (cartesian->linear file rank))
    (vector-ref (get-field chessboard 'position) linear-index))

(define (chessboard-ref-linear chessboard linear-index)
    (vector-ref (get-field chessboard 'position) linear-index))

(define (chessboard-set-num! chessboard file-num rank-num piece)
    (define linear-index (+ (* rank-num 8) file-num))
    (vector-set! (get-field chessboard 'position) linear-index piece))

(define (chessboard-set! chessboard file rank piece)
    (define linear-index (cartesian->linear file rank))
    (vector-set! (get-field chessboard 'position) linear-index piece))

(define (get-side-piece piece side)
    (cond [(= side 'white)
           (cond [(= piece 'p) 'P]
                 [(= piece 'n) 'N]
                 [(= piece 'b) 'B]
                 [(= piece 'r) 'R]
                 [(= piece 'q) 'Q]
                 [(= piece 'k) 'K]
                 [else (error "unknown piece: " piece)])]
          [(= side 'black) piece]))

(define (get-piece-side piece)
    (cond [(= piece 'P) 'white]
          [(= piece 'N) 'white]
          [(= piece 'B) 'white]
          [(= piece 'R) 'white]
          [(= piece 'Q) 'white]
          [(= piece 'K) 'white]
          [(= piece 'p) 'black]
          [(= piece 'n) 'black]
          [(= piece 'b) 'black]
          [(= piece 'r) 'black]
          [(= piece 'q) 'black]
          [(= piece 'k) 'black]
          [else (error "unknown piece: " piece)]))

(define (opponent-side side)
    (cond [(= side 'white) 'black]
          [(= side 'black) 'white]
          [else (error "unknown side: " side)]))

(define (piece-glyph piece)
    (cond [(= piece 'P) "♙ "]
          [(= piece 'N) "♘ "]
          [(= piece 'B) "♗ "]
          [(= piece 'R) "♖ "]
          [(= piece 'Q) "♕ "]
          [(= piece 'K) "♔ "]
          [(= piece 'p) "♟ "]
          [(= piece 'n) "♞ "]
          [(= piece 'b) "♝ "]
          [(= piece 'r) "♜ "]
          [(= piece 'q) "♛ "]
          [(= piece 'k) "♚ "]
          [(= piece '()) "  "]
          [else (error "unknown piece: " piece)]))

(define (print-chessboard chessboard)
    (define rank-num 7)
    (define file-num 0)

    (loop
        (if (< rank-num 0)
            (break))
        (loop
            (if (= file-num 8)
                (break))
            (print (piece-glyph (chessboard-ref-num chessboard file-num rank-num)))
            (set! 'file-num (+ file-num 1)))

        (println "")
        (set! 'rank-num (- rank-num 1))
        (set! 'file-num 0)))
