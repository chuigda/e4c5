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

(define (chessboard-set-num! chessboard file-num rank-num piece)
    (define linear-index (+ (* rank-num 8) file-num))
    (vector-set! (get-field chessboard 'position) linear-index piece))

(define (chessboard-set! chessboard file rank piece)
    (define linear-index (cartesian->linear file rank))
    (vector-set! (get-field chessboard 'position) linear-index piece))

(define (get-piece piece side)
    (cond [(= side 'white)
           (cond [(= piece 'p) 'P]
                 [(= piece 'n) 'N]
                 [(= piece 'b) 'B]
                 [(= piece 'r) 'R]
                 [(= piece 'q) 'Q]
                 [(= piece 'k) 'K]
                 [else (error "unknown piece: " piece)])]
          [(= side 'black) piece]))

(define (opponent-side side)
    (cond [(= side 'white) 'black]
          [(= side 'black) 'white]
          [else (error "unknown side: " side)]))
