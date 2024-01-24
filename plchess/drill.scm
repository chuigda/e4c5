(define c (make-chessboard))

(define moves (vector))

(define (display-move move)
    (define start-x (+ (car move) 1))
    (define start-y (+ (cadr move) 1))
    (define end-x (+ (caddr move) 1))
    (define end-y (+ (cadddr move) 1))

    (define start-file (index->file start-x))
    (define end-file (index->file end-x))

    (println start-file start-y "-" end-file end-y))

(find-all-moves! c 'white moves)

(println "displaying all valid moves for white")
(define idx 0)
(loop
    (if (>= idx (vector-length moves))
        (break))

    (display-move (vector-ref moves idx))
    (set! 'idx (+ idx 1)))
