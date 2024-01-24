(define c (make-chessboard))

(define (display-move move)
    (define start-x (+ (car move) 1))
    (define start-y (+ (cadr move) 1))
    (define end-x (+ (caddr move) 1))
    (define end-y (+ (cadddr move) 1))

    (define start-file (index->file start-x))
    (define end-file (index->file end-x))

    (println start-file start-y "-" end-file end-y))

(define moves (find-valid-moves c 'white))

(println "displaying all valid moves for white")
(define idx 0)
(loop
    (if (>= idx (vector-length moves))
        (break))

    (display-move (vector-ref moves idx))
    (set! 'idx (+ idx 1)))

(println "now play 1. e4 e5")
(apply-move! c (list 4 1 4 3))
(apply-move! c (list 4 6 4 4))

(println "displaying all valid moves for white")
(set! 'moves (find-valid-moves c 'white))
(set! 'idx 0)
(loop
    (if (>= idx (vector-length moves))
        (break))

    (display-move (vector-ref moves idx))
    (set! 'idx (+ idx 1)))
