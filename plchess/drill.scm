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

(print-chessboard c)
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

(print-chessboard c)
(println "displaying all valid moves for white")
(set! 'moves (find-valid-moves c 'white))
(set! 'idx 0)
(loop
    (if (>= idx (vector-length moves))
        (break))

    (display-move (vector-ref moves idx))
    (set! 'idx (+ idx 1)))

(println "now play 2. Qh5? Nc6")
(apply-move! c (list 3 0 7 4))
(apply-move! c (list 1 7 2 5))
(print-chessboard c)

(println "now play 3. Bc4 Nf6??")
(apply-move! c (list 5 0 2 3))
(apply-move! c (list 6 7 5 5))
(print-chessboard c)

(println "now play 4. Qxf7#")
(apply-move! c (list 7 4 5 6))
(print-chessboard c)

(set! 'moves (find-valid-moves c 'black))

(println "now there should be no valid moves for black, and the king is in checkmate")
(assert (= (vector-length moves) 0)
        "there should be no valid moves for black")