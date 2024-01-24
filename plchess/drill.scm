(define c (make-chessboard))

(println "is white in check in this position? " (in-check? c 'white))

; put a black queen on e2 square
(chessboard-set! c 'e 2 'q)

(println "is white in check in this position? " (in-check? c 'white))
