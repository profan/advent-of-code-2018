#lang rackjure

(define (calc-checksum line)
  (for/fold ([h (hash)]) ([c (in-string line)])
    (define cur-val (h c))
    (cond
      [cur-val (h c (+ cur-val 1))]
      [else (h c 1)])))

(define (calc-delta s1 s2)
  (for/sum ([c1 (in-string s1)]
            [c2 (in-string s2)])
    (if (eqv? c1 c2) 0 1)))

(define (cut-delta s1 s2)
  (list->string 
    (for/list ([c1 (in-string s1)]
               [c2 (in-string s2)]
               #:when (eqv? c1 c2)) c1)))

; expand our dict literals to hashes
(current-curly-dict hash)

(match-define (and result (list final-checksum final-box-id)) 
  (call-with-input-file "input.txt"
                        (lambda (in)
                          (define lines (port->lines in))
                          ; part 2 - 1
                          (define total-matches
                            (for/fold ([h {2 0 3 0}]) ([cur-line lines])
                              (define cur-matches
                                (for/hash ([(k v) (calc-checksum cur-line)]
                                           #:when (or (= v 2) (= v 3)))
                                  (values v k)))
                              (define h1 (if (cur-matches 2) (h 2 (+ (h 2) 1)) h))
                              (define h2 (if (cur-matches 3) (h1 3 (+ (h1 3) 1)) h1))
                              h2))
                          (define checksum (for/product ([(k v) total-matches]) v))
                          ; part 2 - 2
                          (define correct-box-ids
                            (for*/set ([e1 lines]
                                       [e2 lines]
                                       #:when (= 1 (calc-delta e1 e2)))
                                      (cut-delta e1 e2)))
                          (list checksum correct-box-ids))))

result
