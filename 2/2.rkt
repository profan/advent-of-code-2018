#lang rackjure

(define (calc-checksum line)
  (for/fold ([h (hash)]) ([c (in-string line)])
    (define cur-val (h c))
    (cond
      [cur-val (h c (+ cur-val 1))]
      [else (h c 1)])))

; expand our dict literals to hashes
(current-curly-dict hash)

(call-with-input-file "input.txt"
                      (lambda (in)
                        (define lines (port->lines in))
                        (define total-matches 
                          (for/fold ([h {2 0 3 0}]) ([cur-line lines])
                            (define cur-matches
                              (for/hash ([(k v) (calc-checksum cur-line)]
                                         #:when (or (= v 2) (= v 3)))
                                (values v k)))
                            (define h1 (if (cur-matches 2) (h 2 (+ (h 2) 1)) h))
                            (define h2 (if (cur-matches 3) (h1 3 (+ (h1 3) 1)) h1))
                            h2))
                        (for/product ([(k v) total-matches]) v)))
