#lang rackjure

(define (perform-reaction-pass reagents [result '()] #:last [last-matched #f])
  (match reagents
    [(list c1 c2 rst ...) 
     (if (and (not (eqv? c1 c2))
              (or (eqv? c1 (char-upcase c2))
                  (eqv? c1 (char-downcase c2)))) 
       (perform-reaction-pass rst result #:last #t)
       (if last-matched
          (perform-reaction-pass rst (cons c2 (cons c1 result))) 
          (perform-reaction-pass (cons c2 rst) (cons c1 result))))]
    [(list c) (cons c result)]
    ['() result]))

(call-with-input-file "test.txt"
                      (lambda (in)
                        (define input-chars (filter char-alphabetic? (port->list read-char in)))
                        (define reaction (perform-reaction-pass input-chars))
                        (length (perform-reaction-pass reaction))))
