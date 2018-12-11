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

(call-with-input-file "input.txt"
                      (lambda (in)
                        (define input-chars (filter char-alphabetic? (port->list read-char in)))
                        (define fully-reacted-polymer
                          (let repeat-until-same ([last-length 0] [cur-list input-chars])
                           (define new-state (perform-reaction-pass cur-list))
                           (define new-length (length new-state))
                           (if (= new-length last-length) cur-list (repeat-until-same new-length new-state))))
                        (length fully-reacted-polymer)))
