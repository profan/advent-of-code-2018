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

(define (fully-react-polymer reagents)
  (let repeat-until-same ([last-length 0] [cur-list reagents])
   (define new-state (perform-reaction-pass cur-list))
   (define new-length (length new-state))
   (cond
     [(= new-length last-length) cur-list]
     [else (repeat-until-same new-length new-state)])))

(define (minimize-polymer-length reagents)
  (define types (for/set ([r reagents]) (char-downcase r)))
  (for/fold ([min-size +inf.0] [shortest '()] #:result min-size) ([t types])
    (define new-polymer (fully-react-polymer (filter (lambda (e) (not (eqv? (char-downcase e) t))) reagents)))
    (define new-size (length new-polymer))
    (if (< new-size min-size)
      (values new-size new-polymer)
      (values min-size shortest))))

(call-with-input-file "input.txt"
                      (lambda (in)
                        (define input-chars (filter char-alphabetic? (port->list read-char in)))
                        (define fully-reacted-polymer (fully-react-polymer input-chars))
                        (displayln (str "[5-1]: units remaining after first scan is: " (length fully-reacted-polymer)))
                        (define minimized-polymer-size (minimize-polymer-length input-chars))
                        (displayln (str "[5-2]: the shortest polymer that can be produced is: " minimized-polymer-size))))
