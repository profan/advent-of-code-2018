#lang racket

(require racket/set)

(define (calc-frequency input init-state)
  (sequence-fold 
    (lambda (acc-v e)
      (match-define (list total fs ds) acc-v)
      (define new-freq (+ total (string->number e)))
      (define new-freq-set (set-add fs new-freq))
      (define new-freq-doubles
        (cond 
          [(and (set-member? fs new-freq) (not (member new-freq ds))) 
           (append ds (list new-freq))]
          [else ds]))
      (list new-freq new-freq-set new-freq-doubles))
    init-state input))

(define (calc-until-has-duplicate lines [state (list 0 (set) '())])
  (match-define (list cur-freq cur-freq-set cur-doubles) 
    (calc-frequency lines state))
  (cond
    [(empty? cur-doubles)
     (calc-until-has-duplicate lines (list cur-freq cur-freq-set cur-doubles))]
    [else
     (list cur-freq cur-freq-set cur-doubles)]))

(match-define (list final-freq final-freq-set final-doubles)
  (call-with-input-file "input.txt"
                        (lambda (in)
                          (define lines (port->lines in))
                          (calc-until-has-duplicate lines))))

(displayln (format "final freq: ~a~nfirst double: ~a" final-freq (first final-doubles)))
