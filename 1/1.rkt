#lang racket

(require racket/set)

(define (calc-frequency input [init-state (list 0 (set) '())])
  (sequence-fold 
    (lambda (acc-v e)
      (match-define (list total fs ds) acc-v)
      (define new-freq (+ total (string->number e)))
      (define new-freq-set (set-add fs new-freq))
      (define new-freq-doubles
        (cond 
          [(and (set-member? fs new-freq) (not (member new-freq ds))) 
           (cons new-freq ds)]
          [else ds]))
      (list new-freq new-freq-set new-freq-doubles))
    init-state input))

(define (calc-until-has-duplicate lines [state (list 0 (set) '())])
  (match-define (and new-state (list cur-freq cur-freq-set cur-doubles))
    (calc-frequency lines state))
  (cond
    [(empty? cur-doubles) (calc-until-has-duplicate lines new-state)]
    [else new-state]))

(match-define (list first-freq final-freq final-freq-set final-doubles)
  (call-with-input-file "input.txt"
                        (lambda (in)
                          (define lines (port->lines in))
                          (cons 
                            (first (calc-frequency lines))
                            (calc-until-has-duplicate lines)))))

(displayln (format "[1-1]: first freq: ~a~n[1-2]: final freq: ~a, first double: ~a" first-freq final-freq (last final-doubles)))
