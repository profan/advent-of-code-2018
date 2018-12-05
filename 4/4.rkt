#lang rackjure

(require gregor)
(require data/integer-set)

(define date-rgx #px"\\[(\\d+)-(\\d+)-(\\d+) (\\d{2}):(\\d{2})\\]\\s*(.+)")
(define guard-begin-rgx #px"Guard #(\\d+)")
(define (s->n s) (string->number s))

(call-with-input-file "input.txt"
                     (lambda (in)
                      (define lines (port->lines in))
                      (define sorted-entries
                        (sort 
                          (for/list ([l lines])
                            (match-define (list _ year month day hour minute rst)
                              (regexp-match date-rgx l))
                            (define timestamp
                              (datetime (s->n year) (s->n month) (s->n day) (s->n hour) (s->n minute)))
                            (cons timestamp rst))
                          (lambda (e1 e2) (datetime<? (car e1) (car e2)))))
                      (match-define (list guard-sleep-times guard-sleep-windows)
                        (for/fold ([cur-guard-id #f]
                                   [last-stamp #f]
                                   [sh (hash)]
                                   [h (hash)]
                                   #:result (list h sh))
                          ([(cur-time rst) (in-dict sorted-entries)])
                          (match-define (list guard-id time-asleep minutes-asleep)
                            (match rst
                              [(pregexp guard-begin-rgx (list _ id))
                               (list id (h id #:else 0) (sh id #:else '()))]
                              ["falls asleep"
                               (list cur-guard-id (h cur-guard-id) (sh cur-guard-id))]
                              ["wakes up"
                               (list cur-guard-id 
                                     (+ (h cur-guard-id) (minutes-between last-stamp cur-time))
                                     (let ([hour-diff (hours-between last-stamp cur-time)])
                                       (for/list ([i (in-range (+ hour-diff 1))])
                                         (cond 
                                           [(and (= i 0) (= hour-diff 0))
                                            (make-range (->minutes last-stamp) (->minutes cur-time))]
                                           [(= i 0)
                                            (make-range (->minutes last-stamp) 59)]
                                           [else
                                            (make-range 0 (if (= i hour-diff)
                                                            (->minutes cur-time)
                                                            59))])))
                                     )]))
                          (values guard-id cur-time
                                  (sh guard-id (cons minutes-asleep (sh guard-id #:else '())))
                                  (h guard-id time-asleep))))
                      guard-sleep-times))
