#lang rackjure

(require gregor)
(require racket/hash)

(define date-rgx #px"\\[(\\d+)-(\\d+)-(\\d+) (\\d{2}):(\\d{2})\\]\\s*(.+)")
(define guard-begin-rgx #px"Guard #(\\d+)")
(define (s->n s) (string->number s))

(define (hash-range from to nv)
  (for/hash ([v (in-range from to)]) (values v nv)))

(define (time-range from to) (hash-range from to 1))

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
                               (list id (h id #:else 0) (sh id #:else (hash)))]
                              ["falls asleep"
                               (list cur-guard-id (h cur-guard-id) (sh cur-guard-id))]
                              ["wakes up"
                               (list cur-guard-id 
                                     (+ (h cur-guard-id) (minutes-between last-stamp cur-time))
                                     (let ([hour-diff (hours-between last-stamp cur-time)])
                                       (for/fold ([h (sh cur-guard-id)])
                                         ([i (in-range (+ hour-diff 1))])
                                         (define new-range 
                                           (cond 
                                             [(and (= i 0) (= hour-diff 0))
                                              (time-range (->minutes last-stamp) (->minutes cur-time))]
                                             [(= i 0)
                                              (time-range (->minutes last-stamp) 59)]
                                             [else
                                              (time-range 0 (if (= i hour-diff)
                                                              (->minutes cur-time)
                                                              59))]))
                                         (hash-union h new-range #:combine/key (lambda (k v1 v2) (+ v1 v2))))))]))
                          (values guard-id cur-time
                                  (sh guard-id minutes-asleep)
                                  (h guard-id time-asleep))))
                      (define-values (sleepiest-guard-id sleepiest-guard-time)
                        (for/fold ([max-id #f] [max-so-far 0]) ([(k v) guard-sleep-times])
                          (if (> v max-so-far) (values k v) (values max-id max-so-far))))
                      (define sleepiest-minute
                        (for/fold ([max-min #f] [max-val 0] #:result max-min) ([(k v) (guard-sleep-windows sleepiest-guard-id)])
                          (if (> v max-val) (values k v) (values max-min max-val))))
                      (displayln
                        (str "Sleepiest Guard is #" sleepiest-guard-id
                             ", they slept: " sleepiest-guard-time " minutes"
                             ", their sleepiest minute was: " sleepiest-minute
                             "\n - Result: ID * " sleepiest-minute " = " (* (s->n sleepiest-guard-id) sleepiest-minute)
                             ))))
