#lang rackjure

(require gregor)
(require racket/hash)

(define date-rgx #px"\\[(\\d+)-(\\d+)-(\\d+) (\\d{2}):(\\d{2})\\]\\s*(.+)")
(define guard-begin-rgx #px"Guard #(\\d+)")
(define (s->n s) (string->number s))

(define (key-for-max-value h)
  (for/fold ([max-key #f] [max-val 0] #:result max-key) ([(k v) h])
    (if (> v max-val) (values k v) (values max-key max-val))))

(define (hash-range from to nv)
  (for/hash ([v (in-range from to)]) (values v nv)))

(define (time-range from to) (hash-range from to 1))

(call-with-input-file "input.txt"
                     (lambda (in)
                      (define lines (port->lines in))
                      ; part 4 - 1
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
                        (let ([id (key-for-max-value guard-sleep-times)])
                         (values id (guard-sleep-times id))))
                      (define sleepiest-minute
                        (key-for-max-value (guard-sleep-windows sleepiest-guard-id)))
                      (displayln
                        (str "[Strategy 1] Sleepiest Guard is #" sleepiest-guard-id
                             ", they slept: " sleepiest-guard-time " minutes"
                             ", their sleepiest minute was: " sleepiest-minute
                             "\n - Result: ID * " sleepiest-minute " = " (* (s->n sleepiest-guard-id) sleepiest-minute)))
                      ; part 4 - 2
                      (define-values (most-asleep-on-same-minute most-which-minute times-asleep)
                        (for/fold ([max-id #f] [max-min 0] [max-val 0])
                          ([(cur-guard-id sh) guard-sleep-windows] #:when (not (hash-empty? sh)))
                          (define most-slept-minute (key-for-max-value sh))
                          (if (> (sh most-slept-minute) max-val)
                            (values cur-guard-id most-slept-minute (sh most-slept-minute))
                            (values max-id max-min max-val))))
                      (displayln
                        (str "[Strategy 2] Guard most asleep on the same minute is: #" most-asleep-on-same-minute
                             " with minute: " most-which-minute " (" times-asleep " times)"
                             "\n - Result: ID * " most-which-minute " = " (* (s->n most-asleep-on-same-minute) most-which-minute)))))
