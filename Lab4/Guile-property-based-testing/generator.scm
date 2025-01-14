(import (srfi :18))

(define empty-string "")
(define now (current-time))
(define now-seconds (time->seconds now))
(define my-random-state (seed->random-state now-seconds))

(define (range start end)
  (cond 
    [(>= start end)'()]
    [else
      (cons start 
        (range (+ start 1) end))]))

(define* (random-float min max #:optional (r-state my-random-state))
  (- (random max r-state) (random min r-state)))
; (display (random-float 0.0 5.0))
; (newline)
; (display (random-float 0.0 5.0 (seed->random-state 0)))
; (newline)

(define* (random-ascii-string n #:optional (left 32)(right 95) (r-state my-random-state))
  (define (random-ascii-string-sub n str)
    (cond
      [(eq? n 0) str]
      [else
        (random-ascii-string-sub (- n 1) 
          (string-append str 
            (string 
              (integer->char 
                (+ (random right r-state) left)))))]))
  (random-ascii-string-sub n empty-string))
; (display (random-ascii-string 5))
; (newline)
; (display (random-ascii-string 5 0 100 (seed->random-state 0)))
; (newline)
; (display (random-ascii-string 5 32 33 (seed->random-state 90)))
; (newline)

(define* (random-int-list n maxi #:optional (r-state my-random-state))
  (define (random-int-list-sub n maxi lst)
    (cond
      [(eq? n 0) lst]
      [else
        (random-int-list-sub
          (- n 1) 
          maxi 
          (append lst (list (random maxi r-state))))]))
  (random-int-list-sub n maxi '()))
; (display (random-int-list 5 10000))
; (newline)
; (display (random-int-list 5 10000 (seed->random-state 0)))
; (newline)

(define* (random-string-list n maxi #:optional (left 32) (right 95) (r-state my-random-state))
  (define (random-string-list-sub n maxi lst)
    (cond
      [(eq? n 0) lst]
      [else
        (random-string-list-sub
          (- n 1) 
          maxi 
          (append lst (list (random-ascii-string (random maxi r-state) left right r-state))))]))
  (random-string-list-sub n maxi '()))
; (display (random-string-list 10 6))
; (newline)
; (display (random-string-list 10 6 32 95 (seed->random-state 0)))
; (newline)

(define* (random-bool #:optional (r-state my-random-state))
  (let ([ a (random 2 r-state)])
    (cond
      [(eqv? a 1) #t]
      [else #f])))
; (display (random-bool))
; (newline)
; (display (random-bool (seed->random-state 0)))
; (newline)

(define* (random-bool-list n #:optional (r-state my-random-state))
  (define (random-bool-list-sub n lst)
    (cond
      [(eq? n 0) lst]
      [else
        (random-bool-list-sub
          (- n 1)  
          (append lst (list (random-bool r-state))))]))
  (random-bool-list-sub n '()))
; (display (random-bool-list 5))
; (newline)
; (display (random-bool-list 5 (seed->random-state 0)))
; (newline)
