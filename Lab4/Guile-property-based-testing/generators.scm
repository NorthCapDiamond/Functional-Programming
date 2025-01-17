(import (srfi :18))

(use-modules (srfi srfi-9)
             (srfi srfi-1)
             (srfi srfi-13))

(define now (current-time))
(define now-seconds (time->seconds now))
(define now-random-state (seed->random-state now-seconds))

(define-record-type <struct-generator>
  (make-struct-generator generator seed)
  struct-generator?
  (generator get-generator)
  (seed get-seed))

(define* (random-int-gen left right #:optional (random-state now-random-state))
  (lambda ()
    (+ (random (- right left) random-state) left)))

(define* (random-float-gen left right #:optional (random-state now-random-state))
  (lambda ()
    (+ (* (random (- right left) random-state) 1.0) left)))

(define empty-string "")
(define* (random-string-gen n #:optional (left 32)(right 95) (random-state now-random-state))
  (define (random-string-gen-sub n str)
    (cond
      [(eq? n 0) str]
      [else
        (random-string-gen-sub (- n 1) 
          (string-append str 
            (string 
              (integer->char 
                (+ (random right random-state) left)))))]))
  (lambda ()
    (random-string-gen-sub n empty-string)))

(define* (random-bool-gen #:optional (random-state now-random-state))
  (lambda ()
    (let ([ a (random 2 random-state)])
      (cond
        [(eqv? a 1) #t]
        [else #f]))))

(define* (random-int-list-gen n mini maxi #:optional (random-state now-random-state))
  (define (random-int-list-gen-sub n mini maxi lst)
    (cond
      [(eq? n 0) lst]
      [else
        (random-int-list-gen-sub
          (- n 1) 
          mini
          maxi 
          (append lst (list ((random-int-gen mini maxi random-state)))))]))
  (lambda ()
    (random-int-list-gen-sub n mini maxi '())))

(define* (random-string-list-gen n maxi #:optional (left 32) (right 95) (random-state now-random-state))
  (define (random-string-list-gen-sub n maxi lst)
    (cond
      [(eq? n 0) lst]
      [else
        (random-string-list-gen-sub
          (- n 1) 
          maxi 
          (append lst (list ((random-string-gen (random maxi random-state) left right random-state)))))]))
  (lambda()
    (random-string-list-gen-sub n maxi '())))

(define* (random-bool-list-gen n #:optional (random-state now-random-state))
  (define (random-bool-list-gen-sub n lst)
    (cond
      [(eq? n 0) lst]
      [else
        (random-bool-list-gen-sub
          (- n 1)  
          (append lst (list ((random-bool-gen random-state)))))]))
  (lambda ()
    (random-bool-list-gen-sub n '())))

(define* (random-any-list-gen n gen #:optional)
  (define (random-any-list-gen-sub n lst)
    (cond
      [(eq? n 0) lst]
      [else
      (random-any-list-gen-sub
        (- n 1)
        (append lst (list (gen))))]))
  (lambda ()
    (random-any-list-gen-sub n '())))

(define (combined-generator generators)
  (lambda args
    (cond 
      [(null? args)
        (map (lambda (gen) (gen)) generators)]
      [else
        (map (lambda (gen arg) (gen arg)) generators args)])))

(define* (merge-generators generators #:optional (random-state now-random-state))
  (lambda ()
    (let ([i (random (length generators) random-state)])
      ((list-ref generators i)))))

; (display ((random-int-gen 1 5)))
; (newline)
; (display ((random-float-gen 1 5.5)))
; (newline)
; (display ((random-string-gen 10)))
; (newline)
; (display ((random-int-list-gen 10 0 10)))
; (newline)
; (display ((random-bool-gen)))
; (newline)
; (display ((random-string-list-gen 10 10)))
; (newline)
; (display ((random-bool-list-gen 5)))
; (newline)

; (display ((random-any-list-gen 10 (random-int-gen 1 5))))
; (newline)

; (newline)
; (display ((combined-generator 
;   (list (random-int-gen 1 5)
;         (random-string-gen 10)
;         (random-float-gen 1 5.5)
;         (random-bool-gen)))))
; (newline)

; (newline)
; (display ((merge-generators 
;   (list (random-int-gen 1 5)
;         (random-string-gen 10)
;         (random-bool-gen)))))
; (newline)

; (newline)
; (display
;   ((merge-generators 
;     (list 
;       (combined-generator 
;       (list (random-int-gen 1 5)
;             (random-string-gen 10)
;             (random-float-gen 1 5.5)
;             (random-bool-gen)))
;       (merge-generators 
;       (list (random-int-gen 1 5)
;             (random-string-gen 10)
;             (random-bool-gen)))))))
; (newline)





