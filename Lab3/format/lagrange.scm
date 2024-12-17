(define (lagrange-function x L)
  (define (inner-loop x L n i j l_i)
    (cond
      ((eq? n j) l_i)
      ((not (eq? i j))
       (let ((deltaX (- (car (list-ref L i)) (car (list-ref L j)))))
         (inner-loop
          x
          L
          n
          i
          (+ j 1)
          (* l_i (/ (- x (car (list-ref L j))) deltaX)))))
      (else (inner-loop x L n i (+ j 1) l_i))))
  (define (lagrange-loop x L n i l_S)
    (cond
      ((eq? i n) (round-two-digits l_S))
      (else (lagrange-loop
             x
             L
             n
             (+ i 1)
             (+ l_S (* (inner-loop x L n i 0 1) (cadr (list-ref L i))))))))
  (lagrange-loop x L (length L) 0 0))

(define (lagrange-function-step L step ans)
  (define (lagrange-function-step-sub L step ans xi xe)
    (cond
      ((< xi xe)
       (lagrange-function-step-sub
        L
        step
        (append
         ans
         (list (list (round-two-digits xi)
                     (round-two-digits (lagrange-function xi L)))))
        (+ xi step)
        xe))
      (else (append
             ans
             (list (list (round-two-digits xi)
                         (round-two-digits (lagrange-function xi L))))))))
  (lagrange-function-step-sub
   L
   step
   '()
   (car (car L))
   (car (list-ref L (- (length L) 1)))))



