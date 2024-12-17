(define (linear-function x X0 Y0 X1 Y1)
  (round-two-digits (+ Y0 (* (- x X0) (/ (- Y1 Y0) (- X1 X0))))))

(define (linear-function-step Xs Ys Xe Ye step ans)
  (cond
    ((< Xs Xe)
     (linear-function-step
      (+ Xs step)
      (linear-function (+ Xs step) Xs Ys Xe Ye)
      Xe
      Ye
      step
      (append ans (list (cons Xs Ys)))))
    (else (append
           ans
           (list (cons (round-two-digits Xe) (round-two-digits Ye)))))))



