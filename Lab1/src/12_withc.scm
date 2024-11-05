(define (divider? a b)
  (zero? (modulo a b)))

(define (inner current i answer n)
  (if (>= i (sqrt current))
      (if (integer? (sqrt current))
          (if (>= (+ answer 2) n)
              current
              0)
          (if (>= answer n)
              current
              0))
      (if (and (divider? current i) (<= i (sqrt current)))
          (inner current (+ i 1) (+ answer 2) n)
          (inner current (+ i 1) answer n))))

