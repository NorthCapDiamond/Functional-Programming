(define (divider? a b)
  (zero? (modulo a b)))

(define (inner current i answer n)
  (if (>= i (sqrt current))
    (if (integer? (sqrt current))
      (if (>= (+ answer 1) n)
	current #f)
      (if (>= answer n)
	current #f))
    (if (and (divider? current i) (<= i (sqrt current)))
    	(inner current (+ i 1) (+ answer 2) n )
    	(inner current (+ i 1) answer n ))))

(define (solution counter current answer n)
  (if(inner current 1 answer n)
    (inner current 1 answer n)
    (solution (+ counter 1) (+ counter current 1) answer n)))

(display (string-append "Answer is " (number->string(solution 1 1 0 (30))) "\n"))

