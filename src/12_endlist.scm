(define (triangle-numbers n i)
  (cons (/ (* (+ n 1) i) 2)
	(delay(triangle-numbers (+ n 1) (+ i 1)))))

(define (natural-numbers n)
  (cons n (delay(natural-numbers (+ n 1)))))

(define (n-th-triangle-number tn n)
  (if (= n 1)
    (car tn)
    (n-th-triangle-number (force (cdr tn)) (- n 1))))

(define (range-1-to-n n)
  (if (= n 0)
      '() 
      (append (range-1-to-n (- n 1)) (list n))))

(define (divisors-counter number list-divs)
  (define (divider? n d) (zero? (modulo n d)))
  (length (filter (lambda (d) (divider? number d)) list-divs)))



(define (solver list-a divs)
  (if ( >= (divisors-counter (car list-a) (range-1-to-n (car list-a))) divs)
    (car list-a)
    (solver (force (cdr list-a)) divs)))
(define triangles (triangle-numbers 1 1))
(display (solver triangles (30)))
(newline)
