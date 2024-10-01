(import (rnrs base)
	(only (srfi :1) fold-right))

(define (divider? a b)
  (zero? (modulo a b)))

(define (days year month)
  (if (or (= month 4) (= month 6) (= month 9) (= month 11))
    30
    (if (= month 2)
      (if (and (divider? year 4) (or (not (divider? year 100)) (divider? year 400)))
	29 28) 31)))


(define (gen-calendar year_id month_id day_id year_limit month_limit day_limit dayname)
  (if (and (<= year_id year_limit) (not (and (>= year_id year_limit) (>= month_id month_limit) (>= day_id day_limit))))
    (begin(if (> day_id (days year_id month_id))
          (gen-calendar year_id (+ month_id 1) 1 year_limit month_limit day_limit dayname)
          (begin (if (> month_id 12)
                (gen-calendar (+ year_id 1) 1 day_id year_limit month_limit day_limit dayname)
                (begin (if (>= dayname 8)
			 (gen-calendar year_id month_id day_id year_limit month_limit day_limit 1)
			 (cons (list day_id dayname) (gen-calendar year_id month_id (+ day_id 1) year_limit month_limit day_limit (+ 1 dayname)))))))))'()))



(define (replace-pairs calendar-list)
  (map (lambda (pair) (if (equal? pair '(1 1)) 1 0))
       calendar-list))

(define (ones-counter list-ones-zeros)
  (fold-right + 0 list-ones-zeros))

(display (ones-counter(replace-pairs (gen-calendar 1901 1 1 2000 12 31 3))))
(newline)
