(define (divider? a b)
  (zero? (modulo a b)))

(define (days year month)
  (if (or (= month 4) (= month 6) (= month 9) (= month 11))
    30
    (if (= month 2)
      (if (and (divider? year 4) (or (not (divider? year 100)) (divider? year 400)))
	29 28) 31)))

(define (solution year_id month_id day_id year_limit month_limit day_limit sunny)
  (if (and (<= year_id 2000) (not(and (= year_id year_limit) (>= month_id month_limit) (>= day_id day_limit))))
    (if (> day_id (days year_id month_id))
	 (solution year_id (+ month_id 1) (- day_id (days year_id month_id)) year_limit month_limit day_limit sunny)
	 (if( > month_id 12)
	   (solution (+ year_id 1) 1 day_id year_limit month_limit day_limit sunny)
	   (if (and(= day_id 1) (> year_id 1900))
	     (solution year_id month_id (+ day_id 7) year_limit month_limit day_limit (+ sunny 1))
	     (solution year_id month_id (+ day_id 7) year_limit month_limit day_limit sunny)))) sunny))
;(display (string-append "Answer is " (number->string(solution 1900 1 0 2000 12 31 0)) "\n"))

