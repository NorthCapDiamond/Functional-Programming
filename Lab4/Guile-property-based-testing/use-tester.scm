(include "tester.scm")
(include "../../Lab2/no-format/rb-bag.scm")

; Default generator No Shrinking
(define (sum-property a b)
	(eqv? (+ a b) (+ b a)))

(print-property-test-stats(test-template sum-property 2 random (list 100 my-random-state) 100))


(define (append-property a b c)
	(equal? (sort-list (append (append a b) c) >) (sort-list (append (append b c) a) >)))

(print-property-test-stats(test-template append-property 3 random-int-list (list 100 1000) 100) #t #f #f #f)


(define (append-property2 a b c)
	(equal? (sort-list (append (append a b) c) <) (sort-list (append (append b c) a) >)))

(print-property-test-stats(test-template append-property2 3 random-int-list (list 100 1000) 100) #t #f #t #f)

;; Default generator Enable Shrinking

(let ([failed
	(test-template 
		append-property2 
		3 
		random-int-list 
		(list 100 1000) 
		100)])
	
	(print-property-test-stats failed #t #f #t #f )
	(print-property-test-stats (default-shrinker-iterative failed 100 2 8 1000) #t #f #t #f))


; Own generator No shrinking

(define* (custom-generator maxi #:optional (random-state 1000))
	(random (+ (- maxi 10) 10)))

(print-property-test-stats(test-template sum-property 2 custom-generator (list 100) 100))

; Own generator Own shrinking

(define (rb-bag-prop a b c)
	(let ([A (rbmset-fill #f a)]
			  [B (rbmset-fill #f b)]
			  [C (rbmset-fill #f c)])
	(compare (union-rbmset (union-rbmset A B ) C) (union-rbmset (union-rbmset B C) A))))

(define (my-parse-args lst params-to-change)
		(define (my-parse-args-sub lst params-to-change fnl-lst)
			(cond
				[(eqv? params-to-change 0) fnl-lst]
				[else
					(cond
						[(> (car lst) 0) (my-parse-args-sub (cdr lst) (- params-to-change 1) (append fnl-lst (list (floor(/ (car lst) 2)))))]
						[(< (car lst) 0) (my-parse-args-sub (cdr lst) (- params-to-change 1) (append fnl-lst (list (floor(/ (car lst) 2)))))]
						[else (my-parse-args-sub (cdr lst) (- params-to-change 1) (append fnl-lst (list (car lst))))])]))
		(my-parse-args-sub lst params-to-change '()))

(define (lab-shrinker answer limit params-to-change cycle)
	(cond
		[(eq? answer-status #t) answer])
	(cond
		[(eqv? cycle 0) answer]
		[else
			(let ([run-tst (test-template
				(answer-user-property answer)
				(answer-args-amount answer)
				(answer-generator answer)
				(my-parse-args
					(answer-generator-args answer) 
					params-to-change)
				limit)])

				(cond 
					[(and (eq? (answer-status answer) #f) (eq? (answer-status run-tst) #t)) answer]
					[else
						(lab-shrinker run-tst limit params-to-change (- cycle 1))]))]))

(print-property-test-stats(lab-shrinker (test-template rb-bag-prop 3 random-int-list (list 100 100) 10) 100 2 1000) #t #f #f)









