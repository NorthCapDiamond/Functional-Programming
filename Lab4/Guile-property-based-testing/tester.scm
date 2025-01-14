(include "generator.scm")

(use-modules (srfi srfi-9)
			 (srfi srfi-1)
             (srfi srfi-13))

(define-record-type <answer>
	(make-answer result logging failed status user-property args-amount generator generator-args)
	answer?
	(result answer-result)
	(logging answer-logging)
	(failed answer-failed)
	(status answer-status)
	(user-property answer-user-property)
	(args-amount answer-args-amount)
	(generator answer-generator)
	(generator-args answer-generator-args))

(define (default-shrinker-args limit shift params-to-change)
	(list limit shift params-to-change))

(define* (print-property-test-stats ans #:optional (res #t) (logg #f) (fail #f) (isok #f))
  (cond 
  	[(answer? ans) 
  	(cond [(or res logg fail) (display "---Answer Record---\n")])
  	(cond [(eq? res #t)
        (display "Result: ")
        (display (answer-result ans))
        (newline)])
  	(cond
        [(eq? logg #t)
        (display "Logging:\n")
        (display (answer-logging ans))
        (newline)])
  	(cond
        [(eq? fail #t)
        (display "Failed:\n")
        (display (answer-failed ans))
        (newline)])
  	(cond
  		[(eq? isok #t)
  		(display "Is passed:\n")
        (display (answer-status ans))
        (newline)])]
  	[else (display "Invalid answer record.")]))

(define (toString obj)
  (cond
    [(string? obj) (string-append obj " ")]
    [(number? obj) (string-append (number->string obj) " ")]
    [(list? obj) (string-append (apply string-append
                          (map toString obj)) " ")]
    [else (format #f "~a" obj)]))

(define (test-template user-property args-amount generator generator-args test-size)
	(define (prepare-args generator generator-args args-amount)
		(define (prepare-args-sub generator generator-args args-amount lst)
			(cond
				[(eqv? args-amount 0) lst]
				[else
					(prepare-args-sub 
						generator 
						generator-args 
						(- args-amount 1) 
						(append lst 
							(list 
								(apply 
									generator 
									generator-args))))]))
		(prepare-args-sub generator generator-args args-amount '()))

	(define* (iter-test user-property args-amount generator generator-args test-size my-answer passed #:optional (i 1))
		(cond
			[(eqv? test-size 0) (list my-answer passed)]
			[else
			(let ([func-args 
				(prepare-args 
					generator 
					generator-args 
					args-amount)])
				(let ([ans (apply user-property func-args)])
					(iter-test 
						user-property
						args-amount
						generator
						generator-args
						(- test-size 1)
						(make-answer
							(answer-result my-answer)
							(string-append
								(answer-logging my-answer)
								"Test number " 
								(number->string i) 
								" Finished with status " 
								(cond
									[(eq? ans #t) "OK"]
									[else "FAIL"])
								"| Test data:"
								(toString func-args)
								"\n")
								(cond
									[(eq? ans #t) (answer-failed my-answer)]
									[else 
										(string-append (answer-failed my-answer) (toString func-args) "\n")])
								(answer-status my-answer)
								(answer-user-property my-answer)
								(answer-args-amount my-answer)
								(answer-generator my-answer)
								(answer-generator-args my-answer))
						(cond
							[(eq? ans #t) (+ 1 passed)]
							[else passed])
						(+ i 1))))]))

	(define (test-loop user-property args-amount generator generator-args test-size my-answer)
		(let ([after-test (iter-test
								   user-property 
								   args-amount 
								   generator 
								   generator-args 
								   test-size 
								   my-answer 
								   0)])
		(make-answer 
			(string-append "Passed " 
				(number->string (cadr after-test))
				" Out of " 
				(number->string test-size))
			(answer-logging (car after-test))
			(answer-failed (car after-test))
			(cond
				[(eqv? test-size (cadr after-test)) #t]
				[else #f])
			(answer-user-property my-answer)
			(answer-args-amount my-answer)
			(answer-generator my-answer)
			(answer-generator-args my-answer))))

	(test-loop user-property args-amount generator generator-args test-size (make-answer "" "" "" #f user-property args-amount generator generator-args)))


(define (parse-args lst params-to-change shift)
		(define (parse-args-sub lst params-to-change fnl-lst shift)
			(cond
				[(eqv? params-to-change 0) fnl-lst]
				[else
					(cond
						[(> (car lst) shift) (parse-args-sub (cdr lst) (- params-to-change 1) (append fnl-lst (list (- (car lst) shift))) shift)]
						[(< (car lst) 0) (parse-args-sub (cdr lst) (- params-to-change 1) (append fnl-lst (list (+ (car lst) shift))) shift)]
						[else (parse-args-sub (cdr lst) (- params-to-change 1) (append fnl-lst (list (car lst))) shift)])]))
		(parse-args-sub lst params-to-change '() shift))

(define (parse-args-iterative lst params-to-change shift)
		(define (parse-args-iterative-sub lst params-to-change fnl-lst shift)
			(cond
				[(eqv? params-to-change 0) fnl-lst]
				[else
					(cond
						[(and (<= (car lst) shift) (>= (car lst) 0)) (parse-args-iterative-sub (cdr lst) (- params-to-change 1) (append fnl-lst (list (car lst))) shift)]
						[(> (car lst) shift) (parse-args-iterative-sub (cdr lst) (- params-to-change 1) (append fnl-lst (list (- (car lst) shift))) shift)]
						[(< (car lst) 0) (parse-args-iterative-sub (cdr lst) (- params-to-change 1) (append fnl-lst (list (+ (car lst) shift))) shift)]
						[else (parse-args-iterative-sub (cdr lst) (- params-to-change 1) (append fnl-lst (list (car lst))) shift)])]))
		(parse-args-iterative-sub lst params-to-change '() shift))


(define (default-shrinker answer limit params-to-change shift cycle)
	; (display (parse-args 
	; 				(answer-generator-args answer) 
	; 				params-to-change
	; 				shift))
	(cond
		[(eq? answer-status #t) answer])
	(cond
		[(eqv? cycle 0) answer]
		[else

			(let ([run-tst (test-template
				(answer-user-property answer)
				(answer-args-amount answer)
				(answer-generator answer)
				(parse-args 
					(answer-generator-args answer) 
					params-to-change
					(random (+ (- shift 1) 1)))
				limit)])

				(cond 
					[(and (eq? (answer-status answer) #f) (eq? (answer-status run-tst) #t)) answer]
					[else
						(default-shrinker run-tst limit params-to-change shift (- cycle 1))]))]))


(define (default-shrinker-iterative answer limit params-to-change shift cycle)
	; (display (parse-args 
	; 				(answer-generator-args answer) 
	; 				params-to-change
	; 				shift))
	(cond
		[(eq? answer-status #t) answer])
	(cond
		[(eqv? cycle 0) answer]
		[else

			(let ([run-tst (test-template
				(answer-user-property answer)
				(answer-args-amount answer)
				(answer-generator answer)
				(parse-args-iterative 
					(answer-generator-args answer) 
					params-to-change
					shift)
				limit)])

				(cond 
					[(and (eq? (answer-status answer) #f) (eq? (answer-status run-tst) #t)) answer]
					[else
						(default-shrinker run-tst limit params-to-change shift (- cycle 1))]))]))





