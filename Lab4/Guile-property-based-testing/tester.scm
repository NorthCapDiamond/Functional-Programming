(include "generators.scm")

(use-modules (srfi srfi-9)
			       (srfi srfi-1)
             (srfi srfi-13))

(define-record-type <response>
	(make-response property generator result logs failed passed status)
	response?
	(property get-response-property)
	(generator get-response-generator)
	(result get-response-result)
	(logs get-response-logs)
	(failed get-response-failed)
	(passed get-response-passed)
	(status get-response-status))

(define (to-string obj)
  (cond
    [(string? obj) (string-append obj " ")]
    [(number? obj) (string-append (number->string obj) " ")]
    [(list? obj) (string-append (apply string-append
                          (map to-string obj)) " ")]
    [else (format #f "~a" obj)]))

(define (shrink-number n)
		(cond
			[(> n 0) (- n 1)]
			[(< n 0) (+ n 1)]))


	(define (shrink-list lst)
	  (cond
	    [(null? lst) (list '())]
	    [(and (pair? lst) (null? (cdr lst))) (list '())]
	    [else
	      (let* ([first (car lst)]
	             [rest (cdr lst)]
	             [shrunk-rest (shrink-list rest)])
	        (cons (cdr lst)
	              (map (lambda (shrunk)
	                     (cons first shrunk))
	                   shrunk-rest)))]))

	(define (shrink-string-to-list str)
			(map list->string (shrink-list (string->list str))))


(define* (response-to-string resp #:optional (full-log #f)(failed #f)(passed #f))
	(display 
		(string-append
			"---Answer Record---\n"
			(get-response-result resp)
			"\n"
			(cond
				[(eq? full-log #t) (string-append "Logs:\n" (to-string (get-response-logs resp)) "\n")]
				[else ""])
			(cond
				[(eq? failed #t) (string-append "Failed:\n" (to-string (get-response-failed resp)) "\n")]
				[else ""])
			(cond
				[(eq? passed #t) (string-append "Passed:\n" (to-string (get-response-passed resp))"\n")]
				[else ""])
			"\n")))

(define* (test property generator test-size #:optional (enable-shrinking #f) (shrinker shrink-number) (depth 10))
	(define (test-iteration response property generator test-size enable-shrinking i)
		(cond
			[(eqv? test-size 0) 
				(make-response
					(get-response-property response)
					(get-response-generator response)
					(string-append "Passed " 
						(number->string (length (get-response-passed response)))
						" Out of "
						(number->string ( + (length (get-response-passed response)) 
							(length (get-response-failed response))))
						"\n")
					(get-response-logs response)
					(get-response-failed response)
					(get-response-passed response)
					(cond
						[(eqv? (length (get-response-passed response))
							(+ (length (get-response-failed response)) 
								(length(get-response-passed response)))) #t]
						[else #f]))]
			[else
				(let ([args (generator)])
					(let ([run-tst (cond 
														[(list? args) (apply property args)]
														[else (property args)])])
						(test-iteration
							(make-response
								(get-response-property response)
								(get-response-generator response)
								(get-response-result response)
								(string-append 
									(get-response-logs response)
									"\nTest "
									(number->string i)
									" with data "
									(to-string args)
									" was "
									(cond
										[(eq? run-tst #t) "Passed\n"]
										[else "Failed\n"]))
								(cond
									[(eq? run-tst #f) (append (get-response-failed response) (list args))]
									[else (get-response-failed response)])
								(cond
									[(eq? run-tst #t) (append (get-response-passed response) (list args))]
									[else (get-response-passed response)])
								(get-response-status response))
							property
							generator
							(- test-size 1)
							enable-shrinking
							(+ i 1))))]))

	(define (get-last-element lst)
	  (if (null? lst)
	      (error "The list is empty.")
	      (car (last lst))))

	(define (parse-answer ans resp)
		(define (failed-passed lst ans)
			(cond 
				[(eq? (car lst)) ans]
				[else 
				(failed-passed 
					(cdr lst)
					(cond
						[(eq? (get-last-element (car lst)) #t) (append (list-ref ans 0) (list (car lst)))]
						[else (append (list-ref ans 1) (list (car lst)))]))]))

		(let ([parsed (failed-passed ans (list '() '()))])
			(make-response
				(get-response-property resp)
				(get-response-generator resp)
				(get-response-result resp)
				ans
				(cadr parsed)
				(car parsed)
				(get-response-status resp))))

	(define (run-shrink response property data depth shrinker answer)
		(define (run-from-list prop lst ans)
			(cond
				[(null? lst) ans]
				[else
				(run-from-list prop (cdr lst) (append ans (list (car lst) (cond

					[(list? (car lst)) (apply prop (car lst))]
					[else (prop (car lst))]))))]))

		(cond
			[(eq? depth 0) answer]
			[else
			(let ([datashr (map shrinker data)])
				(run-shrink 
					response 
					property 
					datashr
					(- depth 1) 
					shrinker 
					(append answer (run-from-list property datashr '()))))]))

	(let([after-test (test-iteration 
		(make-response 
			property
			(get-generator generator)
			""
			""
			'()
			'()
			#f)

		property
		(get-generator generator)
		test-size
		enable-shrinking
		1)])

	(cond
		[(and (eq? enable-shrinking #t) 
			    (eq? (get-response-status after-test) #f)) 

		(parse-answer 
			(run-shrink after-test property 
			(get-response-failed after-test) depth shrinker '()) 
			after-test)]

		[else after-test])))






















