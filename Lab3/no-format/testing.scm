;; Testing module 
(use-modules (srfi srfi-64))

(display "Testing started\n")
(define (display-and-test value2 value1 message)
	(display message)
	(newline)
	(test-equal value1 value2))

(define (display-and-assert value1 message)
	(display message)
	(newline)
	(test-assert value1))



