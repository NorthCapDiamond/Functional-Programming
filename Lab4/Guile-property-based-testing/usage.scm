(include "tester.scm")
(include "../Functional-Programming/Lab2/no-format/rb-bag.scm")

; sum property
; Success. Default generator. No Shrinking
(define (sum-property a b)
	(eqv? (+ a b) (+ b a)))

(let ([int (random-int-gen 0 100)])
	(let([ints (combined-generator (list int int))])
		(response-to-string 
			(test sum-property (make-struct-generator ints now-random-state) 10) 
			#t #t #t)))

; sum property
; Fail. Default generator. No Shrinking
(define (sum-fail-property a b)
	(eqv? (+ a b) (+ b a 1)))

(let ([int (random-int-gen 0 100)])
	(let([ints (combined-generator (list int int))])
		(response-to-string 
			(test sum-fail-property (make-struct-generator ints now-random-state) 10) 
			#t #t #t)))

; Check if value is < 10
; Example of fail. Default generator. No Shrinking
(define (number-prop a)
	(< a 10))

(response-to-string
	(test number-prop (make-struct-generator (random-int-gen 0 20) now-random-state) 10) #t #t #t)

; Combination of generators (sum property) | int -> (int int) | No shrinking
(let ([int (random-int-gen 0 100)])
	(let([ints (combined-generator (list int int))])
		(response-to-string 
			(test sum-property (make-struct-generator ints now-random-state) 10) 
			#t #t #t)))

; Merge generators | int -> int/float -> (int/float int/float) | No Shrinking
(let ([int (random-int-gen 0 100)]
	  [float (random-float-gen 0.2 9.9)])
	(let ([merged-generator (merge-generators(list int float))])
		(let ([combined-generator (combined-generator(list merged-generator int))])
			(response-to-string 
				(test sum-property (make-struct-generator combined-generator combined-generator) 10) 
				#t #t #t))))

; Own generator. No Shrinking
(define* (custom-generator maxi #:optional (random-state 1000))
	(lambda ()
		(random (+ (- maxi 10) 10))))

(response-to-string
	(test number-prop (make-struct-generator (custom-generator 20 now-random-state) now-random-state) 10) #t #t #t)


; Check if value is < 10
; Example of fail. Default generator Shrinking enabled
(response-to-string
	(test number-prop (make-struct-generator (random-int-gen 0 20) now-random-state) 10 #t shrink-number 10) #t #t #t)


; Combination of generators (sum property) | int -> (int int) | No shrinking
(let ([int (random-int-gen 0 100)])
	(let([ints (combined-generator (list int int))])
		(response-to-string 
			(test sum-property (make-struct-generator ints now-random-state) 10 #t shrink-list 1000) 
			#t #t #t)))

; Merge generators | int -> int/float -> (int/float int/float) | Shrinking enabled
(let ([int (random-int-gen 0 100)]
	  [float (random-float-gen 0.2 9.9)])
	(let ([merged-generator (merge-generators(list int float))])
		(let ([combined-generator (combined-generator(list merged-generator int))])
			(response-to-string 
				(test sum-property (make-struct-generator combined-generator combined-generator) 10 #t shrink-list 1000) 
				#t #t #t))))












