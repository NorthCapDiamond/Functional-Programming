(include "rb-bag.scm")
(include "testing.scm")

(set! *random-state* (random-state-from-platform))

(define (random-prob)
	(random 100))


(define (element-generator maxi)
  (let ((value (random-prob)))
    (cond 
    	[(< value 95) (random maxi)]
      [else #f ])))

(define (generate-random-list-with-f maxi length)
	(define (sub-random-list lst maxi length)
		(cond
			[(<= length 0) lst]
			[else 
				(let ([x (element-generator maxi)])
					(cond
						[(eq? x #f) (sub-random-list lst maxi (- length 1))]
						[else
							(sub-random-list (cons x lst) maxi (- length 1))]))]))
	(cond
		[(eq? length 0) #f]
		[else (sub-random-list '() maxi (random length))]))


(define (ordered? op? lst)
      (cond [(null? lst) #t]
      			[(eq? lst #f) #t]
            [(eq? (length lst) 1) #t]
      			[(op? (car lst) (car (cdr lst)))
      				(ordered? op? (cdr lst))]
      			[else #f]))

(define (special-sort lst op)
	(cond
		[(eq? lst #f) '()]
		[else (stable-sort lst op)]))


(test-begin "Property Sorting")
(define (property-sorting tests-size maxi length iter)
	(cond
		[(<= tests-size 0) (display "All sorting tests finished\n")]
		[else
			(display-and-assert (ordered? <= (rbmset->list (rbmset-fill #f (generate-random-list-with-f maxi length)))) (string-append "Property test for sorting number " (number->string iter)))
			(property-sorting (- tests-size 1) maxi length (+ iter 1))]))

(property-sorting 1000 1000000 10000 1)
(test-end "Property Sorting")

(newline)

(test-begin "Property test for appending")
(define (property-append tests-size maxi length iter)
	(cond
		[(<= tests-size 0) (display "All appending tests finished\n")]
		[else
			(let ([x (generate-random-list-with-f (random maxi) (random length))])
				(display-and-test (rbmset->list (rbmset-fill #f x)) (special-sort x <=) (string-append "Property test for appending number " (number->string iter)))
				(property-append (- tests-size 1) maxi length (+ iter 1)))]))
(property-append 1000 1000000 10000 1)
(test-end "Property test for appending")		

(newline)

(define (lists-append lst1 lst2)
	(cond
		[(and (eq? lst1 #f) (eq? lst2 #f)) #f]
		[(eq? lst1 #f) lst2]
		[(eq? lst2 #f) lst1]
		[else (append lst1 lst2)]))


(test-begin "Property test for union")
(define (property-union tests-size maxi length iter)
	(cond
		[(<= tests-size 0) (display "All union tests finished\n")]
		[else
			(let ([x1 (generate-random-list-with-f (random maxi) (random length))]
						[y1 (generate-random-list-with-f (random maxi) (random length))]
						[x2 (generate-random-list-with-f (random maxi) (random length))]
						[y2 (generate-random-list-with-f (random maxi) (random length))]
						[x3 (generate-random-list-with-f (random maxi) (random length))]
						[y3 (generate-random-list-with-f (random maxi) (random length))])
				(display-and-test (rbmset->list (union-rbmset (rbmset-fill #f x1) y1)) (special-sort (lists-append x1 y1) <= ) (string-append "Property test for union (bag u list) number " (number->string iter)))
				(display-and-test (rbmset->list (union-rbmset x2 (rbmset-fill #f y2))) (special-sort (lists-append x2 y2) <= ) (string-append "Property test for union (list u bag) number " (number->string iter)))
				(display-and-test (rbmset->list (union-rbmset (rbmset-fill #f x3) (rbmset-fill #f y3))) (special-sort (lists-append x3 y3) <= ) (string-append "Property test for union (bag u bag) number " (number->string iter)))
				(property-union (- tests-size 1) maxi length ( + 1 iter)))]))

(property-union 1000 1000000 10000 1)
(test-end "Property test for union")

(newline)

(test-begin "Properties of a monoid")
(define (property-union tests-size maxi length iter)
	(cond
		[(<= tests-size 0) (display "All properties of a monoid finished\n")]
		[else
			(let ([x (generate-random-list-with-f (random maxi) (random length))]
						[y (generate-random-list-with-f (random maxi) (random length))])
				(let ([left (special-sort (lists-append x y) <= )]
							[right (special-sort (lists-append y x) <= )]
							[midl (rbmset->list (union-rbmset (rbmset-fill #f x) (rbmset-fill #f y)))]
							[midr (rbmset->list (union-rbmset (rbmset-fill #f y) (rbmset-fill #f x)))])
					(display-and-assert (and (equal? left midl) (equal? midl midr) (equal? midr right)) (string-append "Property test: list1-2 == list1 u list2 == list2 u list1 == list2-1 number " (number->string iter)))
					(property-union (- tests-size 1) maxi length (+ 1 iter))))]))

(property-union 1000 1000000 10000 1)
(test-end "Properties of a monoid")

(newline)

(define (remove-once lst elem)
  (cond
    [(null? lst) '()]
    [(equal? (car lst) elem)
     (cdr lst)]
    [else
     (cons (car lst) (remove-once (cdr lst) elem))]))

(define (delete-and-compare rbmset lst delete-list)
	(cond
		[(eq? rbmset #f) (or (eq? rbmset lst)(eq? lst '()))]
		[(or (eq? lst #f) (eq? lst '())) (eq? rbmset lst)]
		[else
			(cond
				[(or (null? delete-list) (eq? delete-list #f)) (equal? (rbmset->list rbmset) (special-sort lst lower))]
				[else
						(let ([x (car delete-list)])
							(delete-and-compare (remove-rbmset x rbmset) (remove-once lst x) (cdr delete-list)))])]))


(test-begin "Property test for delete")
(define (property-delete tests-size maxi length iter)
	(cond
		[(<= tests-size 0) (display "All delete tests finished\n")]
		[else
			(let ([lst (generate-random-list-with-f (random maxi) (random length))]
						[del (generate-random-list-with-f (random maxi) (random length))])
				(display-and-assert (delete-and-compare (rbmset-fill #f lst) lst del) (string-append "Property test for delete number " (number->string iter)))
				(property-delete (- tests-size 1) maxi length (+ iter 1)))]))

(property-delete 500 1000000 1000 1)
(test-end "Property test for delete")

(newline)

(define (random-string length)
  (let* ([characters "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"]
         [char-count (string-length characters)])
    (define (random-char)
      (string-ref characters (random (- char-count 1))))
    (define (generate-string n)
      (if (<= n 0)
          ""
          (string-append (string (random-char)) (generate-string (- n 1)))))
    (generate-string length)))

(define (element-generator-str maxi)
  (let ((value (random-prob)))
    (cond 
    	[(< value 95) (random-string maxi)]
      [else #f ])))


(define (generate-random-list-with-f-str maxi length)
	(define (sub-random-list lst maxi length)
		(cond
			[(<= length 0) lst]
			[else 
				(let ([x (element-generator-str maxi)])
					(cond
						[(eq? x #f) (sub-random-list lst maxi (- length 1))]
						[else
							(sub-random-list (cons x lst) maxi (- length 1))]))]))
	(cond
		[(eq? length 0) #f]
		[else (sub-random-list '() maxi (random length))]))


(test-begin "Property Sorting Strings")
(define (property-sorting-str tests-size maxi length iter)
	(cond
		[(<= tests-size 0) (display "All sorting tests finished\n")]
		[else
			(display-and-assert (ordered? lower (rbmset->list (rbmset-fill #f (generate-random-list-with-f-str maxi length)))) (string-append "Property test for sorting number " (number->string iter)))
			(property-sorting-str (- tests-size 1) maxi length (+ iter 1))]))

(property-sorting-str 500 1000 100 1)
(test-end "Property Sorting Strings")


(newline)

(test-begin "Property test for appending Strings")
(define (property-append-str tests-size maxi length iter)
	(cond
		[(<= tests-size 0) (display "All appending tests finished\n")]
		[else
			(let ([x (generate-random-list-with-f-str (random maxi) (random length))])
				(display-and-test (rbmset->list (rbmset-fill #f x)) (special-sort x lower) (string-append "Property test for appending number " (number->string iter)))
				(property-append-str (- tests-size 1) maxi length (+ iter 1)))]))
(property-append-str 500 1000 100 1)
(test-end "Property test for appending Strings")		

(newline)

(test-begin "Property test for union Strings")
(define (property-union-str tests-size maxi length iter)
	(cond
		[(<= tests-size 0) (display "All union tests finished\n")]
		[else
			(let ([x1 (generate-random-list-with-f-str (random maxi) (random length))]
						[y1 (generate-random-list-with-f-str (random maxi) (random length))]
						[x2 (generate-random-list-with-f-str (random maxi) (random length))]
						[y2 (generate-random-list-with-f-str (random maxi) (random length))]
						[x3 (generate-random-list-with-f-str (random maxi) (random length))]
						[y3 (generate-random-list-with-f-str (random maxi) (random length))])
				(display-and-test (rbmset->list (union-rbmset (rbmset-fill #f x1) y1)) (special-sort (lists-append x1 y1) lower) (string-append "Property test for union (bag u list) number " (number->string iter)))
				(display-and-test (rbmset->list (union-rbmset x2 (rbmset-fill #f y2))) (special-sort (lists-append x2 y2) lower ) (string-append "Property test for union (list u bag) number " (number->string iter)))
				(display-and-test (rbmset->list (union-rbmset (rbmset-fill #f x3) (rbmset-fill #f y3))) (special-sort (lists-append x3 y3) lower ) (string-append "Property test for union (bag u bag) number " (number->string iter)))
				(property-union-str (- tests-size 1) maxi length ( + 1 iter)))]))

(property-union-str 500 1000 100 1)
(test-end "Property test for union Strings")

(newline)

(test-begin "Properties of a monoid Strings")
(define (property-union-str tests-size maxi length iter)
	(cond
		[(<= tests-size 0) (display "All properties of a monoid finished\n")]
		[else
			(let ([x (generate-random-list-with-f-str (random maxi) (random length))]
						[y (generate-random-list-with-f-str (random maxi) (random length))])
				(let ([left (special-sort (lists-append x y) lower )]
							[right (special-sort (lists-append y x) lower )]
							[midl (rbmset->list (union-rbmset (rbmset-fill #f x) (rbmset-fill #f y)))]
							[midr (rbmset->list (union-rbmset (rbmset-fill #f y) (rbmset-fill #f x)))])
					(display-and-assert (and (equal? left midl) (equal? midl midr) (equal? midr right)) (string-append "Property test: list1-2 == list1 u list2 == list2 u list1 == list2-1 number " (number->string iter)))
					(property-union-str (- tests-size 1) maxi length (+ 1 iter))))]))

(property-union-str 500 1000 100 1)
(test-end "Properties of a monoid Strings")

(newline)


(test-begin "Property test for delete Strings")
(define (property-delete-str tests-size maxi length iter)
	(cond
		[(<= tests-size 0) (display "All delete tests finished\n")]
		[else
			(let ([lst (generate-random-list-with-f-str (random maxi) (random length))]
						[del (generate-random-list-with-f-str (random maxi) (random length))])
				(display-and-assert (delete-and-compare (rbmset-fill #f lst) lst del) (string-append "Property test for delete number " (number->string iter)))
				(property-delete-str (- tests-size 1) maxi length (+ iter 1)))]))

(property-delete-str 100 1000 100 1)
(test-end "Property test for delete Strings")

(newline)














