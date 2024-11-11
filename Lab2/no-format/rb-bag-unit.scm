(include "rb-bag.scm")
(include "testing.scm")

(test-begin "create-rbmset")
(display-and-test (create-rbmset 0) (node-constructor 'black #f 0 #f 1) "Create set with initial value")
(display-and-test (create-rbmset #f) (node-constructor #f #f #f #f 1) "Create empty set")
(test-end "create-rbmset")

(newline)

(test-begin "append-rbmset")
(display-and-test (append-rbmset 1 #f) (node-constructor 'black #f 1 #f 1) "Add to empty set")
(display-and-test (append-rbmset 4 (create-rbmset 2)) (insert 4 (insert 2 #f)) "Add to not empty set")
(display-and-test (append-rbmset-many 5 (create-rbmset 2) 3) (insert 5 (insert 5 (insert 5 (create-rbmset 2)))) "Add  multiple for one operation")
(display-and-test (append-rbmset-many 5 (create-rbmset 2) 0) (create-rbmset 2) "Add  multiple for one operation (Actually zero)")
(test-end "append-rbmset")

(newline)

(test-begin "remove-rbmset")
(let ([t1 (append-rbmset 1 #f)]
	  [ans1 #f]
	  [t2 (append-rbmset 2 (create-rbmset 1))]
	  [ans2 (node-constructor 'black #f 1 #f 1)]
	  [t3 (append-rbmset 2 (create-rbmset 1))]
	  [ans3 (node-constructor 'black #f 2 #f 1)]
	  [t4 (append-rbmset 0 (create-rbmset 1))]
	  [ans4 (node-constructor 'black #f 1 #f 1)]
	  [t5 (append-rbmset 0 (create-rbmset 1))]
	  [ans5 (node-constructor 'black #f 0 #f 1)])
	
	(display (rbmset->list t1))
	(display "\nt1\n")
	(display-and-test (remove-rbmset 1 t1) ans1 "delete root")
	(display (rbmset->list t2))
	(display "\nt2\n")
	(display-and-test (remove-rbmset 2 t2) ans2 "delete right")
	(display (rbmset->list t3))
	(display "\nt3\n")
	(display-and-test (remove-rbmset 1 t3) ans3 "delete root but there is other node")
	(display (rbmset->list t4))
	(display "\nt4\n")
	(display-and-test (remove-rbmset 0 t4) ans4 "delete left")
	(display (rbmset->list t5))
	(display "\nt5\n")
	(display-and-test (remove-rbmset 1 t5) ans5 "delete root but there is other node"))
(test-end "remove-rbmset")

(newline)

(test-begin "union-rbmset")
(let ([e #f]
	  [t1 (append-rbmset 1 (append-rbmset 2 #f))]
	  [t2 (append-rbmset 3 (append-rbmset 4 #f))])

	(display-and-test (union-rbmset e t1) t1 "One E - One Ok")
	(display-and-test (union-rbmset t2 e) t2 "One Ok - One E")
	(display-and-test (union-rbmset e e) e "Both E")
	(display-and-test (rbmset->list (union-rbmset t1 t2)) (rbmset->list (rbmset-fill (rbmset-fill #f (rbmset->list t1)) (rbmset->list t2)))  "Both Ok"))
(test-end "union-rbmset")

(newline)

(define (bigger-zero? value)
	(> value 0))

(define (below-three? value)
	(< value 3))

(test-begin "filter-rbmset")
(let ([t (append-rbmset 0 (append-rbmset 3 (append-rbmset 2 (append-rbmset 5 (append-rbmset 7 (append-rbmset 10 (create-rbmset 6)))))))])
	(display-and-test (rbmset->string (filter-rbmset t bigger-zero?)) " 2  3  5  6  7  10 \n" "Test > 0")
	(display-and-test (rbmset->string (filter-rbmset t below-three?)) " 0  2 \n" "Test < 3")
	(display-and-test (rbmset->string (filter-rbmset t odd?)) " 3  5  7 \n" "Test Odd")
	(display-and-test (rbmset->string (filter-rbmset t even?)) " 0  2  6  10 \n" "Test Even")
	(display-and-test (rbmset->string (filter-rbmset (append-rbmset 2 t) even?)) " 0  2  2  6  10 \n" "Test even with duplicates")
	(display-and-test (rbmset->string (filter-rbmset (append-rbmset 7 t) odd?)) " 3  5  7  7 \n" "Test odd with duplicates (root is doubled)"))
(test-end "filter-rbmset")

(newline)

(define (x2 x)
	(* 2 x))

(define (pow2 x)
	(* x x))

(test-begin "map-rbmset")
(let ([t (append-rbmset 0 (append-rbmset 3 (append-rbmset 2 (append-rbmset 5 (append-rbmset 7 (append-rbmset 10 (create-rbmset 6)))))))])
	(display-and-test (rbmset->string (map-rbmset t x2)) " 0  4  6  10  12  14  20 \n" "Test X*2")
	(display-and-test (rbmset->string (map-rbmset t pow2)) " 0  4  9  25  36  49  100 \n" "Test X^2")
	(display-and-test (rbmset->string (map-rbmset (append-rbmset 3 t) pow2)) " 0  4  9  9  25  36  49  100 \n" "Test X^2 but with duplicate")
	(display-and-test (rbmset->string (map-rbmset (append-rbmset 6 t) x2)) " 0  4  6  10  12  12  14  20 \n" "Test X*2 but with duplicate (root)"))
(test-end "map-rbmset")

(newline)

(test-begin "left-fold-rbmset")

(let ([t (append-rbmset 0 (append-rbmset 3 (append-rbmset 2 (append-rbmset 5 (append-rbmset 7 (append-rbmset 10 (create-rbmset 6)))))))]
	  [t2 #f])
	(display-and-test (left-fold-rbmset + 0 t) 33 "Check fold on sum function")
	(display-and-test (left-fold-rbmset * 1 t) 0 "Check fold on mul (reaction on zero element)")
	(display-and-test (left-fold-rbmset * 1 (remove-rbmset 0 t)) 12600 "Check fold on mul")
	(display-and-test (left-fold-rbmset + 0 t2) (fold + 0 '()) "Check on empty"))
(test-end "left-fold-rbmset")

(newline)

(test-begin "right-fold-rbmset")
(let ([t (append-rbmset 0 (append-rbmset 3 (append-rbmset 2 (append-rbmset 5 (append-rbmset 7 (append-rbmset 10 (create-rbmset 6)))))))]
	  [t2 #f])
	(display-and-test (right-fold-rbmset + 0 t) 33 "Check fold on sum function")
	(display-and-test (right-fold-rbmset * 1 t) 0 "Check fold on mul (reaction on zero element)")
	(display-and-test (right-fold-rbmset * 1 (remove-rbmset 0 t)) 12600 "Check fold on mul")
	(display-and-test (right-fold-rbmset + 0 t2) (fold-right + 0 '()) "Check on empty"))
(test-end "right-fold-rbmset")

(newline)

(test-begin "compare")
(let ([t1 #f]
	  [t2 (create-rbmset 2)]
	  [t3 (insert 1 (create-rbmset 2))]
	  [t4 (insert 1(insert 1 (create-rbmset 2)))])
	(display-and-test (compare t1 t1) #t "Compare two empty")
	(display-and-test (compare t1 t2) #f "Compare one empty and one normal")
	(display-and-test (compare t2 t1) #f "Compare one normal and one empty")
	(display-and-test (compare t2 t3) #f "Compare two different")
	(display-and-test (compare t2 t2) #t "Compare two the same")
	(display-and-test (compare t4 t4) #t "Compare two the same (with duplicates)")
	(display-and-test (compare t2 (create-rbmset 2)) #t "Compare two the same (but second was created again)"))
(test-end "compare")

(newline)
