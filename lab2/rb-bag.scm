(use-modules (srfi srfi-9)
			 (srfi srfi-1)
             (srfi srfi-13))

(use-modules (ice-9 format))


(define-record-type <node>
	(make-node color left value right count)
	node?
	(color node-color)
	(left  node-left)
	(value node-value)
	(right node-right)
	(count node-count))

(define (node-constructor color left value right count)
	(cond [(or (eq? color #f) (eq? value #f)) #f]
		   [else (make-node color left value right count)]))


(define (node->color node)
	(cond
		[(node? node) (node-color node)]
		[else #f]))


(define (node->left node)
	(cond
		[(node? node) (node-left node)]
		[else #f]))


(define (node->value node)
	(cond
		[(node? node) (node-value node)]
		[else #f]))


(define (node->right node)
	(cond
		[(node? node) (node-right node)]
		[else #f]))

(define (node->count node)
	(cond
		[(node? node) (node-count node)]
		[else #f]))


(define (bool->string flag)
 (if flag "yes\n" "no\n"))


(define (node->string node)
 (define (sub-node->string node)
	(if (not (node? node))
		"nil" 
		(string-append 
			"(Color : " 
			(symbol->string (node->color node)) 

			" | Value : " 
			(number->string (node->value node))

			" | Count : "
			(number->string (node->count node)) 

			" | Left : " 
			(if (node->left node)
					(sub-node->string (node->left node))
					"nil") 

			" | Right : " 
			(if (node->right node)
				(sub-node->string (node->right node)) 
				"nil") 

			")")))
 (string-append (sub-node->string node) "\n"))


(define (member? x node)
	(cond 
	 [(node? node) 
		(let ([y (node->value node)])
			(cond 
				 [(< x y) (member? x (node->left node))] 
				 [(eq? x y) #t] 
				 [else (member? x (node->right node))]))]
	 [else #f]))

(define (tree-count x node)
	(cond 
	 [(node? node) 
		(let ([y (node->value node)])
			(cond 
				 [(< x y) (tree-count x (node->left node))] 
				 [(eq? x y) (node->count node)] 
				 [else (tree-count x (node->right node))]))]
	 [else #f]))


(define (recolor-black node)
	(cond [(eq? node #f) #f])
	(node-constructor 'black 
						 (node->left node) 
						 (node->value node) 
						 (node->right node)
						 (node->count node)))


(define (balance node)
 (let ([mode-this node]
			 [left (node->left node)]
			 [right (node->right node)]
			 [left-left (node->left (node->left node))]
			 [left-right (node->right (node->left node))]
			 [right-right (node->right (node->right node))]
			 [right-left (node->left (node->right node))])
	(cond 
		[(and 
			 (eq? 
				 (node->color mode-this) 
				 'black) 
			 (eq? 
				 (node->color left) 
				 'red) 
			 (eq? 
				 (node->color left-left) 
				 'red)
			 (eq? right-right #f)
			 (eq? right-left #f)
			 (eq? (node->left left-right) #f)
			 (eq? (node->right left-right) #f))
		 (node-constructor 'red
								(recolor-black left-left)
								(node->value left)
								(node-constructor 'black 
													 left-right 
													 (node->value mode-this) 
													 right
													 (node->count mode-this))
								(node->count left))]

	 [(and 
			(eq? 
				(node->color mode-this) 
				'black)
			(eq? 
				(node->color left) 
				'red)
			(eq? 
				(node->color left-right) 
				'red))
		(node-constructor 'red 
							 (node-constructor
							 	'black
							 	left-left
							 	(node->value left)
							 	(node->left left-right)
							 	(node->count left))
							 (node->value left-right) 
							 (node-constructor 'black 
										(node->right left-right)
										(node->value mode-this) 
										right
										(node->count mode-this))
							 (node->count left-right))]

	 [(and 
			(eq? 
				(node->color mode-this) 
				'black)
			(eq? 
				(node->color right) 
				'red)
			(eq? 
				(node->color right-left) 
				'red))
		(node-constructor 'red
			       (node-constructor
			       	'black
			       	left
			       	(node->value mode-this)
			       	(node->left right-left)
			       	(node->count mode-this))
			       (node->value right-left)
			       (node-constructor
			       	'black
			       	(node->right right-left)
			       	(node->value right)
			       	right-right
			       	(node->count right))
			       (node->count right-left))]

	 [(and 
			(eq? 
				(node->color mode-this)
				'black)
			(eq? 
				(node->color right) 
				'red)
			(eq? 
				(node->color right-right) 
				'red))
		(node-constructor 'red
			       (node-constructor
			       	'black
			       	left
			       	(node->value mode-this)
			       	right-left
			       	(node->count mode-this))
			       (node->value right)
			       (recolor-black right-right)
			       (node->count right))]
	 (else mode-this))))


(define (insert x node)
 (define (inner-insert x node)
	(cond
	 [(not (node? node))
		 (node-constructor 'red #f x #f 1)]
	 [(< x (node->value node)) 
		 (balance (node-constructor 
								(node->color node) 
								(inner-insert x (node->left node)) 
								(node->value node) 
								(node->right node)
								(node->count node)))]
	 [(= x (node->value node))
		 (node-constructor (node->color node) 
								(node->left node) 
								(node->value node) 
								(node->right node)
								(+ (node->count node) 1))]
	 [(> x (node->value node))
		 (balance (node-constructor 
								(node->color node) 
								(node->left node) 
								(node->value node) 
								(inner-insert x (node->right node))
								(node->count node)))]))
 (recolor-black (inner-insert x node)))



(define (delete x node)
	(define (inner-delete x node)
		(let ([y (node->value node)])
			(cond
				[(eq? y #f) #f]
				[(< x y)
					(delL x node)]
				[(> x y)
					(delR x node)]
				[else
					(cond 
						[(> (node->count node) 1)
							(node-constructor 
								(node->color node)
								(node->left node)
								(node->value node)
								(node->right node)
								(- (node->count node) 1))]
						[else
							(let ([fused
									(fuse (node->left node) (node->right node))])

							(cond [(eq? (node->value fused) #f) #f]
								  [else fused]))])])))
	(recolor-black (inner-delete x node)))


(define (balL node)
	(let ([mode-this node]
			[left (node->left node)]
			[right (node->right node)])
	(cond
		[(and (eq? (node->color mode-this) 'black)
				(eq? (node->color left) 'red))

		(node-constructor 
			'red
			(node-constructor 
				'black 
				(node->left left) 
				(node->value left) 
				(node->right left)
				(node->count left))
			(node->value mode-this) 
			(node->right mode-this)
			(node->count mode-this))]

		[(and (eq? (node->color mode-this) 'black)
				(eq? (node->color right) 'black))

		(balance (node-constructor
			'black
			(node->left mode-this)
			(node->value mode-this)

			(node-constructor
				'red 
				(node->left right)
				(node->value right)
				(node-right right)
				(node->count right))
			(node->count mode-this)))] 


		[(and (eq? (node->color mode-this) 'black)
			  (eq? (node->color right) 'red)
			  (eq? (node->color (node->right right)) 'black)
			  (eq? (node->color (node->left right)) 'black))


		(node-constructor
			'red
			(node-constructor
				'black
				(node->left mode-this)
				(node->value mode-this)
				(node->left 
					(node->left right))
				(node->count mode-this))

			(node->value 
				(node->left right))

			(balance
				(node-constructor
					'black 
					(node->right 
						(node->left right))
					(node->value right)
					(node-constructor
						'red
						(node->left
							(node->right right))
						(node->value 
							(node->right right))
						(node->right
							(node->right right))
						(node->count
							(node->right right)))
					(node->count right)))
			(node->count (node->left right)))]
		[else
			mode-this])))



(define (balR node)
	(let ([mode-this node]
			[left (node->left node)]
			[right (node->right node)])
	(cond
		[(and (eq? (node->color mode-this) 'black)
				(eq? (node->color right) 'red))

		(node-constructor 
			'red 
			(node->left mode-this)
			(node->value mode-this) 
			(node-constructor 
				'black 
				(node->left right) 
				(node->value right) 
				(node->right right)
				(node->count right))
			(node->count mode-this))]

		[(and (eq? (node->color mode-this) 'black)
				(eq? (node->color left) 'black))

		(balance (node-constructor
			'black
			(node-constructor
				'red 
				(node->left left)
				(node->value left)
				(node->right left)
				(node->count left))

			(node->value mode-this)
			(node->right mode-this)
			(node->count mode-this)))] 


		[(and (eq? (node->color mode-this) 'black)
				(eq? (node->color left) 'red)
				(eq? (node->color (node->right left)) 'black)
				(eq? (node->color (node->left left)) 'black))

		(node-constructor
			'red
			(balance (node-constructor
				'black
				(node-constructor
					'red
					(node->left (node->left left))
					(node->value (node->left left))
					(node->right (node->left left))
					(node->count (node->left left)))
				(node->value left)
				(node->left (node->right left))
				(node->count left)))
			(node->value (node->right left))
			(node-constructor
			    'black
			    (node->right (node->right left))
			    (node->value mode-this)
			    (node->right mode-this)
			    (node->count mode-this))
			(node->count (node->right left)))]
		[else
			mode-this])))


(define (delL x node)
	(cond [(eq? node? node) #f
		#f])
	(let ([mode-this node]
			[left (node->left node)]
			[right (node->right node)])

	(cond [(eq? (node? left) #f)
		mode-this]
		  [else
			(cond
				[(eq? (node->color mode-this) 'red)
				(node-constructor
					'red
					(delete x left)
					(node->value mode-this)
					right
					(node->count mode-this))]


				[(eq? (node->color mode-this) 'black)
				(balL 
					(node-constructor 
						'black
						(let([after-delete (delete x left)])
							(cond [(eq? (node->value after-delete) #f) #f]
								  [else after-delete]))
						(node->value mode-this)
						right
						(node->count mode-this)))]
				[else mode-this])])))


(define (delR x node)
	(cond [(eq? node? node) #f
		#f])
	(let ([mode-this node]
			[left (node->left node)]
			[right (node->right node)])

	(cond [(eq? (node? right) #f)
		mode-this]
		  [else
			(cond
				[(eq? (node->color mode-this) 'red)
				(node-constructor
					'red
					left
					(node->value mode-this)
					(delete x right)
					(node->count mode-this))]

				[(eq? (node->color mode-this)'black) 
				(balR
					(node-constructor
						'black
						left
						(node->value mode-this)
						(let([after-delete (delete x right)])
							(cond [(eq? (node->value after-delete) #f) #f]
								  [else after-delete]))
						(node->count mode-this)))])])))


(define (fuse tree1 tree2)
	(cond
		[(and (eq? (node? tree1) #f) (node? tree2))
			tree2]

		[(and (node? tree1) (eq? (node? tree2) #f))
			tree1]

		[(and (eq? (node? tree1) #f) (eq? (node? tree2) #f))
			#f]
		[else
			(cond
				[(and (eq? (node->color tree1) 'black)
					  (eq? (node->color tree2) 'red))
				(node-constructor
					'red
					(fuse 
						tree1
						(node->left tree2))
					(node->value tree2)
					(node->right tree2)
					(node->count tree2))]

				[(and (eq? (node->color tree1) 'red)
					  (eq? (node->color tree2) 'black))

				(node-constructor
					'red
					(node->left tree1)
					(node->value tree1)
					(fuse 
						(node->right tree1)
						tree2)
					(node->count tree1))]


				[(and (eq? (node->color tree1) 'red)
					  (eq? (node->color tree2) 'red))

				(let ([s (fuse (node->right tree1) (node->left tree2))])
				(cond
					[(eq? s #f)
						(node-constructor 'red (node->left tree1) (node->value tree1) (node-constructor 'red #f (node->value tree2) (node->right tree2) (node->count tree2)) (node->count tree1))]
					[(eq? (node->color s) 'red)
					(node-constructor
						'red
						(node-constructor
							'red
							(node->left tree1)
							(node->value tree1)
							(node->left s)
							(node->count tree1))
						(node->value s)
						(node-constructor
							'red
							(node->right s)
							(node->value tree2)
							(node->right tree2)
							(node->count tree2))
						(node->count s))]

					[(eq? (node->color s) 'black)
					(node-constructor
						'red
						(node->left tree1)
						(node->value tree1)
						(node-constructor
							'red
							s
							(node->value tree2)
							(node->right tree2)
							(node->count tree2))
						(node->count tree1))]
					[else #f]))]
					

				[(and (eq? (node->color tree1) 'black)
							(eq? (node->color tree2) 'black))

				(let ([s (fuse (node->right tree1) 
											 (node->left tree2))])

				(cond
					[(eq? s #f)
						(node-constructor 'black (node->left tree1) (node->value tree1) (node-constructor 'black #f (node->value tree2) (node->right tree2) (node->count tree2)) (node->count tree1))]
					[(eq? (node->color s) 'red)
					(node-constructor
						'red
						(node-constructor
							'black
							(node->left tree1)
							(node->value tree1)
							(node->left s)
							(node->count tree1))
						(node->value s)
						(node-constructor
							'black
							(node->right s)
							(node->value tree2)
							(node->right tree2)
							(node->count tree2))
						(node->count s))]

					[(eq? (node->color s) 'black)
					(balL
						(node-constructor
							'black
							(node->left tree1)
							(node->value tree1)
							(node-constructor
								'black
								s
								(node->value tree2)
								(node->right tree2)
								(node->count tree2))
							(node->count tree1)))]
					[else #f]))]
				[else #f])]))

(define (append-string str n)
	(cond [(= n 1) str]
		   [else
		   (if (<= n 0)
		   	""
		   	(string-append (string-append str "  ") (append-string str (- n 1))))]))  

(define (rbmset->string rbmset)
  (define (sub-rbmset->string rbmset amount)
    (if (node? rbmset)
        (let ([left (node->left rbmset)]
               [right (node->right rbmset)]
               [value (number->string (node->value rbmset))])
          (string-append 
           (sub-rbmset->string left (node->count left)) 
           " " 
           (append-string value amount)
           " " 
           (sub-rbmset->string right (node->count right)))
        )
        ""))
  
  (string-append (sub-rbmset->string rbmset (node->count rbmset)) "\n"))


(define (create-rbmset value)
	(cond [(eq? value #f) #f]
		  [else (insert value #f)]))

(define (append-rbmset value rbmset)
	(cond [(eq? value #f) rbmset]
		  [else (insert value rbmset)]))

(define (append-rbmset-many value rbmset amount)
	(cond [(or (eq? value #f) (< amount 1)) rbmset]
		  [else (append-rbmset-many value (insert value rbmset) (- amount 1))]))

(define (remove-rbmset value rbmset)
	(cond [(eq? value #f) rbmset]
		  [else (delete value rbmset)]))

(define (union-rbmset rbmset1 rbmset2)
	(cond [(and (eq? rbmset1 #f) (eq? rbmset2 #f)) #f]
		  [(and (not (eq? rbmset1 #f)) (eq? rbmset2 #f)) rbmset1]
		  [(and (eq? rbmset1 #f) (not (eq? rbmset2 #f))) rbmset2]
		  [else (fuse rbmset1 rbmset2)]))

(define (filter-rbmset rbmset pred?)
	(define (sub-filter-rbmset newrmbset rbmset pred?)
		(let ([l (eq? (node? (node->left rbmset)) #t)]
			  [r (eq? (node? (node->right rbmset)) #t)]
			  [left (node->left rbmset)]
			  [right (node->right rbmset)]
			  [value (node->value rbmset)])
			(cond 
				[(and l r)
					(sub-filter-rbmset (cond [(eq? (pred? value) #t) (append-rbmset-many value (sub-filter-rbmset newrmbset left pred?) (node->count rbmset))] [else (sub-filter-rbmset newrmbset left pred?)]) right pred?)]
				[(and r (not l))
					(sub-filter-rbmset (cond [(eq? (pred? value) #t) (append-rbmset-many value newrmbset (node->count rbmset))] [else newrmbset]) right pred?)]
				[(and l (not r))
					(cond [(eq? (pred? value) #t) (append-rbmset-many value (sub-filter-rbmset newrmbset left pred?) (node->count rbmset) (node->count rbmset))][else (sub-filter-rbmset newrmbset left pred?)])]
				[(not (and l r))
					(cond [(eq? (pred? value) #t) (append-rbmset-many value newrmbset (node->count rbmset))] [else newrmbset])])))
	(sub-filter-rbmset #f rbmset pred?))


(define (cons-n-times element lst n)
  (if (<= n 0)
      lst
      (cons element (cons-n-times element lst (- n 1)))))


(define (rbmset->list rbmset)
	(define (sub-rbmset->list rbmset newlist)
		(let ([l (eq? (node? (node->left rbmset)) #t)]
			  [r (eq? (node? (node->right rbmset)) #t)]
			  [left (node->left rbmset)]
			  [right (node->right rbmset)]
			  [value (node->value rbmset)])
			(cond 
				[(and l r)
					(sub-rbmset->list left (cons-n-times value (sub-rbmset->list right newlist) (node->count rbmset)))]
				[(and l (not r))
					(sub-rbmset->list left (cons-n-times value newlist (node->count rbmset)))]
				[(and r (not l))
					(cons-n-times value (sub-rbmset->list right newlist) (node->count rbmset))]
				[(not (and l r))
					(cons-n-times value newlist (node->count rbmset))])))
	(cond [(eq? rbmset #f) '()]
		  [else
		  	(sub-rbmset->list rbmset '())]))


(define (map-rbmset rbmset f)
	(define (sub-map-rbmset newrmbset rbmset f)
		(let ([l (eq? (node? (node->left rbmset)) #t)]
			  [r (eq? (node? (node->right rbmset)) #t)]
			  [left (node->left rbmset)]
			  [right (node->right rbmset)]
			  [value (node->value rbmset)])
			(cond 
				[(and l r)
					(sub-map-rbmset (append-rbmset-many (f value) (sub-map-rbmset newrmbset left f) (node->count rbmset)) right f)]
				[(and r (not l))
					(sub-map-rbmset (append-rbmset-many (f value) newrmbset (node->count rbmset)) right f)]
				[(and l (not r))
					(append-rbmset-many (f value) (sub-map-rbmset newrmbset left f) (node->count rbmset))]
				[(not (and l r))
					(append-rbmset-many (f value) newrmbset (node->count rbmset))])))
	(sub-map-rbmset #f rbmset f))

(define (left-fold-rbmset f acc lst)
	(fold f acc (rbmset->list lst)))

(define (right-fold-rbmset f acc lst)
	(fold-right f acc (rbmset->list lst)))


(define (hash-numbers lst)
  (let ((list-string (format #f "~s" lst)))
    (hash list-string 1000000000000000)))

(define (compare rbmset1 rbmset2)
	(let ([list1 (rbmset->list rbmset1)]
		  [list2 (rbmset->list rbmset2)])
		(eq? (hash-numbers list1) (hash-numbers list2))))



;; Testing module 

(use-modules (srfi srfi-64))


(display "Testing started\n")
(define (display-and-test value2 value1 message)
	(display message)
	(newline)
	(test-equal value1 value2))


;; Testing set module

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
	(display-and-test (union-rbmset t1 t2) (fuse t1 t2) "Both Ok"))
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

;; Testing rb module

(test-begin "node-constructor")
(let ([no-children (node-constructor 'black #f 0 #f 1)]
	  [left-child (node-constructor 'red (node-constructor 'black #f 1 #f 1) 2 #f 1)]
	  [right-child (node-constructor 'black #f 2 (node-constructor 'red #f 3 #f 1) 1)]
	  [both-children (node-constructor 'red (node-constructor 'black #f 4 #f 1) 2 (node-constructor 'red #f 0 #f 1) 1)])

;; no-children
(display-and-test (node->color no-children) 'black "Check color for no-children case")
(display-and-test (node->value no-children) 0 "Check value for no-children case")
(display-and-test (node->left no-children) #f "Check left child for no-node case")
(display-and-test (node->right no-children) #f "Check right child for no-node case")

;; left-child
(display-and-test (node->color left-child ) 'red "Check color for left-child case")
(display-and-test (node->value left-child ) 2 "Check value for left-child case")
(display-and-test (node->value (node->left left-child)) 1 "Check left child value for left-child case")
(display-and-test (node->color (node->left left-child)) 'black "Check left child color for left-child case")
(display-and-test (node->right left-child ) #f "Check right child for left-child case")

;; right-child
(display-and-test (node->color right-child) 'black "Check color for right-child case")
(display-and-test (node->value right-child) 2 "Check value for right-child case")
(display-and-test (node->value (node->right right-child)) 3 "Check right child value for right-child case")
(display-and-test (node->color (node->right right-child)) 'red "Check right child for right-child case")
(display-and-test (node->left right-child ) #f "Check left child for right-child case")

;; both-children
(display-and-test (node->color both-children) 'red "Check color for both-children case")
(display-and-test (node->value both-children) 2 "Check value for both-children case")
(display-and-test (node->value (node->left both-children)) 4 "Check left child value for both-children case")
(display-and-test (node->color (node->left both-children)) 'black "Check left child color for both-children case")
(display-and-test (node->value (node->right both-children)) 0 "Check right child value for both-children case")
(display-and-test (node->color (node->right both-children)) 'red "Check right child for both-children case"))
(test-end "node-constructor")

(newline)

(test-begin "member?")
(let([tree (node-constructor
				'black
				(node-constructor
					'red
					(node-constructor 'black #f 1 #f 1)
					3
					(node-constructor 'red #f 4 #f 1)
					1)
				7
				(node-constructor 
					'red 
					(node-constructor 'black #f 8 #f 1)
					10
					(node-constructor 'red #f 20 #f 1) 
					1)
				1)])

(display-and-test (member? 3 tree) #t "3 must be in tree")
(display-and-test (member? 1 tree) #t "1 must be in tree")
(display-and-test (member? 4 tree) #t "4 must be in tree")
(display-and-test (member? 8 tree) #t "8 must be in tree")
(display-and-test (member? 7 tree) #t "7 must be in tree")
(display-and-test (member? 10 tree) #t "10 must be in tree")
(display-and-test (member? 20 tree) #t "20 must be in tree")
(display-and-test (member? 100 tree) #f "100 is not in tree")
(display-and-test (member? 0 tree) #f "0 is not in tree")
(display-and-test (member? 6 tree) #f "6 is not in tree"))
(test-end "member?")

(newline)

(test-begin "recolor-black")
(let([tree1 (node-constructor 'red #f 1 #f 1)]
	 [tree2 (node-constructor 'black #f 1 #f 1)])

(display-and-test (node->color (recolor-black tree1)) 'black "Red must be replaced with Black")
(display-and-test (node->color (recolor-black tree2)) 'black "Black must stay Black"))
(test-end "recolor-black")

(newline)

(test-begin "balance")
(let ([a (node-constructor 'black #f 1 #f 1)]
	  [b (node-constructor 'black #f 2 #f 1)]
	  [c (node-constructor 'black #f 3 #f 1)]
	  [d (node-constructor 'black #f 4 #f 1)]
	  [x 5]
	  [y 6]
	  [z 7])

	(let(
	  [t1 (node-constructor 'black (node-constructor 'red (node-constructor 'red a x b 1) y c 1) z d 1)]
	  [t2 (node-constructor 'black (node-constructor 'red a x (node-constructor 'red b y c 1) 1) z d 1)]
	  [t3 (node-constructor 'black a x (node-constructor 'red (node-constructor 'red b y c 1) z d 1) 1)]
	  [t4 (node-constructor 'black a x (node-constructor 'red b y (node-constructor 'red c z d 1) 1) 1)]
	  [t5 (node-constructor 'black #f 0 #f 1)]
	  [t6 (node-constructor 'red #f 0 #f 1)]
	  [ans (node-constructor 'red (node-constructor 'black a x b 1) y (node-constructor 'black c z d 1) 1)])

(display-and-test (balance t1) ans "Case1")
(display-and-test (balance t2) ans "Case2")
(display-and-test (balance t3) ans "Case3")
(display-and-test (balance t4) ans "Case4")
(display-and-test (balance t5) t5 "Case5")
(display-and-test (balance t6) t6 "Case6")))
(test-end "balance")
(newline)

(test-begin "insert")

(let ([tree (node-constructor 'black #f 7 #f 1)])
	(let ([t0 (insert 10 tree)]
		  [t1 (insert 5 tree)]
		  [t2 (insert 5 (insert 0 tree))]
		  [t3 (insert 1 (insert 3 tree))]
		  [t4 (insert 11 (insert 10 (insert 9 tree)))]
		  [t5 (insert 2 (insert 2 (insert 2 (insert 7 tree))))])

	(display-and-test (node->value t1) 7 "Case1: Check root")
	(display-and-test (node->value (node->right t0)) 10 "Case1: add right")
	(display-and-test (node->value (node->left t1)) 5 "Case1: add left")

	(display-and-test (node->value t2) 5 "Case2: replace root")
	(display-and-test (node->value (node->left t2)) 0 "Case2: add left")
	(display-and-test (node->value (node->right t2)) 7 "Case2: add right")

	(display-and-test (node->value t3) 3 "Case3: replace root")
	(display-and-test (node->value (node->left t3)) 1 "Case3: add left")
	(display-and-test (node->value (node->right t3)) 7 "Case3: add right")

	(display-and-test (node->color t4) 'black "Case4: check root color")
	(display-and-test (node->color (node->left t4)) 'black "Case4: check left color")
	(display-and-test (node->color (node->right t4)) 'black "Case4: check right color")
	(display-and-test (node->color (node->right (node->right t4))) 'red "Case4: check right-right color")
	(display-and-test (node->value (node->left t4)) 7 "Case4: add left")
	(display-and-test (node->value t4) 9 "Case4: add root")
	(display-and-test (node->value (node->right t4)) 10 "Case4: add right")
	(display-and-test (node->value (node->right (node->right t4))) 11 "Case4: add right-right")

	(display-and-test (tree-count 2 t5) 3 "Case5: Check amount of 2's")
	(display-and-test (tree-count 7 t5) 2 "Case5: Check amount of 7's")))
(test-end "insert")

(newline)

(test-begin "balL")
(let ([t1 (node-constructor 'black #f 1 #f 1)]
	  [t2 (node-constructor 'black #f 2 #f 1)]
	  [t3 (node-constructor 'black #f 3 #f 1)]
	  [x 4]
	  [y 4]
	  [z 5]
	  [u 6]
	  [l #f]
	  [r #f]
	  [v 7])
	(let (
		  [ex1 (node-constructor 'black (node-constructor 'red t1 x t2 1) y t3 1)]
		  [ans1 (node-constructor 'red (node-constructor 'black t1 x t2 1) y t3 1)]
		  [ex2 (node-constructor 'black t1 y (node-constructor 'black t2 z t3 1) 1)]
		  [ans2 (balance (node-constructor 'black t1 y (node-constructor 'red t2 z t3 1) 1))]
		  [ex3 (node-constructor 'black t1 y (node-constructor 'red (node-constructor 'black t2 u t3 1) z (node-constructor 'black l v r 1) 1) 1)]
		  [ans3 (node-constructor 'red (node-constructor 'black t1 y t2 1) u (balance (node-constructor 'black t3 z (node-constructor 'red l v r 1) 1)) 1)])
	(display-and-test (balL ex1) ans1 "Case1")
	(display-and-test (balL ex2) ans2 "Case2")
	(display-and-test (balL ex3) ans3 "Case3")))
(test-end "balL")

(newline)

(test-begin "balR")
(let ([t1 (node-constructor 'black #f 1 #f 1)]
	  [t2 (node-constructor 'black #f 2 #f 1)]
	  [t3 (node-constructor 'black #f 3 #f 1)]
	  [t4 (node-constructor 'black #f 4 #f 1)]
	  [x 5]
	  [y 6]
	  [z 7]
	  [u 8]
	  [l #f]
	  [r #f]
	  [v 9])
	(let (
		  [ex1 (node-constructor 'black t1 y (node-constructor 'red t2 x t3 1) 1)]
		  [ans1 (node-constructor 'red t1 y (node-constructor 'black t2 x t3 1) 1)]
		  [ex2 (node-constructor 'black (node-constructor 'black t1 z t2 1) y t3 1)]
		  [ans2 (balance (node-constructor 'black (node-constructor 'red t1 z t2 1) y t3 1))]
		  [ex3 (node-constructor 'black (node-constructor 'red (node-constructor 'black l v r 1) z (node-constructor 'black t2 u t3 1) 1) y t4 1)]
		  [ans3 (node-constructor 'red (balance (node-constructor 'black (node-constructor 'red l v r 1) z t2 1)) u (node-constructor 'black t3 y t4 1) 1)])
	(display-and-test (balR ex1) ans1 "Case1")
	(display-and-test (balR ex2) ans2 "Case2")
	(display-and-test (balR ex3) ans3 "Case3")))
(test-end "balR")

(newline)

(define (fuse-helper arg1 arg2 case1 case2 message)
	(let ([ans (fuse arg1 arg2)])
		(cond ((eq? ans case1)
			(display-and-test ans case1 message))
		(else
			(display-and-test ans case2 message)))))


(test-begin "fuse")
(let ([t1 (node-constructor 'black #f 1 #f 1)]
	  [t2 (node-constructor 'black #f 2 #f 1)]
	  [t3 (node-constructor 'black #f 3 #f 1)]
	  [t4 (node-constructor 'black #f 4 #f 1)]
	  [x 5]
	  [y 6]
	  [z 7])
    (let ([s (fuse t2 t3)])
		(let ([s1 (node->left s)]
			  [s2 (node->right s)])
		(display-and-test (fuse t1 (node-constructor 'red t3 y t4 1)) (node-constructor 'red (fuse t1 t3) y t4 1) "Case1")
		(display-and-test (fuse (node-constructor 'red t1 x t2 1) t3 ) (node-constructor 'red t1 x (fuse t2 t3) 1) "Case2")
		(fuse-helper (node-constructor 'red t1 x t2 1) 
			         (node-constructor 'red t3 y t4 1) 
			         (node-constructor 'red (node-constructor 'red t1 x s1 1) z (node-constructor 'red s2 y t4 1) 1)
			         (node-constructor 'red t1 x (node-constructor 'red s y t4 1) 1) "Case3")
		(fuse-helper (node-constructor 'black t1 x t2 1)
			         (node-constructor 'black t3 y t4 1)
			         (node-constructor 'red (node-constructor 'black t1 x s1 1) z (node-constructor 'black s2 y t4 1) 1)
			         (balL (node-constructor 'black t1 x (node-constructor 'black s y t4 1) 1)) "Case4")
		(display-and-test (fuse t1 #f) t1 "Case5")
		(display-and-test (fuse #f t1) t1 "Case6"))))
(test-end "fuse")

(newline)

(test-begin "delL")

(let ([t1 (node-constructor 'black #f 1 #f 1)]
	  [t2 (node-constructor 'black #f 7 #f 1)]
	  [x 7]
	  [y 6])
	(let([test1 (node-constructor 'red t1 y t2 1)]
		 [ans1 (node-constructor 'red (delete x t1) y t2 1)]
		 [test2 (node-constructor 'black t1 y t2 1)]
		 [ans2 (balL (node-constructor 'black (delete x t1) y t2 1))])

	(display-and-test (delL x test1) ans1 "Case1")
	(display-and-test (delL x test2) ans2 "Case2")))
(test-end "delL")

(newline)

(test-begin "delR")
(let ([t1 (node-constructor 'black #f 1 #f 1)]
	  [t2 (node-constructor 'black #f 7 #f 1)]
	  [x 5]
	  [y 6])
	(let([test1 (node-constructor 'red t1 y t2 1)]
		 [ans1 (node-constructor 'red t1  y (delete x t2) 1)]
		 [test2 (node-constructor 'black t1 y t2 1)]
		 [ans2 (balR (node-constructor 'black t1 y (delete x t2) 1))])

	(display-and-test (delR x test1) ans1 "Case1")
	(display-and-test (delR x test2) ans2 "Case2"))) 
(test-end "delR")

(newline)

(test-begin "delete")
(let ([t (insert 2(insert 9(insert 10 (insert 1(insert 7(insert 2 (insert 5 #f)))))))]
	  [t2 (insert 5 #f)]
	  [t3 (insert 5 (insert 5 #f))]
	  [t4 (insert 2 (insert 5 (insert 5 #f)))])
(display-and-test (member? 10 (delete 10 t)) #f "Case1")
(display-and-test (member? 2 (delete 2 t)) #t "Case2")
(display-and-test (member? 9 (delete 9 t)) #f "Case3")
(display-and-test (member? 1 (delete 1 t)) #f "Case4")
(display-and-test (member? 7 (delete 7 t)) #f "Case5")
(display-and-test (member? 5 (delete 5 t)) #f "Case6")
(display-and-test (member? 20 (delete 20 t)) #f "Case7")
(display-and-test (delete 5 t2) #f "Case8")
(display-and-test (delete 5 t3) (insert 5 #f) "Case9")
(display-and-test (delete 5 t4) (insert 2 (insert 5 #f)) "Case10"))
(test-end "delete")

(newline)






