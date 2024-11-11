(include "rb-tree.scm")
(include "testing.scm")

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


