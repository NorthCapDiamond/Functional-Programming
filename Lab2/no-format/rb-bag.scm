(include "rb-tree.scm") 

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

(define (rbmset-fill rmbset lst)
	(define (sub-walk rbmset lst)
		(cond 
			[(null? lst) rbmset]
			[else
				(let ([x (car lst)])
						(list? x) (sub-walk (append-rbmset x rbmset) (cdr lst)))]))

	(cond 
		[(eq? lst #f) rmbset]
		[else
			(sub-walk rmbset lst)]))

(define (remove-rbmset value rbmset)
	(cond [(eq? value #f) rbmset]
		  [else (delete value rbmset)]))

(define (union-rbmset rbmset1 rbmset2)
	(cond
		[(list? rbmset1) (rbmset-fill rbmset2 rbmset1)]
		[(list? rbmset2) (rbmset-fill rbmset1 rbmset2)]
		[(eq? rbmset1 #f) rbmset2]
		[(eq? rbmset2 #f) rbmset1]
		[else (rbmset-fill (rbmset-fill #f (rbmset->list rbmset1)) (rbmset->list rbmset2))]))

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
					(cond [(eq? (pred? value) #t) (append-rbmset-many value (sub-filter-rbmset newrmbset left pred?) (node->count rbmset))][else (sub-filter-rbmset newrmbset left pred?)])]
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







































