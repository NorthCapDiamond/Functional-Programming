(use-modules (srfi srfi-9) (srfi srfi-1) (srfi srfi-13))

(use-modules (ice-9 format))

(define-record-type
 <node>
 (make-node color left value right count)
 node?
 (color node-color)
 (left node-left)
 (value node-value)
 (right node-right)
 (count node-count))

(define (node-constructor color left value right count)
  (cond
    ((or (eq? color #f) (eq? value #f)) #f)
    (else (make-node color left value right count))))

(define (node->color node) (cond ((node? node) (node-color node)) (else #f)))

(define (node->left node) (cond ((node? node) (node-left node)) (else #f)))

(define (node->value node) (cond ((node? node) (node-value node)) (else #f)))

(define (node->right node) (cond ((node? node) (node-right node)) (else #f)))

(define (node->count node) (cond ((node? node) (node-count node)) (else #f)))

(define (bool->string flag) (if flag "yes\n" "no\n"))

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
         (if (node->left node) (sub-node->string (node->left node)) "nil")
         " | Right : "
         (if (node->right node) (sub-node->string (node->right node)) "nil")
         ")")))
  (string-append (sub-node->string node) "\n"))

(define (member? x node)
  (cond
    ((node? node)
     (let ((y (node->value node)))
       (cond
         ((lower x y) (member? x (node->left node)))
         ((eq? x y) #t)
         (else (member? x (node->right node))))))
    (else #f)))

(define (lower a b)
  (cond
    ((and (number? a) (number? b)) (< a b))
    ((and (string? a) (string? b)) (string<? a b))))

(define (lowereq a b)
  (cond
    ((and (number? a) (number? b)) (<= a b))
    (((and (string? a) (string? b)) (string<=? a b)))))

(define (tree-count x node)
  (cond
    ((node? node)
     (let ((y (node->value node)))
       (cond
         ((lower x y) (tree-count x (node->left node)))
         ((eq? x y) (node->count node))
         (else (tree-count x (node->right node))))))
    (else #f)))

(define (recolor-black node)
  (cond ((eq? node #f) #f))
  (node-constructor
   'black
   (node->left node)
   (node->value node)
   (node->right node)
   (node->count node)))

(define (balance node)
  (let ((mode-this node)
        (left (node->left node))
        (right (node->right node))
        (left-left (node->left (node->left node)))
        (left-right (node->right (node->left node)))
        (right-right (node->right (node->right node)))
        (right-left (node->left (node->right node))))
    (cond
      ((and (eq? (node->color mode-this) 'black)
            (eq? (node->color left) 'red)
            (eq? (node->color left-left) 'red)
            (eq? right-right #f)
            (eq? right-left #f)
            (eq? (node->left left-right) #f)
            (eq? (node->right left-right) #f))
       (node-constructor
        'red
        (recolor-black left-left)
        (node->value left)
        (node-constructor
         'black
         left-right
         (node->value mode-this)
         right
         (node->count mode-this))
        (node->count left)))
      ((and (eq? (node->color mode-this) 'black)
            (eq? (node->color left) 'red)
            (eq? (node->color left-right) 'red))
       (node-constructor
        'red
        (node-constructor
         'black
         left-left
         (node->value left)
         (node->left left-right)
         (node->count left))
        (node->value left-right)
        (node-constructor
         'black
         (node->right left-right)
         (node->value mode-this)
         right
         (node->count mode-this))
        (node->count left-right)))
      ((and (eq? (node->color mode-this) 'black)
            (eq? (node->color right) 'red)
            (eq? (node->color right-left) 'red))
       (node-constructor
        'red
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
        (node->count right-left)))
      ((and (eq? (node->color mode-this) 'black)
            (eq? (node->color right) 'red)
            (eq? (node->color right-right) 'red))
       (node-constructor
        'red
        (node-constructor
         'black
         left
         (node->value mode-this)
         right-left
         (node->count mode-this))
        (node->value right)
        (recolor-black right-right)
        (node->count right)))
      (else mode-this))))

(define (insert x node)
  (define (inner-insert x node)
    (cond
      ((not (node? node)) (node-constructor 'red #f x #f 1))
      ((lower x (node->value node))
       (balance
        (node-constructor
         (node->color node)
         (inner-insert x (node->left node))
         (node->value node)
         (node->right node)
         (node->count node))))
      ((eq? x (node->value node))
       (node-constructor
        (node->color node)
        (node->left node)
        (node->value node)
        (node->right node)
        (+ (node->count node) 1)))
      ((lower (node->value node) x)
       (balance
        (node-constructor
         (node->color node)
         (node->left node)
         (node->value node)
         (inner-insert x (node->right node))
         (node->count node))))))
  (recolor-black (inner-insert x node)))

(define (delete x node)
  (define (inner-delete x node)
    (let ((y (node->value node)))
      (cond
        ((eq? y #f) #f)
        ((lower x y) (delL x node))
        ((lower y x) (delR x node))
        (else (cond
                ((lower 1 (node->count node))
                 (node-constructor
                  (node->color node)
                  (node->left node)
                  (node->value node)
                  (node->right node)
                  (- (node->count node) 1)))
                (else (let ((fused (fuse (node->left node) (node->right node))))
                        (cond ((eq? (node->value fused) #f) #f) (else fused)))))))))
  (recolor-black (inner-delete x node)))

(define (balL node)
  (let ((mode-this node) (left (node->left node)) (right (node->right node)))
    (cond
      ((and (eq? (node->color mode-this) 'black) (eq? (node->color left) 'red))
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
        (node->count mode-this)))
      ((and (eq? (node->color mode-this) 'black)
            (eq? (node->color right) 'black))
       (balance
        (node-constructor
         'black
         (node->left mode-this)
         (node->value mode-this)
         (node-constructor
          'red
          (node->left right)
          (node->value right)
          (node-right right)
          (node->count right))
         (node->count mode-this))))
      ((and (eq? (node->color mode-this) 'black)
            (eq? (node->color right) 'red)
            (eq? (node->color (node->right right)) 'black)
            (eq? (node->color (node->left right)) 'black))
       (node-constructor
        'red
        (node-constructor
         'black
         (node->left mode-this)
         (node->value mode-this)
         (node->left (node->left right))
         (node->count mode-this))
        (node->value (node->left right))
        (balance
         (node-constructor
          'black
          (node->right (node->left right))
          (node->value right)
          (node-constructor
           'red
           (node->left (node->right right))
           (node->value (node->right right))
           (node->right (node->right right))
           (node->count (node->right right)))
          (node->count right)))
        (node->count (node->left right))))
      (else mode-this))))

(define (balR node)
  (let ((mode-this node) (left (node->left node)) (right (node->right node)))
    (cond
      ((and (eq? (node->color mode-this) 'black)
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
        (node->count mode-this)))
      ((and (eq? (node->color mode-this) 'black)
            (eq? (node->color left) 'black))
       (balance
        (node-constructor
         'black
         (node-constructor
          'red
          (node->left left)
          (node->value left)
          (node->right left)
          (node->count left))
         (node->value mode-this)
         (node->right mode-this)
         (node->count mode-this))))
      ((and (eq? (node->color mode-this) 'black)
            (eq? (node->color left) 'red)
            (eq? (node->color (node->right left)) 'black)
            (eq? (node->color (node->left left)) 'black))
       (node-constructor
        'red
        (balance
         (node-constructor
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
        (node->count (node->right left))))
      (else mode-this))))

(define (delL x node)
  (cond ((eq? node? node) #f #f))
  (let ((mode-this node) (left (node->left node)) (right (node->right node)))
    (cond
      ((eq? (node? left) #f) mode-this)
      (else (cond
              ((eq? (node->color mode-this) 'red)
               (node-constructor
                'red
                (delete x left)
                (node->value mode-this)
                right
                (node->count mode-this)))
              ((eq? (node->color mode-this) 'black)
               (balL (node-constructor
                      'black
                      (let ((after-delete (delete x left)))
                        (cond
                          ((eq? (node->value after-delete) #f) #f)
                          (else after-delete)))
                      (node->value mode-this)
                      right
                      (node->count mode-this))))
              (else mode-this))))))

(define (delR x node)
  (cond ((eq? node? node) #f #f))
  (let ((mode-this node) (left (node->left node)) (right (node->right node)))
    (cond
      ((eq? (node? right) #f) mode-this)
      (else (cond
              ((eq? (node->color mode-this) 'red)
               (node-constructor
                'red
                left
                (node->value mode-this)
                (delete x right)
                (node->count mode-this)))
              ((eq? (node->color mode-this) 'black)
               (balR (node-constructor
                      'black
                      left
                      (node->value mode-this)
                      (let ((after-delete (delete x right)))
                        (cond
                          ((eq? (node->value after-delete) #f) #f)
                          (else after-delete)))
                      (node->count mode-this)))))))))

(define (fuse tree1 tree2)
  (cond
    ((and (eq? (node? tree1) #f) (node? tree2)) tree2)
    ((and (node? tree1) (eq? (node? tree2) #f)) tree1)
    ((and (eq? (node? tree1) #f) (eq? (node? tree2) #f)) #f)
    (else (cond
            ((and (eq? (node->color tree1) 'black)
                  (eq? (node->color tree2) 'red))
             (node-constructor
              'red
              (fuse tree1 (node->left tree2))
              (node->value tree2)
              (node->right tree2)
              (node->count tree2)))
            ((and (eq? (node->color tree1) 'red)
                  (eq? (node->color tree2) 'black))
             (node-constructor
              'red
              (node->left tree1)
              (node->value tree1)
              (fuse (node->right tree1) tree2)
              (node->count tree1)))
            ((and (eq? (node->color tree1) 'red)
                  (eq? (node->color tree2) 'red))
             (let ((s (fuse (node->right tree1) (node->left tree2))))
               (cond
                 ((eq? s #f)
                  (node-constructor
                   'red
                   (node->left tree1)
                   (node->value tree1)
                   (node-constructor
                    'red
                    #f
                    (node->value tree2)
                    (node->right tree2)
                    (node->count tree2))
                   (node->count tree1)))
                 ((eq? (node->color s) 'red)
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
                   (node->count s)))
                 ((eq? (node->color s) 'black)
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
                   (node->count tree1)))
                 (else #f))))
            ((and (eq? (node->color tree1) 'black)
                  (eq? (node->color tree2) 'black))
             (let ((s (fuse (node->right tree1) (node->left tree2))))
               (cond
                 ((eq? s #f)
                  (node-constructor
                   'black
                   (node->left tree1)
                   (node->value tree1)
                   (node-constructor
                    'black
                    #f
                    (node->value tree2)
                    (node->right tree2)
                    (node->count tree2))
                   (node->count tree1)))
                 ((eq? (node->color s) 'red)
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
                   (node->count s)))
                 ((eq? (node->color s) 'black)
                  (balL (node-constructor
                         'black
                         (node->left tree1)
                         (node->value tree1)
                         (node-constructor
                          'black
                          s
                          (node->value tree2)
                          (node->right tree2)
                          (node->count tree2))
                         (node->count tree1))))
                 (else #f))))
            (else #f)))))

























