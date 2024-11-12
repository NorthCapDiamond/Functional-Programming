# Лабораторная работа 2. Работа с неизменяемыми и рекурсивными структурами данных

Вариант: RB-Bag

Автор: Дробыш Дмитрий Александрович P34082 333219

Цель: освоиться с построением пользовательских типов данных, полиморфизмом, рекурсивными алгоритмами и средствами тестирования (unit testing), а также разделением интерфейса и особенностей реализации.

## Условие

### Задание:

В рамках лабораторной работы вам предлагается реализовать одну из предложенных классических структур данных (список, дерево, бинарное дерево, hashmap, граф...).

Требования:

1. Функции:
    - добавление и удаление элементов;
    - фильтрация;
    - отображение (map);
    - свертки (левая и правая);
    - структура должна быть [моноидом](https://ru.m.wikipedia.org/wiki/Моноид).
2. Структуры данных должны быть неизменяемыми.
3. Библиотека должна быть протестирована в рамках unit testing.
4. Библиотека должна быть протестирована в рамках property-based тестирования (как минимум 3 свойства, включая свойства моноида).
5. Структура должна быть полиморфной.
6. Требуется использовать идиоматичный для технологии стиль программирования. Примечание: некоторые языки позволяют получить большую часть API через реализацию небольшого интерфейса. Так как лабораторная работа про ФП, а не про экосистему языка -- необходимо реализовать их вручную и по возможности -- обеспечить совместимость.
7. Обратите внимание:
    - API должно быть реализовано для заданного интерфейса и оно не должно "протекать". На уровне тестов -- в первую очередь нужно протестировать именно API (dict, set, bag).
    - Должна быть эффективная реализация функции сравнения (не наивное приведение к спискам, их сортировка с последующим сравнением), реализованная на уровне API, а не внутреннего представления.


## Реализация

Полный код представлен в `rb-bag` или `rb-bag-format`.

### Структура узла красно-черного дерева

```Scheme
(define-record-type
 <node>
 (make-node color left value right count)
 node?
 (color node-color)
 (left node-left)
 (value node-value)
 (right node-right)
 (count node-count))
```

### Конструктор узла красно-черного дерева

```Scheme
(define (node-constructor color left value right count)
  (cond
    ((or (eq? color #f) (eq? value #f)) #f)
    (else (make-node color left value right count))))
```

### Добавление нового элемента в дерево

```Scheme
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
```

### Удаление элемента из дерева 

```Scheme
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
```


### Создание мультимножества 

```Scheme
(define (create-rbmset value)
  (cond ((eq? value #f) #f) (else (insert value #f))))
```

### Добавление элемента к мультимножеству

```Scheme
(define (append-rbmset value rbmset)
  (cond ((eq? value #f) rbmset) (else (insert value rbmset))))

(define (append-rbmset-many value rbmset amount)
  (cond
    ((or (eq? value #f) (lower amount 1)) rbmset)
    (else (append-rbmset-many value (insert value rbmset) (- amount 1)))))

(define (rbmset-fill rmbset lst)
  (define (sub-walk rbmset lst)
    (cond
      ((null? lst) rbmset)
      (else (let ((x (car lst)))
              (list? x)
              (sub-walk (append-rbmset x rbmset) (cdr lst))))))
  (cond ((eq? lst #f) rmbset) (else (sub-walk rmbset lst))))
```

### Удаление элемента из мультимножества

```Scheme
(define (remove-rbmset value rbmset)
  (cond ((eq? value #f) rbmset) (else (delete value rbmset))))
```

### Объединение мультимножеств 

```Scheme
(define (union-rbmset rbmset1 rbmset2)
  (cond
    ((list? rbmset1) (rbmset-fill rbmset2 rbmset1))
    ((list? rbmset2) (rbmset-fill rbmset1 rbmset2))
    ((eq? rbmset1 #f) rbmset2)
    ((eq? rbmset2 #f) rbmset1)
    (else (rbmset-fill
           (rbmset-fill #f (rbmset->list rbmset1))
           (rbmset->list rbmset2)))))
```

### Фильтрация мультимножеств

```Scheme
(define (filter-rbmset rbmset pred?)
  (define (sub-filter-rbmset newrmbset rbmset pred?)
    (let ((l (eq? (node? (node->left rbmset)) #t))
          (r (eq? (node? (node->right rbmset)) #t))
          (left (node->left rbmset))
          (right (node->right rbmset))
          (value (node->value rbmset)))
      (cond
        ((and l r)
         (sub-filter-rbmset
          (cond
            ((eq? (pred? value) #t)
             (append-rbmset-many
              value
              (sub-filter-rbmset newrmbset left pred?)
              (node->count rbmset)))
            (else (sub-filter-rbmset newrmbset left pred?)))
          right
          pred?))
        ((and r (not l))
         (sub-filter-rbmset
          (cond
            ((eq? (pred? value) #t)
             (append-rbmset-many value newrmbset (node->count rbmset)))
            (else newrmbset))
          right
          pred?))
        ((and l (not r))
         (cond
           ((eq? (pred? value) #t)
            (append-rbmset-many
             value
             (sub-filter-rbmset newrmbset left pred?)
             (node->count rbmset)
             (node->count rbmset)))
           (else (sub-filter-rbmset newrmbset left pred?))))
        ((not (and l r))
         (cond
           ((eq? (pred? value) #t)
            (append-rbmset-many value newrmbset (node->count rbmset)))
           (else newrmbset))))))
  (sub-filter-rbmset #f rbmset pred?))
```

### Отображение (map) для мультимножеств

```Scheme
(define (map-rbmset rbmset f)
  (define (sub-map-rbmset newrmbset rbmset f)
    (let ((l (eq? (node? (node->left rbmset)) #t))
          (r (eq? (node? (node->right rbmset)) #t))
          (left (node->left rbmset))
          (right (node->right rbmset))
          (value (node->value rbmset)))
      (cond
        ((and l r)
         (sub-map-rbmset
          (append-rbmset-many
           (f value)
           (sub-map-rbmset newrmbset left f)
           (node->count rbmset))
          right
          f))
        ((and r (not l))
         (sub-map-rbmset
          (append-rbmset-many (f value) newrmbset (node->count rbmset))
          right
          f))
        ((and l (not r))
         (append-rbmset-many
          (f value)
          (sub-map-rbmset newrmbset left f)
          (node->count rbmset)))
        ((not (and l r))
         (append-rbmset-many (f value) newrmbset (node->count rbmset))))))
  (sub-map-rbmset #f rbmset f))
```

### Свертки для мультимножеств

```Scheme
(define (left-fold-rbmset f acc lst) (fold f acc (rbmset->list lst)))

(define (right-fold-rbmset f acc lst) (fold-right f acc (rbmset->list lst)))
```

### Сравнение мультимножеств

```Scheme
(define (hash-numbers lst)
  (let ((list-string (format #f "~s" lst)))
    (hash list-string 1000000000000000)))

(define (compare rbmset1 rbmset2)
  (let ((list1 (rbmset->list rbmset1)) (list2 (rbmset->list rbmset2)))
    (eq? (hash-numbers list1) (hash-numbers list2))))
```

### Вывод

Лабораторная работа получилась очень сложной, так как структура данных в моем варианте требует повышенного внимания. Стоит отметить, что использование pattern-matching не упрощает задачу в настоящей версии решения по причине использования собственных типов.
Хочу заметить, что написание тестов на языке Scheme происходит достаточно быстро, что позволило мне оперативно находить ошибки и опечатки в исходном коде.

### Источники 

- https://abhiroop.github.io/Haskell-Red-Black-Tree/
- https://www.gnu.org/software/guile/manual/html_node/Records.html
- https://srfi.schemers.org/srfi-64/srfi-64.html
- https://www.cs.usfca.edu/~galles/visualization/RedBlack.html
  
