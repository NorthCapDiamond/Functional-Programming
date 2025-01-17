# Лабораторная работа 4. Реализация Property-based test Library with Shrinking

Автор: Дробыш Дмитрий Александрович P34082 333219

Цель: Закрепить навыки работы с guile и Property-Based-Tests

## Задание:

В рамках лабораторной работы вам предлагается реализовать библиотеку -- аналог QuickCheck для Языка Haskell

## Реализация

### Структура Генераторов

#### Random float:

```Scheme
(define* (random-float-gen left right #:optional (random-state now-random-state))
  (lambda ()
    (+ (* (random (- right left) random-state) 1.0) left)))
```

Usage:

```
(display ((random-float-gen 1 5.5)))
```

#### Random String:

```Scheme
(define* (random-string-gen n #:optional (left 32)(right 95) (random-state now-random-state))
  (define (random-string-gen-sub n str)
    (cond
      [(eq? n 0) str]
      [else
        (random-string-gen-sub (- n 1) 
          (string-append str 
            (string 
              (integer->char 
                (+ (random right random-state) left)))))]))
  (lambda ()
    (random-string-gen-sub n empty-string)))

```

Usage:

```
(display ((random-string-gen 10)))
```

#### Random Int list:

```Scheme
(define* (random-int-list-gen n mini maxi #:optional (random-state now-random-state))
  (define (random-int-list-gen-sub n mini maxi lst)
    (cond
      [(eq? n 0) lst]
      [else
        (random-int-list-gen-sub
          (- n 1) 
          mini
          maxi 
          (append lst (list ((random-int-gen mini maxi random-state)))))]))
  (lambda ()
    (random-int-list-gen-sub n mini maxi '())))
```

Usage:

```
(display ((random-int-list-gen 10 0 10)))
```

#### Random String list:

```Scheme
(define* (random-string-list-gen n maxi #:optional (left 32) (right 95) (random-state now-random-state))
  (define (random-string-list-gen-sub n maxi lst)
    (cond
      [(eq? n 0) lst]
      [else
        (random-string-list-gen-sub
          (- n 1) 
          maxi 
          (append lst (list ((random-string-gen (random maxi random-state) left right random-state)))))]))
  (lambda()
    (random-string-list-gen-sub n maxi '())))
```

Usage:

```
(display ((random-string-list-gen 10 10)))
```

#### Random Bool:

```Scheme
(define* (random-bool-gen #:optional (random-state now-random-state))
  (lambda ()
    (let ([ a (random 2 random-state)])
      (cond
        [(eqv? a 1) #t]
        [else #f]))))
```

Usage:

```
(display ((random-bool-gen)))
```

#### Random Bool list:

```Scheme
(define* (random-bool-list-gen n #:optional (random-state now-random-state))
  (define (random-bool-list-gen-sub n lst)
    (cond
      [(eq? n 0) lst]
      [else
        (random-bool-list-gen-sub
          (- n 1)  
          (append lst (list ((random-bool-gen random-state)))))]))
  (lambda ()
    (random-bool-list-gen-sub n '())))
```

Usage:

```
(display ((random-bool-list-gen 5)))
```

#### Merge combinators

```Scheme
(define* (merge-generators generators #:optional (random-state now-random-state))
  (lambda ()
    (let ([i (random (length generators) random-state)])
      ((list-ref generators i)))))

```

Usage:

```Scheme
(display ((merge-generators 
  (list (random-int-gen 1 5)
        (random-string-gen 10)
        (random-bool-gen)))))
(newline)
```

#### Combinators

```Scheme
(define (combined-generator generators)
  (lambda args
    (cond 
      [(null? args)
        (map (lambda (gen) (gen)) generators)]
      [else
        (map (lambda (gen arg) (gen arg)) generators args)])))
```

Usage:

```Scheme
(define (combined-generator generators)
  (lambda args
    (cond 
      [(null? args)
        (map (lambda (gen) (gen)) generators)]
      [else
        (map (lambda (gen arg) (gen arg)) generators args)])))
```

### Структура основного тестера:

#### Структура ответа

```Scheme
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
```

Вывод ответа: 

```Scheme
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
```

#### Основная система тестов

```Scheme
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
```


### Usage

sum property Success. Default generator. No Shrinking

```Scheme
(define (sum-property a b)
  (eqv? (+ a b) (+ b a)))

(let ([int (random-int-gen 0 100)])
  (let([ints (combined-generator (list int int))])
    (response-to-string 
      (test sum-property (make-struct-generator ints now-random-state) 10) 
      #t #t #t)))
```

Output

```
---Answer Record---
Passed 10 Out of 10

Logs:

Test 1 with data 98 18   was Passed

Test 2 with data 61 17   was Passed

Test 3 with data 9 41   was Passed

Test 4 with data 78 6   was Passed

Test 5 with data 58 37   was Passed

Test 6 with data 84 58   was Passed

Test 7 with data 91 2   was Passed

Test 8 with data 84 62   was Passed

Test 9 with data 99 46   was Passed

Test 10 with data 25 25   was Passed
 
Failed:
 
Passed:
98 18  61 17  9 41  78 6  58 37  84 58  91 2  84 62  99 46  25 25   
```

sum property
Fail. Default generator. No Shrinking

```Scheme
(define (sum-fail-property a b)
  (eqv? (+ a b) (+ b a 1)))

(let ([int (random-int-gen 0 100)])
  (let([ints (combined-generator (list int int))])
    (response-to-string 
      (test sum-fail-property (make-struct-generator ints now-random-state) 10) 
      #t #t #t)))
```

Check if value is < 10
Example of fail. Default generator. No Shrinking
```Scheme
(define (number-prop a)
  (< a 10))

(response-to-string
  (test number-prop (make-struct-generator (random-int-gen 0 20) now-random-state) 10) #t #t #t)
```

Output:

```
---Answer Record---
Passed 6 Out of 10

Logs:

Test 1 with data 13  was Failed

Test 2 with data 8  was Passed

Test 3 with data 15  was Failed

Test 4 with data 4  was Passed

Test 5 with data 3  was Passed

Test 6 with data 14  was Failed

Test 7 with data 4  was Passed

Test 8 with data 4  was Passed

Test 9 with data 2  was Passed

Test 10 with data 18  was Failed
 
Failed:
13 15 14 18  
Passed:
8 4 3 4 4 2 
```

Combination of generators (sum property) | int -> (int int) | No shrinking

```Scheme
(let ([int (random-int-gen 0 100)])
  (let([ints (combined-generator (list int int))])
    (response-to-string 
      (test sum-property (make-struct-generator ints now-random-state) 10) 
      #t #t #t)))
```

Output:

```
---Answer Record---
Passed 10 Out of 10

Logs:

Test 1 with data 86 64   was Passed

Test 2 with data 49 79   was Passed

Test 3 with data 50 26   was Passed

Test 4 with data 50 7   was Passed

Test 5 with data 29 58   was Passed

Test 6 with data 3 33   was Passed

Test 7 with data 43 71   was Passed

Test 8 with data 16 80   was Passed

Test 9 with data 60 21   was Passed

Test 10 with data 27 35   was Passed
 
Failed:
 
Passed:
86 64  49 79  50 26  50 7  29 58  3 33  43 71  16 80  60 21  27 35 
```

Merge generators | int -> int/float -> (int/float int/float) | No Shrinking

```Scheme
(let ([int (random-int-gen 0 100)]
    [float (random-float-gen 0.2 9.9)])
  (let ([merged-generator (merge-generators(list int float))])
    (let ([combined-generator (combined-generator(list merged-generator int))])
      (response-to-string 
        (test sum-property (make-struct-generator combined-generator combined-generator) 10) 
        #t #t #t))))
```

Output:

```
---Answer Record---
Passed 10 Out of 10

Logs:

Test 1 with data 76 2   was Passed

Test 2 with data 8.788088802479976 35   was Passed

Test 3 with data 5.6189265539457764 47   was Passed

Test 4 with data 43 13   was Passed

Test 5 with data 6.230893012417229 88   was Passed

Test 6 with data 3.2914723270737176 44   was Passed

Test 7 with data 1.8475962070900835 82   was Passed

Test 8 with data 89 87   was Passed

Test 9 with data 3.5036399943367202 94   was Passed

Test 10 with data 83 13   was Passed
 
Failed:
 
Passed:
76 2  8.788088802479976 35  5.6189265539457764 47  43 13  6.230893012417229 88  3.2914723270737176 44  1.8475962070900835 82  89 87  3.5036399943367202 94  83 13   
```

Own generator. No Shrinking

```Scheme
(define* (custom-generator maxi #:optional (random-state 1000))
  (lambda ()
    (random (+ (- maxi 10) 10))))

(response-to-string
  (test number-prop (make-struct-generator (custom-generator 20 now-random-state) now-random-state) 10) #t #t #t)
```

Output:

```
---Answer Record---
Passed 4 Out of 10

Logs:

Test 1 with data 19  was Failed

Test 2 with data 13  was Failed

Test 3 with data 4  was Passed

Test 4 with data 17  was Failed

Test 5 with data 5  was Passed

Test 6 with data 6  was Passed

Test 7 with data 13  was Failed

Test 8 with data 10  was Failed

Test 9 with data 18  was Failed

Test 10 with data 2  was Passed
 
Failed:
19 13 17 13 10 18  
Passed:
4 5 6 2  
```

Check if value is < 10
Example of fail. Default generator Shrinking enabled

```Scheme
(response-to-string
  (test number-prop (make-struct-generator (random-int-gen 0 20) now-random-state) 10 #t shrink-number 10) #t #t #t)
```

```
---Answer Record---
Passed 4 Out of 10

Logs:
13 #f12 #f15 #f15 #f14 #f13 #f12 #f11 #f14 #f14 #f13 #f12 #f11 #f10 #f13 #f13 #f12 #f11 #f10 #f9 #t12 #f12 #f11 #f10 #f9 #t8 #t11 #f11 #f10 #f9 #t8 #t7 #t10 #f10 #f9 #t8 #t7 #t6 #t9 #t9 #t8 #t7 #t6 #t5 #t8 #t8 #t7 #t6 #t5 #t4 #t7 #t7 #t6 #t5 #t4 #t3 #t6 #t6 #t5 #t4 #t 
Failed:
 
Passed:
```

Combination of generators (sum property) | int -> (int int) | No shrinking

```Scheme
(let ([int (random-int-gen 0 100)])
  (let([ints (combined-generator (list int int))])
    (response-to-string 
      (test sum-property (make-struct-generator ints now-random-state) 10 #t shrink-list 1000) 
      #t #t #t)))
```

```
---Answer Record---
Passed 10 Out of 10

Logs:

Test 1 with data 94 95   was Passed

Test 2 with data 90 86   was Passed

Test 3 with data 62 55   was Passed

Test 4 with data 14 61   was Passed

Test 5 with data 79 38   was Passed

Test 6 with data 20 15   was Passed

Test 7 with data 44 40   was Passed

Test 8 with data 0 95   was Passed

Test 9 with data 22 83   was Passed

Test 10 with data 25 5   was Passed
 
Failed:
 
Passed:
94 95  90 86  62 55  14 61  79 38  20 15  44 40  0 95  22 83  25 5 
```

Merge generators | int -> int/float -> (int/float int/float) | Shrinking enabled

```Scheme
(let ([int (random-int-gen 0 100)]
    [float (random-float-gen 0.2 9.9)])
  (let ([merged-generator (merge-generators(list int float))])
    (let ([combined-generator (combined-generator(list merged-generator int))])
      (response-to-string 
        (test sum-property (make-struct-generator combined-generator combined-generator) 10 #t shrink-list 1000) 
        #t #t #t))))
```

Output:

```
---Answer Record---
Passed 10 Out of 10

Logs:

Test 1 with data 3.317330023591337 42   was Passed

Test 2 with data 21 55   was Passed

Test 3 with data 2.2709957245952688 33   was Passed

Test 4 with data 99 78   was Passed

Test 5 with data 3.124367008370015 1   was Passed

Test 6 with data 38 39   was Passed

Test 7 with data 46 0   was Passed

Test 8 with data 31 78   was Passed

Test 9 with data 29 95   was Passed

Test 10 with data 3.5242765705308243 61   was Passed
 
Failed:
 
Passed:
3.317330023591337 42  21 55  2.2709957245952688 33  99 78  3.124367008370015 1  38 39  46 0  31 78  29 95  3.5242765705308243 61   

```



### Заключение 

Лабораторная работа получилась полезной. Я закрепил работу с языком Guile и могу достаточно свободно на нем писать программы.
Хотелось бы иметь такую реализацию на момент выполнения ЛР2. Это бы ускорило процесс разработки в несколько раз