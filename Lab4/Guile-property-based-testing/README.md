# Лабораторная работа 4. Реализация Property-based test Library with Shrinking

Автор: Дробыш Дмитрий Александрович P34082 333219

Цель: Закрепить навыки работы с guile и Property-Based-Tests

## Задание:

В рамках лабораторной работы вам предлагается реализовать библиотеку -- аналог QuickCheck для Языка Haskell

## Реализация

### Структура Генераторов

#### Функция Range:

```Scheme
(define (range start end)
  (cond 
    [(>= start end)'()]
    [else
      (cons start 
        (range (+ start 1) end))]))
```

#### Random float:

```Scheme
(define* (random-float min max #:optional (r-state my-random-state))
  (- (random max r-state) (random min r-state)))
```

Usage:

```
(display (random-float 0.0 5.0))
(newline)
(display (random-float 0.0 5.0 (seed->random-state 0)))
(newline)
```

#### Random String:

```Scheme
(define* (random-ascii-string n #:optional (left 32)(right 95) (r-state my-random-state))
  (define (random-ascii-string-sub n str)
    (cond
      [(eq? n 0) str]
      [else
        (random-ascii-string-sub (- n 1) 
          (string-append str 
            (string 
              (integer->char 
                (+ (random right r-state) left)))))]))
  (random-ascii-string-sub n empty-string))

```

Usage:

```
(display (random-ascii-string 5))
(newline)
(display (random-ascii-string 5 0 100 (seed->random-state 0)))
(newline)
(display (random-ascii-string 5 32 33 (seed->random-state 90)))
(newline)
```

#### Random Int list:

```Scheme
(define* (random-int-list n maxi #:optional (r-state my-random-state))
  (define (random-int-list-sub n maxi lst)
    (cond
      [(eq? n 0) lst]
      [else
        (random-int-list-sub
          (- n 1) 
          maxi 
          (append lst (list (random maxi r-state))))]))
  (random-int-list-sub n maxi '()))
```

Usage:

```
(display (random-int-list 5 10000))
(newline)
(display (random-int-list 5 10000 (seed->random-state 0)))
(newline)
```

#### Random String list:

```Scheme
(define* (random-string-list n maxi #:optional (left 32) (right 95) (r-state my-random-state))
  (define (random-string-list-sub n maxi lst)
    (cond
      [(eq? n 0) lst]
      [else
        (random-string-list-sub
          (- n 1) 
          maxi 
          (append lst (list (random-ascii-string (random maxi r-state) left right r-state))))]))
  (random-string-list-sub n maxi '()))
```

Usage:

```
(display (random-string-list 10 6))
(newline)
(display (random-string-list 10 6 32 95 (seed->random-state 0)))
(newline)
```

#### Random Bool:

```Scheme
(define* (random-bool #:optional (r-state my-random-state))
  (let ([ a (random 2 r-state)])
    (cond
      [(eqv? a 1) #t]
      [else #f])))
```

Usage:

```
(display (random-bool))
(newline)
(display (random-bool (seed->random-state 0)))
(newline)
```

#### Random Bool list:

```Scheme
(define* (random-bool-list n #:optional (r-state my-random-state))
  (define (random-bool-list-sub n lst)
    (cond
      [(eq? n 0) lst]
      [else
        (random-bool-list-sub
          (- n 1)  
          (append lst (list (random-bool r-state))))]))
  (random-bool-list-sub n '()))
```

Usage:

```
(display (random-bool-list 5))
(newline)
(display (random-bool-list 5 (seed->random-state 0)))
(newline)
```

### Структура основного тестера:

#### Структура ответа

```Scheme
(define-record-type <answer>
  (make-answer result logging failed status user-property args-amount generator generator-args)
  answer?
  (result answer-result)
  (logging answer-logging)
  (failed answer-failed)
  (status answer-status)
  (user-property answer-user-property)
  (args-amount answer-args-amount)
  (generator answer-generator)
  (generator-args answer-generator-args))
```

Вывод ответа: 

```Scheme
(define* (print-property-test-stats ans #:optional (res #t) (logg #f) (fail #f) (isok #f))
  (cond 
    [(answer? ans) 
    (cond [(or res logg fail) (display "---Answer Record---\n")])
    (cond [(eq? res #t)
        (display "Result: ")
        (display (answer-result ans))
        (newline)])
    (cond
        [(eq? logg #t)
        (display "Logging:\n")
        (display (answer-logging ans))
        (newline)])
    (cond
        [(eq? fail #t)
        (display "Failed:\n")
        (display (answer-failed ans))
        (newline)])
    (cond
      [(eq? isok #t)
      (display "Is passed:\n")
        (display (answer-status ans))
        (newline)])]
    [else (display "Invalid answer record.")]))
```

#### Основная система тестов

```Scheme
(define (test-template user-property args-amount generator generator-args test-size)
  (define (prepare-args generator generator-args args-amount)
    (define (prepare-args-sub generator generator-args args-amount lst)
      (cond
        [(eqv? args-amount 0) lst]
        [else
          (prepare-args-sub 
            generator 
            generator-args 
            (- args-amount 1) 
            (append lst 
              (list 
                (apply 
                  generator 
                  generator-args))))]))
    (prepare-args-sub generator generator-args args-amount '()))

  (define* (iter-test user-property args-amount generator generator-args test-size my-answer passed #:optional (i 1))
    (cond
      [(eqv? test-size 0) (list my-answer passed)]
      [else
      (let ([func-args 
        (prepare-args 
          generator 
          generator-args 
          args-amount)])
        (let ([ans (apply user-property func-args)])
          (iter-test 
            user-property
            args-amount
            generator
            generator-args
            (- test-size 1)
            (make-answer
              (answer-result my-answer)
              (string-append
                (answer-logging my-answer)
                "Test number " 
                (number->string i) 
                " Finished with status " 
                (cond
                  [(eq? ans #t) "OK"]
                  [else "FAIL"])
                "| Test data:"
                (toString func-args)
                "\n")
                (cond
                  [(eq? ans #t) (answer-failed my-answer)]
                  [else 
                    (string-append (answer-failed my-answer) (toString func-args) "\n")])
                (answer-status my-answer)
                (answer-user-property my-answer)
                (answer-args-amount my-answer)
                (answer-generator my-answer)
                (answer-generator-args my-answer))
            (cond
              [(eq? ans #t) (+ 1 passed)]
              [else passed])
            (+ i 1))))]))

  (define (test-loop user-property args-amount generator generator-args test-size my-answer)
    (let ([after-test (iter-test
                   user-property 
                   args-amount 
                   generator 
                   generator-args 
                   test-size 
                   my-answer 
                   0)])
    (make-answer 
      (string-append "Passed " 
        (number->string (cadr after-test))
        " Out of " 
        (number->string test-size))
      (answer-logging (car after-test))
      (answer-failed (car after-test))
      (cond
        [(eqv? test-size (cadr after-test)) #t]
        [else #f])
      (answer-user-property my-answer)
      (answer-args-amount my-answer)
      (answer-generator my-answer)
      (answer-generator-args my-answer))))

  (test-loop user-property args-amount generator generator-args test-size (make-answer "" "" "" #f user-property args-amount generator generator-args)))
```

### Shrinking (default)

#### Изменение параметров для уменьшения теста

```Scheme
(define (parse-args lst params-to-change shift)
    (define (parse-args-sub lst params-to-change fnl-lst shift)
      (cond
        [(eqv? params-to-change 0) fnl-lst]
        [else
          (cond
            [(> (car lst) shift) (parse-args-sub (cdr lst) (- params-to-change 1) (append fnl-lst (list (- (car lst) shift))) shift)]
            [(< (car lst) 0) (parse-args-sub (cdr lst) (- params-to-change 1) (append fnl-lst (list (+ (car lst) shift))) shift)]
            [else (parse-args-sub (cdr lst) (- params-to-change 1) (append fnl-lst (list (car lst))) shift)])]))
    (parse-args-sub lst params-to-change '() shift))
```


```Scheme
(define (parse-args-iterative lst params-to-change shift)
    (define (parse-args-iterative-sub lst params-to-change fnl-lst shift)
      (cond
        [(eqv? params-to-change 0) fnl-lst]
        [else
          (cond
            [(and (<= (car lst) shift) (>= (car lst) 0)) (parse-args-iterative-sub (cdr lst) (- params-to-change 1) (append fnl-lst (list (car lst))) shift)]
            [(> (car lst) shift) (parse-args-iterative-sub (cdr lst) (- params-to-change 1) (append fnl-lst (list (- (car lst) shift))) shift)]
            [(< (car lst) 0) (parse-args-iterative-sub (cdr lst) (- params-to-change 1) (append fnl-lst (list (+ (car lst) shift))) shift)]
            [else (parse-args-iterative-sub (cdr lst) (- params-to-change 1) (append fnl-lst (list (car lst))) shift)])]))
    (parse-args-iterative-sub lst params-to-change '() shift))
```

#### Shrinker

```Scheme
(define (default-shrinker answer limit params-to-change shift cycle)
  ; (display (parse-args 
  ;         (answer-generator-args answer) 
  ;         params-to-change
  ;         shift))
  (cond
    [(eq? answer-status #t) answer])
  (cond
    [(eqv? cycle 0) answer]
    [else

      (let ([run-tst (test-template
        (answer-user-property answer)
        (answer-args-amount answer)
        (answer-generator answer)
        (parse-args 
          (answer-generator-args answer) 
          params-to-change
          (random (+ (- shift 1) 1)))
        limit)])

        (cond 
          [(and (eq? (answer-status answer) #f) (eq? (answer-status run-tst) #t)) answer]
          [else
            (default-shrinker run-tst limit params-to-change shift (- cycle 1))]))]))
```

```Scheme
(define (default-shrinker-iterative answer limit params-to-change shift cycle)
  ; (display (parse-args 
  ;         (answer-generator-args answer) 
  ;         params-to-change
  ;         shift))
  (cond
    [(eq? answer-status #t) answer])
  (cond
    [(eqv? cycle 0) answer]
    [else

      (let ([run-tst (test-template
        (answer-user-property answer)
        (answer-args-amount answer)
        (answer-generator answer)
        (parse-args-iterative 
          (answer-generator-args answer) 
          params-to-change
          shift)
        limit)])

        (cond 
          [(and (eq? (answer-status answer) #f) (eq? (answer-status run-tst) #t)) answer]
          [else
            (default-shrinker run-tst limit params-to-change shift (- cycle 1))]))]))
```

#### Usage

### default params. No Shrinking

Create property:

```Scheme
(define (sum-property a b)
  (eqv? (+ a b) (+ b a)))
```

Run tests:

```Scheme
(print-property-test-stats(test-template sum-property 2 random (list 100 my-random-state) 100))
```

Result:

```
---Answer Record---
Result: Passed 100 Out of 100
```

Set print params: (result, logging, failed, status)

```
(print-property-test-stats(test-template sum-property 2 random (list 100 my-random-state) 100) #t #t #t #t)
```

Result:

```
---Answer Record---
Result: Passed 100 Out of 100
Logging:
Test number 1 Finished with status OK| Test data:80 92
Test number 2 Finished with status OK| Test data:46 97
Test number 3 Finished with status OK| Test data:37 60
Test number 4 Finished with status OK| Test data:37 47
Test number 5 Finished with status OK| Test data:96 71
Test number 6 Finished with status OK| Test data:36 51
Test number 7 Finished with status OK| Test data:63 50
Test number 8 Finished with status OK| Test data:36 14
Test number 9 Finished with status OK| Test data:16 97
Test number 10 Finished with status OK| Test data:0 19
Test number 11 Finished with status OK| Test data:9 88
Test number 12 Finished with status OK| Test data:74 23
Test number 13 Finished with status OK| Test data:19 73
Test number 14 Finished with status OK| Test data:14 96
Test number 15 Finished with status OK| Test data:40 4
Test number 16 Finished with status OK| Test data:11 44
Test number 17 Finished with status OK| Test data:21 53
Test number 18 Finished with status OK| Test data:56 69
Test number 19 Finished with status OK| Test data:92 82
Test number 20 Finished with status OK| Test data:39 68
Test number 21 Finished with status OK| Test data:54 7
Test number 22 Finished with status OK| Test data:35 83
Test number 23 Finished with status OK| Test data:7 93
Test number 24 Finished with status OK| Test data:73 14
Test number 25 Finished with status OK| Test data:91 71
Test number 26 Finished with status OK| Test data:16 74
Test number 27 Finished with status OK| Test data:51 59
Test number 28 Finished with status OK| Test data:56 1
Test number 29 Finished with status OK| Test data:22 73
Test number 30 Finished with status OK| Test data:84 16
Test number 31 Finished with status OK| Test data:64 39
Test number 32 Finished with status OK| Test data:22 77
Test number 33 Finished with status OK| Test data:86 41
Test number 34 Finished with status OK| Test data:45 4
Test number 35 Finished with status OK| Test data:60 30
Test number 36 Finished with status OK| Test data:24 64
Test number 37 Finished with status OK| Test data:47 17
Test number 38 Finished with status OK| Test data:36 76
Test number 39 Finished with status OK| Test data:13 56
Test number 40 Finished with status OK| Test data:62 72
Test number 41 Finished with status OK| Test data:88 56
Test number 42 Finished with status OK| Test data:4 87
Test number 43 Finished with status OK| Test data:15 61
Test number 44 Finished with status OK| Test data:95 50
Test number 45 Finished with status OK| Test data:45 38
Test number 46 Finished with status OK| Test data:34 25
Test number 47 Finished with status OK| Test data:84 73
Test number 48 Finished with status OK| Test data:43 80
Test number 49 Finished with status OK| Test data:67 6
Test number 50 Finished with status OK| Test data:14 81
Test number 51 Finished with status OK| Test data:0 58
Test number 52 Finished with status OK| Test data:20 76
Test number 53 Finished with status OK| Test data:90 32
Test number 54 Finished with status OK| Test data:15 27
Test number 55 Finished with status OK| Test data:96 82
Test number 56 Finished with status OK| Test data:75 35
Test number 57 Finished with status OK| Test data:73 77
Test number 58 Finished with status OK| Test data:47 81
Test number 59 Finished with status OK| Test data:95 51
Test number 60 Finished with status OK| Test data:37 17
Test number 61 Finished with status OK| Test data:66 40
Test number 62 Finished with status OK| Test data:1 18
Test number 63 Finished with status OK| Test data:66 68
Test number 64 Finished with status OK| Test data:52 82
Test number 65 Finished with status OK| Test data:97 46
Test number 66 Finished with status OK| Test data:26 39
Test number 67 Finished with status OK| Test data:83 18
Test number 68 Finished with status OK| Test data:52 4
Test number 69 Finished with status OK| Test data:54 83
Test number 70 Finished with status OK| Test data:10 42
Test number 71 Finished with status OK| Test data:25 99
Test number 72 Finished with status OK| Test data:28 84
Test number 73 Finished with status OK| Test data:86 52
Test number 74 Finished with status OK| Test data:12 78
Test number 75 Finished with status OK| Test data:14 9
Test number 76 Finished with status OK| Test data:32 96
Test number 77 Finished with status OK| Test data:7 97
Test number 78 Finished with status OK| Test data:1 97
Test number 79 Finished with status OK| Test data:70 24
Test number 80 Finished with status OK| Test data:88 17
Test number 81 Finished with status OK| Test data:77 69
Test number 82 Finished with status OK| Test data:50 11
Test number 83 Finished with status OK| Test data:94 99
Test number 84 Finished with status OK| Test data:50 45
Test number 85 Finished with status OK| Test data:55 13
Test number 86 Finished with status OK| Test data:75 74
Test number 87 Finished with status OK| Test data:77 32
Test number 88 Finished with status OK| Test data:27 88
Test number 89 Finished with status OK| Test data:18 62
Test number 90 Finished with status OK| Test data:19 94
Test number 91 Finished with status OK| Test data:49 26
Test number 92 Finished with status OK| Test data:38 67
Test number 93 Finished with status OK| Test data:88 76
Test number 94 Finished with status OK| Test data:78 23
Test number 95 Finished with status OK| Test data:48 15
Test number 96 Finished with status OK| Test data:51 99
Test number 97 Finished with status OK| Test data:9 94
Test number 98 Finished with status OK| Test data:37 90
Test number 99 Finished with status OK| Test data:77 51
Test number 100 Finished with status OK| Test data:1 4

Failed:

Is passed:
#t
```

#### Use another default generator. No shrinking

```Scheme
(define (append-property a b c)
  (equal? (sort-list (append (append a b) c) >) (sort-list (append (append b c) a) >)))

(print-property-test-stats(test-template append-property 3 random-int-list (list 100 1000) 100) #t #f #f #f)

```

```
---Answer Record---
Result: Passed 100 Out of 100
```

#### Error example

```Scheme
(define (append-property2 a b c)
  (equal? (sort-list (append (append a b) c) <) (sort-list (append (append b c) a) >)))

(print-property-test-stats(test-template append-property2 3 random-int-list (list 100 1000) 100) #t #f #t #f)
```

```
---Answer Record---
Result: Passed 0 Out of 100
```

#### Using own generator:

```Scheme
(define (sum-property a b)
  (eqv? (+ a b) (+ b a)))

(define* (custom-generator maxi #:optional (random-state 1000))
  (random (+ (- maxi 10) 10)))

(print-property-test-stats(test-template sum-property 2 custom-generator (list 100) 100))
```

```
---Answer Record---
Result: Passed 100 Out of 100
```

#### using default-shrinker



#### using custom-generator and default-shrinker/no-shrinking comparasion

```Scheme
(define (rb-bag-prop a b c)
  (let ([A (rbmset-fill #f a)]
        [B (rbmset-fill #f b)]
        [C (rbmset-fill #f c)])
  (compare (union-rbmset (union-rbmset A B ) C) (union-rbmset (union-rbmset B C) A))))

(define (my-parse-args lst params-to-change)
    (define (my-parse-args-sub lst params-to-change fnl-lst)
      (cond
        [(eqv? params-to-change 0) fnl-lst]
        [else
          (cond
            [(> (car lst) 0) (my-parse-args-sub (cdr lst) (- params-to-change 1) (append fnl-lst (list (floor(/ (car lst) 2)))))]
            [(< (car lst) 0) (my-parse-args-sub (cdr lst) (- params-to-change 1) (append fnl-lst (list (floor(/ (car lst) 2)))))]
            [else (my-parse-args-sub (cdr lst) (- params-to-change 1) (append fnl-lst (list (car lst))))])]))
    (my-parse-args-sub lst params-to-change '()))

(define (lab-shrinker answer limit params-to-change cycle)
  (cond
    [(eq? answer-status #t) answer])
  (cond
    [(eqv? cycle 0) answer]
    [else
      (let ([run-tst (test-template
        (answer-user-property answer)
        (answer-args-amount answer)
        (answer-generator answer)
        (my-parse-args
          (answer-generator-args answer) 
          params-to-change)
        limit)])

        (cond 
          [(and (eq? (answer-status answer) #f) (eq? (answer-status run-tst) #t)) answer]
          [else
            (lab-shrinker run-tst limit params-to-change (- cycle 1))]))]))

(print-property-test-stats(lab-shrinker (test-template rb-bag-prop 3 random-int-list (list 100 100) 10) 100 2 1000) #t #f #t)

```

```
---Answer Record---
Result: Passed 0 Out of 100
Failed:
162 20 276 135 884 447 923 387 6 10 533 226 813 719 555 262 224 399 686 326 531 127 421 294 711 298 36 374 724 791 596 210 571 627 218 244 632 845 592 58 640 270 541 927 373 338 763 335 434 57 422 311 706 613 394 62 702 227 848 683 932 692 197 490 768 566 889 235 686 78 787 857 534 797 857 601 434 743 171 380 947 616 312 794 767 970 159 573 862 909 942 302 827 299 148 190 443 280 470 70  753 975 638 442 770 315 498 961 29 871 219 186 802 599 50 788 107 833 14 874 728 189 395 159 496 543 275 662 399 916 331 860 346 918 723 442 315 813 544 860 968 937 728 785 625 121 346 375 915 769 402 306 690 425 910 600 360 147 271 924 873 973 942 637 331 388 618 31 138 372 296 900 101 160 947 987 105 434 625 620 363 956 76 216 857 186 421 84 893 890 696 954 354 245 403 602 217 419 115 161  116 76 258 676 42 673 141 84 594 692 607 380 761 857 401 332 668 356 667 729 715 30 892 838 356 580 168 688 631 687 379 121 892 156 389 648 170 733 905 992 124 87 986 639 258 454 703 697 215 671 771 510 639 873 911 600 599 271 421 672 760 198 964 198 65 122 13 608 846 106 743 145 791 786 949 768 234 733 747 375 507 55 482 911 175 194 265 216 38 18 895 873 788 658 118 111 801 746 711 972   
814 611 921 642 396 660 95 85 637 93 78 728 486 330 285 191 909 85 339 293 771 698 932 489 240 357 563 213 288 579 751 433 604 201 567 410 985 260 543 532 377 682 2 522 586 22 353 473 947 609 942 360 704 657 92 257 890 458 812 913 724 629 666 519 295 269 438 471 622 111 81 683 871 249 666 489 28 457 501 566 892 599 847 258 147 244 728 699 465 812 745 893 463 848 483 707 911 996 20 386  346 493 68 861 580 517 487 642 361 329 640 504 215 733 62 216 324 107 400 82 688 576 410 136 657 210 493 555 161 575 857 386 215 265 157 176 431 745 476 12 643 364 349 410 182 46 747 677 96 375 438 737 377 49 950 474 324 458 231 428 739 348 539 214 613 467 460 920 292 438 974 214 886 597 619 850 780 771 846 501 92 688 843 649 216 474 119 552 454 726 376 102 137 244 232 594 654 99 716 232  977 874 389 867 693 616 548 524 597 705 159 32 321 871 41 682 269 124 223 392 831 991 777 198 866 44 499 32 318 154 232 828 846 73 380 531 275 147 279 367 932 473 264 82 204 48 917 847 526 836 550 745 226 953 611 667 431 924 839 688 31 97 526 28 108 891 690 665 422 65 180 764 40 523 194 661 556 526 566 378 719 895 104 681 188 991 733 327 897 753 706 633 66 312 594 310 6 152 576 253   
819 163 173 428 952 311 813 784 804 601 362 943 420 237 558 619 728 121 606 857 244 858 653 398 614 101 44 918 837 212 748 175 16 418 593 162 736 398 541 508 80 559 140 837 663 497 881 908 734 480 764 453 81 5 699 22 943 244 888 868 815 310 678 495 962 462 706 908 782 904 805 280 209 10 84 835 747 101 969 505 93 772 989 461 731 154 775 216 189 408 852 48 975 176 663 716 269 510 255 45  493 491 793 52 429 157 781 647 332 514 220 638 607 952 733 147 798 54 245 864 721 18 147 369 530 191 523 491 391 331 840 78 156 905 44 171 678 628 940 889 876 133 384 428 378 294 270 454 465 496 214 309 296 518 963 127 748 898 249 485 51 913 0 719 702 701 552 295 195 487 112 372 781 26 812 43 408 0 766 540 340 153 191 390 567 579 480 591 496 64 47 227 722 326 238 173 877 342 93 246  368 353 55 289 954 633 224 702 515 444 549 267 957 557 549 582 853 381 926 41 525 544 908 819 101 617 813 327 218 720 833 713 576 494 552 885 909 455 45 287 147 847 555 74 958 489 199 521 778 454 705 891 202 212 209 545 225 164 454 507 126 426 934 584 578 403 8 466 302 301 105 378 28 961 559 938 114 171 845 992 841 818 396 834 26 287 517 811 12 571 652 196 297 725 690 427 901 97 366 689   
695 398 805 662 861 383 593 306 106 156 990 389 570 28 562 423 718 924 386 985 83 337 4 799 318 350 849 525 438 776 45 258 92 149 106 806 903 638 264 21 439 652 240 29 124 458 601 646 385 223 152 541 793 921 871 484 819 936 210 214 722 294 754 427 817 505 994 811 969 285 732 917 648 419 253 948 966 363 932 533 261 872 660 338 567 589 908 325 283 151 246 903 866 299 626 653 436 691 21 987  879 492 231 752 77 282 582 583 334 15 549 279 430 891 477 986 761 493 448 741 199 667 124 705 244 24 690 556 797 175 926 832 404 257 742 600 342 491 700 662 20 917 776 326 39 264 409 209 456 140 694 374 369 326 508 102 250 695 239 406 46 4 252 822 651 242 223 306 539 308 316 681 797 345 240 196 320 829 112 827 313 115 542 254 720 95 924 631 951 141 116 512 480 883 820 905 507 340 818 963  974 151 556 992 81 814 590 142 541 485 392 516 475 54 748 422 772 133 478 884 451 429 555 568 523 850 497 264 806 450 560 637 342 48 550 484 27 376 222 854 618 412 709 990 944 511 655 894 211 6 592 862 899 595 626 576 128 749 3 816 105 18 372 85 903 449 960 794 961 631 835 658 759 259 555 45 324 545 340 211 931 454 100 631 737 688 239 969 634 455 563 232 114 751 884 24 60 21 670 133   
259 379 211 326 246 964 144 269 242 567 553 480 44 906 575 569 904 766 202 182 975 178 721 390 846 271 314 23 25 76 244 783 247 164 317 547 741 928 527 861 597 535 239 403 252 499 773 339 510 915 56 865 627 243 37 249 984 24 751 939 866 176 108 266 583 51 5 823 763 180 215 706 464 363 2 230 830 99 61 141 491 140 539 25 605 699 823 626 625 25 637 645 385 426 737 324 739 565 297 398  971 836 455 722 964 766 731 846 136 752 769 848 337 314 760 980 380 791 432 121 429 738 79 959 308 734 131 764 210 974 16 980 27 212 489 173 623 561 604 250 535 770 747 786 759 52 404 834 989 75 769 734 961 616 230 440 404 648 129 989 407 314 727 36 160 730 359 412 249 392 887 715 169 648 81 709 53 676 651 156 643 934 39 907 363 882 771 780 513 953 738 538 590 11 232 679 231 599 852 816  656 512 639 533 967 285 331 979 14 250 745 295 298 264 312 106 555 441 193 463 351 716 857 434 429 453 324 235 748 322 692 32 292 302 124 570 340 480 630 41 323 677 181 868 638 854 409 102 265 177 970 315 446 942 416 679 486 854 959 744 105 471 719 255 758 315 543 968 522 2 296 227 87 282 439 119 706 715 511 449 472 528 572 367 664 263 586 762 942 617 418 132 231 729 287 225 170 76 410 944   
635 723 693 377 37 82 318 475 441 775 646 27 891 185 2 903 959 479 838 934 360 158 231 175 692 734 422 211 435 528 183 443 2 389 395 87 740 98 803 126 521 445 637 939 898 664 592 720 141 540 257 387 820 644 27 399 465 165 467 71 186 408 535 897 784 338 257 442 411 225 483 245 91 105 724 88 792 43 379 425 852 273 272 898 64 410 892 471 313 76 287 186 412 451 373 232 461 488 247 355  180 490 102 278 795 192 974 991 912 682 614 702 929 204 854 156 109 532 498 430 164 520 887 708 547 262 328 20 458 15 225 635 841 606 157 136 21 963 226 484 290 648 651 890 957 948 99 375 848 149 683 432 92 575 519 314 522 362 80 803 27 56 991 777 321 829 373 399 784 3 867 684 164 231 595 125 449 14 294 335 536 443 859 946 205 301 486 980 513 483 444 96 94 203 117 412 889 113 884 613  826 291 943 784 399 755 240 717 843 298 717 420 387 990 938 345 908 550 545 35 208 775 860 761 478 875 904 852 111 14 972 79 179 54 303 365 260 879 254 766 800 652 887 303 852 471 944 649 668 535 935 15 476 92 678 897 887 512 609 809 681 945 640 619 882 640 71 293 26 840 386 210 706 420 914 97 284 746 379 459 26 487 939 232 705 476 594 781 361 228 53 107 661 68 150 193 511 75 69 278   
642 50 61 451 484 892 777 962 117 156 443 333 181 837 433 702 161 824 285 191 647 375 461 537 307 38 715 208 771 26 187 423 87 366 210 735 728 645 466 292 385 885 917 225 921 863 529 801 464 860 434 300 650 578 380 142 314 793 674 208 68 285 486 623 393 692 482 559 763 867 886 243 898 280 513 395 72 67 487 784 817 969 477 578 608 57 599 543 836 458 0 496 319 625 442 338 68 575 301 139  599 561 361 667 567 681 515 665 199 213 396 625 820 302 920 210 564 266 972 918 30 922 331 996 791 836 394 680 396 931 341 739 919 661 832 351 710 770 984 236 19 777 651 273 991 418 494 983 598 411 253 7 468 985 816 546 526 66 68 848 123 523 892 751 856 19 750 855 708 475 848 339 224 1 294 221 63 375 507 281 214 744 853 153 445 120 286 285 860 147 250 640 487 997 528 253 709 441 758 931  939 989 917 206 939 894 513 987 94 226 46 658 502 334 555 806 255 35 278 451 821 415 378 981 59 425 729 939 985 339 844 420 840 208 391 590 394 196 775 401 756 361 705 355 703 477 587 462 283 329 437 116 956 140 723 114 232 340 749 159 537 657 274 209 491 394 386 558 212 857 93 543 748 258 939 857 564 274 904 672 68 521 693 879 971 779 564 375 400 478 704 177 644 582 120 524 624 319 559 367   
469 679 70 938 840 102 539 283 564 587 541 28 334 571 129 976 506 190 892 689 8 597 790 318 31 933 909 837 713 492 619 254 474 123 140 686 202 114 369 937 317 969 45 539 173 357 269 544 737 865 84 159 868 786 74 717 500 987 676 102 42 985 766 63 53 719 483 992 842 803 719 670 673 357 776 784 317 740 295 282 747 287 336 431 25 127 502 42 980 731 655 758 165 302 423 950 564 456 342 587  621 331 434 992 100 135 850 711 886 699 735 479 594 271 712 988 655 59 542 826 677 2 926 349 738 156 554 91 479 579 780 310 505 869 433 109 53 675 456 143 957 308 730 662 869 243 449 736 999 571 651 163 400 752 567 583 778 648 813 372 667 588 854 376 928 550 208 40 770 588 239 498 296 524 579 60 887 947 86 233 165 997 629 758 160 931 959 109 313 229 237 635 28 815 318 541 102 6 793 297  323 158 118 555 475 994 380 541 175 783 987 714 310 287 479 862 97 866 823 330 195 868 269 616 164 8 817 923 780 317 203 531 629 461 682 584 204 907 894 277 719 637 911 735 777 685 52 755 134 604 245 371 522 901 338 449 584 603 202 40 55 310 482 820 819 911 389 113 938 27 175 504 636 508 821 232 58 670 548 565 334 134 582 390 996 490 571 179 461 592 320 592 83 355 371 553 190 351 954 329   
453 736 702 437 532 792 878 949 816 870 505 947 383 190 585 119 259 594 779 513 504 737 850 319 808 0 924 632 908 108 660 585 272 510 204 285 786 120 228 926 760 226 890 768 135 652 420 70 723 631 726 191 221 259 417 88 480 544 237 371 111 411 11 892 465 740 79 874 896 508 961 842 405 713 795 384 157 4 522 878 179 743 704 529 36 173 422 580 301 994 163 837 125 0 561 209 878 841 573 951  487 397 709 449 467 979 622 887 901 817 461 426 59 114 618 204 648 805 727 40 128 168 291 465 840 760 330 970 9 862 893 501 182 534 643 410 357 252 881 192 888 864 280 532 106 538 61 82 923 568 184 719 46 246 344 610 289 909 554 490 329 68 332 976 883 685 356 960 595 131 344 366 445 719 270 713 788 607 403 280 537 236 485 244 472 812 617 96 561 985 625 595 519 707 644 838 432 128 850 748  277 554 97 807 741 86 752 721 451 209 821 903 891 832 88 516 123 221 72 269 168 687 83 36 831 879 790 726 610 17 459 314 918 960 885 99 325 616 688 959 429 228 780 998 171 804 551 482 627 180 793 991 498 824 355 181 177 915 215 77 128 989 626 700 524 273 282 974 173 226 924 491 163 746 104 270 918 407 636 935 458 271 135 339 151 53 763 919 371 347 658 595 156 513 242 880 889 247 829 756   
999 102 881 193 372 383 139 675 483 166 257 626 476 427 967 656 259 421 65 283 299 666 903 487 819 566 146 271 227 54 438 48 116 275 320 465 145 558 303 156 798 487 460 1 325 217 968 456 119 927 384 857 550 919 438 426 871 25 792 721 326 511 872 939 278 488 150 20 409 329 785 722 471 906 489 708 767 686 861 628 712 204 786 523 139 611 596 927 661 499 542 235 467 685 838 24 601 861 708 244  666 662 146 747 30 556 790 550 603 243 628 229 543 471 480 266 24 246 419 458 211 26 163 944 510 272 381 93 888 191 968 567 434 420 992 685 185 893 785 618 640 52 556 712 502 317 630 281 651 164 306 585 677 235 881 137 81 200 695 117 660 840 205 345 47 429 671 306 122 962 49 239 213 941 420 229 492 600 749 942 577 544 234 507 5 358 465 277 596 866 175 144 736 201 380 240 583 491 204 573  7 181 298 816 183 659 75 538 790 285 102 385 374 610 53 46 676 590 382 451 724 970 892 157 447 919 204 586 857 359 633 361 914 513 898 405 594 30 843 667 913 670 725 374 916 275 67 867 393 779 259 739 816 34 580 104 90 644 855 753 836 370 904 700 93 118 207 674 389 21 110 632 30 361 950 256 246 618 88 674 276 593 819 19 798 255 889 659 838 911 961 621 519 695 980 545 560 40 302 862   
108 186 967 233 616 736 517 877 647 23 961 189 488 474 334 760 362 368 396 963 899 59 89 482 606 699 925 158 545 701 22 902 585 171 184 120 851 803 597 598 765 990 950 786 754 893 345 599 280 627 354 716 80 585 509 707 199 53 785 728 641 806 685 374 184 100 511 463 458 648 726 824 819 94 971 342 769 468 133 732 738 587 826 708 814 788 476 38 862 346 266 843 181 559 148 450 96 18 911 587  373 517 673 760 856 369 943 183 465 955 582 673 462 227 106 914 364 222 543 752 235 781 339 979 154 169 894 106 866 592 716 547 365 558 519 61 110 734 330 688 151 35 401 512 972 478 418 456 315 189 20 139 606 814 869 847 129 84 90 427 172 374 778 7 406 883 434 577 177 636 96 592 569 68 175 540 13 230 869 790 238 689 998 274 384 880 514 24 155 683 352 439 727 904 366 505 619 372 343 714  189 408 657 668 380 979 256 617 548 642 925 329 799 115 468 84 235 18 769 301 927 791 824 798 112 858 530 205 848 180 524 312 270 114 77 780 167 992 17 315 127 255 605 350 508 424 136 754 444 862 899 510 498 2 965 343 913 883 728 983 55 357 156 815 97 887 204 807 156 682 934 450 325 912 914 736 455 591 206 173 217 824 386 379 310 379 317 115 245 605 504 69 309 682 91 593 676 125 276 968   
342 162 960 487 39 841 638 114 455 37 386 508 303 495 221 286 147 709 901 517 113 243 80 870 575 225 597 994 599 549 153 107 652 662 369 862 441 615 496 717 906 331 446 7 929 362 885 693 96 414 818 356 551 537 422 94 509 54 193 880 74 427 893 309 26 121 749 46 710 156 811 36 565 743 280 241 165 563 741 877 629 225 458 306 305 357 474 641 75 141 734 609 246 121 364 0 133 512 41 594  476 315 97 359 284 292 84 642 963 723 592 890 231 216 157 623 713 358 329 558 581 169 79 578 790 696 371 755 124 861 889 508 785 771 375 353 402 390 445 64 343 835 90 405 764 664 667 228 29 957 181 989 709 400 220 499 736 855 236 591 158 877 662 127 343 442 711 397 128 820 572 824 344 331 27 240 395 339 955 607 744 107 81 693 556 582 395 887 765 748 792 100 653 946 114 88 252 796 724 20  424 216 541 842 279 35 96 657 639 543 270 441 589 929 677 488 569 817 103 269 939 790 548 266 538 205 674 586 460 566 596 988 379 734 638 750 292 463 893 799 274 155 171 323 858 693 700 731 320 689 462 923 820 564 195 818 952 132 431 254 134 480 45 656 154 174 497 851 276 539 672 207 227 228 459 39 481 845 443 360 564 884 406 42 40 622 825 829 911 144 503 323 704 735 697 358 529 224 964 834   
791 644 972 559 435 496 465 678 17 36 908 18 112 741 85 910 383 28 255 197 802 106 754 742 927 888 252 46 855 244 946 242 377 810 788 211 876 48 920 457 109 463 773 700 9 989 290 666 411 125 17 58 702 391 909 949 785 824 288 78 593 781 789 743 682 348 383 913 71 280 1 216 823 563 309 761 441 932 398 111 236 687 986 654 790 237 789 719 729 992 579 72 308 881 898 461 692 474 212 861  519 800 870 786 182 208 969 413 784 56 761 363 461 731 993 499 162 585 318 620 265 266 660 899 869 141 291 703 883 204 832 946 138 816 239 48 706 994 74 577 920 745 312 901 677 913 609 53 405 986 309 298 345 906 868 92 864 591 11 986 714 991 205 276 851 552 733 242 256 378 176 859 171 360 186 11 362 500 526 57 591 972 274 212 121 912 503 138 885 771 962 251 516 625 984 744 949 261 476 201  63 94 983 348 825 365 600 590 503 604 42 469 859 738 500 778 727 993 346 853 949 530 895 524 934 22 580 918 82 721 289 0 690 49 265 350 382 214 478 118 252 145 306 836 187 198 231 724 221 713 800 554 502 693 117 635 249 884 732 63 305 366 821 848 443 681 259 475 306 997 524 862 465 145 787 420 778 40 26 856 513 412 822 631 668 648 246 851 517 37 957 111 692 632 128 803 923 797 199 852   
465 571 238 861 675 263 180 34 932 139 493 851 966 321 873 28 644 67 548 802 805 281 322 994 173 972 266 648 535 864 910 588 469 522 768 241 295 387 1 884 798 658 602 704 582 999 876 732 162 277 629 150 499 19 118 660 372 985 547 503 138 325 35 185 898 534 335 348 866 878 299 739 887 669 510 317 247 384 540 194 448 867 697 893 703 646 757 378 420 197 882 475 143 725 651 19 82 907 156 862  747 122 437 869 327 750 386 89 751 342 242 928 290 470 1 560 900 425 888 855 96 267 778 195 319 210 862 439 273 520 222 352 506 936 203 145 707 37 240 949 802 903 894 960 16 515 740 545 842 348 921 963 34 250 336 435 919 237 0 869 975 503 892 29 938 21 456 68 630 392 452 737 711 699 222 740 613 680 783 894 122 211 307 727 640 822 293 521 644 306 669 963 800 558 361 653 352 952 573 155  984 443 702 624 364 606 375 131 693 309 90 961 305 20 641 938 507 371 999 551 885 564 822 361 481 469 298 973 452 795 327 524 146 492 599 283 733 320 181 16 981 187 962 828 933 192 157 984 310 144 629 581 459 713 353 138 25 971 15 414 318 780 854 727 231 578 335 41 715 882 920 268 621 605 583 445 253 943 445 984 362 997 291 446 318 665 244 918 246 635 709 383 627 511 406 593 923 776 859 708   
386 254 918 324 840 958 280 465 591 467 985 611 421 399 226 313 838 470 861 887 809 778 673 420 686 203 405 766 39 887 965 315 92 495 853 98 539 688 151 602 290 882 315 481 709 668 220 181 531 659 55 891 340 314 597 353 75 898 303 657 690 265 485 108 406 327 189 849 26 340 981 820 151 310 892 73 120 797 555 196 774 95 78 650 820 550 499 531 242 167 857 162 621 776 373 719 705 918 557 781  785 210 608 954 724 710 988 233 255 423 573 39 859 568 653 161 943 712 642 685 178 622 813 713 564 710 772 401 744 493 99 312 220 323 521 987 340 103 180 396 659 403 614 771 884 110 177 913 173 300 68 685 184 885 763 218 608 159 172 436 712 400 956 639 924 898 891 33 539 569 238 927 50 748 626 877 168 933 7 870 985 443 53 421 241 957 95 706 187 329 865 517 18 177 441 660 85 988 787 340  159 216 605 552 428 811 918 979 56 659 530 358 560 783 321 906 440 666 610 200 398 205 984 526 111 371 100 835 456 691 241 898 780 343 618 608 905 540 633 634 912 58 364 218 546 902 46 95 115 65 169 644 340 630 930 921 106 248 343 645 32 431 359 659 442 275 775 638 34 561 169 128 276 728 72 397 947 40 470 721 799 961 236 902 761 442 244 456 205 308 211 150 299 982 607 571 701 389 538 502   
431 634 90 795 423 405 535 168 431 920 781 566 335 457 58 142 518 33 211 107 200 675 601 372 963 762 423 189 607 153 404 279 348 373 788 21 627 403 280 552 701 980 920 703 366 152 316 689 938 589 267 585 839 785 60 798 390 749 344 958 636 567 838 621 534 426 920 181 0 191 186 520 582 984 314 43 410 343 415 178 604 798 901 778 962 181 693 222 968 748 520 507 566 0 358 431 242 73 775 390  20 376 143 348 310 976 381 790 822 51 942 864 433 351 767 160 787 627 937 119 872 314 547 649 305 326 282 798 715 203 30 543 285 921 41 733 107 479 152 899 836 638 879 22 664 712 908 731 906 276 138 98 847 541 540 311 440 197 973 354 369 787 978 431 945 800 214 858 101 693 374 579 744 139 125 595 592 560 493 914 41 800 870 896 486 959 793 488 450 554 115 558 411 518 45 491 361 957 81 757  16 818 482 952 848 805 533 840 168 886 105 552 518 135 143 289 643 395 371 139 599 944 443 142 324 761 599 402 646 795 566 504 755 338 977 177 829 559 748 124 576 14 606 915 960 135 472 575 906 437 816 210 291 371 491 859 437 34 709 496 276 596 180 188 66 589 316 988 392 712 545 383 318 861 421 2 683 28 934 645 130 685 825 423 767 370 967 486 420 276 429 757 278 270 614 678 602 645 367 59   
105 743 841 324 917 660 254 713 81 841 256 807 722 34 520 10 23 251 29 543 718 822 947 726 215 96 851 976 770 13 11 986 450 269 571 747 431 413 440 194 65 480 25 485 399 887 686 249 255 239 656 950 346 296 247 438 643 129 736 474 934 617 782 590 311 573 748 316 485 747 385 3 602 472 210 40 143 760 819 653 604 583 77 890 590 739 515 678 620 146 475 11 670 557 146 563 975 361 447 740  904 851 238 84 655 841 278 835 664 274 996 753 243 575 0 197 700 940 306 507 989 148 196 843 900 71 874 515 319 482 484 204 595 914 406 538 322 197 160 250 68 341 126 630 942 162 214 894 406 8 25 766 8 12 504 43 763 853 159 664 24 207 103 287 869 850 863 516 700 200 959 90 979 404 757 816 244 99 347 602 87 973 959 703 292 329 249 170 696 402 147 202 109 680 383 260 440 811 898 427  190 22 572 100 468 331 409 211 609 164 115 504 993 236 679 102 934 789 674 965 166 567 648 853 644 737 594 817 133 920 414 625 836 878 653 479 259 4 801 397 728 587 279 695 24 587 572 430 978 73 332 85 638 755 740 8 937 651 78 967 502 150 991 972 45 756 868 572 506 836 128 618 120 909 31 880 652 619 356 883 866 211 581 783 784 71 39 88 356 731 229 90 74 155 255 820 807 811 228 160   
799 300 831 807 376 145 920 587 168 204 451 669 567 750 397 779 598 378 344 139 322 298 450 306 992 622 604 528 618 889 211 458 189 701 628 582 199 506 422 940 530 78 684 688 988 472 894 504 327 62 27 648 694 744 871 2 982 445 685 68 183 309 288 15 262 85 65 928 516 145 396 639 201 863 39 842 593 958 191 428 245 803 352 700 975 780 114 351 349 865 236 25 775 301 703 563 587 6 730 669  250 890 373 360 129 142 678 244 782 369 134 523 89 545 981 420 57 262 942 505 835 252 517 720 626 769 206 615 398 354 185 351 679 513 38 449 281 653 487 638 384 459 357 649 796 247 255 99 891 561 767 529 104 923 462 722 145 486 837 966 614 229 461 279 797 393 908 349 269 251 653 601 32 998 106 129 843 646 67 741 661 714 417 13 482 388 258 291 112 324 3 489 628 714 817 239 861 479 692 403  374 212 722 103 145 900 6 564 573 839 944 744 694 519 668 182 317 122 19 155 733 125 909 882 272 883 128 756 996 313 624 612 289 725 808 213 407 916 320 498 424 130 396 293 572 104 407 453 62 589 554 522 972 959 603 618 498 507 511 602 204 74 474 955 438 490 417 348 250 771 307 244 793 796 616 25 633 485 954 393 798 728 135 15 311 597 235 758 717 373 997 123 305 660 819 322 351 137 203 945   
466 336 154 598 720 524 327 73 727 487 541 305 927 617 466 589 285 39 845 645 897 919 736 788 78 571 293 737 8 858 487 979 325 982 966 517 96 349 750 234 135 655 415 353 839 9 260 173 543 737 809 505 761 688 686 705 446 56 758 31 488 556 672 727 453 980 281 541 191 665 22 4 624 203 883 969 640 343 953 68 541 222 194 838 838 484 731 826 276 302 311 86 191 709 864 838 924 666 340 556  929 271 766 448 941 989 566 437 144 68 805 930 614 683 0 178 241 217 59 356 161 357 466 646 476 665 769 725 778 734 202 304 739 220 312 458 133 442 942 353 468 757 858 169 364 384 378 752 86 363 571 159 839 7 18 928 241 999 217 994 79 49 560 81 859 199 387 816 967 74 682 696 702 206 429 892 798 515 126 825 635 706 430 449 137 748 831 185 855 468 780 236 381 870 641 283 947 210 40 242  464 899 743 432 337 889 888 524 240 555 108 592 123 402 555 531 480 343 545 367 251 479 101 494 559 601 972 682 631 114 800 153 913 330 781 328 864 850 409 228 254 238 912 557 889 764 264 285 107 223 648 821 188 251 296 295 614 335 359 592 419 57 500 907 721 443 749 902 61 188 790 673 170 312 564 63 88 446 445 133 981 598 213 684 598 77 342 446 771 361 744 154 736 48 98 873 315 473 353 665   
386 814 504 770 419 92 346 479 721 304 470 183 300 173 10 934 362 478 188 149 983 588 478 53 328 918 78 584 565 294 363 421 316 675 909 316 678 503 935 170 234 832 67 15 70 456 44 899 924 467 762 815 32 18 372 527 891 590 128 827 612 309 424 745 668 513 890 264 122 925 751 444 361 103 29 460 391 435 679 822 227 888 549 239 231 879 116 450 923 324 274 494 714 492 920 716 922 859 874 587  763 356 914 874 747 536 268 175 995 129 87 509 668 476 398 682 262 777 700 799 746 502 962 273 772 67 870 621 41 81 314 144 20 443 16 762 28 891 120 766 832 151 338 17 375 985 759 714 636 876 3 617 988 885 674 413 21 322 317 33 27 350 913 612 439 505 526 44 121 336 698 43 855 901 413 54 2 649 839 490 326 581 383 984 73 633 363 791 944 293 509 1 993 978 507 15 523 719 34 257  772 713 649 197 778 253 765 586 89 262 811 71 552 391 544 506 876 267 366 56 895 14 142 545 563 919 114 271 526 848 94 835 582 908 317 326 94 884 524 379 539 868 350 878 844 524 524 67 984 799 516 339 402 173 333 179 364 711 531 197 878 496 898 937 755 177 471 307 847 473 205 219 581 885 650 554 867 104 137 306 602 803 656 544 842 159 907 750 305 128 430 721 649 255 649 107 829 778 250 147   
612 557 253 530 866 745 48 928 454 260 155 335 538 725 741 545 814 402 60 667 754 442 957 758 467 870 810 681 534 933 174 983 573 929 440 847 276 383 871 208 769 345 822 33 100 0 203 278 398 119 540 591 752 198 699 604 857 47 931 53 665 681 213 459 77 458 585 183 284 163 195 255 984 559 894 350 723 354 148 290 934 843 852 209 671 387 40 444 168 546 856 541 666 838 861 514 567 105 969 879  929 754 420 828 937 53 644 898 121 960 932 977 203 243 504 398 991 593 406 279 791 176 780 620 710 561 62 862 123 316 539 470 162 846 791 921 493 706 753 361 81 115 377 477 972 974 838 148 581 883 359 677 681 116 599 673 26 694 147 718 116 2 257 254 959 946 464 805 590 849 577 295 454 922 27 831 703 691 910 393 944 850 939 423 444 558 746 536 88 924 149 652 297 233 624 226 635 901 36 510  867 734 777 207 42 762 944 242 741 336 582 28 230 4 903 706 533 168 294 471 94 608 834 620 882 654 172 977 275 234 752 780 458 916 787 823 164 369 720 636 654 688 741 548 365 103 282 657 149 210 488 206 403 115 718 722 309 224 656 951 235 757 976 606 51 658 531 882 760 567 199 970 654 65 913 16 918 278 148 676 393 214 646 198 178 185 405 745 621 362 540 174 681 755 656 251 804 60 63 144   
861 194 81 95 457 779 605 240 326 328 64 736 617 759 924 721 206 718 447 206 154 90 902 599 115 96 915 550 90 908 998 534 536 708 670 110 277 12 886 936 524 596 228 66 855 524 550 459 282 598 301 994 62 293 389 599 241 945 690 455 146 55 542 495 408 272 745 819 559 849 413 840 789 148 527 912 179 638 419 104 354 202 746 938 687 992 382 376 8 139 723 858 247 93 123 511 340 652 676 991  433 366 268 967 608 192 843 807 573 107 89 967 126 863 512 921 625 566 751 53 854 213 589 883 407 932 39 636 592 82 385 614 696 202 359 984 331 183 996 955 753 371 657 377 973 452 499 222 74 463 820 869 396 297 503 431 582 395 582 678 339 536 231 835 636 258 600 241 60 443 506 552 144 797 420 869 849 248 727 75 857 800 980 487 87 118 71 850 335 540 288 841 5 948 111 522 283 269 22 561  753 496 701 112 65 252 865 288 397 378 110 340 79 36 784 10 63 981 445 303 975 61 539 841 721 312 73 859 978 736 257 146 379 723 302 431 626 353 554 662 231 128 917 940 635 371 2 967 873 433 387 715 886 688 620 705 651 625 668 563 258 882 729 112 293 956 64 479 661 699 154 327 900 418 475 235 571 186 44 531 783 654 103 243 737 389 772 515 349 831 870 380 222 467 615 741 788 703 67 951   
959 537 844 689 289 759 801 719 79 577 316 186 665 58 557 116 564 859 658 428 734 527 971 284 725 718 222 3 891 600 454 694 125 86 129 438 61 388 898 71 155 718 434 6 514 190 201 920 129 298 697 7 933 264 907 523 537 542 429 817 330 95 412 318 776 956 156 320 862 71 87 265 379 205 631 815 534 758 986 252 990 230 153 678 124 833 293 158 651 323 544 29 487 73 924 1 946 915 667 695  975 614 635 26 811 253 639 554 515 466 469 357 451 367 921 968 586 742 823 613 196 419 211 848 846 61 213 305 602 455 185 191 934 439 569 388 406 737 133 973 554 808 906 740 507 64 735 295 86 840 135 906 591 243 78 600 622 695 564 19 634 935 114 872 923 914 59 351 640 257 14 169 137 684 731 302 537 232 613 366 716 705 635 336 905 222 409 422 293 599 854 63 778 447 78 277 801 552 860 865  441 648 9 368 371 727 406 949 118 652 470 184 133 600 498 880 964 345 583 54 98 179 88 380 707 990 30 921 199 40 353 672 849 812 302 359 636 791 519 965 566 228 137 222 997 836 949 964 154 759 303 41 305 286 522 625 959 117 648 572 196 402 339 955 506 436 616 485 394 455 6 576 642 362 928 912 265 19 769 221 24 521 970 24 134 457 86 650 716 246 966 865 90 722 592 235 112 88 908 588   
54 275 61 176 761 837 148 442 313 7 980 432 796 700 386 422 358 253 704 78 134 733 793 104 498 644 408 806 672 147 608 807 253 279 415 56 910 62 17 502 519 685 512 243 230 201 388 734 762 912 169 477 9 258 23 495 135 641 896 609 570 511 152 335 891 600 167 196 50 226 836 292 374 839 381 969 524 910 805 138 842 213 284 662 11 197 324 319 283 797 592 101 929 628 165 167 419 329 941 810  602 300 520 664 664 839 987 98 737 433 481 18 619 619 75 811 267 186 487 732 667 19 686 252 699 509 229 175 156 993 702 100 66 708 918 856 402 815 998 442 643 333 985 779 715 432 831 295 675 703 248 414 465 141 183 253 236 716 651 61 438 533 612 101 580 417 291 521 360 276 58 983 721 253 838 385 14 699 432 992 455 13 966 151 541 324 393 368 141 974 547 791 923 486 304 180 855 557 879 806  183 335 410 92 464 374 24 582 758 658 230 244 925 462 994 912 185 799 210 46 729 219 720 933 532 298 841 157 915 946 409 445 759 409 604 164 171 580 59 86 694 87 340 27 103 919 719 400 901 362 426 609 623 226 284 886 488 770 614 756 854 114 745 968 573 814 64 941 704 195 572 290 863 55 481 68 417 785 312 974 367 584 408 990 913 450 8 416 522 255 55 271 683 556 563 816 417 886 123 932   
245 126 412 82 102 491 285 423 554 614 855 281 37 55 390 33 127 123 820 627 257 508 374 768 387 75 57 625 700 991 268 771 539 190 600 322 901 330 242 95 87 227 990 761 698 181 800 52 623 777 927 797 261 422 943 629 226 958 119 759 348 968 663 987 493 321 39 456 605 938 188 120 732 759 624 585 316 168 647 800 473 349 651 890 193 249 849 245 636 377 881 670 690 210 796 212 249 590 325 468  278 867 561 654 914 129 155 208 602 352 944 859 231 557 392 284 100 607 721 409 843 693 309 392 727 529 148 159 641 270 304 693 991 700 787 517 697 246 482 216 976 770 373 454 761 799 800 530 472 415 33 596 205 25 554 310 86 637 239 804 400 613 173 646 584 145 553 378 873 267 650 859 569 385 73 46 135 467 720 205 421 798 742 66 153 265 745 179 775 974 106 947 745 804 67 141 172 824 840 1  278 82 370 448 820 233 88 739 898 295 586 994 82 743 300 564 208 505 49 748 549 927 763 303 130 433 808 573 60 156 955 721 376 385 360 497 771 716 632 225 873 158 293 680 785 103 422 248 381 95 714 918 44 481 745 770 392 200 912 71 2 368 140 394 861 889 497 801 200 439 89 173 5 347 375 270 526 763 830 738 709 506 97 984 481 505 99 679 573 955 484 479 515 837 507 565 416 819 306 361   
327 113 18 953 59 927 923 162 769 626 728 858 313 648 102 15 187 157 414 935 324 109 108 52 427 282 97 218 197 883 991 976 881 789 325 120 357 845 300 127 876 604 23 863 217 996 361 661 398 497 79 38 598 132 773 857 480 909 941 844 641 698 626 132 62 179 668 992 868 418 674 698 35 495 598 602 9 904 132 751 336 833 518 922 225 202 912 254 500 76 799 442 677 86 368 724 718 853 58 17  552 763 100 923 967 813 741 825 240 372 841 708 288 714 44 685 437 943 756 59 338 470 689 756 230 483 369 699 204 339 665 268 893 45 596 90 983 473 896 169 241 381 738 320 269 292 318 539 234 416 688 399 230 482 665 759 70 984 732 549 685 242 609 284 697 430 530 813 524 617 63 805 222 548 593 706 649 298 853 436 156 701 22 570 454 596 234 321 567 635 989 216 791 934 364 40 848 232 660 912  996 249 113 5 713 680 561 917 800 422 416 211 874 527 539 40 690 812 214 531 846 161 433 898 81 196 627 360 321 517 477 120 746 226 369 189 710 117 216 344 78 977 412 711 235 512 84 269 341 230 310 850 259 201 522 488 910 358 366 195 302 372 11 642 619 371 491 158 380 771 490 534 285 338 260 405 173 86 4 869 286 950 821 864 610 71 773 644 128 746 513 733 890 290 552 273 724 873 241 853   
124 713 653 49 437 796 332 540 537 75 291 363 144 756 32 195 843 803 544 860 376 119 592 955 309 743 239 11 585 682 121 41 413 208 498 169 46 587 130 719 221 74 111 130 189 738 243 680 31 988 619 122 62 276 147 958 978 953 149 902 157 161 574 849 850 79 919 170 526 362 883 563 649 662 834 941 317 565 92 810 838 648 858 30 128 848 690 326 525 422 763 799 130 835 510 568 578 93 932 482  409 741 199 364 111 47 50 175 756 371 877 101 969 721 547 739 797 418 260 391 959 577 89 920 574 121 740 853 354 195 217 452 770 879 174 599 535 875 455 852 745 639 466 443 920 840 551 934 548 845 427 11 164 771 399 456 202 636 753 282 903 414 57 456 12 125 136 767 835 360 800 204 198 455 369 520 359 722 4 173 525 109 471 938 303 634 161 837 820 438 490 67 56 155 559 597 865 810 682 380  760 666 364 300 821 953 785 377 927 336 942 494 281 248 517 888 165 101 476 848 107 200 741 126 812 908 107 446 545 364 408 767 158 584 232 396 908 6 0 100 875 839 967 384 651 48 539 931 559 90 272 548 789 731 228 103 211 315 8 540 782 120 671 732 773 227 715 459 332 74 561 253 903 750 581 787 185 468 319 531 907 61 646 246 428 548 967 723 751 979 553 409 773 645 635 569 636 375 422 75   
831 297 515 539 603 738 409 716 440 433 292 339 929 376 336 538 207 709 197 47 620 604 210 218 468 494 210 130 659 943 767 340 616 678 121 439 752 762 83 810 220 451 640 787 713 574 330 15 169 417 832 860 854 245 938 839 944 397 518 614 795 150 201 110 819 685 348 340 440 449 326 499 275 905 531 9 182 504 847 41 513 590 993 372 238 276 422 405 601 361 844 299 505 231 152 525 156 931 478 245  456 541 752 583 537 398 154 692 405 823 934 627 560 48 131 506 891 381 457 975 466 581 263 736 926 296 933 585 6 831 9 837 435 505 799 98 568 667 606 636 180 576 548 758 251 802 960 767 513 806 908 662 577 393 898 319 446 543 340 873 677 345 680 41 870 168 160 901 237 559 611 803 348 954 578 117 310 337 630 224 189 746 405 137 886 984 792 481 545 975 596 931 600 920 678 926 88 945 835 912  247 633 295 623 693 51 409 288 443 373 89 242 742 970 643 933 467 710 618 375 392 524 161 627 810 151 680 857 75 516 690 10 487 619 322 615 30 886 984 511 648 733 315 790 472 7 560 628 582 944 922 213 719 857 92 645 906 506 81 780 244 659 146 910 893 885 500 269 388 110 936 966 608 293 577 633 425 714 758 527 917 555 658 720 28 146 652 833 984 915 794 962 345 21 779 641 303 857 416 750   
737 85 194 221 214 964 78 884 687 457 931 881 744 377 973 549 679 318 229 990 8 949 190 872 460 152 71 330 227 701 836 531 280 460 840 254 560 680 860 173 805 621 899 278 753 511 368 462 541 292 530 355 278 217 356 403 751 670 556 241 231 150 670 604 698 2 670 925 146 154 549 128 13 534 614 535 871 807 190 103 113 282 892 732 409 180 315 951 785 119 574 105 861 793 503 937 843 793 213 923  0 663 948 6 25 354 982 342 267 983 810 334 896 106 13 729 744 428 361 231 995 342 418 297 82 591 349 248 193 869 308 487 721 897 968 427 171 501 341 992 51 621 592 855 975 778 998 246 190 101 311 989 645 190 146 56 539 542 370 174 396 478 164 352 646 783 780 702 395 598 412 890 625 748 380 516 279 75 540 536 739 819 765 624 458 466 162 682 436 493 114 674 720 257 105 630 501 322 522 336  265 462 981 92 753 786 883 699 987 317 998 46 483 721 304 149 374 980 596 777 888 314 772 222 303 952 771 145 984 468 541 498 673 782 775 778 708 46 671 264 665 329 288 382 291 603 284 5 700 312 737 619 175 308 337 654 10 36 709 450 188 219 477 768 453 922 18 499 713 960 654 741 994 31 224 460 915 329 954 98 201 856 996 916 398 941 985 425 215 444 475 209 143 128 584 758 475 528 27 274   
554 102 890 360 864 561 306 57 761 202 549 306 833 266 852 381 427 528 775 819 274 429 408 272 109 780 383 780 62 260 921 324 646 101 843 678 349 847 783 859 951 941 166 856 463 968 303 737 501 5 647 426 670 32 910 739 265 438 865 42 13 177 82 737 993 429 657 542 608 989 594 223 818 524 574 989 763 700 13 926 610 890 577 37 780 30 212 725 913 509 346 527 299 310 626 410 641 833 542 95  324 436 923 741 936 592 115 764 60 874 159 671 254 715 942 220 475 118 321 388 490 948 317 115 134 370 198 87 780 84 225 596 560 744 647 64 703 893 264 771 785 228 557 10 696 55 162 230 350 731 861 317 541 255 898 262 642 669 280 378 889 330 580 222 476 766 50 995 490 787 660 292 212 949 606 503 587 472 787 782 372 400 449 864 829 894 935 104 862 723 624 353 477 728 520 355 82 320 231 118  621 644 132 442 497 431 611 984 145 545 97 84 515 541 113 129 996 101 887 514 592 497 363 793 681 745 764 584 859 137 153 323 334 243 801 396 44 764 200 697 133 38 931 876 674 932 187 811 574 702 276 694 814 905 670 800 627 417 928 362 517 93 58 709 57 335 273 491 708 979 914 56 322 744 855 714 296 201 697 346 254 871 359 50 165 909 603 302 769 217 34 621 772 825 415 513 541 228 918 415   
566 732 420 866 79 504 154 87 718 18 357 904 961 333 147 945 771 546 350 557 437 577 856 694 57 785 983 187 76 144 817 388 370 680 408 343 392 785 1 422 296 538 697 677 930 50 358 835 297 591 894 173 903 223 960 771 571 362 589 838 635 717 770 762 83 538 34 810 214 944 868 481 268 777 384 573 744 451 80 371 323 467 384 378 745 810 190 640 430 645 954 124 221 154 24 129 61 338 260 694  755 385 356 928 823 125 565 586 394 920 351 825 356 77 328 899 42 189 758 521 302 419 236 550 913 244 535 795 114 3 925 54 41 181 675 513 86 923 965 237 189 467 843 109 678 517 42 934 524 183 706 820 86 559 698 763 315 528 679 785 873 365 861 213 443 925 41 326 658 234 364 337 120 285 37 67 431 767 222 480 603 980 930 881 804 985 125 747 464 53 32 794 192 208 307 603 447 277 851 950  243 722 362 264 553 475 297 117 62 118 681 673 264 955 797 326 490 779 487 228 248 816 303 54 76 27 26 604 293 576 103 758 65 141 473 305 539 543 496 25 927 717 104 400 387 585 260 53 506 37 198 662 471 847 365 990 493 735 388 355 449 292 807 740 965 803 664 793 394 466 870 105 538 856 164 162 963 113 519 160 998 389 767 202 303 347 512 339 11 500 40 250 380 592 595 708 762 237 317 821   
640 622 795 95 576 430 566 521 415 685 659 995 982 127 222 978 796 390 782 995 716 234 673 180 621 405 574 326 841 529 403 471 355 905 630 275 409 408 905 700 746 539 967 652 99 652 431 130 670 47 773 453 683 149 423 668 207 413 696 700 843 278 833 639 167 361 345 98 46 941 668 652 408 80 808 8 421 300 212 699 55 185 418 529 646 34 567 888 545 414 227 302 567 969 442 536 156 920 550 421  473 495 50 761 890 770 924 74 807 758 385 304 501 634 931 590 95 750 258 331 993 318 466 863 292 670 978 804 835 795 40 258 105 595 879 244 308 285 10 552 555 682 375 352 552 764 773 107 828 885 171 158 698 829 548 901 857 161 173 121 434 515 861 571 429 805 606 730 768 514 225 129 167 224 34 184 916 561 281 919 168 481 81 809 699 280 811 672 738 227 533 260 141 404 151 571 195 801 935 935  575 36 997 620 821 355 205 869 966 801 214 712 934 927 51 551 948 789 615 239 18 174 366 328 727 62 603 730 171 972 53 464 649 411 494 765 183 396 947 734 870 877 840 588 306 658 319 882 209 895 391 914 760 30 29 342 849 59 178 395 112 946 401 373 669 368 171 190 753 31 576 102 610 504 957 91 91 427 15 76 710 957 597 341 169 702 809 288 604 948 81 262 163 93 316 235 937 671 36 740   
6 526 536 784 290 597 20 25 975 407 607 986 466 816 70 789 440 866 206 401 378 877 413 671 645 676 644 797 85 534 23 273 613 324 419 233 67 151 83 774 957 202 715 665 48 900 67 644 708 224 798 273 429 60 138 355 509 302 13 464 829 643 416 121 844 70 432 728 511 156 780 298 761 918 924 617 841 94 457 483 721 592 72 945 331 983 940 576 283 951 946 698 307 176 414 107 640 205 298 121  947 868 682 255 845 119 825 58 940 67 719 129 822 388 973 957 251 495 755 627 200 197 350 34 562 101 971 252 592 22 201 645 38 59 218 916 190 656 477 571 145 905 596 977 211 871 491 476 408 456 925 187 152 403 783 917 993 167 632 920 668 926 224 495 811 624 957 380 684 581 901 317 427 204 166 890 323 991 227 118 417 245 802 923 529 722 762 171 333 586 634 10 640 827 149 132 149 328 282 865  825 11 650 483 493 859 102 168 333 856 60 695 848 773 839 661 167 306 237 961 967 654 673 228 879 246 251 855 594 436 796 524 310 213 516 792 822 510 892 151 37 403 363 722 499 727 533 364 17 334 794 720 870 669 662 92 3 881 195 853 95 148 834 753 983 613 589 535 986 499 636 834 990 485 484 670 993 425 489 394 738 493 280 672 355 13 361 708 794 299 147 434 714 441 25 599 782 797 696 536   
442 301 21 774 543 902 491 553 894 562 220 422 495 29 478 827 545 342 281 253 681 1 921 622 120 278 185 388 245 366 376 291 128 630 243 486 886 139 361 386 150 336 588 672 919 451 194 96 349 721 842 220 359 511 108 853 579 508 980 216 367 526 861 344 358 271 734 815 207 870 262 787 937 678 671 412 83 944 939 251 481 869 869 192 901 327 68 504 228 774 798 173 192 386 37 717 121 94 314 626  201 551 12 340 88 709 908 775 273 933 983 736 916 269 131 198 909 22 679 759 482 885 994 643 166 690 238 487 509 415 920 103 868 519 480 656 801 481 201 212 790 355 866 6 237 966 950 737 761 959 80 850 55 296 61 644 118 760 916 843 486 729 222 421 542 679 9 292 920 587 859 414 92 745 688 876 567 991 5 691 26 594 135 214 476 958 117 338 234 406 792 354 601 786 170 819 805 492 742 30  468 619 798 668 940 847 769 798 422 339 27 481 516 763 917 120 234 32 497 762 868 601 314 632 294 90 491 415 10 322 105 787 595 445 843 545 823 135 969 5 370 949 465 256 310 434 236 416 745 590 254 109 779 796 209 640 359 304 709 157 372 457 663 788 637 570 625 243 563 406 489 243 872 574 219 608 375 911 25 81 426 702 978 395 574 587 392 406 629 53 128 131 41 729 636 369 748 540 205 271   
630 358 700 727 82 752 323 645 244 190 802 99 601 176 403 372 238 643 447 878 875 512 578 904 81 756 816 92 189 546 761 232 695 755 318 122 483 977 889 310 392 570 853 458 391 739 488 150 458 98 529 220 865 594 491 838 916 550 256 246 923 983 86 888 329 771 54 525 328 158 525 846 761 673 962 13 845 783 71 625 641 155 781 848 15 80 951 340 371 114 155 993 14 754 342 579 272 334 251 447  109 461 899 786 896 724 59 226 908 749 942 303 800 688 146 769 381 614 753 149 314 760 997 629 332 374 687 943 18 352 847 152 353 824 234 456 191 510 365 658 638 360 170 234 975 780 87 964 0 420 303 748 357 792 100 717 984 724 3 478 585 878 397 177 300 94 208 55 305 747 994 181 267 912 990 198 523 502 650 334 897 549 384 789 797 495 116 282 648 641 768 625 173 622 476 972 773 909 737 891  95 272 567 175 67 184 434 604 39 444 75 869 403 329 174 553 765 778 80 212 446 20 13 578 335 476 270 363 868 23 943 908 763 159 986 449 684 687 546 897 771 928 449 630 700 307 991 279 594 739 910 935 405 889 694 580 15 908 878 810 553 68 535 381 253 164 380 613 550 699 824 707 855 446 502 40 654 443 912 512 838 962 478 77 328 533 47 702 579 808 148 165 610 991 913 928 391 477 988 445   
697 890 171 328 197 249 849 574 393 39 901 840 772 581 858 827 555 457 781 699 429 518 798 878 109 296 606 623 493 427 483 661 764 598 468 40 959 10 217 198 790 689 344 538 401 19 144 702 87 29 875 763 390 468 261 208 494 776 215 371 111 943 323 335 501 975 877 596 799 913 521 505 322 655 970 152 205 553 458 182 275 337 408 457 508 13 685 832 36 268 174 670 253 791 30 18 668 9 698 384  601 406 254 834 916 43 320 836 331 782 731 774 131 65 929 911 955 336 389 864 638 175 120 670 452 877 111 467 575 469 143 848 7 82 298 74 560 942 605 48 832 384 860 803 676 468 464 81 29 470 888 618 434 890 156 543 713 627 799 473 718 821 981 913 673 249 597 143 196 707 991 354 883 376 752 213 878 608 464 439 144 777 298 608 39 636 47 680 620 505 836 767 111 329 115 106 122 394 688 790  270 917 208 992 50 631 629 18 114 177 432 787 181 730 653 178 922 644 951 684 589 432 736 152 432 98 555 156 320 460 543 749 364 775 198 710 295 984 621 64 371 42 927 614 462 445 233 447 11 359 646 197 52 65 279 533 847 92 283 245 823 393 85 386 345 101 772 96 692 321 915 808 423 213 928 311 775 340 937 875 577 657 866 775 899 578 817 100 284 168 812 910 501 684 105 957 331 641 127 69   
483 588 863 694 30 510 257 369 393 566 815 46 134 486 979 434 517 729 866 974 13 919 217 957 904 575 517 12 328 986 524 422 659 910 215 310 542 862 463 750 191 378 581 829 496 822 203 825 706 756 910 528 950 665 152 521 813 30 732 807 413 534 931 138 421 9 609 92 361 113 255 923 843 37 697 327 247 311 834 259 787 705 883 52 270 394 762 32 793 62 225 347 79 424 238 372 280 302 650 515  985 881 964 894 68 542 168 810 474 720 376 810 296 819 722 810 14 597 474 425 644 282 219 35 808 275 374 189 431 985 671 607 608 901 312 345 607 975 968 708 693 571 555 135 683 2 508 564 136 305 945 157 659 730 150 219 44 452 937 408 71 588 74 899 156 581 910 174 803 505 48 704 14 601 471 506 758 213 932 143 607 930 349 800 373 822 829 59 279 310 839 614 447 220 717 343 702 442 103 216  126 85 277 770 846 84 677 49 206 600 962 416 625 42 521 20 622 830 154 461 952 564 255 797 519 510 787 52 678 528 565 359 775 650 712 564 267 462 732 376 678 760 39 221 744 670 989 174 371 77 766 461 802 213 410 522 625 433 346 37 145 567 591 729 968 647 422 975 837 306 481 179 785 18 118 439 997 600 287 151 429 99 644 985 739 334 52 590 93 512 246 622 427 770 98 799 545 618 661 792   
888 369 434 342 689 112 810 745 464 593 822 891 472 735 644 834 112 367 718 280 272 363 815 545 397 333 232 822 976 577 650 598 259 918 285 727 120 264 662 208 619 350 570 917 756 81 739 108 523 652 818 226 93 114 628 442 688 407 62 799 936 643 207 13 258 38 708 175 462 970 732 380 826 746 187 858 934 241 867 592 146 711 608 635 847 204 31 796 811 473 561 129 283 115 610 325 432 103 100 871  593 753 50 124 306 211 939 924 247 355 831 794 850 33 658 943 719 87 905 700 910 873 778 419 620 908 615 216 915 91 561 74 313 596 434 93 111 665 924 782 70 535 117 464 510 294 395 377 488 216 624 876 749 143 61 857 12 935 518 452 490 145 278 433 991 835 102 735 30 112 375 834 221 444 383 628 83 390 212 162 524 658 522 928 918 105 303 867 95 561 103 216 902 216 305 173 713 167 148 952  784 52 600 411 119 496 273 543 238 29 14 966 488 520 449 669 20 475 966 937 71 786 618 929 250 535 742 785 521 34 547 147 373 193 326 650 495 220 348 568 284 446 360 371 958 872 56 432 478 740 724 198 217 776 214 512 900 253 214 396 281 38 339 9 77 939 793 580 588 927 821 416 617 455 671 824 865 453 216 527 269 204 984 834 451 400 775 107 217 594 489 70 410 618 183 748 391 134 249 198   
209 179 977 117 331 385 148 531 999 172 94 300 558 265 574 427 991 751 803 848 427 458 494 340 86 832 572 550 208 695 437 457 603 134 310 30 777 120 551 240 240 846 562 462 705 527 739 485 887 123 825 284 186 676 277 53 891 169 841 589 834 28 911 398 791 964 405 451 286 84 849 649 910 608 530 20 997 983 855 462 328 624 713 5 540 242 362 746 504 505 516 378 864 719 692 226 51 170 643 31  266 235 275 485 886 870 341 587 813 109 590 125 161 753 649 133 78 464 799 143 384 191 628 734 228 898 52 425 565 837 295 188 947 892 999 957 803 609 32 201 410 580 410 172 136 397 870 405 877 655 562 279 565 568 527 944 500 54 297 692 401 633 272 922 578 691 898 814 954 7 249 857 107 58 871 338 66 175 655 192 767 904 934 689 602 318 870 808 860 792 663 747 35 683 914 873 480 502 632 667  334 462 200 146 144 867 380 621 43 145 966 474 6 997 446 916 79 152 562 966 456 171 51 771 308 955 231 273 439 17 881 272 656 49 815 540 704 177 266 368 808 484 805 46 698 755 443 818 346 292 984 640 438 855 132 932 82 910 306 599 443 592 505 208 953 688 309 459 249 536 238 941 348 305 695 742 134 967 879 610 0 202 871 207 748 745 507 636 947 350 106 430 40 257 274 391 177 712 218 661   
864 929 66 372 509 504 103 354 316 997 439 123 870 35 960 235 58 215 691 808 936 189 24 384 819 792 321 931 332 729 1 645 285 130 644 421 338 62 664 321 174 940 95 759 378 809 765 193 490 421 421 745 227 564 329 581 184 314 36 525 11 36 834 491 283 649 687 266 160 125 817 789 99 418 951 811 581 715 269 148 92 903 375 525 178 334 830 449 325 115 340 400 826 652 4 914 80 165 699 974  20 446 889 924 440 271 696 450 254 783 309 467 594 173 285 61 464 789 125 534 232 296 611 909 343 49 726 725 531 828 410 292 578 170 506 59 350 275 254 511 304 897 70 546 882 949 159 828 802 918 339 246 84 1 570 693 858 428 126 227 221 883 263 416 82 270 984 808 789 18 611 645 968 890 251 522 3 94 244 536 833 713 944 315 937 733 546 926 351 876 692 525 204 890 595 590 98 73 251 236  612 886 223 570 578 492 531 325 524 812 735 609 233 401 434 805 949 14 727 680 180 296 909 802 792 444 201 742 848 431 413 455 563 559 48 280 194 897 300 955 89 277 897 561 475 956 938 373 724 351 149 494 801 548 70 316 584 641 534 281 141 20 927 5 890 217 259 684 53 846 11 551 205 328 286 374 340 159 181 627 301 645 385 698 682 162 683 689 556 49 229 370 180 91 199 107 792 534 752 242   
445 354 870 911 63 925 691 312 703 7 743 618 521 437 941 721 171 989 314 679 90 926 14 826 734 638 398 152 812 775 93 352 621 801 639 801 930 653 744 233 578 655 876 622 137 683 144 624 652 580 801 799 453 470 933 712 276 416 947 119 467 127 442 99 649 365 945 445 152 98 957 20 653 718 620 416 57 979 342 261 414 346 60 639 698 650 220 528 218 477 626 938 436 41 680 559 644 356 788 866  5 967 970 517 654 84 638 70 344 896 447 561 971 211 855 899 33 270 496 136 452 328 816 245 849 736 688 836 184 604 699 387 659 222 344 617 941 601 273 205 135 449 405 95 776 64 83 937 180 478 897 133 101 60 228 389 114 942 904 123 54 641 562 21 917 635 275 257 699 366 498 439 670 977 183 541 974 736 939 114 379 995 168 615 577 297 767 64 720 796 34 729 472 858 493 403 578 2 715 722  772 301 344 338 372 303 104 126 748 980 545 137 506 589 954 234 199 562 810 575 552 793 172 132 921 507 485 878 895 150 408 496 795 961 8 176 695 624 857 375 185 350 194 139 199 823 227 292 965 443 611 596 265 784 229 16 700 449 463 629 549 306 535 797 655 263 40 517 249 36 606 925 390 889 405 791 44 219 39 905 979 982 143 515 771 99 921 617 228 29 452 843 182 93 607 855 276 403 276 116   
542 589 21 295 779 757 243 535 573 341 551 59 725 325 392 26 771 421 472 263 531 744 96 308 80 618 146 165 618 101 450 503 148 505 606 788 880 373 508 526 581 132 30 408 539 565 28 577 274 160 266 461 707 29 657 542 636 548 808 496 126 594 393 730 494 78 124 493 601 811 120 301 204 413 103 122 717 801 154 629 257 897 327 336 897 126 377 3 831 196 112 369 180 670 161 695 875 658 7 125  561 489 146 511 565 660 779 816 265 665 408 424 382 52 453 448 795 653 140 361 154 58 970 737 945 999 916 434 963 645 434 818 633 311 974 601 618 265 529 918 203 677 127 191 440 939 497 423 960 607 822 49 683 291 559 166 201 704 743 81 252 397 141 548 302 30 579 359 561 872 53 433 695 902 884 774 876 832 881 438 889 109 722 555 590 785 642 101 315 561 235 986 376 194 93 256 584 271 144 853  680 705 657 824 34 569 798 394 324 866 639 308 178 178 760 383 26 286 604 348 179 168 349 696 325 454 290 17 43 33 198 415 168 229 634 366 215 286 251 824 995 581 403 462 643 380 626 279 382 719 753 905 469 564 958 841 207 310 666 728 888 772 632 767 614 460 308 129 22 372 729 986 834 434 936 556 409 656 798 28 197 577 973 939 749 236 29 955 423 159 691 386 539 965 570 814 524 24 206 623   
832 592 30 489 11 858 606 43 391 221 865 562 728 58 92 268 19 141 70 830 547 461 236 945 950 951 971 564 219 177 801 450 260 114 536 360 72 65 315 174 932 562 452 370 962 338 223 791 564 147 504 6 249 470 5 250 180 717 159 306 434 809 676 830 343 61 859 202 922 831 638 857 377 908 677 730 725 66 257 753 307 254 244 948 742 266 114 170 574 504 580 730 156 114 969 282 513 847 554 721  773 353 496 316 949 362 529 929 225 216 321 378 410 614 631 348 215 689 456 121 305 160 975 106 239 237 521 771 927 248 759 722 387 30 535 146 814 860 612 852 522 223 449 892 906 221 449 280 962 585 366 702 501 804 222 464 783 867 313 117 283 996 797 516 723 584 104 968 214 97 472 409 39 211 112 279 209 406 472 712 623 477 716 13 253 345 360 379 199 338 182 370 87 955 161 114 432 646 283 44  325 614 246 25 481 356 123 710 790 665 175 4 424 744 987 600 446 218 246 317 866 14 137 149 43 143 807 24 690 312 716 356 637 308 522 613 693 187 984 129 507 876 248 556 626 316 204 583 610 994 435 509 98 41 329 780 210 178 482 268 121 929 837 365 588 397 413 712 451 188 108 184 96 129 954 849 433 520 38 713 625 712 270 787 879 694 465 741 407 493 664 932 294 527 700 827 963 637 504 458   
904 188 378 892 638 332 539 847 536 107 263 587 302 779 892 556 399 406 759 930 922 115 789 21 396 91 538 60 942 110 31 723 200 635 407 399 998 316 217 790 460 433 697 63 624 964 868 383 517 774 532 253 432 782 988 733 244 722 757 692 647 258 817 394 701 763 737 947 707 262 505 434 588 614 516 187 857 109 787 701 121 221 921 894 147 510 708 240 721 923 396 8 471 721 153 636 73 962 385 612  825 709 96 96 195 803 738 601 587 862 742 981 369 27 426 641 756 619 770 100 764 915 17 850 839 697 845 511 463 240 166 778 32 565 118 646 787 5 814 598 545 120 301 22 841 859 805 126 227 6 150 347 109 128 466 777 620 396 660 502 620 820 598 698 913 55 59 207 204 956 511 717 387 546 16 729 336 71 851 501 140 117 883 875 324 883 453 239 286 652 500 836 466 75 398 341 658 372 314 823  901 930 830 217 628 267 805 480 282 948 851 507 729 903 211 13 54 617 660 294 633 230 458 717 158 212 289 391 600 568 535 889 738 337 132 359 330 353 288 521 345 902 436 182 189 242 286 37 295 842 570 263 103 816 372 810 796 435 869 322 696 544 251 942 13 20 75 32 660 13 963 920 330 24 138 203 307 903 341 710 33 807 353 84 896 418 197 316 705 830 443 886 971 899 757 921 616 301 863 123   
478 97 817 769 361 662 607 692 251 980 437 9 665 552 959 556 529 343 184 613 407 135 751 745 366 575 533 600 501 167 399 448 975 647 638 865 777 855 249 827 513 210 453 556 302 105 838 606 497 240 143 901 318 148 323 736 144 636 712 466 129 206 116 501 612 763 258 444 385 10 163 60 582 277 807 182 606 85 898 763 184 888 863 371 254 37 748 372 504 574 811 997 55 381 362 448 730 741 359 584  276 200 239 651 141 223 688 27 905 760 569 392 724 59 27 556 803 855 230 811 252 907 518 511 138 815 957 655 95 476 911 681 340 964 865 946 664 685 958 276 634 932 387 281 784 534 47 314 356 230 805 216 693 815 770 74 343 466 740 715 497 137 315 913 82 89 680 192 921 908 345 85 798 778 864 74 41 80 83 424 318 76 505 213 700 457 239 876 891 875 81 859 417 591 744 674 767 118 776 426  931 415 672 325 621 123 28 897 111 596 49 307 474 116 422 117 56 335 336 329 185 123 524 226 176 690 985 753 207 399 89 97 463 653 431 90 156 729 778 111 371 818 430 242 186 903 805 551 745 916 299 960 996 796 32 138 300 363 452 765 739 420 760 116 275 582 76 933 980 926 997 102 421 682 591 802 648 440 69 77 127 565 384 409 273 177 966 454 616 835 795 801 993 762 449 391 148 85 998 994   
414 203 471 569 579 863 375 290 653 292 737 269 780 388 969 376 205 424 553 906 167 183 500 494 492 494 91 745 128 832 806 26 108 997 199 445 331 974 602 320 612 87 170 862 890 726 718 301 716 833 735 786 5 474 771 666 144 463 280 334 136 145 193 69 969 731 160 879 854 445 826 491 95 191 817 668 155 237 163 390 754 698 398 765 249 284 859 399 806 529 311 491 91 7 494 567 593 87 245 91  687 444 827 330 314 13 330 335 237 252 864 940 350 636 241 916 798 53 978 181 673 62 983 489 413 8 188 185 561 466 613 977 22 544 309 178 186 338 282 406 603 37 195 431 857 957 262 452 57 665 553 211 875 179 534 620 107 906 389 906 59 142 117 836 627 261 82 457 366 789 472 450 299 997 398 63 277 20 934 751 877 870 352 997 368 782 585 343 328 72 860 679 873 950 368 676 439 200 222 779  150 86 36 640 924 604 71 331 784 636 812 321 150 17 455 920 506 37 110 176 132 712 998 621 526 530 164 251 944 533 104 542 979 898 69 857 114 236 230 946 878 733 463 47 168 148 146 148 344 667 793 222 581 200 178 742 202 609 67 615 331 648 824 302 926 308 771 926 256 800 521 704 736 961 585 679 399 560 83 93 271 420 387 975 214 133 252 378 625 793 821 677 681 343 627 765 419 904 866 296   
578 567 698 843 223 700 945 545 409 339 644 953 291 787 859 137 370 470 773 194 945 475 705 393 223 460 664 455 283 656 8 701 591 770 104 723 968 897 891 474 518 217 652 469 562 374 892 917 706 287 983 953 104 347 863 613 365 759 699 569 821 465 898 897 257 311 11 368 255 705 716 683 68 608 500 54 176 239 509 875 185 256 213 937 707 506 491 704 556 76 356 135 737 555 407 544 872 261 424 534  684 690 602 375 534 13 314 42 230 572 750 533 724 923 48 200 104 256 874 539 414 542 366 947 506 70 153 423 119 889 546 965 216 33 539 304 840 226 36 368 334 643 97 878 974 461 406 387 241 113 359 607 18 976 287 33 85 513 141 789 550 770 977 651 649 106 865 692 87 999 551 773 878 201 616 470 531 880 569 361 704 938 139 360 63 135 211 227 543 450 262 731 22 941 998 249 73 900 886 588  504 237 635 920 273 24 341 106 952 437 541 102 536 668 148 938 743 772 641 239 352 512 601 779 938 866 816 819 960 78 939 611 675 708 229 88 713 276 305 886 977 242 15 252 567 441 807 508 335 854 744 584 881 344 174 980 695 216 948 837 814 169 688 911 94 120 156 965 241 195 609 503 254 258 872 865 28 350 354 845 361 789 188 905 623 228 209 579 949 430 596 659 754 331 953 50 699 157 119 537   
713 272 518 92 396 351 707 591 60 641 447 893 809 601 769 955 115 477 889 165 717 320 517 828 142 398 856 997 79 230 517 359 521 249 391 189 430 756 413 519 222 57 951 355 398 29 112 298 439 437 567 622 920 706 925 184 915 949 11 900 84 195 785 622 633 451 296 643 769 338 396 796 71 499 439 361 313 668 776 146 676 309 911 992 891 442 950 690 155 252 217 97 938 992 297 379 48 584 257 780  881 731 188 44 851 436 543 599 682 260 245 934 953 264 599 736 49 937 605 701 624 302 108 435 634 441 214 790 937 546 981 773 858 70 705 552 665 874 208 396 190 214 546 111 610 163 850 264 109 43 876 661 879 975 433 253 900 610 486 745 294 684 253 279 464 137 88 581 556 298 931 170 379 583 167 817 663 781 753 495 93 81 508 678 241 949 667 201 293 69 738 442 727 817 792 955 294 690 662 748  564 784 686 48 927 948 793 132 619 838 278 38 64 240 850 710 263 708 618 298 685 97 693 78 704 464 631 27 874 6 455 191 459 425 813 743 872 444 385 184 626 370 846 653 593 336 967 195 856 809 938 407 189 69 409 207 855 64 64 651 794 223 474 164 164 438 805 169 250 409 219 84 890 553 45 201 565 579 542 723 384 749 310 425 750 556 308 837 437 769 17 207 430 752 489 833 551 705 684 319   
841 24 432 994 630 419 14 70 31 870 607 664 945 636 181 162 315 543 89 487 716 600 190 284 149 735 317 88 492 898 808 708 794 0 700 751 691 953 928 436 306 197 630 597 756 20 180 199 830 564 240 137 431 422 579 350 839 574 478 96 221 950 256 93 353 374 919 617 395 137 527 28 12 480 244 852 447 141 70 350 995 825 210 753 918 167 211 292 597 842 795 636 7 267 82 303 288 400 681 153  325 193 995 734 522 963 191 531 369 371 742 267 5 499 453 996 517 112 84 932 272 447 757 99 506 44 436 557 742 705 236 648 282 594 956 634 767 991 668 868 492 429 722 881 569 109 376 533 352 736 57 54 61 978 964 355 824 598 505 938 119 788 587 770 421 577 626 79 878 118 439 960 807 522 96 914 423 951 693 439 109 218 258 639 639 195 466 163 139 462 723 36 125 345 1 660 249 108 669 714  992 134 394 218 553 864 203 587 901 250 129 846 201 487 428 279 184 961 114 363 365 662 440 348 387 817 416 95 757 644 461 74 559 772 621 639 706 663 716 747 471 111 727 43 170 744 693 793 596 568 861 597 143 423 14 869 165 174 212 610 728 726 864 970 487 168 19 379 765 839 374 801 468 899 803 336 733 350 847 681 846 31 891 952 233 125 314 623 226 488 113 528 415 88 730 156 255 387 35 726   
18 56 391 872 749 489 983 350 786 911 474 386 130 766 828 134 728 375 533 41 729 578 267 599 182 548 907 202 320 798 44 990 52 111 622 600 818 874 109 121 163 832 11 199 954 900 635 568 55 17 647 280 996 273 769 904 778 338 72 222 991 31 255 892 435 11 976 976 64 808 662 194 858 63 704 630 314 803 556 324 532 16 201 57 431 724 649 362 807 429 539 14 157 309 944 747 539 559 112 659  222 486 223 955 346 989 884 351 945 672 420 140 135 529 6 439 581 382 499 968 529 944 598 698 549 989 485 841 727 898 907 625 56 427 625 34 797 972 823 728 914 524 726 987 823 243 929 911 944 568 456 313 852 480 658 626 905 515 426 828 241 748 873 578 701 167 279 932 80 324 330 154 150 196 352 558 861 177 360 227 295 979 478 437 777 253 13 643 428 651 482 161 578 532 367 953 549 844 442 939  402 98 125 341 952 361 63 982 914 941 44 133 319 424 487 210 590 667 465 978 74 52 987 58 952 529 756 462 522 537 776 223 172 661 925 396 837 352 12 737 434 608 957 632 866 838 955 746 430 192 213 600 564 30 541 393 576 94 515 263 955 533 861 29 328 234 415 129 187 811 269 497 178 554 522 859 937 464 752 838 458 467 93 307 83 236 925 998 578 733 526 553 100 404 950 62 482 50 598 91   
515 107 652 431 573 691 939 417 597 662 324 253 123 792 978 581 887 732 695 319 558 828 625 907 620 495 716 560 484 769 397 833 338 768 586 604 391 650 711 513 968 879 19 207 448 739 355 505 553 23 923 486 316 191 992 909 465 375 975 327 104 340 537 660 663 192 525 749 534 53 184 969 477 48 980 592 370 172 445 198 868 523 195 149 296 683 592 690 532 792 243 30 453 443 764 454 431 821 83 471  970 438 544 267 142 909 343 227 341 191 98 151 505 997 256 657 949 980 252 398 204 190 48 738 214 772 592 196 392 454 156 596 969 553 20 298 715 517 402 429 199 144 393 460 279 347 387 871 387 757 165 98 554 621 228 490 93 342 136 801 217 401 328 647 413 14 0 712 114 806 238 699 596 947 470 787 627 802 470 424 254 16 790 343 156 769 55 375 387 369 940 772 208 318 950 265 833 282 750 767  449 934 902 629 684 99 723 714 728 931 794 742 297 192 144 832 328 950 996 918 334 836 884 565 892 645 590 784 488 653 635 519 961 276 505 529 613 660 433 867 997 943 492 530 20 935 836 79 793 640 631 644 980 826 38 538 814 945 98 857 721 692 597 44 943 819 753 825 714 675 483 641 304 707 165 48 490 465 856 692 177 485 174 785 857 449 481 675 172 851 966 806 725 163 697 781 925 33 145 549   
398 638 443 70 319 136 882 111 674 346 158 56 274 460 868 749 71 233 198 831 120 221 435 736 824 859 747 384 326 274 954 639 532 877 871 593 142 590 844 272 792 288 241 781 594 945 524 349 908 542 366 115 131 176 243 194 411 995 520 659 48 940 565 776 636 709 147 844 827 506 817 326 559 943 546 219 814 377 962 995 149 805 890 182 964 172 742 663 750 642 738 695 107 735 85 246 488 120 226 87  407 739 922 38 810 518 196 975 7 203 827 925 284 98 901 819 873 895 749 489 49 997 446 92 824 68 902 296 3 25 949 104 521 981 182 548 520 30 710 94 895 345 55 46 274 68 998 288 509 604 116 816 317 942 224 690 291 211 886 205 223 474 233 56 913 607 581 769 234 700 667 750 5 216 783 891 671 603 472 305 637 188 553 880 627 939 889 300 300 869 225 231 342 586 35 665 105 399 291 416  274 334 903 615 706 127 120 842 354 745 263 108 861 864 586 750 472 472 113 513 881 436 360 130 431 198 401 550 407 905 106 39 316 837 985 289 603 32 124 258 74 65 700 711 895 863 552 410 534 410 442 778 533 274 406 211 421 255 522 945 445 263 160 972 560 370 680 98 855 787 27 275 211 673 143 353 541 488 870 708 772 445 808 892 440 102 99 978 764 969 996 56 955 871 334 972 794 858 577 789   
653 812 73 656 148 473 519 246 596 810 670 96 286 270 346 382 348 526 576 291 303 861 10 966 160 104 805 313 904 188 987 825 584 93 43 671 675 559 100 420 834 996 8 59 745 572 740 413 485 497 561 106 606 691 596 715 403 637 411 731 204 702 860 930 449 470 255 784 355 622 930 599 915 153 370 685 957 300 102 193 509 57 476 277 862 46 119 942 288 43 206 401 620 639 291 10 629 767 834 839  549 792 506 135 326 710 685 616 965 176 10 966 287 705 69 359 267 438 267 8 342 653 993 406 982 202 51 191 247 27 567 907 373 933 444 557 997 229 224 636 357 202 604 258 687 525 615 951 751 622 959 147 404 513 866 43 394 448 519 486 810 939 966 710 689 336 435 23 94 250 623 894 917 949 895 378 895 332 434 267 605 877 916 482 264 559 773 659 99 111 756 513 149 886 753 30 789 374 542 371  524 627 313 678 576 760 823 12 644 354 107 208 969 391 506 994 163 100 159 739 368 876 735 108 658 114 495 847 22 982 997 417 340 252 131 524 234 609 929 89 182 356 72 318 359 487 331 380 262 544 270 677 179 745 2 504 806 199 885 474 625 497 758 522 996 759 419 410 795 764 223 920 161 88 806 369 908 59 597 918 821 332 192 494 731 471 634 411 986 319 92 698 190 603 514 305 32 591 769 459   
201 66 151 148 304 80 513 24 680 875 765 673 432 835 748 705 15 91 26 753 208 705 562 485 504 411 541 412 548 236 948 843 75 708 0 512 57 947 490 299 217 848 282 918 937 828 141 783 348 567 784 103 665 601 287 571 613 385 592 554 119 148 906 558 638 212 555 705 927 954 280 362 35 296 222 514 874 169 478 77 878 160 870 975 212 508 653 563 933 389 366 855 987 283 74 279 851 688 743 799  693 694 625 665 487 307 389 371 782 684 226 475 61 292 362 748 894 592 993 23 739 394 319 973 234 513 53 212 434 493 872 969 908 911 311 268 579 597 884 90 343 559 333 18 65 436 632 224 890 961 673 207 938 472 740 495 59 961 396 66 712 968 801 484 51 434 671 249 673 157 159 834 237 321 201 350 428 825 94 982 902 407 219 636 294 177 969 186 726 391 349 419 77 547 105 26 667 445 529 789  868 917 560 259 737 947 476 817 273 209 309 166 569 558 998 423 701 695 841 889 886 75 251 545 997 953 19 783 578 820 576 454 600 883 273 58 510 573 332 715 320 154 900 102 407 182 546 402 977 728 745 971 900 956 406 559 787 565 39 265 193 141 366 488 629 401 611 192 160 478 277 772 202 88 803 102 532 474 395 353 256 83 182 944 980 752 308 563 219 892 265 498 351 88 370 386 724 239 889 952   
259 941 977 147 126 356 70 374 783 35 945 908 658 660 728 289 538 542 249 163 105 89 869 764 904 788 545 468 330 902 108 859 923 971 483 842 7 857 694 508 578 86 461 218 622 761 401 384 38 291 356 480 638 887 134 958 812 610 467 693 854 593 523 13 243 283 209 556 20 21 333 226 408 201 628 355 675 935 735 257 715 305 500 714 4 685 661 610 627 934 301 888 499 304 897 943 701 848 818 149  694 532 68 583 343 717 535 204 494 46 871 5 744 333 853 508 9 441 587 815 378 413 22 125 462 201 227 830 574 755 276 863 869 558 63 749 887 307 671 349 729 543 67 959 829 380 859 990 811 264 700 636 990 875 603 439 42 345 220 141 305 936 2 615 950 160 348 598 590 690 545 371 823 415 957 721 554 719 246 762 953 479 321 336 396 493 151 319 668 574 37 611 30 409 746 904 34 463 354 871  828 271 77 172 5 48 714 77 699 910 76 896 463 733 998 28 773 153 476 499 125 91 87 970 420 894 269 231 615 784 678 426 454 572 492 62 471 668 134 482 890 809 232 278 853 757 286 755 30 953 962 451 861 478 919 46 581 456 932 773 730 663 965 609 949 701 666 317 245 558 659 730 615 828 439 65 961 692 690 542 415 72 274 975 959 412 698 409 563 654 391 799 106 52 389 618 179 194 802 765   
748 45 307 60 88 994 57 798 175 412 245 576 33 47 759 149 290 563 497 974 619 391 344 135 569 639 538 139 528 802 856 600 553 536 587 395 112 532 114 898 438 41 113 836 570 237 814 88 102 11 901 415 245 727 617 794 757 562 842 740 514 215 850 31 670 894 515 338 189 123 544 672 683 585 539 408 76 80 232 364 147 250 207 570 693 935 403 88 942 407 247 331 826 942 763 829 282 41 410 481  357 486 883 690 691 554 511 66 561 900 421 229 557 369 143 956 944 679 976 181 120 247 354 426 252 249 182 271 388 410 693 764 202 319 635 134 769 144 75 852 245 422 461 322 324 494 186 467 411 236 715 11 745 664 388 95 352 192 212 299 503 837 347 129 313 441 375 702 704 655 181 320 492 385 16 893 396 568 318 793 103 942 220 481 569 187 888 686 184 261 602 340 936 271 371 579 500 434 269 775  17 104 799 684 836 33 435 29 887 441 537 866 419 368 677 729 999 172 914 503 724 555 304 552 281 966 854 64 429 310 149 684 114 571 277 837 807 176 34 918 106 695 821 668 342 920 999 21 223 177 811 172 767 357 192 940 581 325 479 982 764 24 807 269 467 55 858 436 840 859 113 765 945 915 576 802 870 484 255 112 981 27 282 302 802 720 209 881 763 475 885 865 703 209 526 69 978 779 566 384   
343 396 888 375 46 577 230 690 765 129 626 313 399 388 763 556 279 431 904 964 259 937 210 831 994 522 3 536 842 260 206 664 648 785 139 631 951 890 779 109 502 194 182 45 980 312 833 70 592 469 992 249 659 577 188 546 4 92 470 246 735 583 309 512 466 68 504 644 424 824 974 189 711 226 566 158 975 333 233 335 718 450 333 912 609 812 786 805 609 689 370 568 405 730 768 241 40 530 265 367  847 266 190 982 685 155 414 781 515 292 108 535 871 770 866 707 736 574 89 729 532 790 531 974 790 739 681 617 348 140 929 544 751 578 921 765 175 132 924 731 987 317 456 562 63 34 478 502 656 815 213 427 175 221 70 741 672 186 740 354 144 752 720 955 110 239 622 27 668 119 13 771 871 268 428 820 281 121 319 828 844 5 842 812 697 612 296 73 692 373 156 284 226 845 28 762 374 936 182 743  355 989 802 494 392 57 466 157 456 716 703 528 292 620 974 788 148 645 86 312 555 108 609 57 126 292 729 435 643 59 850 622 977 77 962 246 639 170 469 51 715 528 533 813 725 263 369 185 837 88 111 552 253 50 44 255 989 865 825 884 44 435 245 332 121 878 346 265 549 75 305 641 34 380 183 768 67 247 360 384 516 926 399 438 573 863 462 947 771 162 345 779 817 499 231 58 813 349 0 910   
77 79 589 362 866 864 915 441 955 962 112 795 58 900 326 960 670 326 980 617 764 540 320 396 362 840 541 922 891 353 249 753 503 411 443 266 753 118 733 163 717 812 623 947 630 110 654 128 787 282 209 824 137 6 94 513 564 470 93 436 664 63 511 879 240 209 322 741 293 853 595 277 572 400 129 27 369 56 289 661 761 227 643 636 271 754 954 545 537 841 314 732 859 956 604 318 539 89 46 960  337 298 717 726 624 0 271 93 930 658 138 704 715 844 579 238 480 361 473 487 94 65 326 710 141 441 505 736 905 859 677 57 669 88 519 856 373 532 224 523 789 413 136 961 808 839 250 777 805 774 391 493 835 351 217 639 78 905 279 237 810 476 710 226 839 725 620 492 335 762 760 13 10 783 127 746 922 628 280 863 128 884 614 321 56 667 844 249 633 87 517 481 811 214 735 376 582 533 767 502  45 67 991 659 854 620 177 849 479 936 793 462 334 183 441 7 711 174 848 854 720 343 817 241 771 944 515 26 477 818 781 742 941 840 538 958 684 828 172 509 605 281 372 325 990 109 132 679 662 52 413 946 760 695 752 966 328 845 648 596 446 562 187 62 590 749 432 229 45 88 240 408 842 809 52 199 329 401 830 88 481 319 691 272 973 607 585 227 333 794 324 314 172 631 39 407 568 681 600 976   
596 306 302 47 445 35 770 126 808 517 171 809 914 186 724 151 11 638 169 340 344 400 34 547 37 806 544 843 34 308 302 883 609 155 700 160 497 973 20 86 219 982 534 53 668 561 340 611 876 659 340 207 411 16 934 531 769 131 707 508 250 430 864 230 682 334 360 554 914 201 820 936 271 517 807 803 751 265 718 485 806 308 43 583 385 934 647 194 571 181 445 760 568 946 444 98 99 255 458 116  48 599 455 994 651 719 550 177 420 164 295 637 591 563 26 520 56 79 85 691 623 157 371 760 711 553 760 109 691 984 978 84 994 613 112 615 766 901 697 593 759 518 56 517 203 700 75 222 719 602 191 198 296 266 603 647 181 705 803 254 782 715 36 486 929 493 323 35 662 38 170 745 484 229 841 739 728 338 523 999 528 19 153 723 792 472 548 639 695 541 555 510 256 252 275 854 596 372 780 877  850 619 122 498 693 539 169 285 372 162 235 22 107 206 535 531 996 780 344 671 208 817 576 265 749 743 471 425 664 273 165 720 967 607 293 731 869 827 730 70 448 996 236 479 53 536 808 55 56 827 617 11 273 223 768 198 339 214 700 427 316 836 515 367 102 328 931 438 642 575 271 43 410 630 358 975 250 492 118 768 539 16 278 497 865 329 329 680 633 592 268 742 893 524 561 947 150 33 632 587   
459 428 836 143 723 407 387 25 831 34 704 96 529 679 139 421 995 507 0 336 986 348 135 357 908 7 719 618 766 698 874 365 920 638 285 826 774 801 363 134 943 932 932 865 14 526 886 924 643 896 512 570 301 796 21 763 208 589 713 510 827 538 15 959 789 266 175 230 538 281 894 255 237 517 235 451 665 893 448 72 539 269 757 341 531 960 812 360 695 65 791 345 549 119 738 84 57 582 288 893  498 478 9 833 473 132 207 868 473 548 391 979 800 472 364 843 517 637 402 510 660 229 269 99 563 882 458 827 516 528 598 774 15 102 94 480 810 958 658 603 938 106 34 666 786 860 799 878 474 255 609 343 97 318 166 563 582 848 507 575 251 788 265 159 271 214 158 746 824 416 698 586 319 516 248 343 747 589 466 688 469 199 176 398 983 370 301 15 993 39 287 365 685 961 438 953 379 488 446 656  246 645 120 546 116 749 260 570 744 502 365 421 945 202 308 766 22 906 353 606 306 661 165 364 958 739 249 71 740 716 812 763 260 256 309 426 433 458 381 628 405 205 238 819 273 132 668 461 837 32 494 584 907 299 188 871 218 883 649 624 576 25 140 987 818 741 628 43 374 918 322 631 958 272 783 422 71 388 260 365 950 941 819 538 65 97 854 83 605 505 446 290 496 734 577 498 6 916 720 319   
791 132 705 715 832 728 745 577 512 921 575 872 711 300 522 212 160 702 162 186 761 703 607 947 609 571 308 323 448 61 361 481 142 203 328 421 245 688 33 154 466 139 651 553 277 290 573 335 892 245 271 719 746 999 360 300 813 716 421 82 432 28 598 109 586 722 216 392 988 775 82 794 532 36 783 293 474 35 813 576 131 291 282 532 26 843 32 888 728 919 169 651 738 510 35 335 143 68 62 974  57 856 229 275 805 623 438 987 556 883 376 882 914 15 836 815 554 19 972 642 460 925 416 167 421 547 134 945 291 998 859 291 258 868 776 789 778 756 702 887 76 423 106 923 765 654 887 141 279 387 800 206 612 5 943 124 842 107 273 690 452 531 200 687 58 562 666 598 695 66 587 527 122 702 412 100 926 95 11 103 134 796 385 498 806 286 957 601 569 723 824 861 359 385 611 555 935 158 287 902  849 720 458 945 581 583 119 784 597 494 897 893 993 217 573 314 555 559 776 829 127 903 781 381 134 850 850 719 747 235 38 483 944 355 392 201 15 279 635 411 85 279 376 517 80 820 567 289 881 617 31 649 992 819 604 564 380 408 752 328 441 857 346 554 418 427 887 701 675 682 140 107 318 829 346 833 776 344 91 235 572 850 105 39 274 297 877 106 924 274 27 35 111 693 513 959 105 798 905 835   
273 758 999 754 362 218 274 136 963 28 451 507 190 633 422 744 351 886 636 519 518 425 577 95 515 377 842 215 159 141 545 499 823 353 464 621 921 805 180 990 564 618 393 948 665 290 544 11 353 570 673 166 919 365 822 224 191 848 892 449 97 574 213 669 780 532 239 384 215 631 56 197 369 983 81 469 833 373 870 872 161 632 341 949 958 410 824 137 300 57 536 235 175 61 189 832 675 821 256 562  157 679 846 328 395 928 967 926 660 896 454 530 371 663 115 971 840 565 80 981 555 754 102 380 574 340 130 620 644 842 597 375 329 266 755 441 516 196 434 349 997 741 411 509 945 933 808 555 673 530 202 997 849 276 181 873 846 308 169 196 584 912 491 127 432 260 135 457 400 504 786 998 502 840 51 648 922 709 327 974 15 55 57 132 53 509 865 490 957 210 369 472 711 806 700 9 989 521 505 739  753 290 520 521 480 886 378 847 646 563 394 146 969 176 397 629 877 126 647 145 148 239 149 762 770 309 932 885 141 923 974 192 218 511 10 966 998 465 146 51 17 912 360 882 483 158 33 344 114 209 976 350 878 208 650 299 748 508 730 371 458 45 207 278 380 71 840 454 98 349 288 396 576 739 100 107 502 875 525 309 91 581 923 852 195 343 786 320 203 571 409 153 777 815 783 886 492 891 394 424   
156 57 69 966 891 287 339 173 827 743 970 663 695 5 556 357 160 735 993 76 217 691 47 358 247 587 678 553 771 840 574 509 979 813 900 27 298 517 910 850 590 803 660 178 130 168 87 484 92 485 196 70 940 34 975 149 973 888 786 560 214 935 622 923 333 129 833 744 971 618 645 580 698 57 153 739 518 529 986 222 212 488 827 514 948 710 375 96 613 993 530 147 588 303 132 202 808 860 738 993  580 296 285 699 794 151 776 244 461 28 146 29 989 909 62 727 127 764 828 527 607 654 633 270 903 965 155 409 640 311 51 783 574 978 735 703 975 583 850 591 190 518 981 872 2 78 167 301 665 89 681 747 523 444 748 277 464 925 332 335 417 582 568 445 99 825 496 112 378 325 378 864 659 296 371 883 441 2 995 853 998 729 722 13 24 430 771 819 481 114 723 862 54 838 800 443 279 562 653 951  653 12 585 677 914 290 441 534 215 169 110 621 539 992 462 591 77 594 266 221 244 329 408 363 796 456 893 729 873 155 697 459 373 167 795 319 700 524 791 798 678 547 853 771 547 879 944 102 405 327 187 237 289 919 350 624 651 467 458 99 607 552 604 227 647 708 775 144 495 948 66 829 576 591 784 677 594 884 458 313 92 225 958 921 600 181 108 668 808 169 958 664 475 643 871 331 490 179 374 356   
494 261 85 594 733 660 779 827 639 433 960 907 434 771 149 225 239 991 678 818 487 127 659 974 225 449 454 424 745 801 249 765 874 348 599 449 947 433 282 291 381 415 905 908 283 604 969 308 904 788 241 312 639 58 176 285 338 567 76 54 294 371 199 856 232 81 950 120 460 710 83 939 168 124 903 17 766 519 140 248 422 232 675 163 5 929 309 459 974 469 74 102 283 231 757 532 822 235 702 104  633 550 202 301 511 591 375 95 598 358 594 280 404 818 90 783 213 526 340 330 50 666 610 567 593 910 519 522 222 273 556 575 11 612 550 198 211 480 252 477 898 753 212 512 620 439 770 361 403 533 628 776 916 373 553 329 822 134 960 852 700 450 772 318 349 163 80 191 352 94 298 844 164 846 651 61 297 539 66 141 63 230 486 938 257 209 929 512 605 260 219 660 190 88 47 24 864 364 179 617  450 56 970 622 624 872 83 269 48 469 913 152 974 417 144 296 897 17 434 158 868 512 419 353 784 341 902 855 922 671 80 367 344 317 343 374 355 936 334 231 673 493 734 513 219 987 385 292 672 197 64 86 609 439 41 190 110 378 768 778 773 63 32 271 834 285 313 825 359 485 768 748 867 444 674 862 895 534 648 541 997 377 142 239 959 743 147 752 974 857 410 899 56 955 640 56 648 994 190 117   
481 61 750 260 779 284 831 190 341 722 606 176 518 658 797 750 780 892 284 545 103 270 196 671 499 419 522 587 204 912 608 783 531 821 305 47 276 455 144 348 823 903 595 442 941 360 477 704 933 295 657 897 694 478 349 300 892 632 722 603 295 832 92 628 416 718 165 558 18 300 962 769 514 612 236 572 973 700 490 577 163 8 894 625 922 398 30 343 730 730 481 860 590 207 698 449 595 362 195 109  289 845 572 602 183 677 808 912 719 528 862 604 623 514 801 588 267 467 760 92 981 464 830 68 538 61 221 818 578 952 251 603 578 842 557 830 186 994 506 517 781 616 943 399 129 788 525 347 816 695 482 339 294 523 253 219 319 13 368 333 539 607 442 434 314 671 853 286 148 425 159 995 403 328 871 821 370 508 242 509 781 424 901 117 995 590 511 143 810 699 619 548 260 527 690 234 610 697 552 945  917 214 18 720 349 11 169 166 283 869 176 887 576 392 392 291 747 145 338 980 48 816 361 98 173 361 371 470 264 868 936 5 366 454 508 567 488 147 140 304 144 642 158 19 788 208 90 478 569 602 366 772 651 581 244 764 327 998 577 206 629 811 593 930 644 967 326 164 389 740 639 444 77 433 877 959 627 3 229 473 368 193 593 577 148 728 404 470 732 443 366 568 32 883 40 231 131 165 778 283   
828 867 125 295 829 58 544 686 45 335 339 642 517 837 948 738 621 948 409 304 410 779 882 215 237 134 500 670 777 902 192 661 95 773 311 754 525 273 308 635 517 145 312 634 63 79 555 40 760 460 744 256 675 908 169 50 899 15 627 960 103 509 190 528 52 440 196 152 902 709 580 931 469 834 602 841 436 444 141 888 648 726 907 475 618 356 69 970 732 633 977 145 869 931 162 723 857 68 152 16  275 740 276 936 485 860 954 625 666 481 918 100 797 92 693 327 880 232 303 10 481 96 687 532 349 734 189 426 579 637 226 61 350 896 572 614 662 379 245 694 478 162 416 24 335 143 678 164 109 678 669 897 686 252 322 573 468 213 790 459 578 508 755 606 489 897 837 11 75 982 227 183 798 768 550 732 106 988 837 477 420 514 655 297 312 336 886 326 136 39 57 654 986 527 476 916 918 121 969 86  262 584 289 834 243 207 404 961 284 414 536 949 208 133 45 207 536 144 381 697 247 197 788 588 187 869 440 809 903 631 507 192 340 850 387 931 599 887 756 362 949 901 680 820 638 977 289 738 714 857 481 987 811 922 458 386 618 325 777 738 120 89 172 651 2 178 963 864 228 43 162 471 763 177 626 294 79 647 528 304 465 900 115 253 254 979 920 755 265 79 65 537 399 17 263 537 249 710 960 837   
809 802 866 332 974 259 458 630 596 495 292 891 135 449 436 807 672 54 884 938 845 261 426 893 604 319 358 183 124 480 390 731 717 546 201 605 251 449 862 686 338 999 918 295 947 903 92 971 198 49 771 92 632 608 616 161 926 76 420 450 127 962 529 26 911 923 605 473 655 795 720 808 682 328 903 382 944 161 810 629 39 678 709 276 992 909 742 896 952 359 48 460 26 983 551 537 910 644 264 281  940 748 32 759 820 97 891 655 298 551 134 260 194 198 901 685 43 673 740 950 647 703 546 948 116 971 817 749 885 728 727 740 323 787 381 0 499 414 252 931 552 765 730 454 90 802 908 69 66 397 499 67 828 786 245 799 50 495 346 84 7 918 255 573 452 136 196 49 860 7 242 431 338 333 860 30 87 804 301 207 874 499 168 548 280 714 837 563 316 665 778 626 253 1 817 880 243 341 847 442  297 524 904 568 905 196 139 472 330 660 683 425 335 487 263 123 895 107 136 762 957 88 217 940 206 413 385 265 806 693 356 658 914 189 988 889 195 307 171 557 518 604 592 46 157 656 815 442 55 150 734 754 751 578 606 962 678 424 945 738 403 345 835 116 327 695 255 892 871 545 66 34 337 418 92 507 897 46 661 357 226 184 381 499 349 513 859 314 665 869 815 863 242 925 612 83 310 373 651 660   
362 734 128 281 518 326 938 647 211 510 488 819 354 936 548 887 596 440 683 203 200 261 947 248 189 444 971 28 223 794 135 154 168 518 52 135 668 842 277 526 944 395 842 349 656 879 453 378 712 666 241 982 912 145 169 868 830 987 815 498 178 240 661 709 347 420 437 57 690 463 260 616 695 245 307 850 207 757 809 308 84 4 713 336 455 417 244 881 750 960 945 341 357 272 471 676 838 415 187 263  177 285 405 960 143 963 272 663 273 951 780 582 647 449 836 740 793 425 467 261 595 339 190 391 296 897 88 350 823 120 1 303 527 345 281 445 707 547 429 196 722 476 145 374 771 874 831 106 68 517 545 53 97 393 955 218 605 201 552 718 299 242 533 881 577 748 741 959 502 48 519 54 605 166 702 238 949 588 973 626 45 444 347 578 592 344 17 978 400 597 868 824 177 924 896 698 81 936 345 15  528 21 940 830 197 981 232 659 783 182 801 386 767 417 891 89 625 390 481 962 794 669 325 654 382 233 859 719 193 566 884 237 163 287 718 442 77 878 253 965 662 906 668 956 18 4 983 493 715 484 256 387 546 410 904 58 595 417 529 497 761 864 561 959 949 266 357 596 602 947 920 750 242 924 160 183 328 165 622 276 864 484 480 745 536 383 571 942 634 264 450 479 555 127 485 866 374 415 10 546   
395 900 876 640 581 181 267 515 553 316 240 253 259 20 980 731 550 597 577 859 339 128 198 107 687 558 764 550 833 304 181 679 126 724 248 517 154 579 276 983 306 298 652 391 614 166 939 678 50 853 383 723 937 637 641 675 825 989 502 914 778 960 376 274 826 73 249 367 122 541 431 358 390 686 228 492 9 326 831 272 618 451 821 136 75 633 101 199 922 191 237 996 399 734 344 8 749 888 802 792  944 375 897 832 618 175 629 956 838 222 429 293 769 943 120 186 156 62 1 6 590 499 926 949 98 129 259 876 198 420 132 551 482 390 532 868 31 102 284 700 579 665 987 248 985 294 731 672 777 798 500 58 69 759 664 381 192 574 783 101 976 79 92 129 150 723 947 111 815 834 982 541 482 973 738 225 161 820 329 836 289 136 168 514 682 776 418 122 938 782 541 276 967 943 212 621 204 440 45 902  238 658 772 97 412 202 410 378 848 548 213 682 294 60 499 162 390 430 793 110 923 294 253 741 375 284 544 634 628 219 80 754 10 656 528 919 531 601 271 186 455 955 176 783 750 502 887 929 693 251 847 860 390 410 165 909 115 656 522 207 677 652 737 466 183 521 965 842 868 463 702 26 372 993 801 845 465 854 174 338 625 59 143 202 451 271 492 424 292 929 228 459 87 330 655 109 875 674 131 29   
1 875 250 71 358 22 444 820 926 153 956 819 525 840 342 469 897 138 577 376 549 448 808 923 487 761 984 439 634 124 433 669 738 986 214 997 889 788 616 463 900 308 986 193 917 506 46 373 631 593 890 666 961 893 780 39 504 897 653 377 408 370 597 830 911 290 575 987 132 663 225 935 893 74 268 98 730 682 921 274 1 337 649 513 964 287 467 59 673 975 865 913 841 381 881 683 598 729 648 149  435 235 855 25 537 232 220 467 797 650 475 200 65 398 691 125 815 730 789 434 322 516 307 29 865 148 984 601 283 399 210 96 622 802 332 587 48 510 958 878 929 932 609 370 717 137 756 926 641 111 55 457 288 735 591 761 797 892 338 745 319 142 359 894 346 948 754 785 361 514 460 514 638 841 314 847 400 224 202 239 488 532 12 44 121 991 175 448 83 128 151 481 319 763 51 310 839 729 763 678  591 880 183 638 381 564 619 784 712 651 382 938 545 733 450 105 535 593 539 151 554 809 264 100 18 189 76 172 753 452 443 853 463 735 955 286 588 31 949 880 539 199 611 482 528 592 437 172 273 558 612 474 932 535 107 451 491 168 673 856 883 211 551 283 786 805 562 840 853 297 518 35 168 80 970 915 706 372 198 12 453 31 319 600 474 820 809 24 850 168 483 410 397 870 37 646 517 521 59 435   
167 333 819 127 367 748 874 985 804 755 629 150 833 812 197 852 756 716 41 494 972 557 24 626 85 811 779 454 113 26 783 236 263 547 622 640 192 868 370 341 854 783 20 440 933 37 212 16 344 470 162 678 671 958 350 174 685 411 918 411 426 398 676 355 83 515 349 428 171 258 624 4 132 814 635 873 734 158 952 86 985 819 52 397 213 81 507 138 597 840 77 804 526 109 672 238 58 155 922 958  111 406 786 91 394 330 255 241 520 192 984 288 366 743 243 886 362 791 967 885 401 884 759 761 281 417 766 195 59 949 476 415 369 762 635 900 381 645 460 198 607 56 549 392 267 238 633 594 29 241 320 218 544 549 329 330 110 810 909 479 181 11 32 791 959 466 262 515 127 5 457 387 916 568 401 987 386 822 898 5 540 563 527 98 988 976 738 764 71 814 860 919 862 753 915 698 77 885 345 694  974 5 459 713 820 864 509 222 624 569 474 648 655 240 584 782 535 578 449 408 188 821 33 232 819 413 682 95 991 521 744 296 528 305 931 120 694 918 322 686 609 719 557 818 114 902 133 646 639 892 425 910 336 810 393 337 631 941 185 14 491 182 951 939 412 109 124 439 986 790 199 247 508 621 13 197 578 244 78 490 854 773 966 332 856 940 593 59 399 514 777 562 742 777 2 547 420 876 634 915   
626 553 623 340 600 222 120 182 872 422 180 63 709 865 834 42 297 419 632 252 968 250 193 884 10 577 625 570 48 296 675 242 714 691 105 302 983 789 625 352 339 848 983 243 561 323 122 868 667 779 24 567 610 868 59 512 724 887 751 673 917 646 951 454 277 985 647 638 923 125 189 945 307 225 247 634 781 95 677 818 517 797 527 618 187 868 180 498 81 713 18 845 881 679 23 743 2 343 707 833  411 405 544 23 771 713 868 16 431 872 554 230 528 661 406 624 406 846 893 251 950 76 890 945 29 158 537 822 580 385 728 330 391 555 410 439 330 463 989 422 310 698 658 315 980 887 240 745 331 591 188 510 862 986 979 821 103 855 612 698 327 479 732 386 493 491 173 173 223 384 521 628 415 409 500 107 130 549 64 760 158 695 997 218 767 891 462 457 542 933 871 731 990 791 512 818 952 384 534 870  86 360 714 434 969 900 250 393 437 848 972 915 107 142 480 714 955 367 754 731 904 846 663 198 244 791 910 769 751 578 172 27 569 968 950 555 502 128 362 477 30 609 0 299 253 311 460 747 588 677 187 671 17 266 25 621 523 854 156 42 187 610 464 622 102 465 419 629 983 397 791 856 603 235 50 265 834 442 774 10 518 19 402 555 269 84 33 464 83 12 442 839 855 801 774 749 701 0 6 373   
897 592 600 488 992 56 286 962 494 589 23 7 817 693 273 704 139 739 761 269 9 345 319 314 48 229 604 386 671 551 233 688 253 916 732 717 596 697 417 211 198 857 875 375 451 39 960 870 21 159 893 157 88 372 47 154 46 748 618 381 737 873 591 564 782 616 903 10 684 259 547 674 841 923 447 448 973 668 333 338 302 369 60 629 107 468 116 210 716 64 345 568 239 517 124 627 356 154 635 243  800 453 393 589 149 608 862 151 872 154 219 234 716 232 350 426 602 837 499 717 679 175 212 359 605 604 174 135 105 803 959 510 471 264 627 86 573 586 672 225 20 952 916 755 808 69 957 786 263 186 639 554 469 988 368 818 43 689 723 202 561 749 956 849 248 781 925 35 555 516 17 402 684 444 601 577 477 830 929 317 228 258 312 302 83 219 900 28 917 928 741 120 975 160 720 325 462 195 291 376  119 751 971 211 790 711 867 886 635 520 427 407 293 520 247 388 19 115 135 100 62 677 293 760 394 403 329 978 347 105 576 360 817 689 901 738 644 768 848 286 309 142 915 910 245 620 74 461 633 956 380 908 489 737 708 794 223 77 871 281 585 781 97 675 799 921 489 825 983 26 715 610 506 545 999 231 694 113 243 410 529 358 838 941 57 63 864 564 937 799 959 234 527 524 280 573 281 813 362 904   
576 808 344 507 819 629 995 239 591 594 181 302 573 72 131 827 588 827 760 194 899 132 549 690 691 839 824 159 985 981 26 5 236 695 266 793 134 684 560 552 514 941 539 194 622 257 474 185 506 552 873 388 472 867 645 321 211 611 883 495 838 274 597 685 169 732 301 411 701 20 58 368 450 0 127 714 382 4 979 401 26 365 990 479 197 457 96 354 532 85 208 356 752 899 122 408 409 102 933 20  286 159 716 27 4 207 999 498 261 968 943 827 501 442 634 285 639 998 644 137 29 469 836 427 747 884 701 388 952 821 530 265 239 425 926 595 707 16 101 890 992 236 562 280 436 811 722 22 284 499 748 128 775 43 378 901 991 31 301 152 857 111 662 69 562 594 392 497 47 323 655 818 429 150 183 84 344 341 912 993 90 135 804 998 518 798 844 119 645 966 586 190 684 955 262 626 76 218 558 635  161 865 575 751 999 83 930 564 964 946 444 553 890 409 833 101 322 277 80 381 285 516 47 686 541 498 893 113 821 582 334 363 866 835 289 694 115 433 128 257 729 845 948 338 974 431 902 152 914 701 162 565 324 794 244 256 17 116 446 498 961 647 442 523 669 784 690 378 991 936 725 145 460 453 459 951 765 405 725 793 923 965 332 531 205 327 779 256 466 361 581 919 656 3 187 992 336 599 691 137   
344 898 15 380 465 990 594 247 27 971 647 336 823 427 786 131 700 729 737 423 73 235 233 577 640 186 671 728 258 281 664 788 396 770 826 927 771 973 989 196 637 548 345 733 222 311 837 794 700 613 544 301 408 808 640 233 112 446 916 43 125 435 269 184 722 879 56 21 409 961 528 206 409 293 821 908 45 125 640 353 924 700 616 507 153 740 654 589 798 323 917 574 87 299 972 291 504 205 929 483  606 864 934 190 368 719 674 368 989 381 483 37 252 760 547 200 1 908 289 460 269 205 482 695 272 205 235 773 898 167 323 812 230 513 332 414 270 80 610 372 641 731 870 738 67 892 899 240 996 514 887 303 159 573 971 79 247 811 927 553 730 467 330 784 859 476 573 26 345 201 678 277 89 12 313 494 361 546 668 925 365 66 237 249 789 960 78 764 291 564 754 870 120 285 241 140 460 871 406 798  564 49 364 477 547 360 612 906 964 517 70 522 964 173 810 973 455 694 700 566 346 116 576 212 293 644 316 991 581 516 522 109 390 224 117 248 553 895 852 488 784 682 478 787 945 910 861 359 696 14 527 537 653 919 909 125 408 513 348 829 996 250 265 724 489 126 84 435 741 10 865 946 847 564 855 924 681 850 663 607 95 311 14 15 901 889 845 85 748 231 877 175 395 304 704 637 393 419 475 706   
469 93 659 78 695 107 964 994 14 180 89 480 726 419 134 117 542 434 594 376 479 907 538 838 804 582 660 967 538 147 532 419 313 939 335 634 72 112 143 573 364 201 717 21 196 130 285 970 155 295 241 322 170 139 774 306 83 20 377 186 149 220 607 171 348 466 447 392 961 100 604 112 603 235 340 278 266 427 657 98 167 901 772 999 417 544 719 929 130 264 162 211 557 919 1 614 77 459 192 768  41 226 148 725 714 654 4 111 568 804 317 10 115 432 878 140 182 608 795 148 97 501 907 821 913 972 835 798 374 522 575 973 104 877 156 375 425 123 889 643 257 769 599 673 220 349 433 459 649 1 767 872 507 714 977 408 307 180 129 140 606 269 582 217 610 392 373 915 464 828 111 837 290 443 918 151 633 110 27 62 91 765 472 967 888 375 182 326 127 833 966 520 453 990 708 330 502 225 290 468  122 573 604 889 351 351 218 519 887 716 522 655 725 399 795 941 467 34 69 694 113 615 55 730 24 882 904 604 200 133 899 516 487 502 563 1 816 506 718 34 959 19 609 723 711 910 556 248 764 175 940 460 62 450 288 272 515 389 48 459 725 554 151 303 755 533 446 940 384 267 967 76 987 921 375 284 727 277 79 563 679 505 425 713 334 632 578 195 928 564 18 848 108 459 366 482 574 810 567 438   
304 671 724 673 550 521 471 734 993 723 674 833 698 722 636 648 152 458 24 820 480 468 85 587 124 907 112 743 878 746 698 3 203 958 416 60 557 308 556 203 63 428 729 557 706 143 34 481 196 13 611 397 351 675 835 382 603 824 434 849 115 168 123 750 500 531 83 823 945 679 769 382 898 751 339 43 646 525 767 36 804 442 189 760 993 860 441 309 275 297 656 240 109 339 310 524 784 739 730 253  254 160 751 431 483 201 385 613 851 448 93 749 187 224 353 313 241 898 899 295 280 664 391 661 597 733 413 851 468 324 34 876 667 101 339 899 349 995 989 9 281 458 363 67 408 497 894 450 317 576 957 572 293 894 909 99 73 31 944 970 282 77 959 812 972 592 229 832 218 420 784 538 205 270 238 367 436 45 890 748 707 898 154 334 720 84 623 620 398 470 921 608 516 640 322 559 834 951 45 334  991 799 643 538 262 256 768 460 577 996 328 271 252 187 543 29 701 325 574 773 850 45 521 641 613 451 170 362 681 766 851 615 72 922 618 193 754 84 865 827 130 839 59 125 609 761 506 400 422 281 960 162 428 777 750 451 482 122 999 998 409 454 65 404 136 892 477 435 694 183 372 502 241 277 868 590 157 21 837 625 139 27 456 510 606 834 152 616 511 489 944 528 984 278 338 204 673 203 155 146   
353 47 381 851 160 13 780 873 660 259 476 813 79 536 291 203 277 255 383 718 834 694 409 497 391 583 119 548 95 105 396 166 622 492 681 448 215 26 159 634 327 129 127 434 987 193 280 187 133 254 134 734 102 916 356 884 438 961 864 590 892 825 794 865 578 762 270 468 283 599 630 931 971 246 401 295 6 483 777 243 397 750 171 361 221 565 774 557 819 541 842 760 885 170 832 571 678 590 901 258  452 164 353 589 186 486 384 439 105 449 997 829 687 504 924 679 617 154 205 526 558 590 997 953 130 454 39 657 831 775 869 48 879 595 681 369 614 56 642 653 541 258 535 859 51 977 498 154 789 703 213 814 972 194 418 121 92 356 332 478 61 434 975 609 961 164 259 750 270 594 118 925 784 593 477 983 845 819 667 372 261 558 761 97 19 488 142 499 894 212 711 873 17 836 982 314 528 34 711 662  9 945 787 658 417 677 30 185 77 864 413 612 789 591 885 567 957 389 132 574 639 700 942 847 370 893 604 627 548 565 495 819 964 303 323 469 464 64 831 585 739 834 654 969 552 985 558 864 970 327 754 267 189 201 257 915 648 15 204 668 306 537 602 797 17 340 201 793 56 576 943 746 610 50 687 155 889 330 938 617 855 346 906 485 297 458 533 483 557 726 230 565 696 294 848 590 969 935 539 509   
242 290 503 607 122 23 653 810 886 166 817 479 304 443 314 491 902 447 275 991 946 562 768 828 717 686 349 744 672 239 10 783 275 665 364 670 585 922 88 886 760 976 267 101 18 925 411 491 118 372 932 726 525 585 494 493 323 305 488 529 47 454 499 206 604 587 270 635 850 788 247 187 56 64 985 167 189 21 701 316 72 963 873 982 425 392 198 647 897 478 829 867 170 510 997 432 924 622 518 983  340 101 639 867 195 63 480 364 967 162 101 291 492 748 402 658 328 87 50 763 69 329 84 606 360 475 483 772 778 955 912 712 93 675 838 561 609 272 873 385 945 495 871 579 586 212 923 496 289 249 797 631 6 229 927 486 273 319 173 422 776 118 384 591 330 827 997 783 551 668 524 457 379 300 608 518 309 979 341 930 799 153 224 27 874 309 222 5 175 94 218 51 560 204 226 708 815 961 652 663  777 803 121 352 289 574 329 46 383 898 220 750 884 215 63 628 438 622 944 379 523 612 927 712 27 172 476 533 886 490 45 136 137 978 134 550 514 6 3 435 533 567 106 706 565 918 810 126 1 6 348 874 229 192 463 968 40 932 140 796 287 64 733 312 195 942 957 568 383 945 664 252 762 55 235 488 13 270 956 525 142 263 771 614 773 774 840 463 996 238 313 64 243 811 69 467 54 781 990 204   
975 895 465 53 783 352 97 727 583 618 867 996 926 484 375 557 983 599 773 799 595 302 340 15 788 144 897 489 651 452 896 617 592 627 964 739 887 566 559 77 61 998 573 912 125 764 762 294 578 971 621 615 199 145 55 673 431 709 478 635 214 155 665 606 355 866 34 210 868 111 348 601 334 249 939 856 953 526 39 511 663 503 803 880 844 259 779 992 633 637 793 608 893 940 905 575 128 571 326 604  116 830 979 840 41 818 558 413 965 848 930 187 307 496 93 804 182 178 151 625 113 402 830 277 683 955 999 311 89 915 113 382 630 764 588 666 581 878 502 97 443 668 811 144 72 342 222 121 905 348 581 517 747 642 875 388 350 488 474 931 153 184 847 58 775 309 744 446 334 792 191 789 273 237 870 949 366 298 274 945 433 466 464 107 557 280 601 321 929 212 596 532 503 454 667 353 517 74 826 138  513 376 40 767 240 775 208 851 634 969 836 716 500 300 184 718 130 656 458 958 417 866 695 671 423 302 239 15 225 820 808 282 73 605 253 370 78 10 506 363 510 136 281 116 406 55 316 709 768 706 732 143 163 565 306 708 173 546 549 782 618 81 867 864 988 975 897 907 521 162 307 89 438 652 225 744 493 503 244 365 205 575 909 902 431 955 469 948 943 508 52 208 411 972 852 481 649 330 308 317   
944 103 45 338 970 805 679 368 782 919 634 824 63 891 103 885 16 111 835 782 87 730 594 596 372 511 686 230 710 850 997 770 501 992 399 505 879 226 749 485 431 474 927 71 39 131 700 180 798 728 950 126 541 968 906 181 350 551 168 298 799 393 840 136 319 754 57 695 197 218 880 778 130 330 273 463 547 884 630 733 803 388 655 522 917 80 534 382 877 939 750 502 406 180 723 678 589 964 986 448  720 429 362 199 203 286 332 949 855 856 574 869 724 347 516 477 798 519 368 7 979 185 386 765 715 451 940 735 386 884 69 551 190 486 972 615 123 50 776 833 539 619 683 483 364 488 861 156 522 934 50 351 526 421 308 99 114 104 161 739 633 781 972 683 221 364 274 615 328 398 57 501 53 168 450 767 147 512 204 418 714 744 45 951 399 672 571 117 415 557 88 39 100 451 142 964 619 456 273 621  642 326 281 603 339 200 8 438 417 330 73 566 339 240 634 77 225 422 333 791 234 441 933 813 755 889 45 748 196 895 354 709 458 854 321 11 456 333 300 589 483 59 103 796 717 800 67 195 427 388 900 28 269 962 970 274 150 790 519 983 893 620 616 433 815 279 693 265 788 769 81 296 861 967 841 241 915 433 495 959 666 665 940 574 527 847 50 567 929 510 493 425 787 665 894 377 825 578 285 900   
887 811 758 714 454 140 846 454 693 691 722 103 312 664 418 156 403 689 677 476 299 749 484 590 177 164 280 544 371 835 510 486 820 867 271 277 380 993 852 167 754 556 276 203 246 611 917 431 239 865 108 657 317 602 707 982 667 795 814 8 796 314 336 689 888 767 412 786 408 636 359 215 239 894 207 766 521 256 683 413 371 406 758 398 231 693 348 167 709 163 743 503 279 443 201 596 406 180 310 698  313 232 940 614 537 293 55 10 927 607 857 902 559 903 690 117 506 827 367 260 424 191 631 329 795 702 88 138 494 362 495 357 456 671 927 741 46 140 493 138 747 236 910 411 292 207 723 597 689 438 220 786 688 93 648 250 347 388 669 49 294 527 331 174 395 149 325 791 194 65 991 616 767 993 882 877 490 736 963 951 57 528 464 960 727 850 927 690 605 13 349 865 52 65 905 878 204 74 109 418  304 507 424 418 783 635 104 140 280 937 729 648 795 42 575 690 581 168 717 277 702 191 813 136 19 330 568 970 604 428 480 587 818 827 856 628 202 32 755 327 947 278 807 589 914 707 911 282 396 148 759 539 496 922 828 660 574 551 999 950 534 242 674 408 665 448 299 756 988 669 258 742 26 246 905 968 268 62 653 340 276 635 246 968 510 680 731 417 401 450 453 417 252 686 493 31 500 128 867 319   
541 397 359 62 868 11 246 528 220 873 977 900 229 438 734 171 581 675 28 83 679 563 71 393 973 839 577 655 540 565 866 410 229 96 390 530 789 565 830 643 946 804 829 931 989 788 914 387 125 103 271 533 142 335 779 215 131 929 646 793 390 521 133 857 684 383 838 156 485 18 220 676 536 714 138 677 599 949 204 528 411 3 876 443 464 702 260 643 162 517 678 828 665 468 172 921 455 294 668 189  106 388 97 849 436 781 726 192 649 773 780 468 317 470 411 527 238 215 278 813 161 125 575 77 649 160 532 422 350 323 104 417 882 451 308 906 47 959 781 154 113 630 115 243 906 697 347 443 153 19 532 381 775 649 50 677 849 722 186 172 613 10 144 763 630 315 40 667 861 247 684 331 455 520 745 863 443 780 859 381 769 432 947 723 870 547 534 36 18 382 230 191 558 59 369 261 231 764 326 523  756 605 215 462 343 984 395 382 770 299 844 572 214 302 81 532 819 327 517 680 576 631 486 301 501 432 482 868 841 644 909 426 916 765 4 632 4 666 69 832 817 858 316 435 734 191 168 550 910 482 419 125 417 318 327 883 404 814 147 480 774 113 338 470 701 351 986 168 810 452 571 35 70 943 773 246 616 638 580 385 706 219 23 222 533 776 730 438 886 596 53 428 710 942 272 793 894 898 58 485   
711 404 852 543 193 777 122 630 266 823 541 950 294 21 561 227 539 948 345 298 589 949 202 972 824 130 38 992 825 422 162 349 143 427 105 716 648 261 690 192 266 642 421 299 981 518 406 409 586 981 463 894 312 410 872 963 602 719 758 556 613 595 479 423 501 60 7 171 386 781 793 413 479 336 503 771 841 731 612 624 278 120 768 367 326 525 378 860 274 849 300 231 9 712 892 628 346 241 452 569  481 727 169 933 977 581 333 406 511 54 377 270 243 70 301 168 191 28 135 95 644 372 525 144 217 496 521 412 751 591 342 3 665 577 882 176 632 374 489 165 948 8 185 742 967 51 351 854 682 138 116 283 190 558 920 542 848 526 370 795 916 941 947 940 272 457 912 480 287 857 759 885 366 614 904 732 415 582 762 715 643 986 114 638 948 686 146 258 115 214 905 868 806 182 149 101 124 927 350 950  109 898 259 450 453 170 941 334 379 904 569 913 93 815 255 780 624 531 660 826 950 525 1 798 619 706 640 202 132 369 679 871 739 587 189 261 442 232 596 211 316 182 803 923 470 473 753 328 481 269 577 827 665 574 518 888 831 107 171 173 116 998 369 433 917 700 6 999 813 180 355 347 543 289 743 134 59 270 404 730 654 170 607 995 628 382 744 851 249 584 609 953 783 974 145 278 530 753 187 261   
313 84 784 509 584 260 113 211 916 983 948 610 848 872 249 430 999 182 554 0 889 202 522 48 607 862 905 874 482 18 724 424 336 161 444 57 459 815 402 520 191 378 885 6 829 983 108 810 164 36 3 258 353 253 472 577 986 500 999 50 137 175 169 367 820 126 628 332 395 952 455 26 157 748 260 949 311 437 653 274 144 98 976 745 166 858 112 65 69 279 556 893 987 336 143 552 272 398 713 881  257 417 180 520 936 183 367 696 448 620 730 100 956 593 645 738 537 945 157 656 303 782 643 14 12 751 254 604 839 110 142 264 118 704 91 311 444 797 306 542 9 659 172 893 468 211 392 285 376 876 472 625 620 827 762 126 731 145 992 9 708 214 829 986 605 841 828 586 307 866 400 796 307 894 783 602 889 608 344 998 833 82 837 340 983 120 427 865 44 15 628 902 166 414 878 482 637 196 300 64  146 108 966 69 721 285 101 595 390 295 462 334 859 867 933 358 655 111 735 475 85 733 86 648 170 287 394 89 588 142 390 7 13 798 72 249 546 473 542 35 978 611 845 505 728 427 631 251 220 705 277 661 920 716 548 118 29 991 987 20 782 722 554 904 437 576 528 143 418 147 870 303 762 308 382 554 243 905 160 395 56 462 687 912 848 390 396 647 343 687 701 327 265 898 959 84 774 970 422 907   
950 553 416 174 109 760 170 561 190 524 210 751 94 208 922 631 173 192 861 933 681 456 804 7 719 31 572 930 968 958 470 467 337 9 119 654 897 349 597 903 725 930 455 693 740 295 617 750 837 343 425 920 374 310 819 904 25 391 201 854 737 623 992 805 722 677 889 663 586 779 962 392 50 976 647 711 194 612 741 217 718 873 103 288 586 341 535 637 267 533 858 550 669 538 872 119 157 957 923 169  364 174 692 162 958 991 850 656 406 856 52 275 916 456 874 441 857 589 928 567 421 680 465 402 484 895 572 51 852 771 931 635 0 463 421 482 503 414 305 788 202 394 911 382 107 135 359 515 592 653 870 706 372 260 937 24 943 305 572 293 291 93 25 78 929 49 900 691 391 755 347 116 85 174 50 162 662 492 684 768 811 535 945 380 931 976 430 535 377 946 63 620 582 241 25 449 62 812 246 255  99 159 722 214 666 794 425 732 303 483 558 34 912 771 614 315 942 519 565 913 825 826 48 7 18 413 132 897 595 685 518 583 498 587 84 100 142 169 569 889 440 592 586 104 358 37 729 192 305 918 165 754 259 115 96 905 513 883 518 321 973 269 231 890 981 399 792 205 376 969 739 747 226 186 218 308 277 593 163 843 404 422 455 48 984 108 724 976 169 17 243 406 850 153 352 747 600 493 137 334   
586 623 670 590 576 265 267 270 797 131 594 122 614 676 880 358 788 654 459 716 535 403 317 138 334 442 953 293 264 501 968 516 682 242 525 34 461 618 880 367 963 143 721 470 898 613 95 412 367 480 843 92 341 388 142 781 612 960 743 443 199 795 838 533 421 342 578 540 400 66 942 939 276 734 329 128 92 283 895 41 73 196 412 306 467 326 810 999 840 241 857 274 420 940 2 813 798 971 567 455  980 805 779 611 458 235 216 50 212 704 225 254 641 924 618 498 491 737 311 797 827 367 370 662 971 288 754 644 97 360 850 292 911 500 640 889 186 101 127 600 268 509 559 708 870 841 932 865 548 484 186 35 252 775 665 755 725 43 285 917 473 221 234 998 556 655 409 821 273 217 122 70 334 496 104 197 696 426 673 929 202 693 246 345 907 194 890 669 166 714 323 296 459 803 400 567 211 248 201 688  470 97 951 9 287 85 686 198 326 481 37 184 560 648 830 387 168 593 734 89 619 741 229 562 2 68 934 519 24 462 497 621 202 721 257 772 640 918 903 688 378 115 677 454 210 31 511 74 708 851 320 123 936 329 424 823 633 861 763 769 888 358 121 602 159 997 47 70 243 615 220 994 197 830 979 314 577 371 936 732 406 40 198 254 253 698 887 457 364 322 499 483 973 613 245 634 633 746 653 865   
52 295 291 428 254 17 513 528 943 255 725 474 795 143 440 218 213 376 463 720 517 344 99 237 774 757 784 324 766 526 169 586 275 355 879 23 529 588 257 773 482 599 832 945 364 307 303 362 549 772 566 567 111 553 581 447 817 466 419 739 802 268 794 316 407 814 193 533 131 962 41 93 331 918 225 829 930 163 273 278 339 699 481 136 22 74 639 353 19 849 505 869 111 664 765 634 730 611 709 507  99 46 163 872 816 250 606 62 679 952 361 1 715 388 85 788 190 465 917 880 955 319 108 719 623 495 187 365 939 472 77 104 591 406 200 0 444 225 157 493 965 283 340 800 333 29 91 647 725 864 380 668 61 904 102 212 930 976 309 50 203 153 576 382 149 668 405 995 21 10 413 543 466 315 319 37 364 760 762 644 348 111 353 892 716 436 325 293 49 312 726 715 64 209 979 858 376 623 748 263  97 629 930 121 732 862 337 277 622 139 344 901 249 214 408 451 571 375 866 578 502 755 161 181 607 619 307 971 191 970 302 442 212 167 295 500 889 903 877 903 191 719 243 247 984 974 330 846 95 596 883 245 331 800 880 639 488 17 193 649 654 55 450 454 39 671 555 366 903 782 337 750 766 28 209 760 69 762 671 292 817 945 444 532 339 190 480 456 342 439 213 753 771 928 687 224 816 538 695 783   
121 161 593 924 117 757 88 523 757 487 755 782 854 782 561 688 283 209 363 123 370 665 755 313 243 487 391 496 368 336 819 669 589 614 22 325 354 34 188 446 927 137 642 399 337 520 738 759 965 855 266 983 573 870 994 754 745 492 549 759 199 395 995 871 708 211 574 708 550 776 558 546 981 890 458 468 578 757 150 558 314 758 139 365 357 151 156 427 866 989 555 655 792 373 161 997 401 105 728 680  902 393 786 608 26 764 542 161 638 543 983 12 953 674 341 556 917 726 59 862 714 926 909 354 691 301 290 468 432 329 600 491 765 385 859 692 706 67 82 961 823 655 742 341 707 559 566 269 723 289 47 594 114 344 122 730 159 728 481 799 550 832 141 129 335 998 713 165 10 148 648 466 497 505 237 868 355 492 41 14 360 767 942 651 817 314 153 33 165 767 279 661 753 780 380 747 993 562 708 475  650 452 819 326 178 829 421 780 159 109 337 691 16 720 230 199 179 562 424 2 742 824 355 479 118 410 621 134 568 360 281 611 85 438 606 888 103 424 750 123 857 299 799 744 196 211 265 377 344 768 287 551 978 286 423 845 723 40 289 20 472 439 587 309 649 93 479 834 385 857 368 295 253 850 645 830 843 99 61 102 98 114 586 109 31 610 793 147 649 975 639 298 328 825 789 890 568 191 5 353   
810 729 782 172 819 90 91 346 66 568 275 535 36 429 650 28 289 289 577 108 846 195 97 463 547 749 604 115 961 596 791 637 691 22 308 950 795 115 879 300 221 718 589 181 265 883 397 582 450 894 335 699 265 170 793 917 749 133 710 70 90 728 355 659 469 276 573 737 522 972 692 234 317 83 284 41 938 143 568 79 735 178 972 843 889 64 271 590 850 676 579 602 136 623 649 184 774 264 475 231  932 359 8 711 385 981 542 61 987 300 53 762 850 539 895 502 721 289 5 880 180 906 689 3 254 928 301 529 108 320 253 156 552 443 502 377 811 422 812 566 350 721 145 68 704 947 908 571 422 156 185 153 401 16 335 985 388 703 65 894 3 292 267 426 559 581 510 231 610 461 674 337 548 366 458 92 232 885 278 343 532 858 29 998 695 758 780 243 53 715 828 666 357 558 905 347 51 70 941 122  811 691 781 280 914 375 120 43 171 461 611 791 603 305 730 47 47 326 885 210 645 233 251 718 52 380 567 854 423 728 582 699 627 157 120 358 279 335 833 987 976 527 745 498 733 495 993 2 893 671 41 457 325 2 633 309 438 730 952 170 555 506 164 562 611 478 755 474 282 59 148 781 753 226 825 493 507 388 161 998 161 819 324 967 894 349 47 5 282 806 641 946 858 669 403 384 647 376 978 241   
126 190 569 853 252 45 791 143 542 622 285 517 239 301 671 916 639 832 194 875 895 988 721 514 525 508 252 431 911 262 331 439 520 383 140 911 392 275 289 556 575 389 313 176 364 143 110 174 36 408 871 699 902 63 561 412 984 258 838 523 133 102 571 838 598 576 482 64 352 885 29 394 602 335 178 157 807 617 609 752 231 326 435 601 621 46 239 46 881 828 397 575 560 12 242 726 939 139 728 995  635 704 867 584 735 809 78 284 187 793 391 929 99 233 288 734 454 12 444 53 784 418 567 558 717 429 24 865 598 106 728 988 610 217 338 665 754 243 451 913 694 358 300 27 131 80 459 273 318 654 319 443 648 548 68 567 798 213 688 387 810 742 915 877 115 305 227 854 565 881 78 567 131 701 508 860 49 86 356 396 593 366 133 245 506 178 339 123 511 251 866 648 114 941 465 174 137 602 501 817  608 787 254 753 538 841 920 687 750 210 449 55 415 792 578 918 797 133 290 483 417 688 257 516 168 718 364 902 249 862 849 617 251 648 395 7 692 670 267 800 426 378 140 845 52 274 324 824 807 552 417 32 853 594 422 692 201 863 241 408 836 776 997 281 986 986 79 0 111 885 557 440 963 72 743 790 347 546 215 395 444 266 927 387 824 286 889 237 971 551 602 295 513 245 612 836 558 573 466 636   
856 425 95 212 695 664 31 448 832 631 794 550 364 419 637 713 261 719 51 703 875 878 599 64 947 64 142 107 688 108 418 529 744 924 830 122 3 102 879 70 53 38 368 208 577 92 98 542 543 29 911 604 547 686 231 334 308 709 630 74 179 208 519 933 814 993 473 273 325 802 97 508 361 734 651 993 928 351 292 477 894 418 739 591 706 405 240 726 754 150 472 508 161 417 537 649 948 588 800 540  741 679 177 97 720 401 305 80 591 771 56 270 340 483 806 766 373 633 238 488 479 206 521 938 315 401 704 994 778 758 45 97 522 35 651 165 194 501 632 537 764 862 601 244 900 221 92 389 990 179 478 798 697 901 352 609 412 444 904 362 216 82 385 755 348 34 234 935 63 523 0 394 992 192 204 529 106 509 614 682 52 833 953 44 220 807 464 75 17 405 508 383 323 386 814 570 511 691 928 705  187 383 722 710 635 703 510 190 880 271 97 756 452 77 232 886 993 234 154 718 592 529 113 242 485 615 340 838 292 18 888 836 624 614 515 895 172 965 193 779 18 632 287 320 109 104 771 506 392 904 990 815 286 951 202 973 944 795 675 543 387 244 796 776 132 775 991 355 48 743 709 73 977 469 609 778 21 835 991 995 86 589 614 167 54 288 339 647 396 255 643 518 189 297 184 537 105 642 401 940   
333 679 404 114 10 776 546 633 384 425 143 315 167 98 77 211 661 143 648 233 799 989 530 572 43 252 654 699 935 115 314 538 827 92 937 441 393 959 130 532 840 28 128 666 517 404 919 340 805 805 424 63 561 669 713 503 800 919 153 356 87 698 16 252 125 750 960 904 835 156 304 990 731 311 340 269 463 966 135 616 344 915 540 590 895 571 538 826 475 891 446 568 352 172 320 922 766 868 823 619  271 375 897 135 165 249 995 233 417 568 773 187 841 768 80 550 967 62 329 442 572 504 185 399 621 513 932 589 900 188 681 779 993 214 515 28 547 796 35 165 949 991 717 471 416 974 141 594 362 697 745 517 975 43 621 244 754 170 975 745 672 818 713 706 23 713 10 974 871 282 603 178 472 905 399 454 333 381 679 306 414 669 246 904 577 225 90 111 884 51 190 280 643 835 76 186 391 889 602 725  238 848 782 740 721 920 237 512 833 161 768 296 179 174 637 271 292 594 248 380 614 494 125 638 253 483 821 237 925 386 766 474 316 653 568 459 640 952 232 505 222 727 895 409 0 535 192 175 833 563 624 979 59 710 504 844 65 544 331 866 555 236 78 294 970 700 153 622 926 18 876 78 29 342 479 758 677 931 126 213 71 676 423 705 609 646 442 32 344 492 930 913 332 683 699 330 65 47 988 341   
374 173 776 610 636 651 234 567 67 412 530 629 599 619 971 32 701 649 753 106 927 907 954 549 877 133 52 424 283 55 609 582 398 922 162 63 310 790 381 510 683 853 478 806 251 347 59 890 545 233 341 330 411 55 6 272 119 998 820 894 216 228 942 820 297 22 357 678 845 150 131 298 223 432 440 945 237 644 856 203 330 44 513 145 935 54 475 467 538 244 590 930 241 536 586 903 186 950 477 670  393 671 226 787 745 380 707 441 891 935 755 193 387 549 90 768 58 847 777 835 962 812 252 831 330 957 963 126 831 701 108 682 976 162 536 0 204 275 566 834 181 36 398 280 191 209 785 708 228 88 565 486 278 506 80 208 173 935 991 540 295 54 807 700 2 446 962 297 254 84 643 46 18 607 459 238 241 142 990 984 528 720 769 467 596 495 421 568 812 8 469 671 768 816 307 627 577 257 37 264  812 515 602 233 933 493 228 497 990 162 575 236 312 267 788 158 937 309 140 516 773 352 699 974 547 403 684 293 429 285 421 743 772 931 677 637 89 915 775 376 910 420 611 73 326 178 966 252 142 538 463 995 753 137 904 806 116 14 356 158 694 458 429 880 586 565 646 869 39 836 936 420 550 641 231 856 887 261 185 870 857 564 871 769 127 25 739 577 318 740 947 681 855 707 617 39 580 808 835 689   
227 295 644 188 905 391 90 104 742 117 654 477 475 918 630 415 141 27 707 803 824 342 538 829 915 206 197 5 319 958 202 230 202 463 45 261 84 598 949 102 818 495 478 816 127 37 932 735 21 609 230 24 924 618 646 128 380 494 695 302 39 698 841 954 86 236 6 800 175 35 798 952 445 150 204 902 941 542 436 158 167 582 600 587 888 504 135 96 270 962 409 943 631 424 803 357 416 358 942 248  99 885 715 349 381 987 703 573 95 397 424 57 624 801 788 761 764 617 680 140 345 738 436 534 199 464 64 125 166 84 64 538 826 86 929 514 275 454 279 953 725 266 817 489 682 478 265 833 885 682 0 344 26 632 83 360 484 949 400 790 925 296 883 350 216 328 478 358 293 969 808 810 748 515 144 753 579 938 109 358 650 411 712 873 975 527 276 219 792 296 840 570 735 801 427 713 117 123 384 690  481 61 260 641 497 23 361 914 527 900 633 207 261 259 420 288 522 734 994 405 319 922 713 117 950 36 808 153 457 508 969 969 408 498 585 648 981 614 257 877 871 583 7 699 946 435 645 908 959 333 931 301 950 735 575 883 77 434 972 848 886 26 196 892 278 346 499 163 85 443 106 617 689 980 457 712 610 726 192 827 800 841 364 95 101 174 478 23 864 666 28 673 125 198 864 196 657 810 46 265   
28 386 425 427 231 250 932 461 591 3 1 307 972 792 799 21 985 420 945 470 461 407 770 839 423 259 306 563 325 387 885 576 394 723 581 285 997 997 718 713 644 953 122 709 203 686 486 177 252 40 59 580 962 650 822 99 686 812 730 773 822 347 177 757 27 136 69 982 562 547 737 985 952 456 562 441 545 230 308 764 394 489 61 40 26 722 160 918 771 534 151 138 404 852 53 117 349 165 165 634  185 537 703 505 645 64 45 38 682 356 266 410 719 380 950 815 292 263 644 145 148 348 979 921 458 994 658 958 72 313 783 536 183 606 526 770 889 118 20 776 398 967 15 293 394 419 366 150 29 411 725 69 794 600 511 266 879 881 327 399 538 602 289 478 33 707 117 484 728 580 395 832 885 464 535 625 509 757 533 404 244 726 573 839 0 520 701 907 835 226 234 236 912 959 928 219 152 187 980 773  575 742 237 291 452 670 298 495 164 137 188 553 657 65 603 15 134 35 851 660 115 844 415 218 352 34 689 930 418 388 775 0 118 531 777 178 443 931 402 493 651 238 602 396 160 91 815 702 64 430 720 483 33 614 288 246 433 27 72 569 644 590 84 612 717 821 764 594 284 66 494 197 902 477 811 960 577 14 635 991 634 748 300 419 801 437 49 905 972 129 114 265 913 49 567 231 275 635 859 13   
951 184 505 463 204 313 348 11 413 5 951 371 695 193 720 289 352 605 580 494 571 47 991 345 493 661 221 463 621 258 621 938 57 955 97 491 365 798 42 885 970 585 954 488 361 547 42 717 62 233 875 958 391 523 987 224 713 386 698 998 339 420 557 823 890 343 682 205 294 31 628 996 564 527 552 678 1 606 480 213 48 965 779 519 658 364 71 704 883 487 531 272 520 555 507 487 187 812 731 969  302 692 127 477 137 904 791 916 105 643 135 519 497 480 612 852 68 63 94 44 977 37 274 814 394 340 243 120 5 149 100 405 22 59 69 316 939 146 567 519 253 753 664 681 651 29 45 23 438 729 423 721 28 13 278 244 764 881 467 102 292 930 102 138 545 735 478 43 604 384 110 145 955 383 828 458 193 73 11 627 164 313 412 495 163 782 955 418 571 282 360 249 279 273 575 18 173 402 140 436  651 944 797 335 604 661 94 227 79 642 50 697 354 603 932 949 164 834 144 924 364 206 580 757 327 820 873 250 207 886 639 406 304 682 414 889 43 510 954 433 810 197 195 139 708 812 419 522 781 632 566 507 126 577 554 147 491 371 16 823 740 369 561 147 396 233 724 312 246 872 391 730 268 659 292 866 487 582 861 239 703 67 366 474 469 739 385 554 49 686 324 402 873 344 73 412 841 287 403 416   
201 227 329 690 682 609 723 540 101 834 435 538 429 950 970 832 958 421 312 199 74 668 821 862 317 117 966 231 913 101 552 221 390 604 906 22 538 655 289 143 644 822 240 4 957 776 594 209 41 168 580 953 195 326 844 682 51 201 795 484 290 771 789 258 223 656 136 477 547 608 666 794 667 824 558 800 734 436 351 698 975 278 190 499 165 917 695 470 636 517 694 269 446 333 68 127 401 480 736 621  382 786 886 935 876 220 44 705 282 741 658 425 507 309 86 69 607 650 522 720 831 416 483 775 22 460 811 489 29 965 853 754 113 141 105 144 926 743 729 55 430 536 336 573 316 164 447 152 192 179 480 35 187 862 427 788 245 27 82 409 194 726 400 488 112 645 543 855 970 30 702 289 989 553 199 825 810 950 540 503 910 89 851 673 216 698 694 957 243 381 188 552 968 93 999 712 818 798 390 463  404 456 108 589 807 16 892 557 905 54 126 667 257 689 744 293 89 335 764 23 538 418 403 445 993 206 94 715 260 97 470 436 155 113 512 480 114 431 172 358 559 475 764 429 413 638 706 100 267 86 483 755 764 36 233 392 94 692 735 841 164 650 920 489 937 229 247 695 411 718 626 244 165 114 117 441 473 328 933 294 97 416 685 304 4 535 633 19 663 115 912 894 174 478 748 523 988 859 892 139   
826 43 186 61 847 559 363 771 911 168 687 542 915 531 585 231 467 595 767 340 212 962 974 798 873 326 399 961 231 24 58 192 372 636 535 43 134 301 830 514 54 923 597 405 266 466 82 107 112 730 645 782 36 486 136 191 460 433 699 192 261 715 344 810 237 781 81 25 786 960 633 535 955 492 577 843 51 270 624 17 330 363 683 58 889 841 95 808 508 769 926 696 255 688 255 573 670 807 756 985  436 99 121 914 367 100 953 904 877 915 629 205 359 767 135 86 89 8 743 141 377 39 74 148 503 974 33 47 410 517 10 620 650 484 930 50 397 38 224 118 208 313 480 203 720 782 726 196 628 828 605 368 828 4 818 983 973 853 233 107 705 194 230 464 182 728 182 231 703 926 513 697 141 393 266 399 103 113 680 501 244 574 698 705 833 172 651 776 394 880 993 583 670 224 817 224 732 608 189 297  14 373 919 461 852 607 244 65 972 102 698 692 909 305 338 368 615 739 216 870 101 960 371 384 852 234 389 809 807 182 391 221 83 357 141 829 583 690 981 539 223 155 335 61 399 84 627 399 222 773 313 771 172 188 263 417 417 475 927 855 732 773 428 235 862 227 327 507 1 592 65 917 996 575 256 193 87 111 921 792 507 359 676 146 810 26 253 725 1 846 831 122 364 247 581 502 98 814 330 793   
384 19 813 420 770 299 869 491 34 437 573 760 682 588 637 523 681 628 846 377 701 713 961 347 192 904 951 671 849 189 482 276 708 252 184 673 387 612 45 648 850 719 856 671 636 986 269 646 191 215 464 896 847 882 40 366 951 70 599 589 331 946 621 294 701 738 24 930 553 317 397 166 410 452 269 947 917 376 207 892 48 921 785 159 40 791 572 645 943 460 586 956 346 471 226 39 445 51 931 818  973 593 245 195 708 154 95 952 142 210 481 826 526 735 321 628 506 77 838 530 603 334 861 759 941 50 100 895 817 663 830 33 965 512 535 219 183 160 320 41 725 77 443 880 924 627 472 576 337 446 392 311 731 714 441 818 490 978 593 139 92 329 278 205 714 396 562 502 794 763 976 483 84 692 803 717 741 820 752 922 627 600 484 427 359 217 445 178 998 352 783 941 306 750 583 675 275 852 367 945  205 339 939 150 392 53 703 277 531 328 977 956 23 801 987 47 600 45 340 959 992 771 413 531 396 196 15 244 568 86 458 777 289 463 484 787 742 301 229 804 556 898 291 827 526 970 195 824 620 534 969 547 357 106 934 973 905 854 372 547 638 63 630 129 341 657 537 346 708 722 804 16 124 903 699 559 980 52 384 746 865 675 31 302 10 486 293 814 567 847 675 712 369 590 8 431 811 644 867 586   

---Answer Record---
Result: Passed 1 Out of 100
Failed:
2  5  0   
1  1  5   
2  4  2   
1  3  0   
4  0  0   
1  0  3   
2  3  3   
0  0  1   
1  2  5   
0  0  3   
0  5  0   
1  1  5   
0  4  5   
5  1  4   
2  5  1   
2  0  4   
0  4  5   
2  4  3   
0  2  3   
3  5  2   
0  5  4   
3  0  1   
1  4  4   
0  1  3   
1  4  0   
4  5  4   
0  4  1   
5  3  4   
3  1  4   
1  3  4   
0  0  2   
1  3  1   
1  1  4   
1  3  1   
5  0  0   
5  5  0   
0  3  1   
1  3  2   
0  1  2   
4  1  3   
2  5  2   
0  4  4   
4  3  1   
4  3  0   
3  0  5   
4  5  3   
5  4  1   
2  5  1   
5  1  4   
3  3  2   
3  3  2   
3  1  4   
3  0  0   
1  2  3   
4  4  0   
0  3  4   
3  1  1   
5  3  3   
5  3  1   
2  1  2   
1  1  5   
5  2  2   
4  3  0   
2  2  5   
0  5  1   
3  1  3   
2  2  4   
1  5  1   
3  1  4   
1  4  2   
4  0  3   
2  2  5   
5  0  4   
5  4  0   
2  1  5   
2  3  3   
3  2  4   
5  0  5   
1  1  4   
3  0  0   
0  1  3   
5  0  5   
1  2  1   
2  3  2   
3  3  0   
1  2  4   
5  4  1   
5  2  2   
0  3  1   
4  1  2   
0  2  4   
4  5  2   
0  3  2   
1  4  0   
3  2  1   
3  4  4   
5  4  4   
4  4  5   
1  1  2   
```

#### Custom generator and custom


```Scheme

(define (rb-bag-prop a b c)
  (let ([A (rbmset-fill #f a)]
        [B (rbmset-fill #f b)]
        [C (rbmset-fill #f c)])
  (compare (union-rbmset (union-rbmset A B ) C) (union-rbmset (union-rbmset B C) A))))

(define (my-parse-args lst params-to-change)
    (define (my-parse-args-sub lst params-to-change fnl-lst)
      (cond
        [(eqv? params-to-change 0) fnl-lst]
        [else
          (cond
            [(> (car lst) 0) (my-parse-args-sub (cdr lst) (- params-to-change 1) (append fnl-lst (list (floor(/ (car lst) 2)))))]
            [(< (car lst) 0) (my-parse-args-sub (cdr lst) (- params-to-change 1) (append fnl-lst (list (floor(/ (car lst) 2)))))]
            [else (my-parse-args-sub (cdr lst) (- params-to-change 1) (append fnl-lst (list (car lst))))])]))
    (my-parse-args-sub lst params-to-change '()))

(define (lab-shrinker answer limit params-to-change cycle)
  (cond
    [(eq? answer-status #t) answer])
  (cond
    [(eqv? cycle 0) answer]
    [else
      (let ([run-tst (test-template
        (answer-user-property answer)
        (answer-args-amount answer)
        (answer-generator answer)
        (my-parse-args
          (answer-generator-args answer) 
          params-to-change)
        limit)])

        (cond 
          [(and (eq? (answer-status answer) #f) (eq? (answer-status run-tst) #t)) answer]
          [else
            (lab-shrinker run-tst limit params-to-change (- cycle 1))]))]))

(print-property-test-stats(lab-shrinker (test-template rb-bag-prop 3 random-int-list (list 100 100) 10) 100 2 1000) #t #f #t)

```

```
---Answer Record---
Result: Passed 100 Out of 100
Failed:

```

### Заключение 

Лабораторная работа получилась полезной. Я закрепил работу с языком Guile и могу достаточно свободно на нем писать программы.
Хотелось бы иметь такую реализацию на момент выполнения ЛР2. Это бы ускорило процесс разработки в несколько раз