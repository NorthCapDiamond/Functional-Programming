# Лабораторная работа 3. Реализация одной из лабораторных работ по предмету "Вычислительная математика" с использованием функционального программирования. Ввод/вывод. Управление вычислительным процессом. Интерфейс командной строки.

Автор: Дробыш Дмитрий Александрович P34082 333219

Цель: получить навыки работы с вводом/выводом, потоковой обработкой данных, командной строкой.

## Условие

### Задание:

В рамках лабораторной работы вам предлагается повторно реализовать лабораторную работу по предмету "Вычислительная математика" посвящённую интерполяции (в разные годы это лабораторная работа 3 или 4) со следующими дополнениями:

обязательно должна быть реализована линейная интерполяция (отрезками, link);
настройки алгоритма интерполяции и выводимых данных должны задаваться через аргументы командной строки:

какие алгоритмы использовать (в том числе два сразу);
частота дискретизации результирующих данных;
и т.п.;


входные данные должны задаваться в текстовом формате на подобии ".csv" (к примеру x;y\n или x\ty\n) и подаваться на стандартный ввод, входные данные должны быть отсортированы по возрастанию x;
выходные данные должны подаваться на стандартный вывод;
программа должна работать в потоковом режиме (пример -- cat | grep 11), это значит, что при запуске программы она должна ожидать получения данных на стандартный ввод, и, по мере получения достаточного количества данных, должна выводить рассчитанные точки в стандартный вывод;


## Реализация

### Структура Input/Output

#### Чтение из файла:

```Scheme
(define (read-file filename)
  (define (read-file-sub port array)
    (let ((line (read-line port)))
      (cond
        ((eof-object? line) (close-input-port port) array)
        (else (let ((tmp-array (list (string-split line #\,))))
                (read-file-sub
                 port
                 (append
                  array
                  (list (list (round-two-digits
                               (string->number (car (car tmp-array))))
                              (round-two-digits
                               (string->number (cadr (car tmp-array)))))))))))))
  (let ((port (open-input-file filename))) (read-file-sub port '())))
```

#### Чтение из стандартного потока ввода:

```Scheme
(define (read-x-y)
  (display "Enter x and y:")
  (newline)
  (let ((x (read)) (y (read))) (list x y)))

(define (display-newline data) (display data) (newline))

```

#### Чтение аргументов командной строки:

```Scheme
(define (read-args)
  (let ((args (command-line)))
    (cond
      ((not (eq? (length args) 4))
       (display
        "Usage: guile runner.scm [mode][func][step]\nWhere:\n\tmode: 0 - cmd or filelink\n\tfunc: 1 - linear 2 - lagrange 3 - both\n\tstep is number\n"))
      (else (list (cadr args)
                  (string->number (caddr args))
                  (string->number (cadddr args)))))))

```

### Интерполяция (Лагранж):

```Scheme
(define (lagrange-function x L)
  (define (inner-loop x L n i j l_i)
    (cond [(eq? n j) l_i]
        [(not (eq? i j))
          (let([deltaX (- (car(list-ref L i)) (car(list-ref L j)))])
            (inner-loop x L n i (+ j 1) (* l_i (/ (- x (car (list-ref L j))) deltaX))))]
        [else (inner-loop x L n i (+ j 1) l_i)]))

  (define (lagrange-loop x L n i l_S)
    (cond
      [(eq? i n) (round-two-digits l_S)]
      [else (lagrange-loop x L n (+ i 1) (+ l_S (* (inner-loop x L n i 0 1) (cadr(list-ref L i)))))]))

  (lagrange-loop x L (length L) 0 0))

(define (lagrange-function-step L step ans)
  (define (lagrange-function-step-sub L step ans xi xe)
    (cond
      [(< xi xe) (lagrange-function-step-sub L step (append ans (list (list (round-two-digits xi) (round-two-digits(lagrange-function xi L))))) (+ xi step) xe)]
      [else (append ans (list (list (round-two-digits xi) (round-two-digits(lagrange-function xi L)))))]))
  (lagrange-function-step-sub L step '() (car (car L)) (car (list-ref L (- (length L) 1)))))
```

### Интерполяция (Линейная):

```Scheme
(define (linear-function x X0 Y0 X1 Y1)
  (round-two-digits (+ Y0 (* (- x X0) (/ (- Y1 Y0) (- X1 X0))))))

(define (linear-function-step Xs Ys Xe Ye step ans)
  (cond
    [(< Xs Xe) (linear-function-step (+ Xs step) (linear-function (+ Xs step) Xs Ys Xe Ye) Xe Ye step (append ans (list (cons Xs Ys))))]
    [else (append ans (list (cons (round-two-digits Xe) (round-two-digits Ye))))]))
```

### Управление программой (подробнее в runner.scm)

```Scheme
(define (mission-control args)
  (let ([mode (car args)]
      [func (cadr args)]
      [step (caddr args)])

    (run-funcs (parse-mode mode) (parse-func func) step (parse-type mode))))
```

### Утилиты

#### Округление

```Scheme
(define (round-two-digits x)
  (let ((factor 100))
    (/ (round (* x factor)) factor)))
```

#### Модуль для тестов

```Scheme
;; Testing module 
(use-modules (srfi srfi-64))

(display "Testing started\n")
(define (display-and-test value2 value1 message)
  (display message)
  (newline)
  (test-equal value1 value2))

(define (display-and-assert value1 message)
  (display message)
  (newline)
  (test-assert value1))
```

### Тесты

```Scheme
(include "testing.scm")
(include "io.scm")
(include "lagrange.scm")
(include "linear.scm")

(test-begin "linear-interpolation")
(display-and-test (linear-function-step 1 1 5 5 1 '()) (list '(1 . 1) '(2 . 2) '(3 . 3) '(4 . 4) '(5 . 5)) "Linear test 1")
(display-and-test (linear-function-step 0 1 3 4 0.5 '()) (list '(0 . 1) '(0.5 . 1.5) '(1.0 . 2.0) '(1.5 . 2.5) '(2.0 . 3.0) '(2.5 . 3.5) '(3 . 4)) "Linear test 2")
(display-and-test (linear-function-step -2 -7 5 5 3 '()) (list '(-2 . -7) '(1 . -93/50) '(4 . 82/25) '(5 . 5)) "Linear test 3")
(test-end "linear-interpolation")

(newline)

(test-begin "lagrange-interpolation")
(display-and-test (lagrange-function-step (list '(1 1) '(2 2) '(3 3) '(4 4) '(5 5)) 0.5 '()) (list '(1 1) '(1.5 1.5) '(2.0 2.0) '(2.5 2.5) '(3.0 3.0) '(3.5 3.5) '(4.0 4.0) '(4.5 4.5) '(5.0 5.0)) "Lagrange test 1")
(display-and-test (lagrange-function-step (list '(0 0) '(1 1) '(2 4) '(3 9) '(4 16)) 0.8 '()) (list '(0 0) '(0.8 0.64) '(1.6 2.56) '(2.4 5.76) '(3.2 10.24) '(4.0 16.0)) "Lagrange test 2")
(display-and-test (lagrange-function-step (list '(-2 -7) '(1 -93/50) '(4 82/25) '(5 5)) 1 '()) (list '(-2 -7) '(-1 -132/25) '(0 -357/100) '(1 -93/50) '(2 -3/20) '(3 39/25) '(4 82/25) '(5 5)) "Lagrange test 3")
(test-end "lagrange-interpolation")

(newline)

(test-begin "read-file")
(display-and-test (read-file "test1.csv") (list '(-1 1) '(2 5) '(3 10) '(10 0) '(11 4) '(12 7)) "Read file test 1")
(display-and-test (read-file "test2.csv") (list '(0 1) '(1 5) '(2 10) '(3 0) '(4 4) '(5 7)) "Read file test 2")
(display-and-test (read-file "test3.csv") (list '(1 1) '(2 4) '(3 9) '(4 16) '(5 25) '(6 36) '(7 49) '(8 64) '(9 81) '(10 100)) "Read file test 3")
(test-end "read-file")

(newline)

(test-begin "parse-mode")
(display-and-test (parse-mode "0") '() "parse-mode test 1")
(display-and-test (parse-mode "test1.csv") (list (read-file "test1.csv")) "parse-mode test 2")
(test-end "parse-mode")

(newline)

(test-begin "parse-type")
(display-and-test (parse-type "0") 0 "parse-type test 1")
(display-and-test (parse-type "test1.csv") 1 "parse-type test 2")
(test-end "parse-type")

(newline)

(test-begin "parse-func")
(display-and-test (parse-func 1)(list linear-function-step) "parse-func test 1")
(display-and-test (parse-func 2)(list lagrange-function-step) "parse-func test 2")
(display-and-test (parse-func 3)(list linear-function-step lagrange-function-step) "parse-func test 3")
(test-end "parse-func")

(newline)
```

### Результаты тестов 

```Scheme
%%%% Starting test linear-interpolation  (Writing full log to "linear-interpolation.log")
Linear test 1
Linear test 2
Linear test 3
# of expected passes      3

%%%% Starting test lagrange-interpolation  (Writing full log to "lagrange-interpolation.log")
Lagrange test 1
Lagrange test 2
Lagrange test 3
# of expected passes      3

%%%% Starting test read-file  (Writing full log to "read-file.log")
Read file test 1
Read file test 2
Read file test 3
# of expected passes      3

%%%% Starting test parse-mode  (Writing full log to "parse-mode.log")
parse-mode test 1
parse-mode test 2
# of expected passes      2

%%%% Starting test parse-type  (Writing full log to "parse-type.log")
parse-type test 1
parse-type test 2
# of expected passes      2

%%%% Starting test parse-func  (Writing full log to "parse-func.log")
parse-func test 1
parse-func test 2
parse-func test 3
# of expected passes      3
```

### Заключение 

Лабораторная работа получилась достаточно простой (если сравнивать с ЛР 2), так как основывается на работе прошлого курса. При написании программы возникли трудности с реализацией смещения элементов в списке, но я смог найти простое и аккуратное решение.
Хочу заметить, что написание тестов на языке Scheme происходит достаточно быстро, что позволило мне оперативно находить ошибки и опечатки в исходном коде.

  