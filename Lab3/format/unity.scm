(include "testing.scm")

(include "io.scm")

(include "lagrange.scm")

(include "linear.scm")

(test-begin "linear-interpolation")

(display-and-test
 (linear-function-step 1 1 5 5 1 '())
 (list '(1 . 1) '(2 . 2) '(3 . 3) '(4 . 4) '(5 . 5))
 "Linear test 1")

(display-and-test
 (linear-function-step 0 1 3 4 0.5 '())
 (list '(0 . 1)
       '(0.5 . 1.5)
       '(1.0 . 2.0)
       '(1.5 . 2.5)
       '(2.0 . 3.0)
       '(2.5 . 3.5)
       '(3 . 4))
 "Linear test 2")

(display-and-test
 (linear-function-step -2 -7 5 5 3 '())
 (list '(-2 . -7) '(1 . -93/50) '(4 . 82/25) '(5 . 5))
 "Linear test 3")

(test-end "linear-interpolation")

(newline)

(test-begin "lagrange-interpolation")

(display-and-test
 (lagrange-function-step (list '(1 1) '(2 2) '(3 3) '(4 4) '(5 5)) 0.5 '())
 (list '(1 1)
       '(1.5 1.5)
       '(2.0 2.0)
       '(2.5 2.5)
       '(3.0 3.0)
       '(3.5 3.5)
       '(4.0 4.0)
       '(4.5 4.5)
       '(5.0 5.0))
 "Lagrange test 1")

(display-and-test
 (lagrange-function-step (list '(0 0) '(1 1) '(2 4) '(3 9) '(4 16)) 0.8 '())
 (list '(0 0) '(0.8 0.64) '(1.6 2.56) '(2.4 5.76) '(3.2 10.24) '(4.0 16.0))
 "Lagrange test 2")

(display-and-test
 (lagrange-function-step (list '(-2 -7) '(1 -93/50) '(4 82/25) '(5 5)) 1 '())
 (list '(-2 -7)
       '(-1 -132/25)
       '(0 -357/100)
       '(1 -93/50)
       '(2 -3/20)
       '(3 39/25)
       '(4 82/25)
       '(5 5))
 "Lagrange test 3")

(test-end "lagrange-interpolation")

(newline)

(test-begin "read-file")

(display-and-test
 (read-file "test1.csv")
 (list '(-1 1) '(2 5) '(3 10) '(10 0) '(11 4) '(12 7))
 "Read file test 1")

(display-and-test
 (read-file "test2.csv")
 (list '(0 1) '(1 5) '(2 10) '(3 0) '(4 4) '(5 7))
 "Read file test 2")

(display-and-test
 (read-file "test3.csv")
 (list '(1 1)
       '(2 4)
       '(3 9)
       '(4 16)
       '(5 25)
       '(6 36)
       '(7 49)
       '(8 64)
       '(9 81)
       '(10 100))
 "Read file test 3")

(test-end "read-file")

(newline)

(test-begin "parse-mode")

(display-and-test (parse-mode "0") '() "parse-mode test 1")

(display-and-test
 (parse-mode "test1.csv")
 (list (read-file "test1.csv"))
 "parse-mode test 2")

(test-end "parse-mode")

(newline)

(test-begin "parse-type")

(display-and-test (parse-type "0") 0 "parse-type test 1")

(display-and-test (parse-type "test1.csv") 1 "parse-type test 2")

(test-end "parse-type")

(newline)

(test-begin "parse-func")

(display-and-test
 (parse-func 1)
 (list linear-function-step)
 "parse-func test 1")

(display-and-test
 (parse-func 2)
 (list lagrange-function-step)
 "parse-func test 2")

(display-and-test
 (parse-func 3)
 (list linear-function-step lagrange-function-step)
 "parse-func test 3")

(test-end "parse-func")

(newline)







































