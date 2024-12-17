(include "util.scm")

(use-modules (ice-9 rdelim))

(define (read-file filename)
  (define (read-file-sub port array)
    (let ([line (read-line port)])
      (cond
        [(eof-object? line)
          (close-input-port port)
          array]
        [else
          (let ([tmp-array (list (string-split line #\,))])
            (read-file-sub port (append array (list (list (round-two-digits(string->number (car(car tmp-array)))) (round-two-digits(string->number (cadr (car tmp-array)))))))))])))




  (let ([port (open-input-file filename)])
    (read-file-sub port '())))

(define (read-x-y)
  (display "Enter x and y:")
  (newline)
  (let ([x (read)] 
        [y (read)])
      (list x y)))

(define (display-newline data)
  (display data)
  (newline))

(define (console-input current func)
  (let ([new-curr (append current (list (read-x-y)))])
    (func new-curr)
    (console-input new-curr func)))

(define (read-args)
  (let ([args (command-line)])
    (cond
      [(not (eq? (length args) 4)) (display "Usage: guile runner.scm [mode][func][step]\nWhere:\n\tmode: 0 - cmd or filelink\n\tfunc: 1 - linear 2 - lagrange 3 - both\n\tstep is number\n")]
      [else
        (list (cadr args)(string->number(caddr args))(string->number (cadddr args)))])))


(define (parse-mode mode)
  (cond
    [(not(equal? mode "0")) (list (read-file mode))]
    [else '()]))

(define (parse-type mode)
  (cond
    [(not(equal? mode "0")) 1]
    [else 0]))


(define (parse-func func)
  (cond
    [(eq? func 1) (list linear-function-step)]
    [(eq? func 2) (list lagrange-function-step)]
    [(eq? func 3) (list linear-function-step lagrange-function-step)]))


