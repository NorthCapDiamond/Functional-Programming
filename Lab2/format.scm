(use-modules (ice-9 pretty-print))

(define args (command-line))

(if (< (length args) 2)
    (begin 
      (display "Not enough parameters, need a file name.")
      (newline))
    (let ([fd (open-file (list-ref args 1) "r")])
      (let loop ((sexp (read fd)))
        (if (not (eof-object? sexp))
          (begin 
            (pretty-print sexp)
            (newline)
            (loop (read fd))
            (newline)
          #F)))))
