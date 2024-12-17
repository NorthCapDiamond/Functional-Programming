(include "io.scm")

(include "lagrange.scm")

(include "linear.scm")

(define (get-next app-list idx type)
  (cond
    ((eq? type 0) (let ((xy (read-x-y))) (list xy)))
    (else (cond
            ((< idx (length (car app-list)))
             (list (list-ref (car app-list) idx)))
            (else (exit))))))

(define (run-linear real-list update-list func step idx type)
  (cond
    ((< (length update-list) 2)
     (run-funcs-sub real-list update-list func step (+ 1 idx) type))
    (else (display "Linear : ")
          (display
           (linear-function-step
            (car (list-ref update-list (- (length update-list) 2)))
            (cadr (list-ref update-list (- (length update-list) 2)))
            (car (list-ref update-list (- (length update-list) 1)))
            (cadr (list-ref update-list (- (length update-list) 1)))
            step
            '()))
          (newline))))

(define (run-lagrange real-list update-list func step idx type)
  (cond
    ((>= (/ (- (car (list-ref update-list (- (length update-list) 1)))
               (car (car update-list)))
            step)
         5)
     (display "Lagrange : ")
     (display (lagrange-function-step update-list step '()))
     (newline)
     (run-funcs-sub real-list (cdr update-list) func step (+ 1 idx) type))
    (else (run-funcs-sub real-list update-list func step (+ idx 1) type))))

(define (run-funcs-sub real-list current-list func step idx type)
  (let ((update-list (append current-list (get-next real-list idx type))))
    (cond
      ((= (length func) 2)
       (run-linear real-list update-list func step idx type)
       (run-lagrange real-list update-list func step idx type))
      ((eq? (car func) linear-function-step)
       (run-linear real-list update-list func step idx type)
       (run-funcs-sub real-list (cdr update-list) func step (+ 1 idx) type))
      ((eq? (car func) lagrange-function-step)
       (run-lagrange real-list update-list func step idx type)))))

(define (run-funcs app-list func step type)
  (run-funcs-sub app-list '() func step 0 type))

(define (mission-control args)
  (let ((mode (car args)) (func (cadr args)) (step (caddr args)))
    (run-funcs (parse-mode mode) (parse-func func) step (parse-type mode))))

(mission-control (read-args))











