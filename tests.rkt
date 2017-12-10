#lang scheme/base

(define MAX_VERTS 3)

(define (gen-graph min-dom)
  (define (add-random-ver graph root start num)
    (if (eq? num 0) (cons graph start)
        (add-random-ver (append graph (list (list root (+ start 1)))) root (+ start 1) (- num 1))))
  
  (define (add-subgraph min-dom graph start)
    (cond ((eq? min-dom 0) (car (add-random-ver graph start start (+ (random MAX_VERTS) 1))))
          (else (let ((ret (add-random-ver graph start start (+ (random MAX_VERTS) 1))))
                  (let ((start (cdr ret)) (graph (car ret)) (root (random (cdr ret))))
                    (add-subgraph (- min-dom 1) (append graph (list (list root (+ start 1)) (list (+ start 1) (+ start 2)))) (+ start 2)))))))
                
  (if (< 0 min-dom) (add-subgraph (- min-dom 1) '() 0)
      'error))

(define (run-cycle-tests solution)
  (define cycle-graphs
    (list
     (cons (list(list 0 1) (list 0 2) (list 0 3) (list 1 3)) 1)
     (cons (list (list 0 1) (list 0 2) (list 2 3) (list 3 4) (list 0 5) (list 5 6) (list 6 7) (list 1 3) (list 1 6) (list 4 7)) 3)
     (cons (list (list 0 1) (list 0 2) (list 1 3) (list 3 4) (list 4 5) (list 4 6) (list 4 7) (list 6 8) (list 5 8) (list 7 8)) 3)))
        
  (define (run-tests file graphs)
    (if (null? graphs) '()
        (begin
          (write (list 1 (cdar graphs) (caar graphs)) file)
          (write (list 2 (cdar graphs) (caar graphs)) file)
          (write (list 3 (cdar graphs) (caar graphs)) file)
          (write (list 4 (cdar graphs) (caar graphs)) file)
          (write (list 5 (cdar graphs) (caar graphs)) file)
          (run-tests solution (cdr graphs)))))
  (run-tests solution cycle-graphs))

(define (create-test k min-dom)
  (list k min-dom (gen-graph min-dom)))

(define (write-tests filename)
  
  (let ((file (open-output-file (read))))
    (begin
          (write (create-test  2 1) file)
          (write (create-test  1 1) file)
          (write (create-test  1 2) file)
          (write (create-test  2 2) file)
          (write (create-test  10 2) file)
          (write (create-test  1 3) file)
          (write (create-test  2 3) file)
          (write (create-test  3 3) file)
          (write (create-test  5 3) file)
          (write (create-test  1 7) file)
          (write (create-test  7 7) file)
          (write (create-test  10 7) file)
          (run-cycle-tests file)
          (close-output-port file)
          )))

(write-tests (lambda (x y) (list #t 1 (list 0))))