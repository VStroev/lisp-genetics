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

(define (score set graph)
  (define (add-to-all link all-verts)
    (let ((contain-first (member (car link) all-verts)) (contain-second (member (cadr link) all-verts)))
      (cond ((and contain-first contain-second) all-verts)
            (contain-first (cons (cadr link) all-verts))
            (else (cons (car link) all-verts)))))
            
  (define (count-score set graph covered-verts all-verts)
    (if (null? graph) (/ (length covered-verts) (length all-verts))
        (let ((link (car graph)))
          (cond ((member (car link) set) (count-score set (cdr graph) (cons (cadr link) covered-verts) (add-to-all link all-verts)))
                ((member (cadr link) set) (count-score set (cdr graph) (cons (car link) covered-verts) (add-to-all link all-verts)))
                (else (count-score set (cdr graph) covered-verts (add-to-all link all-verts)))))))
  (count-score set graph set set))
  
(define (test solution k graph min-dom)
  (print 'min-dom=)
  (println min-dom)
  (println graph)
  (print 'k=)
  (println k)
  (let ((ret (solution graph k)))
    (println ret)
    (let ((answer (and (eq? (score (caddr ret) graph) 1)
                  (eq? (<= min-dom k) (car ret))
                  (or (not (car ret))
                      (and (<= (cadr ret) k) (<= min-dom (cadr ret)))))))
    (println answer)
    (newline)
      answer)))

(define (run-cycle-tests solution)
  (define cycle-graphs
    (list
     (cons (list(list 0 1) (list 0 2) (list 0 3) (list 1 3)) 1)
     (cons (list (list 0 1) (list 0 2) (list 2 3) (list 3 4) (list 0 5) (list 5 6) (list 6 7) (list 1 3) (list 1 6) (list 4 7)) 3)
     (cons (list (list 0 1) (list 0 2) (list 1 3) (list 3 4) (list 4 5) (list 4 6) (list 4 7) (list 6 8) (list 5 8) (list 7 8)) 3)))
        
  (define (run-tests solution graphs)
    (if (null? graphs) '()
        (and
          (test solution 1 (caar graphs) (cdar graphs))
          (test solution 2 (caar graphs) (cdar graphs))
          (test solution 3 (caar graphs) (cdar graphs))
          (test solution 4 (caar graphs) (cdar graphs))
          (test solution 5 (caar graphs) (cdar graphs))
          (run-tests solution (cdr graphs)))))
  (run-tests solution cycle-graphs))

(define (create-test solution k min-dom)
  (test solution k (gen-graph min-dom) min-dom))

(define (run-tests solution)
  (if (and
       (create-test solution 2 1)
       (create-test solution 1 1)
       (create-test solution 1 2)
       (create-test solution 2 2)
       (create-test solution 10 2)
       (create-test solution 1 3)
       (create-test solution 2 3)
       (create-test solution 3 3)
       (create-test solution 5 3)
       (create-test solution 1 7)
       (create-test solution 7 7)
       (create-test solution 10 7)
       (run-cycle-tests solution))
      (println 'all_tests_passed)
      (println 'error)))

(run-tests (lambda (x y) (list #t 1 (list 0))))