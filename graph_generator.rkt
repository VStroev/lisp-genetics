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

(define (test solution k min-dom)
  (let ((graph (gen-graph min-dom)))
    (println graph)
    (println k)
    (let ((ret (solution graph k)))
      (println ret)
      (println (and (eq? (<= min-dom k) (car ret))
                    (or (not (car ret))
                        (and (<= (cdr ret) k) (<= min-dom (cdr ret)))))))))

(test (lambda (x y) (cons #t 5)) 5 6)