#lang scheme/base

(define population_size 100)
(define rounds 100)

(define (score set graph k)
  (define (add-to-all link all-verts)
    (let ((contain-first (member (car link) all-verts)) (contain-second (member (cadr link) all-verts)))
      (cond ((and (not contain-first) (not contain-second)) (append link all-verts))
            ((and contain-first (not contain-second)) (cons (cadr link) all-verts))
            ((not contain-first) (cons (car link) all-verts))
            (else all-verts))))
            
  (define (count-score set graph covered-verts all-verts)
    (if (null? graph) (/ (length covered-verts) (length all-verts))
        (let ((link (car graph)))
          (cond ((and (member (car link) set) (not (member (cadr link) covered-verts))) (count-score set (cdr graph) (cons (cadr link) covered-verts) (add-to-all link all-verts)))
                ((and (member (cadr link) set) (not (member (car link) covered-verts))) (count-score set (cdr graph) (cons (car link) covered-verts) (add-to-all link all-verts)))
                (else (count-score set (cdr graph) covered-verts (add-to-all link all-verts)))))))
  (if (> (length set) k) 0
      (count-score set graph set set)))

(define (solution graph k)
  (define (pick-random lst)
    (list-ref lst (random (length lst))))

  (define (sort-score list)
    (sort list (lambda (x y) (< (cdr x) (cdr y)))))
  
  (define (get-all-verts graph verts)
    (cond ((null? graph) verts)
          ((and (not (member (caar graph) verts)) (not (member (cadar graph) verts)))
           (get-all-verts (cdr graph) (append verts (list (caar graph) (cadar graph)))))
          ((not (member (caar graph) verts)) (get-all-verts (cdr graph) (append verts (list (caar graph)))))
          ((not (member (cadar graph) verts)) (get-all-verts (cdr graph) (append verts (list (cadar graph)))))
          (else (get-all-verts (cdr graph) verts))))
        
  (define (init-population graph population_size)
    (define (init graph verts population_size population)
      (if (eq? population_size 0) population
          (let ((set (list (pick-random verts))))
          (init graph verts (- population_size 1) (cons (cons set (score set graph k)) population)))))
    (let ((verts (get-all-verts graph '())))
      (init graph verts population_size '())))

  (define (crossing population graph)
    (define (cross p1 p2)
      (cond ((null? p2) p1)
            ((not (member (car p2) p1)) (cross (cons (car p2) p1) (cdr p2)))
            (else (cross p1 (cdr p2)))))

    (define (cross-par par1 par2 graph)
      (let ((child (mutate (cross (car par1) (car par2)) graph)))
        (cons child (score child graph k))))

    (define (mutate node graph)
      (let ((verts (get-all-verts graph '())))
        (if (< (random) 0.1)
            (let ((gen (pick-random verts)))
              (if (member gen node) node
                  (cons gen node)))
            node)))
    
    (define (crossing-pop new-population population graph)
      (if (null? population) new-population
          (let ((child (cross-par (car population) (pick-random population) graph)))
            (crossing-pop (append new-population (list child (car population))) (cdr population) graph))))
    (crossing-pop '() population graph))

  (define (select population population_size)
    (define (select-sorted population population_size)
      (if (eq? population_size 0) population
          (select-sorted (cdr population) (- population_size 1))))
    (select-sorted (sort-score population) population_size))

  (define (round population graph population_size)
    (select (crossing population graph) population_size))

  (define (run-evolution pop graph population_size rounds)
    (if (eq? rounds 0) (select-best pop)
        (let ((new-pop (round pop graph population_size)))
          (println new-pop)
          (if (car (select-best new-pop)) (select-best new-pop)
              (run-evolution new-pop graph population_size (- rounds 1))))))

  (define (select-best population)
    (cond ((null? population) (list #f #f #f))
          ((< (cdar population) 1) (select-best (cdr population)))
          (else (list #t (length (caar population)) (caar population)))))
        
  
  (let ((pop (init-population graph population_size)))
    (println pop)
    (run-evolution pop graph population_size rounds)))

(provide (all-defined-out))


  
      