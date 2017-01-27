#lang racket
(define root car)
(define left_tree cadr)
(define right_tree caddr)
(define (make_tree a b c) (list a b c))
(define (leaf? t) (and ((null? (left_tree t)) (null? (right_tree t)))))
(define (make_leaf_tree a) (make_tree a '() '()))
(define (union_trees a b) (make_tree (+ (root a) (root b)) a b))

(define (make_list_of_trees llist)
    (if (null? llist) '()
        (cons (make_leaf_tree (car llist)) (make_list_of_trees (cdr llist)))
    )
)

(define (fenwick llist)
    (define (helper trees)
        (cond ((null? (cdr trees)) (car trees))
              ((null? (cddr trees)) (union_trees (car trees) (cadr trees)))
              (else (helper (fenwick_step trees)))
        )
    )
    (define (fenwick_step trees)
        (cond ((null? trees) trees)
              ((null? (cdr trees)) (list (union_trees (car trees) (make_leaf_tree 0))))
              (else (cons (union_trees (car trees) (cadr trees))
                          (fenwick_step (cddr trees))
                    ))
        )
    )
    (if (null? llist) '()
        (helper (make_list_of_trees llist))
    )
)

(define l_1 (list 1 3 5 (- 1) 2 0 (- 4) 3))
(define l_2 '(1 2 3 4 5 6 7))
(define tree_1 (fenwick l_1))
(define tree_2 (fenwick l_2))