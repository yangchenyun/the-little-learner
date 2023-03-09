;; # Interlude I
;; The More We Extend, the Less Tensor We Get

(require racket/include)
(include "ch2-the-more-we-learn-the-tenser-we-become.ss")

;; Let's build extended function used to work on scalar

;; Utility function
(define zip
  (lambda (l1 l2)
    ;; (assert (eq (length l1) (length l2)))
    (cond
      ((and (null? l1) (null? l2)) '())
      (else
       (cons (list (car l1) (car l2))
             (zip (cdr l1) (cdr l2)))))))
(define t1 '(5 6 7))
(define t2 '(2 0 1))
(zip t1 t2)

(define add-extended
  (lambda (x y)
    (cond ((> (rank x) (rank y)) (add-extended y x))
          ((= 0 (rank y)) (+ x y))
          ((= (rank x) (rank y))
           (map
            (lambda (xyi) (add-extended (car xyi) (car (cdr xyi))))
            (zip x y)))
          ;; broadcasting rule, lower the rank of y
          (else
           (map (lambda (yi) (add-extended x yi)) y))
          )))
(add-extended 17 42)
(add-extended '(17) '(42)) ; tensor-1, 1 element
(add-extended '(17 42) '(11 12)) ; tensor-1, 2 elements
;; works on tensor-2
(add-extended '((4 6 7) (2 0 1)) '((1 2 2) (6 3 1)))
;; works on scalar and tensor-1
(add-extended 4 '(3 6 5))
;; works on tensor-1 and tensor-2
(add-extended '(6 9 1) '((4 3 8) (7 4 7)))
;; works on scalar and tensor-3
(add-extended 3 '((4 3 8) (7 4 7)))


;; Now let's generalize the function for operator between scalar
(define op-extended
  (lambda (op)
    (letrec
        ([op-extended
          (lambda (x y)
            (cond ((> (rank x) (rank y)) (op-extended y x))
                  ((= 0 (rank y)) (op x y))
                  ((= (rank x) (rank y))
                   (map
                    (lambda (xyi) (op-extended (car xyi) (car (cdr xyi))))
                    (zip x y)))
                  ;; broadcasting rule, lower the rank of y
                  (else
                   (map (lambda (yi) (op-extended x yi)) y))
                  ))])
      op-extended
      )))

;; test out * operator
;; (define add-extended (op-extended +))
(define mul-extended (op-extended *))
(mul-extended 17 42)
(mul-extended '(17) '(42)) ; tensor-1, 1 element
(mul-extended '(17 42) '(11 12)) ; tensor-1, 2 elements
;; works on tensor-2
(mul-extended '((4 6 7) (2 0 1)) '((1 2 2) (6 3 1)))
;; works on scalar and tensor-1
(mul-extended 4 '(3 6 5))
;; works on tensor-1 and tensor-2
(mul-extended '(6 9 1) '((4 3 8) (7 4 7)))
;; works on scalar and tensor-3
(mul-extended 3 '((4 3 8) (7 4 7)))

;; And let's work on unary extended functions, i.e. sqrt
(define unary-op-extended
  (lambda (op)
    (letrec
        ([op-extended
          (lambda (x)
            (cond
              ((scalar? x) (op x))
              (else
               (map (lambda (x_i) (op-extended x_i)) x))
              ))
          ])
      op-extended
      )))
(define sqrt-extended (unary-op-extended sqrt))
(sqrt-extended '(9 16 25))
(sqrt-extended '((49 81 16) (64 25 36)))

;; Another extended function to sum up scalars of a tensor_1
(define sum-1
  (lambda (t)
    (cond
      ((null? t) 0)
      (else (+ (tensor-ref t 0) (sum-1 (cdr t)))))))
(sum-1 '(10.0 12.0 14.0))

;; Try again using "the Law of Simple Accumulator Passing"
(define summed
  (lambda (t i a)
    (cond
      ((zero? i) (+ (tensor-ref t i) a))
      (else (summed t (sub1 i) (+ a (tensor-ref t i)))))))

(define sum-1
  (lambda (t)
    (summed t (sub1 (num-elements t)) 0)))
(sum-1 '(10.0 12.0 14.0))

;; Modify the unary-op-extended to incorporate sum-1
(define unary-op-extended
  (lambda (op op_rank)
    (letrec
        ([op-extended
          (lambda (x)
            (cond
              ((= (rank x) op_rank) (op x))  ; allow specify the op_rank stop descend
              (else
               (map (lambda (x_i) (op-extended x_i)) x))
              ))
          ])
      op-extended
      )))

;; continue working for sqrt
(define sqrt-extended (unary-op-extended sqrt 0))
(sqrt-extended '(9 16 25))
(sqrt-extended '((49 81 16) (64 25 36)))

;; also works for sum
(define sum-extended (unary-op-extended sum-1 1))
(sum-extended '(9 16 25))
(sum-extended '((49 81 16) (64 25 36)))

;; The Law of Sum
;; For a tensor t with rank r > 0, the rank of (sum t) is r − 1.

;; Composed function with extended functions work in extended fashion too!
;; let's redefine line using extended functions from chapter 1
(define line
    (lambda (x)
        (lambda (θ) (add-extended (second θ) (mul-extended x (first θ))))))
((line '(2 7 5 11)) '(4 6))
