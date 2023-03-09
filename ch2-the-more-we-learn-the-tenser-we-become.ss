;; # Chapter 2 - The More We Learn, the Tenser We Become


(require racket/include)
(include "ch1-the-lines-sleep-tonight.ss")

;; scalars = real numbers
(define pi 3.141592653589793)

;; Reference
;;
;; George Boole (1815-1864) was a British mathematician and philosopher who is
;; best known as the founder of Boolean algebra. His work on logic and algebra
;; laid the foundations for modern digital computer logic, and his ideas have
;; had a profound impact on the development of computer science.

;; tensor one groups one or more scalars together
(define tensor-1 '(5.0 7.18 pi))
(define tensor-2 '((7 6 2 5) (3 8 6 9) (9 4 8 5)))

;; Reference
;;
;; Gregorio Ricci-Curbastro and Tullio Levi-Civita invented tensor calculus.

;; Function to return number of tensors
(define num-elements (lambda (t) (length t)))
(num-elements tensor-2)
(num-elements '[[[[8]]]])

;; Definition: Rank
;;
;; - the rank of a tensor is the nested depth
;; - tensor with rank m + 1 *must* contains tensor with rank m with the same elements
;; - rank of scalar 0

;; tensor-ref gets the nth element of the tensor
(define
  tensor-ref list-ref)
(tensor-ref tensor-2 2)
;; scalar predicate
(define scalar? real?)

(define rank
  (lambda (t) (if (scalar? t) 0 (+ 1 (rank (tensor-ref t 0))))))
(rank '[[[8] [9]] [[4] [7]]])

;; Definition: Shape
;;
;; - the shape of tensor is a list of number of elements for itself and nested tensor
;; - shape of scalar is empty list

(define shape
  (lambda (t) (if (scalar? t) '()
    (cons (num-elements t) (shape (tensor-ref t 0))))))
(shape 9)
(shape '[[[8] [9]] [[4] [7]]])
(shape '[[[5] [6] [8]] [[7] [9] [5]]])

;; ## The Rule of Uniform Shape:
;; All elements of a tensor must have the same shape.

;; ## The Law of Rank and Shape
;; The rank of a tensor is equal to the length of its shape

(define t '[[[5] [6] [8]] [[7] [9] [5]]])
(eq? (length (shape t)) (rank t))

;; tail-recursion form of rank
(define ranked
  (lambda (t a)
    (cond ((scalar? t) a)
          (else (ranked (tensor-ref t 0) (+ 1 a))))))
(define rank (lambda (t) (ranked t 0)))

(rank t)

;; ## The Law of Simple Accumulator Passing
;; Tail recursion is important to handle computation complexity for large tensors.
;; Implementation such as SBCL and Racket required to support optimization
