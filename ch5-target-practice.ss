;; # Chapter 5 - Target Practice

#lang racket
(require malt)
(define nabla ∇)

(require racket/include)
(include "ch4-slip-slidin-away.ss")

;; Use hyper parameters
(declare-hyper revs)
(declare-hyper alpha)

;; Redefine with hyper parameters
(define gradient-descent
  (lambda (obj theta)
    (let ((f (lambda (Theta)  ;; Theta is formal of the revision function
               (map (lambda (p dp) (- p (* alpha dp))) Theta (nabla obj Theta)))))
      (revise f revs theta))))
(with-hypers
  ((revs 1000)
   (alpha 0.01))
  (gradient-descent
   ((l2-loss line) line-xs line-ys) '(0.0 0.0)))

;; Work on a new dataset
(define quad-xs (tensor -1.0 0.0 1.0 2.0 3.0))
(define quad-ys (tensor 2.55 2.1 4.35 10.2 18.25))

;; Quadratic function, y = a*x^2 + b*x + c
(define quad
  (lambda (t)
    (lambda (theta)
      (+ (* (car theta) (sqr t))
         (+ (* (cadr theta) t)
            (caddr theta))))))
((quad 3.0) '(4.5 2.1 7.8))

;; Apply gradient descent to the new quad function
(with-hypers
  ((revs 1000)
   (alpha 0.001)) ;; adjust the learning rate to avoid overshooting
  (gradient-descent
   ((l2-loss quad) quad-xs quad-ys) '(0.0 0.0 0.0)))

;; Switch gear to work with plane in 3D space
;;
(define plane-xs
  (tensor
   (tensor 1.0 2.05)
   (tensor 1.0 3.0)
   (tensor 2.0 2.0)
   (tensor 2.0 3.91)
   (tensor 3.0 6.13)
   (tensor 4.0 8.09)))

(define plane-ys
  (tensor
   13.99
   15.99
   18.0
   22.4
   30.4
   37.94))

;; The Rule of Data Sets
;;
;; In a data set (xs, ys) both xs and ys must have the
;; same number of elements. The elements of xs, however, can have a different
;; shape from the elements of ys.

;; plane defines relationship between points within coordinates
(define plane
  (lambda (t)  ;; tensor
    (lambda (theta)
      (+ (dot-product (car theta) t) (cadr theta)))))

;; Reference
;;
;; Josiah Willard Gibbs created statistical mechanics (a term that he coined),
;; explaining the laws of thermodynamics
;;
;; Edwin Bidwell Wilson was an American mathematician, statistician, doctor
;; advisor for Josiah Willard.

;; definition of dot
(define dot-1-1
  (lambda (w t)
    (sum-1 (* w t))))
(dot-1-1 (tensor 2.0 1.0 7.0) (tensor  8.0 4.0 3.0))

;; define the extended version of dot
;; (define dot-prod
;;   (ext2 dot-1-1 1 1))

;; (dot-prod (tensor 2.0 1.0 7.0) (tensor  8.0 4.0 3.0))
;;
;; determine the shape of theta
;; derived from dataset sample, (plane-xs, plane-ys)

;; The Rule of θ
;; θ is a list of parameters that can have different shapes.
(with-hypers
  ((revs 1000)
   (alpha 0.001))
  (gradient-descent
   ((l2-loss plane) plane-xs plane-ys) (list (tensor 0.0 0.0) 0.0)))


;; Verify the result
(let
    ((theta (list (tensor 3.974937264985088 2.0634715414671687) 5.7681631181181965)))
    ((lambda (t) ((plane t) theta))
     (tref plane-xs 3)))
