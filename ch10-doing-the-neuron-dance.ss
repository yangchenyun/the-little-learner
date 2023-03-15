;; Chapter 10 - Doing the Neuron Dance
#lang racket
(require malt)

;; ## Reference
;;
;; The Pointer Sisters is an American R&B singing group that was formed in
;; Oakland, California in 1969. Their music combined elements of R&B, pop,
;; disco, and funk, and their energetic live performances made them popular with
;; audiences around the world.

;; Non-linear activation function
(define rectify-0
  (lambda (s)
    (cond
      ((< s 0.0) 0.0)
      (else s))))

(define rectify
  (ext1 rectify-0 0))

;; Define a linear function to use with rectify
;; dot-product uses only addition and multiplication, and hence is a linear
;; operation
(define linear-1-1
  (lambda (t)
    (lambda (theta)
      (+ (dot-product (ref theta 0) t) (ref theta 1)))))

;; Definition
;; relu: rectifying linear unit
;;
;; The theta consistes of weights and bias, each controls how the elements in
;; a tensor would be used for activation.
(define relu-1-1
  (lambda (t)
    (lambda (theta)
      (rectify ((linear-1-1 t) theta)))))
((relu-1-1 (tensor 2.0 1.0 3.0))
 (list (tensor 7.1 4.3 -6.4) 0.6))

;; The Rule of Artificial Neurons
;;
;; An artificial neuron is a parameterized linear function composed with a
;; nonlinear decider function.


;; ## Reference
;;
;; Warren Sturgis McCulloch and Walter Pitts created the brain theory
;; which later became the foundation the application of neural networks to
;; artificial intelligence.

;; Relu the thetas controls the shape of relu on a 2D-plane
(define test-relu-1
  (lambda (x)
     ((relu-1-1 (tensor x)) (list (tensor 1.0) -1.0))))
(map test-relu-1 '(0.5 1.0 1.5 2.0 2.5 3.0))

;; build a one-end strip by combining two relus
(define half-strip
  (lambda (x theta)
    (- ((relu-1-1 (tensor x)) (list (ref theta 0) (ref theta 1)))
       ((relu-1-1 (tensor x)) (list (ref theta 0) (ref theta 2))))))

(map
 (lambda (x)
   (half-strip x (list (tensor 1.0) -1.0 -1.5)))
 '(-0.5 0 0.5 1.0 1.5 2.0 2.5 3.0 3.5))

;; build a full strip by combining two half-strips
(define full-strip
  (lambda (x theta)
    (- (half-strip x (list (ref theta 0) (ref theta 1) (ref theta 2)))
       (half-strip x (list (ref theta 3) (ref theta 4) (ref theta 5))))))

(map
 (lambda (x)
   (full-strip x (list (tensor 1.0) -1.0 -1.5 (tensor 1.0) -3.0 -3.5)))
 '(-0.25 0 0.25 1.0 1.25 2.0 2.25 3.0 3.25))

;; Full-strip and half-strip could be combined
(map
 (lambda (x)
   (+
    (full-strip x (list (tensor 1.0) -1.0 -1.5 (tensor 1.0) -3.0 -3.5))
    (half-strip x (list (tensor 1.0) -1.0 -1.5))
    ))
 '(-0.25 0 0.25 1.0 1.25 2.0 2.25 3.0 3.25 3.75 4.0))

;; This is building blocks behind universal approximation.
;;
;; ## Reference
;;
;; George Cybenko, Maxwell Stinchcombe, Halbert Lynn White Jr. and
;; Kurt Hornik all contributed to the universal approxmation theory.
;;
;; Universal approximation theorems imply that neural networks can represent a
;; wide variety of interesting functions when given appropriate weights.
