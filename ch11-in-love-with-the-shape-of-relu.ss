;; # Chapter 11 - In Love with the Shape of Relu
#lang racket
(require malt)


;; ## Reference:
;; Ed Sheeran - Shape of You

;; A layer is a group of neurons all operate on same tensor and produces result
;; in a new tensor.

;; Start with a construction of M neurons layer
;; It accepts t_N, theta has w (M, N), b (N) and produces t_M
;; This is called __dense__ layer, or fully-connected layer
;; M is called the width of the layer
(lambda (t)
  (lambda (theta)
    (let ((w (ref theta 0))
          (b (ref theta 1)))
      (tensor
       ((relu-1-1 t) (list (ref w 0) (ref b 0)))
       ((relu-1-1 t) (list (ref w 1) (ref b 1)))
       ((relu-1-1 t) (list (ref w 2) (ref b 2)))
       ((relu-1-1 t) (list (ref w 3) (ref b 3)))))))

;; ## The Law of Dense Layers
;;
;; A dense layer function invokes m neurons on an n- element input tensor1 that
;; produces an m-element output tensor1 in a single invocation of *2,1.

;; Use *-2-1 to define dot-product-2-1
(define dot-product-2-1
  (lambda (w t)
    (sum (*-2-1 w t))))  ;; sum would "descend" into each element

;; Give it a spin
(let
((w (tensor (tensor 2.0 1.0 3.1) (tensor 3.7 4.0 6.1)))
 (t (tensor  1.3 0.4 3.3)))
    (dot-product-2-1 w t))

;; Use the dot-product-2-1 to redefine linear
;; It returns tensor_1 with the length the same as the number of neurons.
(define linear
  (lambda (t)
    (lambda (theta)
      (+ (dot-product-2-1 (ref theta 0) t) (ref theta 1)))))

;; Refine relu using the linear
(define relu
  (lambda (t)
    (lambda (theta)
      (rectify ((linear t) theta)))))
(let*
    (
     (w1 (tensor (tensor 2.0 1.0 3.1) (tensor 3.7 4.0 6.1)))
     (b1 (tensor 1.0 2.0))
     (theta (list w1 b1))
     (t (tensor 1.3 0.4)))
  ((relu t) theta))

;; Definition
;; shape list of the layer: the shape of w and b combined
;;   For a dense layer of M neurons, the shape list is '((m n) (m))
;;
;; network function: assembles layer functions together, output of one layer
;; becomes the input of next layer

;; 1-layer relu network
(define 1-relu
  (lambda (t)
    (lambda (theta)
      ((relu t) theta))))

;; 2-layer relu network
(define 2-relu
  (lambda (t)
    (lambda (theta)
      ((relu
        ((relu t) theta))  ;; only using the first two
       (refr theta 2)))))

;; Reference
;;
;; Kenneth Eugene Iverson invented APL (A Programming Language). Its central
;; datatype is the multidimensional array. It uses a large range of special
;; graphic symbols to represent most functions and operators, leading to very
;; concise code. ↓ is a drop operator.

;; Let's generalize and define k-relu
;; Used Currying here
(define k-relu
  (lambda (k)
    (lambda (t)
      (lambda (theta)
        (cond
          ((zero? k) t)
          (else
           (((k-relu (sub1 k)) ((relu t) theta))
            (refr theta 2))
           ))))))
(let*
    (
     (w1 (tensor (tensor 2.0 1.0 3.1) (tensor 3.7 4.0 6.1)))
     (b1 (tensor 1.0 2.0))
     (w2 (tensor (tensor 1.0 0.5 5.1) (tensor 7.7 4.1 3.3)))
     (b2 (tensor 0.5 0.2))
     (w3 (tensor (tensor 1.0 0.5 5.1) (tensor 7.7 4.1 3.3)))
     (b3 (tensor 0.5 0.2))
     (theta (list w1 b1 w2 b2 w3 b3))
     (t (tensor 1.3 0.4)))
    (((k-relu 3) t) theta))
