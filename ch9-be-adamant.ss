;; # Chapter 9 - Be Adamant
#lang racket
(require malt)
(define nabla ∇)

;; Besides using momentum, could use other ways to velocity of convergence.
;;
;; Adaptive approach, the fraction is decided based on gradient and historical values.
;;
;; We would like the fraction of gradient used to reduce more slowly than
;; gradient itself. We would need a modifier to act inversely as the gradient.
;;
;; In g * (a * 1 / G), G is the modifier.
;;
;; Use the histortically accumulated g for G and define an update function

;; beta is a hyper parameter
(declare-hyper beta)

;; special treatment to ensure G > 0
(define epsilon 1e-8)
(define rms-u
  (lambda (P g)  ;; P = (p, accumulated_gradient)
    (let ((r (smooth beta (ref P 1) (sqr g))))  ;; g could be negative
      (let ((alpha-hat (/ alpha (+ epsilon (sqrt r)))))    ;; add small number to avoid being 0.0
        (list (- (ref P 0) (* alpha-hat g)) r)))))

;; deflate and inflate function
(define rms-i
  (lambda (p) (list p (zeroes p))))
(define rms-d
  (lambda (P) (car P)))

;; Define rms gradient descent
;; The gradient descent is called RMSProp, RMS stands for room mean square, Prop
;; stands for back propagation.
;; It is invented by Geoffrey Everest Hinton.
(define rms-gradient-descent (gradient-descent rms-i rms-d rms-u))

;; Give is a spin
(define try-plane
  (lambda (a-gradient-descent a-revs an-alpha)
    (with-hypers
      ((revs a-revs)
       (alpha an-alpha)
       (batch-size 4))
      (a-gradient-descent
       (sampling-obj
        (l2-loss plane) plane-xs plane-ys)
       (list (tensor 0.0 0.0) 0.0)))))
(with-hypers
  ((beta 0.9))
  (try-plane rms-gradient-descent 3000 0.01))

;; Combine the both the velocity and RMS smooth idea to define adam
;; Adam is short for ADAptive Moment estimation
(define adam-u
  (lambda (P g)  ;; P = (p, accumulated_gradient)
    (let ((r (smooth beta (ref P 2) (sqr g))))
      (let ((alpha-hat (/ alpha (+ epsilon (sqrt r))))
            (v (smooth mu (ref P 1) g)))  ;; Using a smoothed version
        (list (- (ref P 0) (* alpha-hat v)) v r)
        ))))

(define adam-i
  (lambda (p) (list p (zeroes p) (zeroes p))))
(define adam-d
  (lambda (P) (ref P 0)))
(define adam-gradient-descent (gradient-descent adam-i adam-d adam-u))

;; Give it a spin
(with-hypers
  ((beta 0.9)
   (mu 0.85))
  (try-plane adam-gradient-descent 1500 0.01))  ;; Errata(p174, frame 44) in the book

;; The Law of Gradient Descent
;;
;; The θ for a target function is learned by using one of the gradient descent
;; functions.
