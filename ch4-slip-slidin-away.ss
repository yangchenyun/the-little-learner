;; # Chapter 4 - Slip-slin's Away
;;
;; Reference
;;
;; Name from "Slip Slidin' Away" by Paul Frederic Simon who is an American musician, singer, and songwriter.
;;
#lang racket

(require malt)
(define nabla ∇) ;; literal definition

;; Because the "magic" function nabla, which computes the gradient, introduced
;; fundamental data structure and procedure chagnes; it is not straightforward
;; to continue our own implementation. Switching to following malt for now. And
;; revisit later.

;; Compute losses for multiple thetas
(define line-xs (tensor 2.0 1.0 4.0 3.0))
(define line-ys (tensor 1.8 1.2 4.2 3.3))

;; The l2-loss function for the fixed dataset
(define obj
  (lambda (theta)
    (((l2-loss line) line-xs line-ys) theta)))

;; sample a few theta parameters for its losses
(define theta-vals '(-1.0 0.0 1.0 2.0 3.0))
(map obj
     (map (lambda (theta0) (list theta0 0.0)) theta-vals))

;; Definition
;; - tangent: the line touches loss curve at exact one point
;; - slope of the tangent: is the rate of change
;; - gradient: rate of change for _all_ parameters of a parameterized function

;; To compute the gradient, using nabla function, which returns a list of gradients in respect to each
;; with parameter.
;;
;; The following nabla implementation implements the algorithm, but it requires changes to
;; scalar data type and all primitive functions (and extended ones).
;;
;; (require "appendix-1-ghost-in-the-machine.ss")
;; ;; test out on simple functions
;; (define sqr-0
;;   (lambda (x)
;;     (*-0-0 x x)))
;; (nabla (λ (θ) (sqr-0 (car θ))) '(27.0))
;;
;; The curried version (∇ f) is the equivalent to the mathematical definition of
;; the gradient of f.
(nabla (λ (θ) (sqr (car θ))) '(27.0))

;; Compute gradient for parameters (0.0, 0.0)
(nabla obj '(0.0 0.0))

;; Law of Revision
;; θi = θi − (α × rate of change of loss w.r.t. θi)
;;
;; iteration on revision
(define revise
  (lambda (f revs theta)
    (cond
      ((zero? revs) theta)
      (else
       (revise f (sub1 revs) (f theta))))))
(revise
 (lambda (theta) (map (lambda (p) (- p 3)) theta))
 5
 '(1 2 3))

;; Implement the algorihm: optimization by gradient descent
(define ogd
  (lambda (f alpha)
    (lambda (theta)
      (map (lambda (p dp) (- p (* alpha dp))) theta (nabla f theta)))))
(revise (ogd obj 0.01) 1000 '(0.0 0.0))

;; Reference
;;
;; Augustin-Louis Cauchy, contributions to the theory of functions the study of
;; partial differential equations and the calculus of variations.

;; Rewrite and extract out the alpha and revs; both could be defined outside the algorithm
(define alpha 0.01)
(define revs 1000)
(define gradient-descent
  (lambda (obj theta)
    (let ((f (lambda (Theta)  ;; Theta is formal of the revision function
               (map (lambda (p dp) (- p (* alpha dp))) Theta (nabla obj Theta)))))
      (revise f revs theta))))
(gradient-descent
 ((l2-loss line) line-xs line-ys) '(0.0 0.0))
