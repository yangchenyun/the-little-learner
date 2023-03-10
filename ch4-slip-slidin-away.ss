;; # Chapter 4 - Slip-slin's Away
;;
;; Reference
;;
;; Name from "Slip Slidin' Away" by Paul Frederic Simon who is an American musician, singer, and songwriter.

#lang racket
(require racket/include)
(include "ch3-running-down-a-slippery-slope.ss")

(define theta-list
  (lambda (l)
    (map (lambda (theta0) (list theta0 0.0)) l)))
(theta-list '(-1.0 0.0 1.0 2.0 3.0))

(define obj
  (lambda (theta)
    (((l2-loss line) line-xs line-ys) theta)))
(define theta-vals '(-1.0 0.0 1.0 2.0 3.0))
(define loss-vals (map obj (theta-list theta-vals)))

;; Plot the theta and loss values in pairs
(require plot)
(plot-new-window? #t)
(parameterize ([plot-width    150]
               [plot-height   150]
               [plot-x-label  #f]
               [plot-y-label  #f])
  ((plot (points (map vector theta-vals loss-vals )
                 #:x-min -2 #:x-max 5
                 #:y-min -10 #:y-max 200
                 ))))

;; Definition
;; - tangent: the line touches loss curve at exact one point
;; - slope of the tangent: is the rate of change
;; - gradient: rate of change for _all_ parameters of a parameterized function

;; Introduce the "magic" function nebla which computes the gradient.
;; The curried version (∇ f) is the equivalent to the mathematical definition of the gradient of f.
;; (require (only-in malt ∇))
(∇ (λ (θ) (sqr (car θ))) '(27.0))

;; Apply ∇ to calculate gradients in our obj functino
;;
(∇
 ;; (lambda (theta)
 ;;   (((l2-loss line) line-xs line-ys) theta))
 (lambda (theta) (* (car theta) (car (cdr theta))))
 '(2.0 1.0))
