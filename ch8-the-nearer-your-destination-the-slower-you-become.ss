;; # Chapter 8 - The Nearer Your Destination, the Slower You Become
#lang racket
(require malt)

;; ## Reference
;;
;; Lyrics in "Slip slidin' away" by Paul Frederic Simon: "You know the nearer
;; your destination The more you're slip slidin' away."

;; ## Reference
;; The Marx Brothers American family comedy on Broadway and in motion pictures.

;; The core idea of strategy it to let slower runner inherits some velocity from
;; faster runners.

;; Adapt the strategy in our gradietn descent
;; velocity: rate of change for each parameter

(declare-hyper mu)
(define velocity-i (lambda (p) (list p (zeroes p))))
(define velocity-d (lambda (P) (ref P 0)))
(define velocity-u (lambda (P g)
                     (let ((v (- (* mu (ref P 1)) (* alpha g))))
                       (list (+ (ref P 0) v)
                             v))))
(define velocity-gradient-descent
  (gradient-descent velocity-i velocity-d velocity-u))

;; Try it out with few revisions
(define try-plane
  (lambda (a-gradient-descent a-revs)
    (with-hypers
      ((revs a-revs)
       (alpha 0.001)
       (batch-size 4))
      (a-gradient-descent
       (sampling-obj
        (l2-loss plane) plane-xs plane-ys)
       (list (tensor 0.0 0.0) 0.0)))))
(with-hypers
  ((mu 0.9))
  (try-plane velocity-gradient-descent 5000))

;; It is known as momentum gradient descent
;;
;; ## Reference
;;
;; David E. Rumelhart, formal analysis of human cognition, symbolic artificial
;; intelligence, parallel distributed processing.
;;
;; Geoffrey Hinton and Ronald J. Williams, with David, created back-propagation
;; algorithm to multi-layer neural networks; and simulation of perceptrons in
;; neural network.
