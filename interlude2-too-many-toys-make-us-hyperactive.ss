;; # Interlude II - Too Many Toys Make Us Hyperactive

#lang racket
(require malt)

;; Definition hyperparameters
;; - Control the behaviors of function behaviors
;; - Have global scope
;;
;; The Rule of Hyperparameters
;; Every hyperparameter either is a scalar or has no value.

;; declare-hyper declares and inits parameter
(declare-hyper smaller)
(declare-hyper larger)
;; with-hypers provides new value for pre-existing declared hyper parameter
(with-hypers
  ((smaller 1)
   (larger 2000))
  (+ smaller larger))

;; Reference
;;
;; Arthur C. Clarke was a British science fiction writer and futurist who is
;; best known for his novel "2001: A Space Odyssey".

;; Demo of nested examples of hyper parameters
(with-hypers
  ((smaller 1)
   (larger 2000))
  (with-hypers
    ((smaller 42))
    (displayln (+ smaller larger)))
  (displayln (+ smaller larger)))

;; Demo fo dynamic scope variables
(define non-sense?
  (lambda (x)
    (= (sub1 x) smaller)))
(non-sense? 10)
(with-hypers
  ((smaller 5))
  (non-sense? 6))  ;; the smaller takes on the values within `with-hypers`
