;; Running Down a Slippery Slope

(require racket/include)
(include "interlude1-the-more-we-extend.ss")

;; Reference
;;
;; Café du Monde is a coffee shop located in New Orleans, Louisiana.
;; It was established in 1862 and is known for its beignets, a type of fried
;; pastry covered in powdered sugar.

;; Reference
;;
;; Sir Isaac Newton (1643–1727) and Joseph Raphson developed
;; successive approximations method to find square root.
;;
;; Mission, find a well fitted theta using successive approximations
;; Define a scalar to measure how well the fit is
(define line-xs '(2.0 1.0 4.0 3.0))
(define line-ys '(1.8 1.2 4.2 3.3))
(define init-theta '(0.0 0.0))

;; Would need minus-extended, and sqr-extended
(define minus-extended (op-extended -))
(define sqr-extended (unary-op-extended (lambda (x) (* x x)) 0))

;; Arrive at l2-loss function
;; This belongs to a family to function using exponents to find loss, in this case, the power is 2.
;; The square root of this sum is named L2-norm
(define l2-loss
  (lambda (xs ys)
    (lambda (theta)
      (let ([pred-ys ((line line-xs) theta)])
        (sum-extended
         (sqr-extended
          (minus-extended ys pred-ys)))))))
((l2-loss line-xs line-ys) init-theta)

;; This concept could be applied other than lines
;; Let's generalize l2-loss
(define l2-loss
  (lambda (target)
    (lambda (xs ys)
      (lambda (theta)
        (let ([pred-ys ((target line-xs) theta)])
          (sum-extended
           (sqr-extended
            (minus-extended ys pred-ys))))))))
(((l2-loss line) line-xs line-ys) init-theta)

;; Definition:
;; expectant function - function which expects a dataset as arguments. (l2-loss line)
;; objective function - function awaits theta as its arguments. ((l2-loss line) line-xs line-ys)
(define revised-theta0
  (lambda (theta delta)
    (cons (+ delta (first theta)) (cdr theta))))


(((l2-loss line) line-xs line-ys)
 (revised-theta0 init-theta 0.0099))

;; Reference
;;
;; Derivative theory, Gottfried Wilhelm Leibniz (1646–1716) and thanks, Sir Isaac Newton.

;; Calculate derivative and apply them to make changes
(define naive-derivate-theta0
  (lambda (theta delta)
    (let* ([l2 ((l2-loss line) line-xs line-ys)]
           [loss1 (l2 theta)]
           [loss2 (l2 (revised-theta0 theta delta))])
      (/ (- loss2 loss1) delta))))
(naive-derivate-theta0 init-theta 0.0099)

;; Demostration of the overshooting problem
(define dtheta0
  (naive-derivate-theta0 init-theta 0.0099))
(((l2-loss line) line-xs line-ys) (revised-theta0 init-theta dtheta0))

;; Definition
;; learning rate: a small scalar value as a multiplier of derivative to regulate changes
;;
;; The Law of Revision
;; new theta_0 = theta_0 - alpha * rate_of_change
(define alpha 0.01)
(define new-theta0
  (lambda (theta)
    (let* ([dtheta 0.0099])
      (revised-theta0 theta (- (* alpha (naive-derivate-theta0 theta dtheta)))))))
(((l2-loss line) line-xs line-ys) (new-theta0 init-theta))

;; Repeat it one more time, is there better way?
(((l2-loss line) line-xs line-ys)
 (new-theta0
  (new-theta0 init-theta)))
