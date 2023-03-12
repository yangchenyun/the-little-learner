;; # Chapter 6: An Apple a Day
#lang racket
(require malt)
(define nabla ∇)

;; Using a batch to calculate the loss during revision instead of using the
;; full data set.
;;

;; ## The Rule of Batches
;; A batch of indices consists of random indices that are natural numbers smaller than

;; Define samples in tail recursion form
(define sampled
  (lambda (n i a)
    (cond
      ((zero? i) a)
      (else (sampled n (sub1 i) (cons (random n) a))))))
(define samples
  (lambda (n i)
    (sampled n i '())))
(samples 20 3)

;; The method to select samples from dataset using indices
(trefs (tensor 5.0 2.8 4.2 2.3 7.4 1.7 8.1) '(6 0 3 1))

;; The method works on tensor with rank > 1
(define t-7-3
  (build-tensor (list 7 3) (lambda (idx) (random 100))))
(print t-7-3)
(trefs t-7-3 '(6 6 0 3 1))

;; Make batch-size a hyper parameter
(declare-hyper batch-size)

;; Refined our objective function using sampled dataset
(define sampling-obj
  (lambda (expectant xs ys)
    (let ((n (tlen xs)))
      (lambda (theta)
        (let ((b (samples n batch-size)))
          ((expectant (trefs xs b) (trefs ys b)) theta))))))

;; The Law of Batch Sizes
;;
;; Each revision in stochastic gradient descent uses only
;; a batch of size batch-size from the data set and the ranks of the tensors in
;; the batch are the same as the ranks of the tensors in the data set.
(let
    ((line-xs (tensor 2.0 1.0 4.0 3.0))
     (line-ys (tensor 1.8 1.2 4.2 3.3)))
    (with-hypers ((revs 1000)
                  (alpha 0.01)
                  (batch-size 4))
      (gradient-descent
       (sampling-obj
        (l2-loss line) line-xs line-ys) '(0.0 0.0))))

;; The sampling function is generic and applies to plane dataset
(with-hypers
  ((revs 15000)
   (alpha 0.001)
   (batch-size 4))
  (gradient-descent
   (sampling-obj
    (l2-loss plane) plane-xs plane-ys)
   (list (tensor 0.0 0.0) 0.0)))
