;; # Chapter 14: It's Really Not That Convoluted
#lang racket
(require racket/function)
(require malt/learner)

;; Helper funtion to display tensor in learner
(define display-tensor (curry map* ρ))

;; To detect patterns in a signal.
;;
;; Definition:
;; filter: a signal-1 to match against source, also called kernels.
;; overlap: elements in the source which are overlapped with a filter during a scan

(define morse-e
  (tensor 0.0 0.0 0.0
          1.0 0.0 0.0
          0.0 0.0 0.0
          0.0 0.0 0.0
          0.0 0.0 0.0
          0.0))

(define kronecker-filter-1
  (tensor 0.0 1.0 0.0))

;; Using dot-product to measure the how similar overlap is to filter.
(define dot-product-1-1 (λ (t0 t1) (sum-1 (* t0 t1))))
(define dot-product (ext2 dot-product-1-1 1 1))
(dot-product (tensor 0.0 1.0 0.0) (tensor 0.0 0.0 0.0))


;; Define overlap with respect to the filter length m
(define overlap
  (lambda (signal i f)
    (let* ((m (tlen f))
           (d (rank f))
           (p (quotient m 2))
           (zeroes
            (lambda ()
              (let ((t (tref signal 0)))
                (cond
                  ((scalar? t) 0.0)
                  (else (tmap (lambda (x) 0.0) t))))))
           (tref-p
            (lambda
              (t i)
              (cond
                ((< i 0) (zeroes))   ;; take care of high dimension initializer
                ((>= i (--ρ (tlen t) 1)) (zeroes))
                (else (tref t i))))))
      (build-tensor
       (list m)
       (lambda (idx)
         (let ((i (+-ρ (ref idx 0) (--ρ i p))))
           (tref-p signal i)))))))
(overlap morse-e 2 kronecker-filter-1)
(overlap morse-e 15 kronecker-filter-1)

;; To collect signals at the head and tail, pad the source signal with 0.0.
;;
;; Then, it would collect measurement between a filter and overlap into result
;; signal with the same length of the source.
;;
;; The Law of Correlation (Single Filter Version)
;;
;; The correlation of a filter of length m with a source signal1 of length n,
;; where m is odd (given by 2p + 1), is a signal1 of length n obtained by
;; sliding the filter from overlap position −p to overlap position n − p − 1,
;; where each segment of the result signal1 is obtained by taking the dot
;; product of the filter and the overlap in the source at each overlap position.

;; Define the correlation
(define correlate-filter
  (lambda (signal filter)
     (build-tensor
      (list (tlen signal))
      (lambda (idx)
        (dot-product
         filter
         (overlap signal (ref idx 0) filter))))))

(define head-tail-e
  (tensor 1.0 0.0 0.0
          0.0 0.0 0.0
          0.0 0.0 0.0
          0.0 0.0 0.0
          0.0 0.0 0.0
          1.0))
(display-tensor
 (correlate-filter morse-e kronecker-filter-1))
(display-tensor
 (correlate-filter head-tail-e kronecker-filter-1))

;; The result signal has the same length of signal and could be used to cascade
;; correlations.

;; Features could be learned by NN.
;;
;; The Rule of Filters
;; Filters are tensor parameters in a θ.

;; Builds an analogous operation to correlate against multiple filters in a
;; single function filters in a single function

;; filter bank: A list of signle-1 filter with same length
(define feature-bank-1
  (tensor
   (tensor 0.0 2.0 1.0)
   (tensor 1.0 2.0 -1.0)))

;; zipping the correlation captured by each filter, in order to get a signal
;; with dimensions with the same number of filters.
;; filter must have the same depth of the source
(define zip
   (curry tmap tensor))

(define correlate-1
  (lambda (signal feature-bank)
    (apply zip
           (vector->list  ;; TODO: could we perform apply on vector/tensor?
            (tmap (lambda (f)
                    (correlate-filter signal f)) feature-bank)))))

(display-tensor
 (correlate-1 morse-e feature-bank-1))

;; The Law of Correlation (Filter Bank Version)
;;
;; The correlation of a filter bank of shape (list b m d) with a source signal2
;; of shape (list n d) is a signal2 of shape (list n b) resulting from zipping
;; the b signals1 resulting from correlating the b filters2 in the bank with the
;; source.
(define st
  (zip
   (correlate-filter morse-e kronecker-filter-1)
   (correlate-filter head-tail-e kronecker-filter-1)))
(display-tensor st)

;; Define b=4, m=3, d=2 filter bank to test
(define feature-bank-2
  (tensor
   (tensor (tensor 0.0 0.0) (tensor 0.0 1.0) (tensor 0.0 0.0))
   (tensor (tensor 0.0 0.0) (tensor 1.0 0.0) (tensor 0.0 0.0))
   (tensor (tensor 0.0 1.0) (tensor 1.0 0.0) (tensor 0.0 0.0))
   (tensor (tensor 0.0 0.0) (tensor 1.0 0.0) (tensor 1.0 0.0))))

;; Wrapping up our correlate definition on 2d signals
(define correlate-2
  (lambda (signal feature-bank)
    (sum
     (correlate-1 signal feature-bank))))

(display-tensor
  (correlate-2 st feature-bank-2))

;; This is the version implemented in malt/learner
(display-tensor
 (correlate feature-bank-2 st))
