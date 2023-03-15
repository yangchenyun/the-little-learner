;; # Chapter 12 - Rock Around the Block
#lang racket
(require malt)

;; ## Reference
;; Bill Haley & His Comets - Rock Around The Clock (OST, 1956)


;; Parameters used in testing
(define w1 (tensor (tensor 2.0 1.0 3.1) (tensor 3.7 4.0 6.1)))
(define b1 (tensor 1.0 2.0))
(define w2 (tensor (tensor 1.0 0.5 5.1) (tensor 7.7 4.1 3.3)))
(define b2 (tensor 0.5 0.2))
(define w3 (tensor (tensor 1.0 0.5 5.1) (tensor 7.7 4.1 3.3)))
(define b3 (tensor 0.5 0.2))

;; Definition
;; A block associates a layer function with its shape list.
(define block
  (lambda (fn shape-ls)
    (list fn shape-ls)))
(define block-fn
  (lambda (ba) (ref ba 0)))
(define block-ls
  (lambda (ba) (ref ba 1)))

(define layer1
  (block relu '((64 32) (64))))
(define layer2
  (block relu '((45 64) (45))))
(define layer3
  (block relu '((26 45) (26))))

;; neural network consists of network function and a shape list
;;
;; let's define a method to compose blocks
(define block-compose
  (lambda (f g j)
    (lambda (t)
      (lambda (theta)
        ((g ((f t) theta)) (refr theta j))))))

;; 2-relu and 3-relu could be defined with block-compose
(define 2-relu
  (block-compose relu relu 2))
(define 3-relu
  (block-compose relu 2-relu 2))
(let* ((theta (list w1 b1 w2 b2 w3 b3))
       (t (tensor 1.3 0.4)))
  ((2-relu t) theta)
  ((3-relu t) theta))

;; Define procedure which stacks two blocks and takes care of shape list composition
(define block-stack2
  (lambda (ba bb)
    (block
     (block-compose (block-fn ba) (block-fn bb) (length (block-ls ba)))
     (append (block-ls ba) (block-ls bb)))))

;; Now define block-stack recursive form (use tail recursion)
(define block-stack
  (lambda (bls)
    (block-stacked (refr bls 1) (ref bls 0))))

(define block-stacked
  (lambda (rbls b) ;; b is the accumulator
    (cond
      ((null? rbls) b)
      (else
       (block-stacked (refr rbls 1) (block-stack2 b (ref rbls 0)))))))

;; The Law of Blocks
;; Blocks can be stacked to form bigger blocks and complete networks.
(let* ((theta (list w1 b1 w2 b2 w3 b3))
       (t (tensor 1.3 0.4)))
  (((block-fn
     (block-stack (list layer1 layer2 layer3))) t) theta))

;; define a dense block
(define dense-block
  (lambda (n m)
    (block relu
           (list (list m n) (list m)))))

;; Redefine layers
(define layer1 (dense-block 32 64))
(define layer2 (dense-block 64 45))
(define layer3 (dense-block 45 26))
(let* ((theta (list w1 b1 w2 b2 w3 b3))
       (t (tensor 1.3 0.4)))
  (((block-fn
     (block-stack (list layer1 layer2 layer3))) t) theta))

;; Block abstracts the "stacking" inside k-relu and allows using arbitrary
;; functions to compose a large neural network.
