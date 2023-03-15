;; Chapter 13 - An Eye for an Iris
#lang racket
(require malt)
(require malt/examples/iris)

;; ## Reference
;;
;; Crêpes Suzette is a French dessert consisting of crêpes with beurre Suzette,
;; a sauce of caramelized sugar and butter, tangerine or orange juice, zest, and
;; Grand Marnier, triple sec or orange Curaçao liqueur on top, flambéed
;; tableside. Julia Child popularized it.

;; ## Reference
;; Edgar Anderson was an American botanist. His research was focused on
;; developing techniques to quantify geographic variation in Iris versicolor.

;; ## Reference
;; Leopold Kronecker invented one-hot encoding, named Kronecker delta
;; The 1.0 here represents degree of belief

;; Dataset scheme is
;; X: [sepal_width, sepal_length, petal_width, petal_length] is a 4-tensor
;; Y: [0, 0, 0], 3-tensor using one-hot encoding for {Setosa, Versicolor, Virginica}

;; Design our network suited for the dataset.
;; - Input and output ranks are fixed by the dataset
;; - The hyperparameters such as layer numbers and other are often learned from exploring the dataset
(define iris-network
  (stack-blocks
   (list (dense-block 4 6)
         (dense-block 6 3))))

;; Preparing training, solve two problems.
;;
;; - Breaking the symmetry during initialization
;; - Avoding the exploding and vanishing gradient problem

;; Kaiming initialization: for network uses rectify (non-linear activation), the
;; best variance is 2 / n where n is length of input network.
;;
;; The Rule of Layer Initialization
;;
;; The bias tensor1 of a layer is initialized to contain only 0.0 The weight
;; tensor2 of a layer is initialized to random scalars with a central value of
;; 0.0 and a variance of 2/n where n is the length of the input to the layer.
(define init-theta
  (lambda (shapes)
    (map init-shape shapes)))

(define init-shape
  (lambda (s)
    (cond
      ((= (length s) 1) (zero-tensor s)) ;; bias layer
      ((= (length s) 2) (random-tensor 0.0 (ρ (/ 2.0 (ref s 1))) s)))))

;; Use pre-split dataset train and get iris-theta
(define iris-classifier
  (block-fn iris-network))
(define iris-theta-shapes
  (block-ls iris-network))
(define iris-theta
  (with-hypers
    ((revs 2000)
     (alpha 0.0002)
     (batch-size 8))
    (naked-gradient-descent
     (sampling-obj
      (l2-loss iris-classifier) iris-train-xs iris-train-ys)
     (init-theta iris-theta-shapes))))

;; The classifier together with iris-theta forms an idealized model
(define model
  (lambda (target theta)
    (lambda (t)
        ((target t) theta))))
(define iris-model
  (model iris-classifier iris-theta))

;; Give the model a spin check its loss against train and test dataset
(define mean-1
  (lambda (t)
    (/ (sum-1 t) (tlen t))))
(define eval-model-loss
  (lambda (model xs ys)
    (let ((eval-model
           (lambda (t) (lambda (_) (model t))))
          (n (tlen xs)))
      (mean-1 (((l2-loss eval-model) xs ys) '()))
      )))
(eval-model-loss iris-model iris-test-xs iris-test-ys)
(eval-model-loss iris-model iris-train-xs iris-train-ys)
