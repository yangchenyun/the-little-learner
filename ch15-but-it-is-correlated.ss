;; # Chapter 15: ...But It is Correlated!
#lang racket

(require malt)
(require malt/examples/morse)

(define display-tensor (curry map* ρ))

;; Define corr as a linear function, theta_0 represents the feature bank;
;; theta_1 is the bias.
(define corr
  (lambda (t)
    (lambda (theta)
      (+ (correlate (ref theta 0) t) (ref theta 1)))))

;; Defining recu, rectifying correlational unit
(define recu
  (lambda (t)
    (lambda (theta)
      (rectify ((corr t) theta)))))

;; The shape list for recu is '((b m d) b), use it to define the block
;; watch, the theta_1 is used to descend into the result from correlation.
(define recu-block
  (lambda (b m d)
    (block recu
           (list
            (list b m d)
            (list b)))))

;; Definition
;; Networks use correlation is known as Convolutional Neural Networks or CNNs.
;;
;; David H. Hubel and Torsten Wiesel's study showed how the visual system builds
;; an image from simple stimuli into more complex representations.
;;
;; Kunihiko Fukushima developed the original deep CNN architecture.
;;
;; Difference between correlation and convolution: in convolution, filters are
;; mirrored in order learn in different directions.

;; Build the block, the output is tensor of shape 26
;; if we serve recu with a bank of filters each represents one letter, it would creates
;; a result in shape (n 26), we need to summon up the filtered results on each segment.
(define sum-2 sum-1)
(define sum-cols
  (ext1 sum-2 2))
(display-tensor
 (sum-2 (tensor
         (tensor 1 2 3 4)
         (tensor 10 20 30 40))))

;; Convert the collected measurement use global average pooling
(define signal-avg
  (lambda (t)
    (lambda (theta)
      (let* ((tni (- (rank t) 2))
             (sshape (refr (shape t) tni))
             (ns (ref sshape 0))) ;; (ref (shape t) tni) ?
        (/ (sum-cols t) ns)))))
(display-tensor
 ((signal-avg (tensor
               (tensor 1 2 3 4)
               (tensor 10 20 30 40))) '()))

(define signal-avg-block
  (block signal-avg '()))

;; Design another recurring block for the network
;; Fully convolutional network
(define fcn-block
  (lambda (b m d)
    (stack-blocks
     (list
      (recu-block b m d)
      (recu-block b m b)))))

;; Define the morse convolution network
;;
;; Filter number selection intuition:
;; - At the input, match features in from the input
;; - At the output, match output dimensions
(define morse-fcn
  (stack-blocks
   (list
    (fcn-block 4 3 1)
    (fcn-block 8 3 4)
    (fcn-block 16 3 8)
    (fcn-block 26 3 16)
    signal-avg-block)))

;; Initialize the theta for training
;;
;; Adjust the n used for random variance.
;;
;; In Keming initialization, n is the number of scalars from the input that are
;; multiplied by weights in the layer to produce a single scalar
(define init-shape
  (lambda (s)
    (cond
      ((= (length s) 1) (zero-tensor s)) ;; bias layer
      ((= (length s) 2) (random-tensor 0.0 (ρ (/ 2.0 (ref s 1))) s))
      ((= (length s) 3) (random-tensor 0.0 (ρ (/ 2.0 (* (ref s 1) (ref s 2)) )) s)))))

(define init-theta
  (lambda (shapes)
    (map init-shape shapes)))

;; Preprocessing the data:
;; - Conform to the dimension expected from network
;; - Shift the data to avoid clustering around 0.0, as they would diminish the weight

;; Train setup
(define trained-morse
  (lambda (target theta-shapes)
    (model target
           (adam-gradient-descent
            (sampling-obj
             (l2-loss target) morse-train-xs morse-train-ys)
            (init-theta theta-shapes)))))

(define train-morse
  (lambda (network)
    (with-hypers
      ((alpha 0.0005)
       (revs 20000)
       (batch-size 8)
       (mu 0.9)
       (beta 0.999))
      (trained-morse
       (block-fn network)
       (block-ls network)))))

(accuracy
 (train-morse morse-fcn)
 morse-test-xs morse-test-ys)

;; Skip network by adding the input to the output
;; The input t is converted to match the output shape (n b)
;;
;; The Law of Skip Connections
;; A skip connection for a block with an input of depth d and an output of depth b requires a bank of shape (list b 1 d) in θ.
(define skip
  (lambda (f j)
    (lambda (t)
      (lambda (theta)
        (+ ((f t) theta)
           (correlate (ref theta j) t))))))

(define skip-block
  (lambda (ba in-d out-d)
    (let ((shape-list (block-ls ba)))
      (block
       (skip (block-fn ba) (length shape-list))  ;; consume the length of shape?
       (append
        shape-list
        (list  ;; new shape for filter bank
         (list out-d 1 in-d)))))))

;; Create residual block and network
(define residual-block
  (lambda (b m d)
    (skip-block
     (fcn-block b m d) d b)))

(define morse-residual
  (stack-blocks
   (list
    (residual-block 4 3 1)
    (residual-block 8 3 4)
    (residual-block 16 3 8)
    (residual-block 26 3 16)
    signal-avg-block)))

(accuracy
 (train-morse morse-residual)
 morse-test-xs morse-test-ys)
