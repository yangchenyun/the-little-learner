;; Conceptually, gradient is computed with very small delta value.
;; This procedure could be applied function with multiple arguments
;;
;; For a function which produces scalar, define its gradient to be a tensor with
;; the same shape with the scalar value replaced with corresponding gradient.
;;
;; In the base with primitive functions, differential calculus computes the formula.
;; the gradient could be computed from formula in respect to primitive functions.
;;
;; In the recursive case with function composition, the gradient could be
;; computed with multiplicative accumulator with chain rules.
;;
;; There are two orders to walk the chain, the forward mode start with innermost
;; primitive function, this would result in large number of multiplicative
;; accumulators.
;;
;; The reverse mode, accumulate from the outmost function, construct the chain
;; explicitly, but only use a fixed number of multiplicative accumulator.

;; Implementation
;;
;; Primitive operation not only determine numerical results, but also the chain
;; structure. Define a new concept to capture the structural information.
#lang racket
(module system-a racket
  (define dual
    (lambda (r k)
      (vector dual r k)))  ;; the function itself is used as symbol for test

  ;; predicate
  (define dual?
    (lambda (d)
      (cond
        ((vector? d) (eq? (vector-ref d 0) dual))
        (else #f))))
  (dual? (dual 1 null))

  ;; For literal constants read from I/O are all treated as a dual
  ;; Extended the concept of scalar to equal duals
  (define scalar?
    (lambda (d)
      (cond
        ((number? d) #t)
        (else (dual? d)))))

  (scalar? 1.0)
  (scalar? (dual 1.5 null))

  ;; Update accessor to work with scalars (including duals)
  (define rho
    (lambda (d)
      (cond
        ((dual? d) (vector-ref d 1))
        (else d))))

  (define kappa
    (lambda (d)
      (cond
        ((dual? d) (vector-ref d 2))
        (else end-of-chain))))  ;; NOTE: end-of-chain is defined below

  ;; Differentiable functions products a scalar loss with respect to theta
  ;; Simplify the one using the row-wise sum for each batch input
  ;
  ;; Definition
  ;; differentiables: [scalars | list of differentiables | vectors of differentiables]
  ;;
  ;; It is a recursive data structure with leaf carrying the chain information produced them.

  ;; function to produce a new differentiable with leaf scalar being replaced
  (define map*
    (lambda (f y)
      (cond ((scalar? y) (f y))
            ((list? y)
             (map (lambda (lm) (map* f lm))
                  y))
            ((vector? y)
             (vector-map (lambda (ve) (map* f ve))
                         y)))))

  ;; converts scalar to truncated dual, link is always end-of-chain
  ;; When applied on differentiable, it produces truncated differentiable,
  ;; contains only truncated dual at its leaf
  (define dual*
    (lambda (d)
      (dual (rho d) end-of-chain)))

  ;; wrt = with respect to
  (define nabla
    (lambda (f theta)
      (let ((wrt (map* dual* theta)))  ;; restricts the gradient to be determiend
        ;; (f wrt) performs the function on wrt, it produces a differentiable
        ;; whose values are determined in nable-once
        (nabla-once (f wrt) wrt))))

  ;; gradient state keeps track of accumulator for each scalar d in wrt
  (define nabla-once
    (lambda (y wrt)
      (let ((sigma (nabla-sigma y (hasheq))))
        (map* (lambda (d) (hash-ref sigma d 0.0)) wrt)))) ;; this produces the wrt structure but with gradient

  ;; recursive traversal logic, base case is invoked by chain-link
  (define nabla-sigma
    (lambda (y sigma)
      (cond
        ((scalar? y)
         (let ((k (kappa y)))
           ;; The chain fn, takes scalar whose chain would be walked, initial value, gradient state
           (k y 1.0 sigma)))
        ((list? y)
         (nabla-list y sigma))
        ((vector? y)
         (nabla-vector y (sub1 (length y)) sigma)))))

  (define nabla-list
    (lambda (l sigma)
      (cond
        ((null? l) sigma)
        (else
         (let ((sigma-1 (nabla-sigma (car l) sigma)))
           (nabla-list (cdr l) sigma-1))))))

  (define nabla-vector
    (lambda (y i sigma)
      (let ((sigma-1 (nabla-sigma (vector-ref i) sigma)))
        (cond
          ((zero? i) sigma-1)
          (else
           (nabla-vector y (sub1 i) sigma-1))))))

  ;; The leaf chain link to collect the results
  (define end-of-chain
    (lambda (d z sigma)
      (let ((g (hash-ref sigma d 0.0)))  ;; Read out the gradient state, may happen multiple times
        (hash-set sigma d (+ z g)))))    ;; Remember the multiplicative accumulator

  ;; The primitive operator produces a new dual, the signature is [dual, [dual...]] -> dual
  ;; It captures both the real value and accumulate the gradient value walking the chain
  (define +-0-0^  ;; ^: premature
    (lambda (da db)
      (dual (+ (rho da) (rho db))
            (lambda (d z sigma)  ;; d is ignored
              ;; d(a + b)/da = 1.0, d(a + b)/db = 1.0,
              ;; saving both gradient on the accumulator
              (let ((sigma-1 ((kappa da) da (* 1.0 z) sigma)))
                ((kappa db) db (* 1.0 z) sigma-1))))))
  (define exp-0^
    (lambda (da)
      (dual (exp (rho da))
            (lambda (d z sigma)
              ((kappa da) da (* (exp (rho da)) z) sigma)))))

  ;; The primitive operator definition could be generalized
  (define prim1
    ;; one fn operates on real values
    ;; one fn operates on link
    ;; returns a dual -> dual
    (lambda (rho-fn nabla-fn)
      (lambda (da)
        (let ((ra (rho da)))
          (dual (rho-fn ra)
                (lambda (d z sigma)
                  (let ((ga (nabla-fn ra z)))
                    ((kappa da) da ga sigma))))))))

  ;; Do it for binary operator as well
  (define prim2
    (lambda (rho-fn nabla-fn)
      (lambda (da db)
        (let ((ra (rho da))
              (rb (rho db)))
          (dual (rho-fn ra rb)
                (lambda (d z sigma)
                  (let-values ([(ga gb) (nabla-fn ra rb z)])
                    (let ((sigma-1 ((kappa da) da ga sigma)))
                      ((kappa db) db gb sigma-1)))))))))

  ;; Primitive functions for scalars
  (define +-0-0
    (prim2 + (lambda (ra rb z) (values z z))))
  (define --0-0
    (prim2 - (lambda (ra rb z) (values z (- z)))))
  (define *-0-0
    (prim2 * (lambda (ra rb z) (values (* rb z) (* ra z)))))
  (define /-0-0
    (prim2 / (lambda (ra rb z) (values (* (/ 1.0 rb) z)
                                  (* (/ (- ra) (* rb rb)))
                                  ))))
  (define expt-0-0
    (prim2 expt
           (lambda (ra rb z) (values (* (* rb (expt ra (- rb 1))) z)
                                (* (* (expt ra rb) (log ra)) z)))))

  ;; Unary functions
  (define log-0
    (prim1 log
           (lambda (ra z) (* (/ 1 ra) z))))
  (define exp-0
    (prim1 exp
           (lambda (ra z) (* (exp ra) z))))
  (define sqrt-0
    (prim1 (lambda (ra) (expt ra 1/2))
           (lambda (ra z) (/ z (* 2 (sqrt ra))))))

  ;; Special treatment on disjoint function abs
  (define abs-0-ρ
    (λ (x)
      (cond
        ((< x 0) (* -1 x))
        (else x))))

  (define abs-0-∇
    (λ (x z)
      (cond
        ((< x 0) (- z))
        (else z))))

  (define abs-0
    (prim1 abs-0-ρ abs-0-∇))

  ;; Convenient macros for testing
  (define-syntax-rule (check-binary-fn f theta gradients)
    (check equal?
           gradients
           (nabla (lambda (x) (f (car x) (car (cdr x)))) theta)))

  (define-syntax-rule (check-unary-fn f theta gradients)
    (check equal?
           gradients
           (nabla (lambda (x) (f (car x))) theta)))

  ;; testing for both primitive cases and composed cases
  (require rackunit)
  (let ((a 2) (b 3))
    (check-binary-fn +-0-0 (list a b) (list 1.0 1.0))
    (check-binary-fn --0-0 (list a b) (list 1.0 -1.0))
    (check-binary-fn *-0-0 (list a b) (list 3.0 2.0))
    (check-binary-fn /-0-0 (list a b) (list 0.3333333333333333 -0.2222222222222222))
    (check-binary-fn expt-0-0 (list a b) (list 12.0 5.545177444479562))

    (check-unary-fn exp-0  (list a) (list (exp 2.0)))
    (check-unary-fn log-0  (list a) (list 0.5))
    (check-unary-fn sqrt-0 (list a) (list 0.35355339059327373)))
  ;; check abs-0
  (check-unary-fn abs-0 (list -1) (list -1.0))
  (check-unary-fn abs-0 (list 3) (list 1.0))
  ;; check compsosed function
  (let ((z (lambda (x y)
             (+-0-0 (log-0 x) (*-0-0 x y)))
           ))
    (check-binary-fn z (list 2 2) (list 2.5 2.0))
    (check-binary-fn z (list 3 3) (list 3.3333333333333335 3.0))
    (check-binary-fn z (list 4 4) (list 4.25 4.0)))

  ;; expose for external usage
   (provide
    +-0-0 --0-0 *-0-0 /-0-0 expt-0-0
    exp-0 log-0 abs-0 sqrt-0

    nabla))
