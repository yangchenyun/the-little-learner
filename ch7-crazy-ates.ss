;; Chapter 7 - The Crazy "ates"

#lang racket
(require malt)
(define nabla ∇)

;; Modify `gardient-descent` to be more flexible
(define gradient-descent
  (lambda (obj theta)
    (let ((f (lambda (Theta)  ;; Theta is formal of the revision function
               (map (lambda (p dp) (- p (* alpha dp))) Theta (nabla obj Theta)))))
      (revise f revs theta))))

;; ## The Law of Revisions
;;
;; As long as we make sure that gradient-descent accepts an initial θ and
;; results in a well-fitted θ, any reasonable way of revising it from the first
;; to the last revision is okay.
;;
;; ## Definition
;; singleton: a list with a single number
;; an accompanied parameter: a parameter wrapped in a list

;; Use the "The Law of Revisions", wrap the Theta used within in a list.
;; The the procedure is named "i"nflate and "d"eflate
(define lonely-i
  (lambda (theta)
    (map (lambda (p) (list p)) theta)))

(define lonely-d
  (lambda (Theta)
    (map (lambda (P) (car P)) Theta)))

(define lonely-u
  (lambda (Theta gs)
    (map (lambda (P g)
           (list (- (car P) (* alpha g)))) Theta gs)))

;; Generalize the gradient-descent with the inflate, deflate and update functions
(define gradient-descent
  (lambda (inflate deflate update)
    (lambda (obj theta)
      (let ((f (lambda (Theta) (update Theta (nabla obj (deflate Theta))))))
        (deflate
          (revise f revs (inflate theta)))))))
(define lonely-gradient-descent
  (gradient-descent lonely-i lonely-d lonely-u))

;; Define a helper function test it out some gradient descent method
(define try-plane
  (lambda (a-gradient-descent)
    (with-hypers
      ((revs 15000)
       (alpha 0.001)
       (batch-size 4))
      (a-gradient-descent
       (sampling-obj
        (l2-loss plane) plane-xs plane-ys)
       (list (tensor 0.0 0.0) 0.0)))))
(try-plane lonely-gradient-descent)

;; Let's redefine the original one using the same concepts
;; Notice despite the value didn't change, the name of variale still reflect those concepts.
(define naked-i
  (lambda (theta)
    (map (lambda (p)
           (let ((P p))
             P))
         theta)))

(define naked-d
  (lambda (Theta)
    (map (lambda (P)
           (let ((p P))
             p))
         Theta)))

(define naked-u
  (lambda (Theta gs)
    (map (lambda (P g)
           (- P (* alpha g))) Theta gs)))

;; test it out
(define naked-gradient-descent
  (gradient-descent naked-i naked-d naked-u))

(try-plane naked-gradient-descent)


;; The Law of the Crazy “ates”
;;
;; For any representation, the three “ates” are concerned with only one
;; parameter and its accompaniments, and are not directly concerned with either
;; θ or Θ.
;;
;; Merge the map form of inflate/delfate/update into the gradient-descent
;; itself.
(define gradient-descent
  (lambda (inflate deflate update)
    (lambda (obj theta)
      (let ((f (lambda (Theta) (map update Theta (nabla obj (map deflate Theta))))))
        (map deflate
          (revise f revs (map inflate theta)))))))

;; revised naked gradient descent
(define naked-i (lambda (p) (let ((P p)) P)))
(define naked-d (lambda (P) (let ((p P)) p)))
(define naked-u (lambda (P g) (- P (* alpha g))))
(define naked-gradient-descent
  (gradient-descent naked-i naked-d naked-u))
(try-plane naked-gradient-descent)


;; revised lonely gradient descent
(define lonely-i (lambda (p) (list p)))
(define lonely-d (lambda (P) (car P)))
(define lonely-u (lambda (P g) (list (- (car P) (* alpha g)))))
(define lonely-gradient-descent
  (gradient-descent lonely-i lonely-d lonely-u))
(try-plane lonely-gradient-descent)
