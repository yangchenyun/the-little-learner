;; Chapter 9 - Smooth Operator
;;
;; ## Reference
;;
;; Smooth Operator is a featured song in the first song is Sade's debut album,
;; "Diamond Life" (1984). Sade is a English band features features elements of
;; soul, quiet storm, smooth jazz and sophisti-pop. Still active.

;; Smooth definition
(define smooth
  (lambda (decay-rate average g)
    (+ (* decay-rate average)
       (* (- 1.0 decay-rate) g))))

;; It blends two tensors
(smooth 0.9 0.0 50.3)
(smooth 0.9 (smooth 0.9 0.0 50.3)
        22.7)
(smooth 0.9
        (smooth 0.9 (smooth 0.9 0.0 50.3) 22.7)
        4.3)

;; decay-rate is the rate of contribution
;; it represents the contribution from historical information

;; smooth is an extended operation working on matrix
(define smooth-reduce
  (lambda (decay-rate l a)
    (cond
      ((null? l) a)
      (else
       (smooth-reduce decay-rate (cdr l)
                      (smooth decay-rate a (car l)))))))
(smooth-reduce 0.9
               (list
                 [tensor 1.0 1.1 3.0]
                 [tensor 13.4 18.2 41.4]
                 [tensor 1.1 0.3 67.3])
               (tensor 0.8 3.1 2.2))
