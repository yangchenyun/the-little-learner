;; # Interlude V - Extensio Magnifico
#lang racket
(require malt/interlude-V)


;; ## Reference
;;
;; David Berglas, British magician and mantalist, one of the first magicians to
;; appear on UK television.

;; A pre-mature implementation is covered in interlude I, here we walk through them again.

;; built based on tmap
(tmap + (tensor 3 4 6 1) (tensor 1 3 5 5))

;; define sqrt
(define sqrt
  (lambda (t)
    (cond ((scalar? t) (sqrt-0 t))
          (else (tmap sqrt t)))))
(sqrt (tensor 4 9 16))

;; generalize the scalar to test on the rank
(define of-rank?
  (lambda (n t)
    (cond ((zero? n) (scalar? t))
          ((scalar? t) #f)
          (else
           (of-rank? (sub1 n) (tref t 0))))))
(of-rank? 3 (tensor (tensor (tensor 8) (tensor 9)) (tensor 4) (tensor 9)))
(of-rank? 2 #(#(8) #(9)))

;; Use of-rank? in sqrt
(define sqrt
  (lambda (t)
    (cond ((of-rank? 0 t) (sqrt-0 t))
          (else (tmap sqrt t)))))
(sqrt (tensor 4 9 16))

;; Generalize it for unary operator
(define ext1
  (lambda (f n)
    (lambda (t)
      (cond ((of-rank? n t) (f t))
            (else (tmap (ext1 f n) t))))))
(define sqrt (ext1 sqrt-0 0))
(sqrt (tensor 4 9 16))

;; define zeroes as a functino
(define zeroes (ext1 (lambda (x) 0.0) 0))
(zeroes (tensor 4 9 16))

;; works on sum, with rank-1 base function
(define sum (ext1 (lambda (t-1) (sum-1 t-1)) 1))
(sum (tensor 4 9 16))

;; example of base function with rank2
(define flatten-2
  (lambda (t-2)
    (apply vector-append (vector->list t-2))))
(flatten-2 #(#(1.0 0.5) #(3.1 2.2) #(7.3 2.1)))

;; define general flatten
(define flatten (ext1 flatten-2 2))

(flatten
 #(#(#(1.0 0.5) #(3.1 2.2) #(7.3 2.1))
   #(#(2.9 3.5) #(0.7 1.5) #(2.5 6.4))))

;; rank comparison function
(define rank>
  (lambda (t u)
    (cond
      ((scalar? t) #f)
      ((scalar? u) #t)
      (else
       (rank> (tref t 0) (tref u 0))))))

;; binary rank checks
(define of-ranks?
  (lambda (n t m u)
    (and (of-rank? n t) (of-rank? m u))))

;; now ready to define the ext2
;; the descending logic depends on the two ranks
(define ext2
  (lambda (f n m)
    (lambda (t u)
      (cond ((of-ranks? n t m u) (f t u))
            (else
             (desc (ext2 f n m) n t m u))))))

;; descending logic
(define desc
  (lambda (g n t m u)
    (cond
      ((of-rank? n t) (desc-u g t u))
      ((of-rank? m u) (desc-t g t u))
      ((= (rank t) (rank u))
       (if (= (tlen t) (tlen u))
           (tmap g t u)
           (error 'ext2
              "Shapes are incompatible for ext2: ~a and ~a for min ranks ~a and ~a~%"
              (shape t) (shape u) n m)))
      ((rank> t u) (desc-t g t u))
      ((rank> u t) (desc-u g t u)))))
(define desc-t
  (lambda (g t u)
    (tmap (lambda (et) (g et u)) t)))
(define desc-u
  (lambda (g t u)
    (tmap (lambda (eu) (g t eu)) u)))

;; use it to define +/*
(define +
  (ext2 +-0-0 0 0))
(+ #(#(1.0 0.5) #(3.1 2.2))
   #(#(2.0 1.8) #(5.1 7.2)))
(define *
  (ext2 *-0-0 0 0))
(* #(#(1.0 0.5) #(3.1 2.2))
   #(#(2.0 1.8) #(5.1 7.2)))

;; also the dot product
(define dot
  (ext2 dot-product-1-1 1 1))
(dot #(1.0) #(1.0))
(dot #(#(1.0 0.5) #(3.1 2.2))
   #(#(2.0 1.8) #(5.1 7.2)))

;; Use it to change the behavior for base functions on different ranks
(define *-2-1
  (ext2 * 2 1))

;; behaves the same
(let
    ((p #(#(3 4 5) #(7 8 9)))
     (t #(2 4 3)))
    (* p t))
(let
    ((p #(#(3 4 5) #(7 8 9)))
     (t #(2 4 3)))
    (*-2-1 p t))

;; different behavior between * and *-2-1
(let
    ((p #(#(8 1) #(7 3) #(5 4)))
     (t #(#(6 2) #(4 9) #(3 8))))
    (* p t))
(let
    ((p #(#(8 1) #(7 3) #(5 4)))
     (t #(#(6 2) #(4 9) #(3 8))))
    (*-2-1 p t))
