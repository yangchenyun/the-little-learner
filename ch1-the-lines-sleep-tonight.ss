;;; First attempt to define line lambda

; Concepts:
; - arguments
; - parameters, determine the function output given arguments
; - parameterized functions
;   Used for figuring out values of parameters before returning y
;   figure out result from "possibilile values"

; In the following case:
; x is known prior to w and b, x is an argument; w and b are parameters
(define line 
    (lambda (x)
        (lambda (w b) (+ b (* x w)))))

((line 8) 4 6)

;; data set
(define line-xs '(2.0 1.0 4.0 3.0))
(define line-ys '(1.8 1.2 4.2 3.3))

;; Make plot on graph (Only works in REPL)
;;
;; (require plot)
;; (plot-new-window? #t)
;; (parameterize ([plot-width    150]
;;                  [plot-height   150]
;;                  [plot-x-label  #f]
;;                  [plot-y-label  #f])
;;     (list (plot (points (map vector line-xs line-ys)))))

;; Concepts:
;; - learning. Finding parameters of a function from a dataset
;; - parameter sets

;; Rewrite y = wx + b with parameter sets
(define first car)
(define second (λ (l) (car (cdr l))))
(define line
    (lambda (x)
        (lambda (θ) (+ (second θ) (* x (first θ))))))
        
((line 7.3) (list 1.0 0.0))

;; The Rule of Parameters (v1)
;; - Every parameter is a number
