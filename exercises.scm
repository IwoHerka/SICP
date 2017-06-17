;; Exercise 1.3 ----------------------------------------------------------------

(define (square x) (* x x))

(define (sum-of-squares x y) (+ (square x) (square y)))

(define (max-sum-of-squares x y z)
  (cond ((and (< x y) (< x z)) (sum-of-squares y z))
        ((and (< y x) (< y z)) (sum-of-squares x z))
        (else (sum-of-squares x y))))


;; Exercise 1.7 ----------------------------------------------------------------

(define (sqrt x precision)
  (define (enough? new old)
    (< (abs (- new old)) precision))
  (define (avg x y)
    (/ (+ x y) 2))
  (define (improve guess)
    (avg guess (/ x guess)))
  (define (sqrt-iter guess prev-guess)
    (if (enough? guess prev-guess)
         guess
         (sqrt-iter (improve guess) guess)))
  (sqrt-iter 1.0 0.0))

;; Exercise 1.8 ----------------------------------------------------------------

(define (sqcb x precision)
  (define (enough? new old)
    (< (abs (- new old)) precision))
  (define (improve guess)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))
  (define (sqcb-iter guess prev-guess)
    (if (enough? guess prev-guess)
        guess
        (sqcb-iter (improve guess) guess)))
  (sqcb-iter 1.0 0.0))
    
