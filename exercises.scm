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

;; Exercise 1.11 ---------------------------------------------------------------

(define (recursive-fn n)
    (if (< n 3)
        n
        (+ (fn (- n 1)) (* 2 (fn (- n 2))) (* 3 (fn (- n 3))))))

(define (iterative-fn n)
    (define (iter a b c count)
        (define val (+ (* 3 a) (* 2 b) c))
        (if (= count n)
            val
            (iter b c val (+ count 1))))
    (iter 0 1 2 3))

;; Exercise 1.12 ---------------------------------------------------------------

(define (pascal row col)
    (cond ((or (= col 1) (= row col)) 1)
          ((or (> col row) (< row 0) (< col 1)) 0)
          (else (+ (pascal (- row 1) (- col 1)) (pascal (- row 1) col)))))

;; Exercise 1.16 ---------------------------------------------------------------

(define (fast-iter-expt base n)
    (define (expt-iter n a b)
        (cond ((= n 0) a)
              ((even? n) (expt-iter (/ n 2) a (square b)))
              (else (expt-iter (- n 1) (* a b) b))))
    (expt-iter n 1 base))

;; Exercise 1.17 ---------------------------------------------------------------

(define (* x y)
    (define (halve x) (/ x 2))
    (define (double x) (+ x x))
    (define (mul-iter x y)
        (cond ((= y 1) x)
              ((even? y) (mul-iter (double x) (halve y)))
              (else (+ x (mul-iter (+ x) (- y 1))))))
    (mul-iter x y))

;; Exercise 1.18 ---------------------------------------------------------------

(define (* x y)
    (define (halve x) (/ x 2))
    (define (double x) (+ x x))
    (define (mul-iter a b c)
        (cond ((= c 0) b)
              ((even? c) (mul-iter (double a) b (halve c)))
              (else (mul-iter a (+ b a) (- c 1)))))
    (mul-iter x 0 y))

