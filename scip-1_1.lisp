zsh:1: permission denied: scmindent.rkt
;; 1.2

(/ (+ 5
      4
      (- 2
         (- 3
            (+ 6
               (/ 4 5)))))
   (* 3
      (- 6 2)
      (- 2 7)))

;; 1.3

(define (square x) (* x x))

(define (sum-of-squares x y) (+ (square x) (square y)))

(define (max x y) (if (x > y) x))

(define (min x y) (if (x < y) x))

(define (largest-sum-of-squares x y z)
  (sum-of-squares (max x y) (max (min x y) z)))

;; 1.5

(define (p) p)

(define (test x y)
  (if (= x 0)
    0
    y))

;;; applicative order

(test 0 (p))

; -> does not return anything as p assignment loops infinitely

;;; normal order

(if (= 0 0) 0 p)

; -> returns 0

;; 1.6

(define (square x) (* x x))

(define (abs x) (if (< x 0) (- x) x))

(define  (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter  (improve guess x) x)))

(define  (improve guess x)
    (average guess  (/ x guess)))

(define  (average x y)
    (/  (+ x y) 2))

(define  (good-enough? guess x)
    (<  (abs  (-  (square guess) x)) 0.001))

(define  (sqrt x)
    (sqrt-iter 1.0 x))

; The function does not return anything. The interpreter evaluates
; each statement of a conditional whereas only one argument gets
; evaluated after the predicate resolves in an if statement

;; 1.7

; Statement: The good-enough? test used in computing square roots
; wiill not be very effective for finding the square roots of
; very small numbers
; Reason: The limit of the good-enough equation as x tends towards
; 0 is the square root of 0.001. Therefore for very small
; numbers the sqrt-iter function approximates the root of 0.001
; as the recursive iterations will stop once this is reached

(square (sqrt 0.002))
-> 0.002513152551653305

(square (sqrt 0.0009))
0.0016241401856992538

(square (sqrt 0.0001))
0.0010438358335233748

(square (sqrt 0.00001))
0.0009832294718753643

; Statement: in real computers, arithmetic operations are almost
; always performed with limited precision. This makes our test
; inadequate for very large numbers.
; Reason:
;


;; Statement: Design a square-root procedure that uses this kind of end test

(define  (sqrt-iter guess x)
    (if (good-enough? guess (improve guess x))
        guess
        (sqrt-iter  (improve guess x) x)))

(define  (good-enough? guess improvement)
    (<  (abs (/ (- improvement guess) guess)) 0.001))

;; 1.8

(define (improve-cube guess x)
  (/ (+ (/ x (square guess))
        (* 2 guess))
     3))

(define (cubert-iter guess x)
   (if (good-enough? guess (improve-cube guess x))
     guess
     (cubert-iter (improve-cube guess x) x)))

(define (cubert x)
  (cubert-iter 1.0 x))