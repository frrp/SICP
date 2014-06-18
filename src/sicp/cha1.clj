;; Code and exercises from SICP book, 2nd ed


;; Chapter 1
;; Building Abstractions with Procedures

(ns sicp.cha1)


;; 1.1  The Elements of Programming

(def size 2)
(def pi 3.14159)
(def radius 10)

(defn circumference [] (* 2 pi radius))
(defn area [] (* pi (* radius radius)))
(defn square [x] (* x x))

(defn sum-of-squares [x y]
  (+ (square x) (square y)))

(defn f [a] (sum-of-squares (+ a 1) (* a 2)))
;;(f 5)

(defn abs [x]
  (if (< x 0) (- x)
      x))

;; Exercise 1.3

(defn sum-squares-of-largests [a b c]
  (let [[x y z] (sort [a b c])]
       (sum-of-squares y z) ))

;; Exercise 1.4
;; compound expressions

(defn a-plus-abs-b [a b]
  ((if (> b 0) + -) a b))
;;(a-plus-abs-b 1 -5)

;; Exercise 1.5
;; normal-order evaluation vs. applicative-order evaluation.
;; the definition of p is invalid - calling itself recursively
;; Clojure uses applicative-order evaluation, same as Scheme
;; othervise the p would not be called in this case

(defn p [] (p))
(defn test [x y]
  (if (= x 0)
    0
    y))
;;(test 0 (p))

;; 1.1.7 Example: Square Roots by Newton’s Method

(defn average [x y]
  (/ (+ x y) 2.0) )

(defn improve [guess x]
  (average guess (/ x guess)) )

(defn good-enough? [guess x]
  (< (abs (- (square guess) x)) 0.001) )

(defn sqrt-iter [guess x]
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)) )

(defn sqrt1 [x]
  (sqrt-iter 1 x))

(defn sqrt2 [x]
  (loop [guess 1 iters 0]
    (cond (good-enough? guess x) guess
          (> iters 10000) guess
          :else (recur (improve guess x) (inc iters)) )))

;; Exercise 1.6
;; applicative-order evaluation in new-if causes an infinite loop when used in sqrt-iter

(defn new-if [predicate then-clause else-clause]
  (cond (predicate then-clause)
        (else-clause)))

;;(println-str "new-if")
;;(new-if (= 2 3) 1 0)
;;Java Exception: java.lang.Boolean cannot be cast to clojure.lang.IFn

;;(println-str "if")
;;(if (= 2 3) 1 0)


;; Exercise 1.7 - improving sqrt for small and large numbers

(defn new-good-enough? [guess x]
  (< (abs (- (square guess) x)) 0.001))

(defn new-improve [guess x]
  (average guess (/ x guess)))

(defn new-sqrt-iter [guess x]
    (loop [guess 1 iters 0]
     (cond (new-good-enough? guess x) guess
           (> iters 10000) guess
           :else (recur (new-improve guess x) (inc iters)) )))

(defn sqrt [x]
  (new-sqrt-iter 1.0 x))

;; original functions are failing
(sqrt1 0.00000000005)
;;(sqrt1 99990000000000000000000)
;; with stack overflow....

;; but the new one works (almost)
(sqrt 0.000000025) ;; this does not work yet !!
(sqrt 99990000000000000000000)

;; Exercise 1.8
;; much slower but more precise

(defn better-improve [guess x]
  (/ (+ (/ x (square guess))
        (* 2 guess))
     3))




;; 1.2  Procedures and the Processes They Generate


;; 1.2.1  Linear Recursion and Iteration

;; recursive factorial
(defn factorial-rec [n]
  (if (= n 1)
    1
    (* n (factorial-rec (dec n)))))

(factorial-rec 6)

;; iterative factorial
;; we have to tell the compiler to use **tail call optimization**
(defn factorial-iter-a [product counter max-count]
  (if (> counter max-count)
    product
    (factorial-iter-a (* counter product)
                      (+ counter 1)
                      max-count)))
(defn factorial-iter [n]
  (factorial-iter-a 1 1 n))

;; in Clojure, to benefit from tail call optimization we will use the 'recur' form
(defn factorial [n]
     (loop [cnt n acc 1]
       (if (zero? cnt)
            acc
          (recur (dec cnt) (* acc cnt)))))

(factorial-iter 7)
(factorial 15)

;; Exercise 1.9

;; recursive
(defn add-rec [a b]
  (if (= a 0)
    b
    (inc (add-rec (dec a) b))))

(add-rec 5 2)

;; iterative (but not in Clojure)
(defn add-iter [a b]
  (if (= a 0)
    b
    (add-iter (dec a) (inc b))))

;; iterative (in Clojure)
(defn add-iter1 [a b]
  (loop [cnt a acc b]
    (if (zero? cnt)
    acc
    (recur (dec cnt) (inc acc)))))

(= (add-iter 4 3) (add-iter1 4 3) )

;; Exercise 1.10
;; Ackermann’s function
(defn A [x y]
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (:else (A (- x 1)
                 (A x (- y 1))))))


;; 1.2.2  Tree Recursion

;; Exercise 1.11

;; recursive
(defn F [n]
    (cond (< n 3) n
          :else (+ (F (- n 1))
                    (* 2 (F (- n 2)))
                    (* 3 (F (- n 3))))))

;; iterative processes to compute F:
(defn F-iter [n]
  (loop [acc 2 b 1 c 0 count n]
    (if (= count 2)
      acc
      (recur (+ acc (* 2 b) (* 3 c)) acc b (- count 1)))))

(= 59 (F-iter 6) (F 6))

;; 1.2.3  Orders of Growth

;; 1.2.4  Exponentiation

;; 1.2.5  Greatest Common Divisors

;; 1.2.6  Example: Testing for Primality


;; 1.3  Formulating Abstractions with Higher-Order Procedures
