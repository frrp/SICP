;; Section 1.1 exercises of SICP book, 2nd ed

(ns sicp.sec1.1)

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

;; Section 1.1.7 Example: Square Roots by Newton’s Method

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

;; Exercise 1.7

