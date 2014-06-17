;; chapter 1 exercises of SICP book

(defn r [] (require :reload 'chap1))

(def size 2)
(def pi 3.14159)
(def radius 10)

(defn circumference [] (* 2 pi radius))
(defn area [] (* pi (* radius radius)))

(defn square [x] (* x x))

(defn sum-of-squares [x y]
  (+ (square x) (square y)))

(defn f [a] (sum-of-squares (+ a 1) (* a 2)))

(defn abs [x]
  (if (< x 0) (- x)
      x))

;; Exercise 1.3

(defn sum-squares-of-largests [a b c]
  (let [[x y z] (sort [a b c])]
       (sum-of-squares y z) ))

;; Exercise 1.5

(defn p [] (p))
(defn test1 [x y]
  (if (= x 0)
    0
    y))

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

(defn sqrt [x]
  (letfn
   [(my-good-enough? [guess]  (< (abs (- (square guess) x)) 0.001))
    (my-improve [guess]       (my-average guess (/ x guess)))
    (my-average [x y]         (/ (+ x y) 2.0))]
   (loop [guess 1 iters 0]
     (cond (my-good-enough? guess) guess
           (> iters 10000) guess
           :else (recur (my-improve guess) (inc iters)) ))))


