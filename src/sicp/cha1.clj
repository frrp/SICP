;; Code and exercises from the SICP book, 2nd ed


;; Chapter 1
;; Building Abstractions with Procedures

(ns sicp.cha1)

;; macro for benchmarking from the book Programming Clojure
;; get back elapsed time in nanoseconds
(defmacro bench [expr]
  `(let [start# (System/nanoTime)
	 result# ~expr]
     {:result result#
      :elapsed (- (System/nanoTime) start#)}))

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


(defn fib-iter [a b count]
  (if (zero? 0)
    b
    (fib-iter (+ a b) a (- count 1))))

(defn fib [n]
  (fib-iter 1 0 n))

(fib 3)


;; counting change
(defn count-change [amount coin-values]
  (cond (= amount 0) 1
        (or (< amount 0) (empty? coin-values)) 0
        :else (+ (count-change amount (rest coin-values)) (count-change (- amount (first coin-values)) coin-values))
        ))

(count-change 5 [1 2])

;; Exercise 1.11

;; recursive function F
(defn F [n]
    (cond (< n 3) n
          :else (+ (F (- n 1))
                    (* 2 (F (- n 2)))
                    (* 3 (F (- n 3))))))

;; iterative processes to compute F
(defn F-iter [n]
  (loop [acc 2 b 1 c 0 count n]
    (if (= count 2)
      acc
      (recur (+ acc (* 2 b) (* 3 c)) acc b (- count 1)))))

(= 59 (F-iter 6) (F 6))

;; Exercise 1.12
;; binomial coefficients, iterative version

(defn bc [row col]
  (loop [r row c col acc 1]
    (if (or (= c 0) (= r 0) (= r c))
      acc
      (recur (dec r) (dec c) (/ (* acc r) c)))))

(bc 6 2)
(bc 5 2)

;; Exercise 1.13
;; Prove that (= fib(n) (closest_integer (/ (exp GR n) (sqrt 5)))) ,
;; where GR is the golden ratio

;; 1.2.3  Orders of Growth

;; Exercise 1.14
;; space is O(n), time is O(poly)

;; Exercise 1.15

(defn cube [x]
  (* x x x))

(defn p [x]
  (-  (* 3 x)
      (* 4 (cube x))))

(defn sine [angle]
  (if (not (> (abs angle) 0.1))
    angle
    (p (sine (/ angle 3.0)))))

(sine 12.15)

;; p is applied 5 times for computing (sine 12.15)
;; runtime of sine function is O(log_3 (n))
;; space is O(log n) - linear recursion


;; 1.2.4  Exponentiation

;; Recursive version
(defn fast-expt [b n]
  (cond (zero? n) 1
        (even? n) (square (fast-expt b (/ n 2)))
        :else (* b (fast-expt b (- n 1)))))

(fast-expt 2 30)

;; Exercise 1.16
;; iterative exponentiation process for expt
(defn fast-expt-iter [b n]
  (loop [bb b cnt n acc 1]
    (cond (zero? cnt) acc
          (even? cnt) (recur (square bb) (/ cnt 2) acc)
          :else (recur bb (dec cnt) (* bb acc)))
      ))

(fast-expt-iter 2 7)

;; recursive multiplication
(defn mult [a b]
  (if (= b 1) a
      (+ a (mult a (dec b))) ))

;; Exercise 1.17

(defn double [x]
  (* x 2))

(defn halve [x]
  (/ x 2))

(defn fast-mult [a b]
  (cond (zero? b) 0
        (= b 1) a
        (even? b) (double (fast-mult a (halve b)))
        :else (+ a (fast-mult a (dec b)))))

(fast-mult 4 6)

;; Exercise 1.18

(defn fast-mult-iter [i j]
   (loop [a i b j acc 0]
     (cond (zero? b) acc
         (even? b) (recur (double a) (halve b) acc)
         :else (recur a (dec b) (+ acc a)) )))

(fast-mult-iter 5 4)

;; Exercise 1.19

(defn ffib-iter-a [a b p q count]
  (cond (zero? count) b
        (even? count)
          (ffib-iter-a a
                       b
                       (+ (* p p) (* q q))     ; p'
                       (+ (* 2 p q) (* q q))   ; q'
                       (/ count 2))
        :else (ffib-iter-a (+ (* b q) (* a q) (* a p))
                              (+ (* b p) (* a q))
                              p
                              q
                              (dec count)))
  )

(defn ffib-iter [n]
  (ffib-iter-a 1 0 0 1 n))

(= 55 (ffib-iter 10))


;; 1.2.5  Greatest Common Divisors

;; Exercise 1.20

(comment
 (gcd 206 40))

;; The number of remainder operations in (gcd 206 40) when
;; the rules used by the interpreter are:
;; * applicative order: 4
;; * normal order: 18

;; 1.2.6  Example: Testing for Primality

(defn find-divisor [n test-divisor]
  (cond (> (square test-divisor) n) n
        (divides? test-divisor n) test-divisor
        :else (find-divisor n (+ test-divisor 1))))

(defn smallest-divisor [n]
  (find-divisor n 2))

(defn divides? [a b]
  (= (rem b a) 0))


(defn expmod [base exponent m]
  (cond (= exponent 0) 1
        (even? exponent) (rem (square (expmod base (/ exponent 2) m))
                m)
        :else (rem  (* base (expmod base (- exponent 1) m))
                m)))

(defn fermat-test [n]
  (defn try-it [a]
    (= (expmod a n n) a))
  (try-it (+ 1 (rand (- n 1)))))

(defn fast-prime? [n times]
  (cond (= times 0) true
        (fermat-test n) (fast-prime? n (- times 1))
        :else false))

(fast-prime? 19999 10)

;; Exercise 1.21

(smallest-divisor 199)

(smallest-divisor 1999)

(= 7 (smallest-divisor 19999))


;; Exercise 1.22
;; is the order of growth of testing for primes really Θ(√n) ?
;; yes, roughly

;; timed prime
;; returns {:elapsed x, :result y, :num z}
(defn prime? [n]
  (conj {:num n}
  (bench (= n (smallest-divisor n)))))

(prime? 1999)

;; prime numbers generator
(defn prime-seq [n]
  (map #(list (:num %) (:elapsed %))
       (filter #(true? (:result %))
	       (map #(prime? (+ % n)) (iterate inc 2)))))

;; finds the first 3 primes larger than n
(defn search-for-primes [n]
  (let [x (if (even? n) (inc n) n)]
    (map #(nth (prime-seq x) %) [0 1 2])))

(search-for-primes 1000)
(search-for-primes 10000)
(search-for-primes 100000)
(search-for-primes 1000000)
(search-for-primes 10000000)

;; Exercise 1.23
;; taking only the odd half of the divisors does speed up
;; the primarility test roughly by a factor of two

(defn next1 [n]
  ( if (= n 2) 3 (+ n 2)))

(defn find-divisor1 [n test-divisor]
  (cond (> (square test-divisor) n) n
        (divides? test-divisor n) test-divisor
        :else (find-divisor1 n (next1 test-divisor))))

(defn smallest-divisor1 [n]
  (find-divisor1 n 2))

(defn prime?1 [n]
  (conj {:num n}
  (bench (= n (smallest-divisor1 n)))))

(defn prime-seq1 [n]
  (map #(list (:num %) (:elapsed %))
       (filter #(true? (:result %))
	       (map #(prime?1 (+ % n)) (iterate inc 2)))))

(defn search-for-primes1 [n]
  (let [x (if (even? n) (inc n) n)]
    (map #(nth (prime-seq1 x) %) [0 1 2])))

(search-for-primes1 1000)
(search-for-primes1 10000)
(search-for-primes1 100000)
(search-for-primes1 1000000)
(search-for-primes1 10000000)

;; Exercise 1.24
;; replacing the primarility test with Fermat's test of growth Θ(log(n))
;; so far it takes longer, but the complexity should grow a bit slower...

;; using fermat-test, repeating 3 times
(defn prime?-f [n]
  (conj {:num n}
 	(bench (fast-prime? n 3))))

;(prime?-f 1999)

(defn prime-seq-f [n]
  (map #(list (:num %) (:elapsed %))
       (filter #(true? (:result %))
	       (map #(prime?-f (+ % n)) (iterate inc 2)))))

(defn search-for-primes-f [n]
  (let [x (if (even? n) (inc n) n)]
    (map #(nth (prime-seq-f x) %) [0 1 2])))

(search-for-primes-f 1000)
(search-for-primes-f 10000)
;(search-for-primes-f 100000)
;(search-for-primes-f 1000000)
;(search-for-primes-f 10000000)

;; Exercise 1.25


;; 1.3  Formulating Abstractions with Higher-Order Procedures


