;; Section 1.2 exercises of SICP book, 2nd ed
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

(factorial-iter 7)


;; Exercise 1.9

;; recursive
(defn add-rec [a b]
  (if (= a 0)
    b
    (inc (new+ (dec a) b))))
(add-rec 5 2)

;; iterative
(defn add-iter [a b]
  (if (= a 0)
    b
    (newx+ (dec a) (inc b))))

(add-iter 2 3)


;; 1.2.2  Tree Recursion

;; Exercise 1.11

