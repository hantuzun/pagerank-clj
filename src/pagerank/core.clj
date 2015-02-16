(ns pagerank.core)

; n is the total number of webpages.
(def n 3)

; m is the directed link graph of n times n
(def m [[1 1 0] [1 0 0] [0 1 1]])

; beta is the damping coefficient
(def beta 0.85)

; epsilon is the value for checking pagerank vector convergence
(def epsilon 1e-3)

(defn preprocess-vector [v]
  (map #(/ % (reduce + v)) v))

(defn preprocess-matrix [g]
  (map preprocess-vector (apply map vector g)))

(defn diff [a b]
  (if (< a b) (- b a) (- a b)))

(defn vector-diff [a b]
  (reduce + (map diff a b)))

(defn double-vector-equal [a b]
  (< (vector-diff a b) epsilon))

(defn initial-pagerank [n]
  (vec (repeat n (/ 1 n))))

(defn pagerank-row [pr row]
  (reduce + (map * pr row)))

(defn emrehan-map [f x coll]
  (let [n (count coll)
        x-coll (repeat n x)]
    (map f x-coll coll)))

; re-inserts the leaked pagerank
(defn normalize-pagerank [r]
  (let [n (count r)]
  (map #(+ % (/ (- 1 (reduce + r)) n)) r)))

(defn transpose [g]
  (vec (apply map vector g)))

(defn make-pagerank-calculator [beta g]
  (fn [pr] (normalize-pagerank (emrehan-map * beta (emrehan-map pagerank-row pr (transpose g))))))

(defn make-stabilized? [epsilon]
  (fn [x-old x] (< (reduce + (map #(Math/abs (double %)) (map - x-old x))) epsilon)))

(defn recur-improve [x-old x improve stabilized?]
  (if (stabilized? x-old x) x
      (recur-improve x (improve x) improve stabilized?)))

(defn pagerank [beta epsilon g]
  (let [n (count g)]
    (recur-improve (vec (repeat n 1)) (initial-pagerank n) (make-pagerank-calculator beta (preprocess-matrix g)) (make-stabilized? epsilon))))
