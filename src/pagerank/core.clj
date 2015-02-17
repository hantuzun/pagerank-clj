(ns pagerank.core)

; normalize a vector such that the sum of the vector is 1
(defn preprocess-vector [v]
  (map #(/ % (reduce + v)) v))

; normalize a matrix such that every column sums to 1
(defn preprocess-matrix [g]
  (map preprocess-vector (apply map vector g)))

; returns the difference of two values
(defn diff [a b]
  (if (< a b) (- b a) (- a b)))

; returns whether if fractions or floats are almost equal
(defn almost-equal [x y]
  (<= (diff x y) 0.00000001))

; returns the sum of all elements' differences of two vectors
(defn vector-diff [a b]
  (reduce + (map diff a b)))

; returns whether if two vectors are almost equal
(defn vector-almost-equal [a b epsilon]
  (< (vector-diff a b) epsilon))

; initialize pagerank of a graph of n elements such that all elements are 1/n
(defn initial-pagerank [n]
  (vec (repeat n (/ 1 n))))

; calculates the pagerank of a page by multiplying its inlinks and their pageranks
(defn pagerank-row [pr row]
  (reduce + (map * pr row)))

; applies map to an element and a vector where the element is used for every item in the vector
(defn emrehan-map [f x coll]
  (let [n (count coll)
        x-coll (repeat n x)]
    (map f x-coll coll)))

; re-inserts the leaked pagerank such that it sums up to 1
(defn normalize-pagerank [r]
  (let [n (count r)]
  (map #(+ % (/ (- 1 (reduce + r)) n)) r)))

; transpose the matrix
(defn transpose [g]
  (vec (apply map vector g)))

; returns a fuction that that takes a pagerank vector and returns
(defn make-pagerank-calculator [beta g]
  (fn [row] (normalize-pagerank (emrehan-map * beta (emrehan-map pagerank-row row (transpose g))))))

; returns a function with determined sensitivity that checks whether if two vectors are almost equal
(defn make-stabilized? [epsilon]
  (fn [x-old x] (vector-almost-equal x-old x epsilon)))

; calls the improve function on x until the difference between x among iterations is less than the stabilized? function
(defn recur-improve [x-old x improve stabilized?]
  (if (stabilized? x-old x) x
      (recur-improve x (improve x) improve stabilized?)))

; calculates the pagerank using recur-improve
(defn pagerank [beta epsilon g]
  (let [n (count g)]
    (recur-improve (repeat n 1) (initial-pagerank n) (make-pagerank-calculator beta (preprocess-matrix g)) (make-stabilized? epsilon))))
