(ns pagerank.core_test
  (:require [clojure.test :refer :all]
            [pagerank.core :refer :all]))

(defn double= [x y]
  (<= (Math/abs (- (double x) (double y))) 0.0001))

(defn double-coll= [x-coll y-coll]
  (every? true? (map double= x-coll y-coll)))

(deftest double=-test-1
  (testing "double= for doubles"
    (is (true? (double= 1 0.99999999)))))

(deftest double=-test-2
  (testing "double= for fractions"
    (is (true? (double= 1/3 0.3333)))
    (is (true? (double= 1/3 3/9)))))

(deftest double-coll=-test-1
  (testing "double-coll= on vectors"
    (is (true? (double-coll= [1 1/3 0.421] [1 1/3 0.421])))))

(deftest double-coll=-test-2
  (testing "double-coll= on lists"
    (is (true? (double-coll= '(1 1/3 0.421) '(1 1/3 0.421))))))

;  (testing "double-coll= on a list and a vector"

(deftest base-test
  (testing "base test."
    (is (= 1 1))))

(deftest diff-test-1
  (testing "diff 1 2."
    (is (= 1 (diff 1 2)))))

(deftest diff-test-2
  (testing "diff 2 1."
    (is (= 1 (diff 2 1)))))

(deftest diff-test-3
  (testing "diff 2.3 1."
    (is (= 1.5 (diff 2.5 1)))))

(deftest vector-diff-test-1
  (testing "diff [2 1] [1 2]."
    (is (= 2 (vector-diff [2 1] [1 2])))))

(deftest vector-diff-test-2
  (testing "diff [2.5 1] [1 2.5]."
    (is (= 3.0 (vector-diff [2.5 1] [1 2.5])))))

(deftest double-vector-equal-test-1
  (testing "the same vectors."
    (is (true? (double-vector-equal [1 3.342 543.99] [1 3.342 543.99])))))

(deftest double-vector-equal-test-2
  (testing "vectors with an element of 1e-2 diff."
    (is (false? (double-vector-equal [1 3.342 544] [1 3.342 543.99])))))

(deftest double-vector-equal-test-3
  (testing "vectors of multiple elements with diff 1e-7."
    (is (true? (double-vector-equal [1.0000001 3.3420001 543.99] [1 3.342 543.99])))))

(deftest initial-pagerank-test
  (testing "initial pagerank of 3 elements."
    (is (= [1/3 1/3 1/3] (initial-pagerank 3)))))

(deftest normalize-pagerank-test-1
  (testing "a vector sums up to 1."
    (is (= [1/3 1/3 1/3] (normalize-pagerank [1/3 1/3 1/3])))))

(deftest normalize-pagerank-test-2
  (testing "a vector sums up to less than 1."
    (is (= [17/60 17/60 13/30] (normalize-pagerank [1/10 1/10 1/4])))))

(deftest preprocess-vector-test
  (testing "converting a link vector to link weight vector."
    (is (= [1/6 0 1/3 1/2] (preprocess-vector [1 0 2 3])))))

(deftest preprocess-matrix-test
  (testing "converting a link matrix to link weight matrix."
    (is (= [[1 0][5/8 3/8]] (preprocess-matrix [[3 5][0 3]])))))

(deftest emrehan-map-test
  (testing "emrehan-map applies map f to the first parameter to every element of the second parameter."
    (is (= [0 2 4] (emrehan-map * 2 [0 1 2])))))

(deftest emrehan-map-test-2
  (testing "emrehan-map for vectors."
    (is (= [[5 8 11] [10 21 42]] (emrehan-map #(map + %1 %2) [0 1 2] [[5 7 9] [10 20 40]])))))

(deftest pagerank-row-test
  (testing "calculating pagerank of an element."
    (is (double= 0.15 (pagerank-row [0.1 0.5 0.2] [0.3 0.2 0.1])))))

(deftest make-pagerank-calculator-test
  (testing "make-pagerank-calculator."
    (is (double= (first ((make-pagerank-calculator 0.85 [[1]]) [1])) 1))))

(deftest make-stabilized?-test-1
  (testing "epsilon is 1."
    (is ((make-stabilized? 1) [0.1 0.2 0.4] [0.2 0.1 0.1]))
    (is not ((make-stabilized? 0.1) [0.1 0.2 0.4] [0.2 0.1 0.1]))))

(deftest recur-improve-test
  (testing "dividing 1024 to 2 until the diff is less than 10."
    (defn smalldiff? [x y e] (<= (Math/abs (- x y)) e))
    (defn smalldiff?-maker [e] #(smalldiff? %1 %2 e))
    (defn half [x] (double (/ x 2)))
    (is (double= 4.0 (recur-improve 0 1024 half (smalldiff?-maker 5))))))

(deftest pagerank-test-1
  (testing "a 1x1 matrix with beta and epsilon are 1."
    (is (= [1] (pagerank 1 1 [[1]])))))

(deftest pagerank-test-2
  (testing "a 3x3 matrix with beta is 1."
    (is (double-coll= [0.4 0.2 0.4] (pagerank 1 1.0E-10 [[0 0 1] [1 0 0] [1 1 0]])))))

(deftest pagerank-test-3
  (testing "a 3x3 matrix with beta is 0.7."
    (is (double-coll= [0.1 0.135 0.765] (pagerank 0.7 1.0E-10 [[0 0 0] [1 0 0] [1 1 1]])))))
