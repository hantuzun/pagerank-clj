(ns pagerank.core_test
  (:use [clojure.test]
        [pagerank.core]))

(deftest diff-test-1
  (testing "diff 1 2."
    (is (= 1 (diff 1 2)))))

(deftest diff-test-2
  (testing "diff 2 1."
    (is (= 1 (diff 2 1)))))

(deftest diff-test-3
  (testing "diff 2.3 1."
    (is (= 1.5 (diff 2.5 1)))))

(deftest almost-equal-test-1
  (testing "almost-equal for doubles"
    (is (true? (almost-equal 1 0.999999999)))))

(deftest almost-equal-test-2
  (testing "almost-equal for fractions"
    (is (true? (almost-equal 1/3 3/9)))
    (is (false? (almost-equal 1/3 0.3)))
    (is (true? (almost-equal 1/3 0.333333333)))))

(deftest vector-diff-test-1
  (testing "diff [2 1] [1 2]."
    (is (= 2 (vector-diff [2 1] [1 2])))))

(deftest vector-diff-test-2
  (testing "diff [2.5 1] [1 2.5]."
    (is (= 3.0 (vector-diff [2.5 1] [1 2.5])))))

(deftest vector-almost-equal-test-1
  (testing "the same vectors."
    (is (true? (vector-almost-equal [1 3.342 543.99] [1 3.342 543.99] 1E-3)))))

(deftest vector-almost-equal-test-2
  (testing "vectors with an element of 1e-2 diff."
    (is (false? (vector-almost-equal [1 3.342 544] [1 3.342 543.99] 1E-3)))))

(deftest vector-almost-equal-test-3
  (testing "vectors of multiple elements with diff 1e-7."
    (is (true? (vector-almost-equal [1.0000001 3.3420001 543.99] [1 3.342 543.99] 1E-3  )))))

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

(deftest pagerank-row-test
  (testing "calculating pagerank of an element."
    (is (almost-equal 0.15 (pagerank-row [0.1 0.5 0.2] [0.3 0.2 0.1])))))

(deftest make-pagerank-calculator-test
  (testing "make-pagerank-calculator."
    (is (almost-equal (first ((make-pagerank-calculator 0.85 [[1]]) [1])) 1))))

(deftest make-stabilized?-test-1
  (testing "epsilon is 1."
    (is ((make-stabilized? 1) [0.1 0.2 0.4] [0.2 0.1 0.1]))
    (is not ((make-stabilized? 0.1) [0.1 0.2 0.4] [0.2 0.1 0.1]))))

(deftest recur-improve-test
  (testing "dividing 1024 to 2 until the diff is less than 10."
    (defn smalldiff? [x y e] (<= (Math/abs (- x y)) e))
    (defn smalldiff?-maker [e] #(smalldiff? %1 %2 e))
    (defn half [x] (double (/ x 2)))
    (is (almost-equal 4.0 (recur-improve 0 1024 half (smalldiff?-maker 5))))))

(deftest pagerank-test-1
  (testing "a 1x1 matrix with beta and epsilon are 1."
    (is (= [1] (pagerank 1 1 [[1]])))))

(deftest pagerank-test-2
  (testing "a 3x3 matrix with beta is 1."
    (is (vector-almost-equal [0.4 0.2 0.4] (pagerank 1 1.0E-10 [[0 0 1] [1 0 0] [1 1 0]]) 0.0001))))

(deftest pagerank-test-3
  (testing "a 3x3 matrix with beta is 0.7."
    (is (vector-almost-equal [0.1 0.135 0.765] (pagerank 0.7 1.0E-10 [[0 0 0] [1 0 0] [1 1 1]]) 0.0001))))
