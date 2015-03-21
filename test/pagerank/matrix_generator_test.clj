(ns pagerank.matrix_generator_test
  (:use [pagerank.matrix_generator]
        [clojure.core.matrix]
        [clojure.java.io]
        [clojure.test]))

(deftest generate-matrix-test
  (testing "generate a matrix for a number of edges.")
  (is (= 0.0 (esum (generate-matrix 100)))))

(deftest get-reader-test
  (testing "get reader of edges file."
    (with-open [rdr (get-reader)]
      (.readLine rdr) ;read first line
      (is (= (.readLine rdr) "# Webgraph from the Google programming contest, 2002")))))

(deftest get-lines-test
  (testing "get lines from a reader."
      (is (= (first (get-lines)) ["0" "11342"]))))

; 1000000 is more than the biggest node id in the given data
; 5105039 is the number of edges in the given data
(deftest populate-matrix-test
  (testing "populate a matrix using an edges list."
    (def m (generate-matrix 1000000))
    (populate-matrix m)
    (is (= 5105039.0 (esum m)))))

; 5105039 is the number of edges in the given data
(deftest get-matrix-test
  (testing "get a matrix of the edges list."
    (is (= 5105039.0 (esum (get-matrix))))))
