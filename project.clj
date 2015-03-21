(defproject pagerank "0.1.0-SNAPSHOT"
  :description "A Clojure implementation of the pagerank algorithm"
  :url "http://github.com/emrehan/pagerank-clj"
  :license {:name "The MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [net.mikera/core.matrix "0.33.2"]
                 [net.mikera/vectorz-clj "0.29.0"]
                 [local-file "0.1.0"]]
  :main pagerank.main)
