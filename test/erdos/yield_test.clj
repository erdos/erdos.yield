(ns erdos.yield-test
  (:require [clojure.test :refer [deftest testing is are]]
            [erdos.yield :refer :all]))

(deftest empty-sequences
  (is (= () (gen-seq (when false (yield 1))))))

(deftest sequential-yields
  (is (= '(1)     (gen-seq 2 3 (yield 1) 3 4)))
  (is (= '(1 2 3) (gen-seq (yield 1) (yield 2) (yield 3)))))

(deftest support-dotimes
  (is (= '(0 1 2 3) (gen-seq (dotimes [i 4] (yield i))))))

(deftest support-loop
  (is (= '(4 3 2 1)
         (gen-seq
          (loop [i 4]
            (when (pos? i)
              (yield i)
              (recur (dec i))))))))

(deftest support-let
  (is (= '(1 2 3)
         (gen-seq
          (let [i 1
                j 2
                k 3]
            (yield i)
            (yield j)
            (yield k))))))

(deftest infinite-seq
  (is (= (range 10)
         (take 10
               (gen-seq
                (loop [i 0]
                  (yield i)
                  (recur (inc i))))))))
