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

(deftest support-case
  (is (= '(0 1 2 3 4 5 6)
         (let [f (fn [x]
                   (gen-seq
                    (case x
                      :one-two-three (do
                                       (yield 1)
                                       (yield 2)
                                       (yield 3))
                      :four-five-six (do
                                       (yield 4)
                                       (yield 5)
                                       (yield 6))
                      (yield 0))))]
           (mapcat f '(:zero
                       :one-two-three
                       :four-five-six))))))

(deftest infinite-seq
  (is (= (range 10)
         (take 10
               (gen-seq
                (loop [i 0]
                  (yield i)
                  (recur (inc i))))))))


(deftest infinite-seq
  (is (= (range 10)
         (take 10
               (gen-seq
                (loop [i 0]
                  (yield i)
                  (recur (inc i))))))))

(deftest test-if
  (is (= '(:even :even :even)
         (gen-seq
          (doseq [i (range 5)]
            (if (even? i)
              (yield :even)
              (list 1 2 3)))))))

(deftest test-case
  (is (= '(:default :odd :even :odd :even)
         (gen-seq
          (doseq [i (range 5)]
            (case i
              1 (yield :odd)
              2 (list 1 2 3)
              3 (yield :odd)
              4 (list 1 2 3)
              (yield :default))
            (case i
              1 (list 1 2 3)
              2 (yield :even)
              3 (list 1 2 3)
              4 (yield :even)
              (list 1 2 3)))))))


(deftest sdf
  (println (rewrite-val '(do (yield 1) (yield 2)))))
