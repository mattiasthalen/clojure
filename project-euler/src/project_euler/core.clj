(ns project-euler.core
  (:require [clojure.math.numeric-tower :as math]
          [clojure.math.combinatorics :as combo]))

(defn primes-below
  "Generate primes using Sieve of Eratosthenes"
  [x]
  (let [sieve (->> (range 3 x 2)
                   (cons 2 ,,,)
                   (set ,,,)
                   (transient ,,,))]
    (loop
      [s sieve
       f 3]
      (if
        (> (math/expt f 2) x)
        (persistent! s)
        (recur
          (reduce disj! s (range (math/expt f 2) x f))
          (inc f))))))
      
(defn problem-10
  "Task: What's the sum of all primes below 2 million?
   Answer: 142913828922.
   Note: Repl.it is too weak for 2e6."
  ([]
   (problem-10 2e6))
  ([x]
   (->> x
        (primes-below ,,,)
        (apply + ,,,))))