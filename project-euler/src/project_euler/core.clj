(ns project-euler.core
  (:require [clojure.math.numeric-tower :as math]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as string]))

(defn problem-1
  "Find the sum of all the multiples of 3 or 5 below 1000."
  ([]
   (problem-1 1000))
  ([x]
   (->> (range x)
        (filter #(zero? (min (mod % 3)
                             (mod % 5)))
                ,,,)
        (reduce + ,,,))))

(def fibs
  "Generate list of fibonacci numbers"
  (lazy-cat
    [0 1]
    (map + (rest fibs) fibs)))

(defn problem-2
  "By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms."
  ([]
   (problem-2 4e6))
  ([x]
   (->> fibs
        (filter even? ,,,)
        (take-while (partial >= x) ,,,)
        (reduce + ,,,))))

(defn prime?
  "Function to validate if a number is a prime number"
  [x]
  (cond
    (<= x 1) false
    (= x 2) true
    :else (not-any?
            #(zero? (mod x %))
            (cons 2 (range 3 (inc (Math/sqrt x)) 2)))))

(defn prime-factors
  "Find the prime factors of x"
  [x]
  (as-> x xm
        (math/sqrt xm)
        (inc xm)
        (range 3 xm 2)
        (cons 2 xm)
        (filter #(zero? (mod x %)) xm)
        (filter #(prime? %) xm)))

(defn problem-3
  "What is the largest prime factor of the number 600851475143?"
  ([]
   (problem-3 600851475143))
  ([x]
   (->> x
        (prime-factors ,,,)
        (apply max ,,,))))

(defn palindrome? 
  "Check if input is a palindrome"
  [s]
  (->> (str s)
       (string/reverse ,,,)
       (= (str s) ,,,)))

(defn max-palindromic-product
  "Find the largest palindromic product of n digit factors"
  [n]
  (let [start (->> n
                   (math/expt 10 ,,,)
                   (int ,,,)
                   (dec ,,,))
        
        stop (->> n
                  (dec ,,,)
                  (math/expt 10 ,,,)
                  (int ,,,))
        
        max-palindrom (atom 0)]
      
        (loop
          [a start]
          (when
            (>= a stop)
            (loop [b start]
              (let [ab (* a b)]
                (when
                  (and
                    (< a b)
                    (> ab @max-palindrom))
                  (if
                    (palindrome? ab)
                    (reset! max-palindrom ab))
                  (recur (dec b)))))
            (recur (dec a))))
            
          @max-palindrom))

(defn problem-4
  ([] 
   (problem-4 3))
  ([n]
   (max-palindromic-product n)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn primes-below
  "Generate primes below x using Sieve of Eratosthenes"
  [x]
  (let [sieve (->> (range 3 x 2)
                   (cons 2 ,,,)
                   (set ,,,)
                   (transient ,,,))]
    (loop
      [s sieve
       f 3]
      (let [ff (math/expt f 2)]
        (if
          (> ff x)
          (persistent! s)
          (recur
            (->> (range ff x f)
                 (reduce disj! s ,,,))
            (inc f)))))))

(defn problem-10
  "Find the sum of all the primes below two million."
  ([]
   (problem-10 2e6))
  ([x]
   (->> x
        (primes-below ,,,)
        (apply + ,,,))))