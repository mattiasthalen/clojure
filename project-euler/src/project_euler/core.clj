(ns project-euler.core
  (:require [clojure.math.numeric-tower :as math]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as string]))

(defn sum
  "Return sum of coll"
  [xs]
  (reduce + xs))

(def fibs
  "Generate list of fibonacci numbers"
  (lazy-cat
    [0 1]
    (map + (rest fibs) fibs)))

(defn prime?
  "Validate if a number is a prime number"
  [x]
  (cond (<= x 1) false
        (= x 2) true
        :else (not-any?
                #(zero? (mod x %))
                (cons 2
                      (range 3
                             (inc (Math/sqrt x))
                             2)))))

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

(defn square
  "Return the square of x"
  [x]
  (* x x))

(defn sum-of-squares 
  "Return the sum of all squares"
  [xs]
  (->> xs
       (map square ,,,)
       (sum ,,,)))

(defn square-of-sum
  "Return the square of sum"
  [xs]
  (->> xs
       (sum ,,,)
       (square ,,,)))

(defn digits
  "Return a lazy seq of all the digits in x"
  [x]
  (->> x
       (str ,,,)
       (map (comp read-string str) ,,,)))

(defn product
  "Return the product of coll"
  [xs]
  (reduce * xs))

(defn pythagorian-triple-product
  "Using x, calculate the product of the Pythagorian triple."
  [x]
  (let [abc (atom [])]
    (loop
      [a 1]
      (when
        (<= a (/ x 3))
        (loop
          [b (inc a)]
          (let [c (- x a b)
                a2 (* a a)
                b2 (* b b)
                c2 (* c c)]
            (when
              (<= b (/ x 2))
              (if
                (= c2 (+ a2 b2))
                (reset! abc [a b c])
                (recur (inc b))))))
      (recur (inc a))))
    (reduce * @abc)))

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
      (let [ff (square f)]
        (if
          (> ff x)
          (persistent! s)
          (recur
            (->> (range ff x f)
                 (reduce disj! s ,,,))
            (inc f)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Problems
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn problem-1
  "Find the sum of all the multiples of 3 or 5 below 1000."
  ([]
   (problem-1 1000))
  ([x]
   (->> (range x)
        (filter #(zero? (min (mod % 3)
                             (mod % 5))) ,,,)
        (sum ,,,))))

(defn problem-2
  "By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms."
  ([]
   (problem-2 4e6))
  ([x]
   (->> fibs
        (filter even? ,,,)
        (take-while (partial >= x) ,,,)
        (sum ,,,))))

(defn problem-3
  "What is the largest prime factor of the number 600851475143?"
  ([]
   (problem-3 600851475143))
  ([x]
   (->> x
        (prime-factors ,,,)
        (reduce max ,,,))))

(defn problem-4
  "Find the largest palindrome made from the product of two 3-digit numbers."
  ([] 
   (problem-4 3))
  ([n]
   (max-palindromic-product n)))

(defn problem-5
 "What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?"
  ([]
   (problem-5 21))
  ([x]
   (reduce math/lcm (range 1 x))))

(defn problem-6
  "Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum."
  ([]
   (problem-6 100))
  ([n]
   (let [xs (range (inc n))
         sq-sum (square-of-sum xs)
         sum-sq  (sum-of-squares xs)]
     (- sq-sum sum-sq))))

(defn problem-7
  "What is the 10 001st prime number?"
  ([]
   (problem-7 10001))
  ([n]
   (->> (iterate inc 2)
        (filter prime? ,,,)
        (drop (dec n) ,,,)
        (first ,,,))))

(defn problem-8
  "Find the thirteen adjacent digits in the 1000-digit number that have the greatest product. What is the value of this product?"
  ([]
   (problem-8 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450))
  ([x]
   (->> x
        (str ,,,)
        (digits ,,,)
        (partition 13 1 ,,,)
        (map #(product %) ,,,)
        (reduce max ,,,))))

(defn problem-9
  "There exists exactly one Pythagorean triplet for which a + b + c = 1000. Find the product abc."
  ([]
   (problem-9 1000))
  ([x]
   (pythagorian-triple-product x)))

(defn problem-10
  "Find the sum of all the primes below two million."
  ([]
   (problem-10 2e6))
  ([x]
   (->> x
        (primes-below ,,,)
        (sum ,,,))))