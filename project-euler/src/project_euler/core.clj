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

(defn partition-rows
  "Partition matrix row by row"
  [n step coll]
  (if
    (pos? (count coll))
    (concat (partition n step (first coll))
            (partition-rows n step (rest coll)))
    []))

(defn transpose
  "Transpose a matrix"
  [m]
  (apply mapv vector m))

(defn partition-columns
  "Partition matrix column by column"
  [n step coll]
  (partition-rows n step (transpose coll)))

(defn left-diagonals
  "Get the left diagonals in a matrix"
  [coll]
  (let [rows (count coll)
        columns (count (first coll))]
    (->> (for [row (range rows)
               column (range columns)]
           {(+ row (- columns column)) [(nth (nth coll row) column)]})
         (apply merge-with into ,,,)
         (vals ,,,))))

(defn right-diagonals
  "Get the right diagonals in a matrix"
  [coll]
  (left-diagonals (reverse coll)))

(defn diagonals
  "Get the diagonals in a matrix"
  [coll]
  (concat (left-diagonals coll)
          (right-diagonals coll)))

(defn partition-diagonals
  "Partition matrix diagonal by diagonal"
  [n step coll]
  (partition-rows n step (diagonals coll)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PROBLEMS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defn problem-11
  "What is the greatest product of four adjacent numbers in the same direction (up, down, left, right, or diagonally) in the 20×20 grid?"
  ([]
   (problem-11 [[ 8  2 22 97 38 15  0 40  0 75  4  5  7 78 52 12 50 77 91  8]
                [49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48  4 56 62  0]
                [81 49 31 73 55 79 14 29 93 71 40 67 53 88 30  3 49 13 36 65]
                [52 70 95 23  4 60 11 42 69 24 68 56  1 32 56 71 37  2 36 91]
                [22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80]
                [24 47 32 60 99  3 45  2 44 75 33 53 78 36 84 20 35 17 12 50]
                [32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70]
                [67 26 20 68  2 62 12 20 95 63 94 39 63  8 40 91 66 49 94 21]
                [24 55 58  5 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72]
                [21 36 23  9 75  0 76 44 20 45 35 14  0 61 33 97 34 31 33 95]
                [78 17 53 28 22 75 31 67 15 94  3 80  4 62 16 14  9 53 56 92]
                [16 39  5 42 96 35 31 47 55 58 88 24  0 17 54 24 36 29 85 57]
                [86 56  0 48 35 71 89  7  5 44 44 37 44 60 21 58 51 54 17 58]
                [19 80 81 68  5 94 47 69 28 73 92 13 86 52 17 77  4 89 55 40]
                [ 4 52  8 83 97 35 99 16  7 97 57 32 16 26 26 79 33 27 98 66]
                [88 36 68 87 57 62 20 72  3 46 33 67 46 55 12 32 63 93 53 69]
                [ 4 42 16 73 38 25 39 11 24 94 72 18  8 46 29 32 40 62 76 36]
                [20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74  4 36 16]
                [20 73 35 29 78 31 90  1 74 31 49 71 48 86 81 16 23 57  5 54]
                [ 1 70 54 71 83 51 54 69 16 92 33 48 61 43 52  1 89 19 67 48]]))
  ([m]
   (reduce max
           (map product
                (concat (partition-rows 4 1 m)
                        (partition-columns 4 1 m)
                        (partition-diagonals 4 1 m))))))