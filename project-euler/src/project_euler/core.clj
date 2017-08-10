(ns project-euler.core
  (:gen-class))

(defn ** [x]
  "Calculate the square of x"
  (* x x))

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
        (> (** f) x)
        (sort (persistent! s))
        (recur
          (reduce disj! s (range (** f) x f))
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