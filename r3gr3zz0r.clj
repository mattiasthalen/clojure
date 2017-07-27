(require '[clojure.set :as set])

;random number generator
(defn unique-random-numbers [n]
  (let [a-set (set (take n (repeatedly #(rand-int n))))]
    (concat a-set (set/difference (set (take n (range)))
                                  a-set))))

;generate x & y lists
(def rand_x (unique-random-numbers 100))
(def rand_y (unique-random-numbers 100))

;define r3gr3zz0r
(defn r3gr3zz0r [known xs ys x_or_y]
  ;define sums
  (let [x_sum (reduce + xs)
        y_sum (reduce + ys)
        xy_sum (reduce + (map * xs ys))
        xx_sum (reduce + (map (fn [n] (* n n)) xs))
        n (count xs)
        slope (float (/ (- (* y_sum xx_sum) (* x_sum xy_sum)) (- (* n xx_sum) (* x_sum x_sum))))
        intercept (float (/ (- (* n xy_sum) (* x_sum y_sum)) (- (* n xx_sum) (* x_sum x_sum))))]
    
    ;check if predicting x or y
     (if (= "x" (clojure.string/lower-case x_or_y))
       (println (str "y should be: " (+ slope (* intercept known))))
       (println (str "x should be: " (/ (- known slope) intercept))))))

;testing
(r3gr3zz0r
  (do (println "What's the known number?") (flush) (Integer. (re-find  #"\d+" (read-line))))
  rand_x
  rand_y
  (do (println "Is that x or y?") (flush) (read-line)))