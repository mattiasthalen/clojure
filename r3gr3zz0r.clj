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
(defn r3gr3zz0r [known xs ys & [predict_x]]
  ;define sums
  (let [x_sum (reduce + xs)
        y_sum (reduce + ys)
        xy_sum (reduce + (map * xs ys))
        xx_sum (reduce + (map (fn [n] (* n n)) xs))
        n (count xs)
        slope (float (/ (- (* y_sum xx_sum) (* x_sum xy_sum)) (- (* n xx_sum) (* x_sum x_sum))))
        intercept (float (/ (- (* n xy_sum) (* x_sum y_sum)) (- (* n xx_sum) (* x_sum x_sum))))]
    
    ;check if predicting x or y
     (if predict_x (/ (- known slope) intercept) (+ slope (* intercept known)))))

;testing
(r3gr3zz0r 123 rand_x rand_y true)
;testing