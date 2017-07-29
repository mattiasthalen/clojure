(require '[clojure.string :as str])

;generate x & y lists
(def rand_x (take 100 (repeatedly #(rand-int 100))))
(def rand_y (take 100 (repeatedly #(rand-int 100))))

;define r3gr3zz0r
(defn linear_regression [known xs ys x_or_y]
  ;define sums
  (let [x_sum (reduce + xs)
        y_sum (reduce + ys)
        xy_sum (reduce + (map * xs ys))
        xx_sum (reduce + (map (* % %) xs))
        yy_sum (reduce + (map (* % %) ys))
        n (count xs)
        
        slope (float (/ (- (* y_sum xx_sum) (* x_sum xy_sum)) (- (* n xx_sum) (* x_sum x_sum))))
        intercept (float (/ (- (* n xy_sum) (* x_sum y_sum)) (- (* n xx_sum) (* x_sum x_sum))))
        r (/ (- (* n xy_sum) (* x_sum y_sum)) (Math/sqrt (* (- (* n xx_sum) (* x_sum x_sum)) (- (* n yy_sum) (* y_sum y_sum)))))
        r2 (* r r)
        
        y_ (+ slope (* intercept known))
        x_ (/ (- known slope) intercept)]
    
    ;check if predicting x or y
     (if (= "x" (str/lower-case x_or_y))
       ;print y' formula
       (println (str "y' = " y_))
       ;print x' formula
       (println (str "x' = " x_)))

    ;print stats
    (println (str "slope = " slope))
    (println (str "intercept = " intercept))
    (println (str "r2 = " r2))))


;trigger regression
(linear_regression
  (do (println "What's the known number?") (flush) (Integer. (re-find  #"\d+" (read-line))))
  rand_x
  rand_y
  (do (println "Is that x or y?") (flush) (read-line)))