(ns project-euler.core
  (:require [clojure.math.numeric-tower :as math]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as string]))

(def primes
  "Lazy sequence of all the prime numbers."
  (concat [2 3 5 7]
          (lazy-seq
            (let [primes-from (fn primes-from [n [f & r]]
                                (if (some #(zero? (rem n %))
                                          (take-while #(<= (math/sqrt %) n)
                                                      primes))
                                  (recur (+ n f) r)
                                  (lazy-seq (cons n
                                                  (primes-from (+ n f)
                                                               r)))))
                  wheel (cycle [2 4 2 4 6 2 6 4 2 4 6 6 2 6  4 2
                                6 4 6 8 4 2 4 2 4 8 6 4 6 2  4 6
                                2 6 6 4 2 4 6 2 6 4 2 4 2 10 2 10])]
              (primes-from 11 wheel)))))

(defn sum
  "Return sum of coll"
  [xs]
  (reduce + xs))

(def fibs
  "Generate list of fibonacci numbers"
  (lazy-cat
    [0 1]
    (map + (rest fibs) fibs)))

(defn factor?
  "Check if y is a factor of x"
  [y x]
  (zero? (rem x y)))

(defn prime?
  "Validate if a number is a prime number"
  [x]
  (cond (<= x 1) false
        (= x 2) true
        :else (not-any?
                #(factor? % x)
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
        (filter #(factor? % x) xm)
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
          (when (>= a stop)
            (loop [b start]
              (let [ab (* a b)]
                (when (and (< a b)
                           (> ab @max-palindrom))
                  
                  (if (palindrome? ab)
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
      (when (<= a (/ x 3))
        (loop
          [b (inc a)]
          (let [c (- x a b)
                a2 (* a a)
                b2 (* b b)
                c2 (* c c)]
            
            (when (<= b (/ x 2))
              (if (= c2 (+ a2 b2))
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
        (if (> ff x)
          (persistent! s)
          (recur (->> (range ff x f)
                      (reduce disj! s ,,,))
                 (inc f)))))))

(defn partition-rows
  "Partition matrix row by row"
  [n step matrix]
  (if (pos? (count matrix))
    (concat (partition n step (first matrix))
            (partition-rows n step (rest matrix)))
    []))

(defn transpose
  "Transpose a matrix"
  [m]
  (apply mapv vector m))

(defn partition-columns
  "Partition matrix column by column"
  [n step matrix]
  (partition-rows n step (transpose matrix)))

(defn left-diagonals
  "Get the left diagonals in a matrix"
  [matrix]
  (let [rows (count matrix)
        columns (count (first matrix))]
    (->> (for [row (range rows)
               column (range columns)]
           {(+ row (- columns column)) [(nth (nth matrix row) column)]})
         (apply merge-with into ,,,)
         (vals ,,,))))

(defn right-diagonals
  "Get the right diagonals in a matrix"
  [matrix]
  (left-diagonals (reverse matrix)))

(defn diagonals
  "Get the diagonals in a matrix"
  [matrix]
  (concat (left-diagonals matrix)
          (right-diagonals matrix)))

(defn partition-diagonals
  "Partition matrix diagonal by diagonal"
  [n step matrix]
  (partition-rows n step (diagonals matrix)))

(defn triangular
  "Get the nth triangular number"
  [i]
  (-> i
      (inc ,,,)
      (/ ,,, 2)
      (* ,,, i)))

(def triangulars
  "Lazy seq of triangular numbers"
  (->> (iterate inc 1)
       (map triangular ,,,)))

(defn factorize
  "Factorize x"
  [x]
  (loop [y x [p & ps] primes factors []]
    (cond (= 1 y) factors
          (factor? p y) (recur (/ y p)
                               primes
                               (conj factors p))
          :else (recur y ps factors))))

(defn factors
  "Count the factors in x"
  [x]
  (->> x
       (factorize ,,,)
       (frequencies ,,,)
       (vals ,,,)
       (map inc ,,,)
       (product ,,,)))

(defn collatz
  "Starting from x, return the collatz collection"
  [x]
  (let [coll (atom [])]
    (loop [i x]
      (swap! coll conj i)
      (when (> i 1)
        (if (even? i)
          (recur (/ i 2))
          (recur (inc (* i 3))))))
    @coll))

(defn collatz-len
  "Get the length Collatz chain starting at x."
  [x]
  (->> x
       (collatz ,,,)
       (count ,,,)
       (vector x ,,,)))

(defn factorial
  "Calculate the factorial for x"
  [x]
  (->> x
       (bigdec ,,,)
       (inc ,,,)
       (range 1M ,,,)
       (product ,,,)))

(defn combinations
  "Return the combinations of x y"
  [x y]
  (cond
    (zero? x) 0
    (zero? y) 1
    :else (/ (factorial x)
             (* (factorial (- x y))
                (factorial y)))))

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
        (take-while #(< % x) ,,,)
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
  ([matrix]
   (reduce max
           (map product
                (concat (partition-rows 4 1 matrix)
                        (partition-columns 4 1 matrix)
                        (partition-diagonals 4 1 matrix))))))

(defn problem-12
 "What is the value of the first triangle number to have over five hundred divisors?"
  ([]
   (problem-12 500))
  ([n]
   (first (drop-while #(< (factors %) n) triangulars))))

(defn problem-13
  "Work out the first ten digits of the sum of the following one-hundred 50-digit numbers."
  ([]
   (problem-13 [37107287533902102798797998220837590246510135740250
                46376937677490009712648124896970078050417018260538
                74324986199524741059474233309513058123726617309629
                91942213363574161572522430563301811072406154908250
                23067588207539346171171980310421047513778063246676
                89261670696623633820136378418383684178734361726757
                28112879812849979408065481931592621691275889832738
                44274228917432520321923589422876796487670272189318
                47451445736001306439091167216856844588711603153276
                70386486105843025439939619828917593665686757934951
                62176457141856560629502157223196586755079324193331
                64906352462741904929101432445813822663347944758178
                92575867718337217661963751590579239728245598838407
                58203565325359399008402633568948830189458628227828
                80181199384826282014278194139940567587151170094390
                35398664372827112653829987240784473053190104293586
                86515506006295864861532075273371959191420517255829
                71693888707715466499115593487603532921714970056938
                54370070576826684624621495650076471787294438377604
                53282654108756828443191190634694037855217779295145
                36123272525000296071075082563815656710885258350721
                45876576172410976447339110607218265236877223636045
                17423706905851860660448207621209813287860733969412
                81142660418086830619328460811191061556940512689692
                51934325451728388641918047049293215058642563049483
                62467221648435076201727918039944693004732956340691
                15732444386908125794514089057706229429197107928209
                55037687525678773091862540744969844508330393682126
                18336384825330154686196124348767681297534375946515
                80386287592878490201521685554828717201219257766954
                78182833757993103614740356856449095527097864797581
                16726320100436897842553539920931837441497806860984
                48403098129077791799088218795327364475675590848030
                87086987551392711854517078544161852424320693150332
                59959406895756536782107074926966537676326235447210
                69793950679652694742597709739166693763042633987085
                41052684708299085211399427365734116182760315001271
                65378607361501080857009149939512557028198746004375
                35829035317434717326932123578154982629742552737307
                94953759765105305946966067683156574377167401875275
                88902802571733229619176668713819931811048770190271
                25267680276078003013678680992525463401061632866526
                36270218540497705585629946580636237993140746255962
                24074486908231174977792365466257246923322810917141
                91430288197103288597806669760892938638285025333403
                34413065578016127815921815005561868836468420090470
                23053081172816430487623791969842487255036638784583
                11487696932154902810424020138335124462181441773470
                63783299490636259666498587618221225225512486764533
                67720186971698544312419572409913959008952310058822
                95548255300263520781532296796249481641953868218774
                76085327132285723110424803456124867697064507995236
                37774242535411291684276865538926205024910326572967
                23701913275725675285653248258265463092207058596522
                29798860272258331913126375147341994889534765745501
                18495701454879288984856827726077713721403798879715
                38298203783031473527721580348144513491373226651381
                34829543829199918180278916522431027392251122869539
                40957953066405232632538044100059654939159879593635
                29746152185502371307642255121183693803580388584903
                41698116222072977186158236678424689157993532961922
                62467957194401269043877107275048102390895523597457
                23189706772547915061505504953922979530901129967519
                86188088225875314529584099251203829009407770775672
                11306739708304724483816533873502340845647058077308
                82959174767140363198008187129011875491310547126581
                97623331044818386269515456334926366572897563400500
                42846280183517070527831839425882145521227251250327
                55121603546981200581762165212827652751691296897789
                32238195734329339946437501907836945765883352399886
                75506164965184775180738168837861091527357929701337
                62177842752192623401942399639168044983993173312731
                32924185707147349566916674687634660915035914677504
                99518671430235219628894890102423325116913619626622
                73267460800591547471830798392868535206946944540724
                76841822524674417161514036427982273348055556214818
                97142617910342598647204516893989422179826088076852
                87783646182799346313767754307809363333018982642090
                10848802521674670883215120185883543223812876952786
                71329612474782464538636993009049310363619763878039
                62184073572399794223406235393808339651327408011116
                66627891981488087797941876876144230030984490851411
                60661826293682836764744779239180335110989069790714
                85786944089552990653640447425576083659976645795096
                66024396409905389607120198219976047599490197230297
                64913982680032973156037120041377903785566085089252
                16730939319872750275468906903707539413042652315011
                94809377245048795150954100921645863754710598436791
                78639167021187492431995700641917969777599028300699
                15368713711936614952811305876380278410754449733078
                40789923115535562561142322423255033685442488917353
                44889911501440648020369068063960672322193204149535
                41503128880339536053299340368006977710650566631954
                81234880673210146739058568557934581403627822703280
                82616570773948327592232845941706525094512325230608
                22918802058777319719839450180888072429661980811197
                77158542502016545090413245809786882778948721859617
                72107838435069186155435662884062257473692284509516
                20849603980134001723930671666823555245252804609722
                53503534226472524250874054075591789781264330331690]))
  ([coll]
   (->> coll
        (map str ,,,)
        (map #(subs % 0 12) ,,,)
        (map read-string ,,,)
        (sum ,,,)
        (str ,,,)
        (#(subs % 0 10) ,,,)
        (read-string ,,,))))

(defn problem-14
  "Which starting number, under one million, produces the longest chain?"
  ([]
   (problem-14 1e6))
  ([x]
   (->> x
        (range 1 ,,,)
        (map collatz-len ,,,)
        (apply max-key second ,,,)
        (first ,,,))))

(defn problem-15
  "How many such routes are there through a 20×20 grid?"
  ([]
   (problem-15 20))
  ([n]
   (combinations (* n 2) n)))