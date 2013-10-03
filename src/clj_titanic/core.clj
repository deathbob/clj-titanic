(ns clj-titanic.core
  (:require [clojure.math.numeric-tower :as math])
  (:require [clojure-csv.core :as csv])
  )


(def read-train
  (csv/parse-csv (slurp "data/train.csv")))

(def read-test
  (csv/parse-csv (slurp "data/test.csv")))

(defn column
  [n xs]
  (rest (remove clojure.string/blank? (map #(nth % n) xs))))

(defn ages[xs]
  (map read-string (column 5 xs)))

(defn mean
  [xs]
  (/ (reduce + xs) (count xs)))

(defn variance
  [xs]
  (let [avg (mean xs)
        dev-sums (map #(* % %) (map #(- % avg) xs))]
    (. Math sqrt (/ (reduce + dev-sums) (count dev-sums)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (println "Hello, World!"))

(defn p-survived[xs]
  (let [sur (map read-string(column 1 xs))]
    ( / (reduce + sur) (count sur))))

(defn p-died[xs]
  (- 1 (p-survived xs)))

(defn age-column-within-7
  [row age]
  (let [val (read-string (nth row 4))
        eps 5]
    (and (> val (- age eps)) (< val (+ age eps)))))

(defn passengers-with-similar-age
  [age]
  (filter #(age-column-within-7 % age) (remove #(clojure.string/blank? (nth % 4))(rest read-train))))

(defn probability-similar-age-survived
  [age]
  (p-survived (passengers-with-similar-age age)))

(defn probability-similar-age-died
  [age]
  (p-died (passengers-with-similar-age age)))

(defn probability-survived-given-age
  [age]
  (let [p-b-a (* (probability-similar-age-survived age) (p-survived read-train))
        ppp (* (probability-similar-age-died age) (p-died read-train))]
;    (prn p-b-a "asdf" ppp)
    (/ p-b-a (+ p-b-a ppp))))

(defn predict-survived-based-on-age
  [age]
  (if (clojure.string/blank? age)
    nil
    (if (> (probability-survived-given-age (read-string age)) 0.5)
      1
      0)))

(defn test-age-predictions
  []
  (let [test-ages (map #(nth % 3) (rest read-test))]
    (map predict-survived-based-on-age test-ages)))

;;; sexy
(defn passengers-with-similar-sex[sex]
  (filter #(= (nth % 3) sex) (rest read-train)))

(defn probability-same-sex-survived
  [sex]
  (p-survived (passengers-with-similar-sex sex)))

(defn probability-same-sex-died
  [sex]
  (p-died (passengers-with-similar-sex sex)))

(defn probability-of-survival-given-sex
  [sex]
  (let [p-b-a (* (probability-same-sex-survived sex) (p-survived read-train))
        ppp (* (probability-same-sex-died sex) (p-died read-train))]
    (/ p-b-a (+ p-b-a ppp))))

(def survivors
  (filter #(= "1" (second %)) (rest read-train)))

(def died
  (filter #(= "0" (second %)) (rest read-train)))

(def name-freqs-of-survivors
  (frequencies (re-seq #"\w+" (clojure.string/join " " (map #(nth % 3) survivors)))))

(def name-freqs-of-died
  (frequencies (re-seq #"\w+" (clojure.string/join " " (map #(nth % 3) died)))))

(defn combine-probabilities
  "(combine-probabilities (vector (probability-survived-given-age 47 ) (prob-by-full-name \"Kelly, Mr. James\")))"
  [arr]
  (let [pt (reduce * arr)
        ft (reduce * (map #(- 1 %) arr))]
    (if (empty? arr)
      nil
      (/ pt (+ pt ft)))
    ))

(defn probability-of-survival-given-name
  [name]
  (let [survivor-count-of-name (get name-freqs-of-survivors name 0)
        survivor-count (count survivors)
        died-count (count died)
        died-count-of-name (get name-freqs-of-died name 0)
        p-b-a (* (/ survivor-count-of-name survivor-count) (p-survived read-train))
        ppp (* (/ died-count-of-name died-count) (p-died read-train))]
    (if (= 0 ppp)
      nil
      (/ p-b-a (+ p-b-a ppp))
      )))

(defn prob-by-full-name
  [name]
  (let [arr (re-seq #"\w+" name)
        ar2 (remove nil? (map probability-of-survival-given-name arr))]
    (combine-probabilities ar2)
    ))


(defn predict-by-name
  []
  (map #(if (> % 0.2) 1 0 )(map prob-by-full-name (column 2 read-test))))


(defn train-row-to-map[[passenger-id survived pclass name sex age sibsp parch ticket fare cabin embarked]]
  (hash-map
   :passenger-id passenger-id
   :survived survived
   :pclass pclass
   :name name
   :sex sex
   :age age
   :sibsp sibsp
   :parch parch
   :ticket ticket
   :fare fare
   :cabin cabin
   :embarked embarked))


(defn safe-read-string[str]
  (if (clojure.string/blank? str)
    0
    (read-string str)))


(defn test-row-to-map[[passenger-id pclass name sex age sibsp parch ticket fare cabin embarked]]
  (hash-map
   :passenger-id passenger-id
   :pclass (read-string pclass)
   :name name
   :sex sex
   :age   (safe-read-string age)
   :sibsp (safe-read-string sibsp)
   :parch (safe-read-string parch)
   :fare  (safe-read-string fare)
   :ticket ticket
   :cabin cabin
   :embarked embarked))


(defn predict-by-h20
  [mp]
  (let [male-true (if (= (mp :sex) "male") 1 0)
        interior (- (+
     (* -2.63484 male-true)
     (* -1.24224  (mp :pclass))
     (* -0.04395  (mp :age))
     (* -0.37575  (mp :sibsp))
     (* -0.06193  (mp :parch))
     (* 0.00216   (mp :fare))
     5.389003))
        probability (/ 1 (+ 1 (math/expt 2.71828 interior)))]
    (if (> probability 0.5) 1 0)
    ))


(defn do-h20-pred[]
  (map predict-by-h20 (map test-row-to-map (rest read-test))))




;P(S|A) = ((Pr(A|S) * Pr(S)) / ((Pr(A|S) * Pr(S)) + (Pr(A|D) * Pr(D))))

;P(survived|age) = (probability somebody of a similar age survived) * (probability of survival) /
;foo + (probability somebody of similar age died) * (probability of death)


; (spit "asdf.csv" (apply str (map #(str (clojure.string/join "," %) "\n") (map vector (range 892 1310) (predict-by-name)))))


;(spit "qwer.csv" (apply str (map #(str (clojure.string/join "," %) "\n") (map vector (map first (rest read-test)) (do-h20-pred)))))
