(ns clj-titanic.core
  (:require [clojure-csv.core :as csv])
  )

(def read-train
  (csv/parse-csv (slurp "data/train.csv")))

(def read-test
  (csv/parse-csv (slurp "data/test.csv")))

(defn column
  [n xs]
  (rest (map read-string (remove clojure.string/blank? (map #(nth % n) xs)))))

(defn ages[xs]
  (column 4 xs))

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
  (let [sur (column 0 xs)]
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
  (filter #(= "1" (first %)) (rest read-train)))

(def died
  (filter #(= "0" (first %)) (rest read-train)))

(def name-freqs-of-survivors
  (frequencies (re-seq #"\w+" (clojure.string/join " " (map #(nth % 2) survivors)))))

(def name-freqs-of-died
  (frequencies (re-seq #"\w+" (clojure.string/join " " (map #(nth % 2) died)))))

(defn combine-probabilities
  "(combine-probabilities (vector (probability-survived-given-age 47 ) (prob-by-full-name \"Kelly, Mr. James\")))"
  [arr]
  (prn arr)
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








;P(S|A) = ((Pr(A|S) * Pr(S)) / ((Pr(A|S) * Pr(S)) + (Pr(A|D) * Pr(D))))

;P(survived|age) = (probability somebody of a similar age survived) * (probability of survival) /
;foo + (probability somebody of similar age died) * (probability of death)
