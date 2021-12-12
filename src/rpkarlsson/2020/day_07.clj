(ns rpkarlsson.2020.day-07
  (:require [clojure.string :as str]
            [rpkarlsson.util.file :as file]))

(defn ->formatted-kw
  [s]
  (-> (str/trim s)
      (str/replace #" " "-")
      (keyword)))

(def parse-number
  (juxt (comp ->formatted-kw second)
        (comp #(some-> % (Integer/parseInt)) first)))

(defn parse-line
  [line]
  (let [[container containees] (str/split line #"bags? contain")]
    [(->formatted-kw container)
     (->> (str/split containees #",")
          (map #(drop 1 (re-find #"(\d+) (\w+ \w+)" %)))
          (map #(if (seq %)
                  (parse-number %)
                  %))
          (filter seq)
          (into []))]))

(def parse-input-xf
  (comp
   (map parse-line)
   (map (juxt first
              (fn [[_ xs]]
                (->> xs
                     (map first)
                     (into #{})))))))

(defn can-contain-gold-bag?
  [bag bags]
  (let [children (get bags bag)]
    (cond
      (empty? children) false
      (contains? children :shiny-gold) true
      :else (some true? (map #(can-contain-gold-bag? % bags) children)))))

(defn part-1
  []
  (let [bags (->> (file/xf-day-lines 7 parse-input-xf)
                  (into {}))]
    (->> (keys bags)
         (map #(can-contain-gold-bag? % bags))
         (filter true?)
         (count))))

(defn number-of-contained-bags
  [bag bags]
  (let [children (get bags bag)]
    (cond
      (empty? children) 0
      (number? (second children)) (second children)
      :else (reduce +
                    (map (fn [[k v]]
                           (+ v (* v (number-of-contained-bags k bags))))
                         children)))))

(defn part-2
  []
  (number-of-contained-bags
   :shiny-gold
   (->> (file/xf-day-lines 7 (map parse-line))
        (into {}))))
