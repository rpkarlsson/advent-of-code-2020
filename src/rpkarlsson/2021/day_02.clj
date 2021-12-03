(ns rpkarlsson.2021.day-02
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def sample ["forward 5" "down 5" "forward 8" "up 3" "down 8" "forward 2"])

(defn update-depth
  [{:keys [aim] :as m} n-forward]
  (update m :depth #(+ % (* aim n-forward))))

(defn calculate-position
  [{:keys [] :as acc} [move n]]
  (case move
    "forward" (update acc :horizontal #(+ % n))
    "down" (update acc :depth #(+ % n))
    "up" (update acc :depth #(- % n))))

(defn calculate-position-2
  [{:keys [] :as acc} [move n]]
  (case move
    "forward" (-> acc
                  (update :horizontal #(+ % n))
                  (update-depth n))
    "down" (update acc :aim #(+ % n))
    "up" (update acc :aim #(- % n))))

(defn mul [{:keys [horizontal depth]}] (* horizontal depth))

(defn run
  [position-calculator-fn]
  (with-open [f (io/reader (io/resource "2021/day_02.txt"))]
    (->> (line-seq f)
         (map #(str/split % #" "))
         (map (fn [v] (update v 1 #(Integer/parseInt %))))
         (reduce position-calculator-fn {:horizontal 0 :depth 0 :aim 0})
         (mul))))

(defn part-1 [] (run calculate-position))

(defn part-2 [] (run calculate-position-2))

(comment
  (->> sample
       (map #(str/split % #" "))
       (map (fn [v] (update v 1 #(Integer/parseInt %))))
       (reduce calculate-position-2 {:horizontal 0 :depth 0 :aim 0})
       (mul))

  (part-1)
  (part-2)

  ,)
