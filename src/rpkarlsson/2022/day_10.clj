(ns rpkarlsson.2022.day-10
  (:require [clojure.string :as str]))

(def cycles
  #{20, 60, 100, 140, 180, 220})

(defn calculate-signal-strength
  [xs cycle register]
  (if (contains? cycles cycle)
    (conj xs (* cycle register))
    xs))

(defn draw [screen register cycle]
  (if (<= (dec register) (mod (dec cycle) 40) (inc register))
    (assoc-in screen [(dec cycle)] "#")
    screen))

(defn run
  [{:keys [register cycle signal-strengths] :as state} [op step]]
  (let [register' (if (= :noop op)
                    register
                    (+ register step))]
    (-> state
        (assoc :cycle (inc cycle)
               :register register'
               :signal-strengths (calculate-signal-strength signal-strengths cycle register))
        (update :screen draw register cycle))))

(defn solve [input]
  (->> input
       (str/split-lines)
       (map #(str/split % #" "))
       (map (juxt (comp keyword first)
                  (comp (fnil parse-long "0") second)))
       (reduce (fn [coll [op _ :as step]]
                 (if (not= op :noop)
                   (conj coll [:noop] step)
                   (conj coll step)))
               [])
       (reduce run {:cycle 1
                    :register 1
                    :signal-strengths []
                    :screen (->> (range 0 240)
                                 (map (constantly "."))
                                 vec)})))

(defn part-1
  []
  (->> #_sample
       (-> "resources/2022/day_10.txt" slurp)
       (solve)
       :signal-strengths
       (reduce +)))

(defn part-2
  []
  (->> #_sample
       (-> "resources/2022/day_10.txt" slurp)
       (solve)
       (:screen)
       (partition 40)
       (map str/join)
       (map println)))
