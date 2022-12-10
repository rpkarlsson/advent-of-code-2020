(ns rpkarlsson.2022.day-10
  (:require [clojure.string :as str]
            [rpkarlsson.utils :as u]))

(def operation->cycles
  {:addx 2
   :noop 1})

(def sample
  "addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop")

(def cycles
  #{20, 60, 100, 140, 180, 220})

(defn calculate-signal-strength
  [xs cycle register]
  (if (contains? cycles cycle)
    (do (println cycle register)
      (conj xs (* cycle register)))
    xs))

(defn solve
  [{:keys [register cycle signal-strengths] :as state} [op step]]
  (let [cycle' (inc cycle)
        register' (if (= :noop op)
                    register
                    (+ register step))]
    (-> state
        (assoc :cycle cycle'
               :register register'
               :signal-strengths (calculate-signal-strength signal-strengths cycle' register)))))

(->> sample
     (str/split-lines)
     (map #(str/split % #" "))
     (map (juxt (comp keyword first)
                (comp (fnil parse-long "0") second)))
     (reduce (fn [coll [op _ :as step]]
               (if (not= op :noop)
                 (conj coll [:noop] step)
                 (conj coll step)))
             [])
     (reduce solve {:cycle 1 :register 1 :signal-strengths []}))
