(ns rpkarlsson.day-05
  (:require
   [criterium.core :as c]
   [clojure.set :as set]
   [rpkarlsson.util.file :as file]))

(def row-range (range 128))
(def col-range (range 8))
(def row-placement-length 7)

(defn position->seat-id [{:keys [row col]}] (+ (* row 8) col))

(def char->fn
  {\F #(take (/ (count %) 2) %)
   \B #(drop (/ (count %) 2) %)
   \L #(take (/ (count %) 2) %)
   \R #(drop (/ (count %) 2) %)})

(defn find-seat-index
  [seat-range target]
  (->> (map #(get char->fn %) target)
       (reduce (fn [xs f] (f xs)) seat-range)
       (first)))

(defn seat-position
  [placement]
  (let [[row col] (split-at row-placement-length placement)]
       {:row (find-seat-index row-range row)
        :col (find-seat-index col-range col)}))

(def input->seat-id
  (comp
   (map seat-position)
   (map position->seat-id)))

(defn part-1 [] (apply max (file/xf-day-lines 5 input->seat-id)))

(defn find-first-non-sequential-number
  [coll]
  (reduce
   (fn [prev next]
     (if (not= (inc prev) next)
       (reduced next)
       next))
   coll))

(defn part-2
  []
  (let [booked-seat-ids (file/xf-day-lines 5 input->seat-id)
        all-seats (into #{} (range (apply max booked-seat-ids)))
        booked-seats (into #{} booked-seat-ids)]
    (-> (set/difference all-seats booked-seats)
        (sort)
        (find-first-non-sequential-number))))

(comment
  (c/bench (part-1))
  (c/bench (part-2)))
