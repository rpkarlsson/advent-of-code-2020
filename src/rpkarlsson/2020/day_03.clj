(ns rpkarlsson.2020.day-03
  (:require
   [criterium.core :as c]
   [rpkarlsson.util.file :as file]))

(defn trees-in-row
  [row]
  (->> row
       (map-indexed (fn [idx char]
                      (when (= \# char)
                        idx)))
       (remove nil?)
       (into #{})))

(defn tree-count
  [lines {:keys [move-row move-column]}]
  (let [width (count (first lines))
        row->trees (map trees-in-row lines)]
    (loop [row 1
           column 1
           trees 0
           steps (count row->trees)]
      (if (zero? steps)
        trees
        (recur (+ row move-row)
               (+ column move-column)
               (if (contains? (nth row->trees (dec row) #{})
                              (mod (dec column) width))
                 (inc trees)
                 trees)
               (dec steps))))))

(defn part-1
  []
  (tree-count (file/xf-day-lines 3 identity)
              {:move-column 3 :move-row 1}))

(def moves
  [{:move-column 1 :move-row 1}
   {:move-column 3 :move-row 1}
   {:move-column 5 :move-row 1}
   {:move-column 7 :move-row 1}
   {:move-column 1 :move-row 2}])

(defn part-2
  []
  (let [lines (file/xf-day-lines 3 identity)]
    (->> moves
         (map #(tree-count lines %))
         (remove zero?)
         (reduce *))))

(comment
  (c/bench (part-1))
  (c/bench (part-2)))
