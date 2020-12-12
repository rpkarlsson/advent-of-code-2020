(ns rpkarlsson.day-11
  (:require
   [rpkarlsson.util.file :as file]))

(defn adjecent-coords
  [row col]
  (let [rows (range (- row 1) (+ row 2))
        cols (range (- col 1) (+ col 2))
        coords (->> (for [x rows
                          y cols]
                      [x y]))]
    (concat (take 4 coords) (drop 5 coords))))

(defn get-coord
  [[row col] xs]
  (-> xs
      (nth row nil)
      (nth col nil)))

(defn neighbours
  [row-index col-index state]
  (->> (adjecent-coords row-index col-index)
       (map #(get-coord % state))))

(defn apply-rules
  [state row-index row]
  (->> row
       (map-indexed (fn [col-index v]
                      (if (= \. v)
                        \.
                        (cond
                          (and (= \L v)
                               (not (some #{\#} (neighbours row-index col-index state))))
                          \#

                          (and (= \# v)
                               (<= 4 (->> (neighbours row-index col-index state)
                                          (filter #{\#})
                                          (count))))
                          \L

                          :else v))))))

(defn run-until-stable
  [init-state]
  (loop [state init-state]
    (let [next-state (->> state (map-indexed (partial apply-rules state)))]
      (if (= state next-state)
        state
        (recur next-state)))))

(defn part-1
  []
  (let [init-state (file/xf-day-lines 11 (map #(map identity %)))]
    (->> (run-until-stable init-state)
         (mapcat identity)
         (filter #{\#})
         (count))))
