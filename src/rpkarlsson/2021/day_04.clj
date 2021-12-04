(ns rpkarlsson.2021.day-04
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.java.io :as io]))

(def sample
  "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7
")

(defn winner [board draws]
  (cond
    (some #(set/subset? % draws) (map #(into #{} %) board)) board
    (some #(set/subset? % draws) (map #(into #{} %)(apply map (fn [& args] args) board))) board
    :else nil))

(comment
  (winner [[1 2 3] [4 5 6]] #{1 2 3})
  (winner [[1 2 3] [4 5 6]] #{1 5})
  (winner [[1 2 3] [4 5 6]] #{1 4})
  (winner [[1 2 3] [4 5 6] [7 8 9]] #{1 2 5 8})
  ,)

(defn sum-score
  [board draws]
  (->> board
       (mapcat identity)
       (remove draws)
       (reduce +)))

(def boards+draw-order
  (let [sections (str/split #_sample (slurp (io/resource "2021/day_04.txt")) #"\n\n")
        draw-order (map #(Integer/parseInt %) (str/split (first sections) #","))
        boards (->> (rest sections)
                    (map #(str/split % #"\n"))
                    (map (partial map (fn [s] (str/trim s))))
                    (map (partial map (fn [s] (str/replace s "  " " "))))
                    (map (partial map (fn [s] (str/split s #" "))))
                    (map (partial map (partial map (fn [s] (Integer/parseInt s))))))]
    [boards draw-order]))

(defn draws-seq
  ([xs]
   (draws-seq xs 1))
  ([xs i]
   (when-not (= i (count xs))
     (lazy-seq (cons (take i xs) (draws-seq xs (inc i)))))))

(defn find-winners
  []
  (let [[boards draw-order] boards+draw-order]
    (->> (draws-seq draw-order)
         (reduce (fn [winners draw]
                   (let [winning-boards (into #{} (mapcat #(map second %) winners))
                         next-winning-boards (->> boards
                                                  (reduce (fn [draw-winners next-board]
                                                            (if (and (not (contains? winning-boards next-board))
                                                                     (winner next-board (into #{} draw)))
                                                              (conj draw-winners next-board)
                                                              draw-winners))
                                                          []))]
                     (if (empty? next-winning-boards)
                       winners
                       (merge winners
                              (map #(vector draw %) next-winning-boards)))))
                 []))))

(defn part-1
  []
  (let [[drawn winning-board] (ffirst (find-winners))]
       (* (last drawn) (sum-score winning-board (into #{} drawn)))))

(defn part-2
  []
  (let [[drawn winning-board] (last (last (find-winners)))]
    (* (last drawn) (sum-score winning-board (into #{} drawn)))))

(comment
  (part-1)

  (part-2)

  ,)
