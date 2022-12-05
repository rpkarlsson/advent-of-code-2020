(ns rpkarlsson.2022.day-05
  (:require [clojure.string :as str]
            [rpkarlsson.utils :as u]))

(def sample
  "    [D]
[N] [C]
[Z] [M] [P]
 1   2   3

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")

(defn parse-moves
  [s]
  (->> (str/split-lines s)
       (map #(str/split % #" "))
       (map #(map vec (partition 2 %)))
       (u/mmap (juxt (comp keyword first) (comp parse-long second)))
       (map #(into {} %))))

(defn make-initial-stacks
  [init]
  (let [stacks (->> (range 0 (parse-long (str (last (last init)))))
                    (map (fn [_] '()))
                    (into []))
        boxes (->> init
                   (drop-last)
                   (map #(str/replace % #"    " " [] "))
                   (map #(str/split % #" "))
                   (map #(remove str/blank? %))
                   (reverse))]
    (reduce (fn [stacks row]
              (reduce-kv (fn [s idx box]
                           (if (= box "[]")
                             s
                             (update s idx conj box)))
                         stacks
                         (zipmap (range 10) row)))
            stacks
            boxes)))

(defn make-move
  [stacks {:keys [move from to]}]
  (let [to-move (take move (get stacks (dec from)))]
    (-> stacks
        (update (dec from) nthrest move)
        (assoc (dec to) (concat #_(reverse) to-move (get stacks (dec to)))))))
;; restore reverse to solve part 1

(defn solve
  [s]
  (let [[init' moves'] (str/split s #"\n\n")
        initial-stacks (make-initial-stacks (str/split-lines init'))]
    (map first
     (loop [moves (parse-moves moves')
            stacks initial-stacks]
       (if (empty? moves)
         stacks
         (recur (rest moves) (make-move stacks (first moves))))))))

#_(solve sample)
#_(-> "resources/2022/day_05.txt" slurp solve)
