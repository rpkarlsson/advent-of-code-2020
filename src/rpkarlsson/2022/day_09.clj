(ns rpkarlsson.2022.day-09
  (:require
   [rpkarlsson.utils :as u]
   [clojure.math :as math]))

(def sample
  "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")

(def direction->change
  {\U [-1 0]
   \D [1 0]
   \L [0 -1]
   \R [0 1]})

(defn adjacent?
  [[x1 y1] [x2  y2]]
  (and (<= (abs (- x1 x2)) 1)
       (<= (abs (- y1 y2)) 1)))

(defn parse
  [s]
  (->> s
       (re-seq #"(L|R|U|D) (\d+)" )
       (map (juxt (comp first second) (comp parse-long #(nth % 2))))))

(defn expand
  [[direction steps]]
  (repeat steps direction))

(def dg (u/make-grid 5 6 \.))

(defn print-grid
  [things]
  (println "\n------------\n")
  (doseq [l (->>  (reduce (fn [g thing]
                            (assoc-in g thing "#"))
                          dg
                          (last things)))]
    (println l)))

(defn move
  [coord [dx dy]]
  (-> coord
      (update 0 + dx)
      (update 1 + dy)))

(defn follow
  [coll follower]
  (let [leader (last coll)]
    (if (adjacent? follower leader)
      (conj coll follower)
      (let [dx (- (first leader) (first follower))
            dy (- (second leader) (second follower))]
        (conj coll
              [(long (+ (first follower) (math/signum dx)))
               (long (+ (second follower) (math/signum dy)))])))))

(defn run-steps [acc step]
  (let [[head & tail] (last acc)
        head' (move head (get direction->change step))
        rope (->> tail
                  (reduce follow [head']))]
    (conj acc rope)))

(defn solve
  [initial-state]
  (->> "resources/2022/day_09.txt"
       slurp
       parse
       (mapcat expand)
       (reductions run-steps initial-state)
       last
       (map last)
       (into #{})
       (count)))

(defn solve-1
  []
  (solve [(vec (repeat 2 [4 0]))]))

(def sample-2
  "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20")

(defn solve-2
  []
  (solve [(vec (repeat 10 [4 0]))]))


(comment
  ;; Print each step
  (do (println "\n---------- START ---------------\n")
      (doseq [t (reductions run-steps [[[4 0] [4 0]]] (->> sample parse (mapcat expand)))]
        (print-grid t)))

  ,)
