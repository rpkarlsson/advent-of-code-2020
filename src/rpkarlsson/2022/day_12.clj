(ns rpkarlsson.2022.day-12
  (:require
   [rpkarlsson.utils :as u]
   [loom.graph :as lgraph]
   [loom.alg :as lalg]))

(def sample
  "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi")

(defn find-coord [g n]
  (let [coords (u/generate-coords-in-grid (count g) (count (first g)))]
    (->> coords
         (filter #(= n (get-in g %)))
         (first))))

(defn elevation-char->number [char]
  (case char
    \S 1
    \E (- (int \z) 96)
    (- (int char) 96)))

(def start \S)
(def end \E)

(defn g->graph
  [g]
  (reduce (fn [coll coord]
            (let [height (get-in g coord)]
              (assoc coll coord
               (->> (u/neighbour-coords coord)
                    (map (fn [neighbour] [neighbour (get-in g neighbour)]))
                    (remove (comp nil? second ))
                    (filter (comp #(<= (dec %) height) second))
                    (into {})))))
          {}
          (u/generate-coords-in-grid (count g) (count (first g)))))

(defn ->graph
  [s]
  (let [char-grid (u/str->grid s)
        g (->> char-grid
               (u/mmap elevation-char->number)
               (map vec)
               vec)
        start (find-coord char-grid start)
        end (find-coord char-grid end)]
    {:graph (->> g g->graph)
     :start start
     :end end}))

(defn part-1 [input]
  (let [{:keys [graph start end]} (-> input ->graph)
        g (->> graph (map (fn [[k v]] [k (keys v)])) (into {}))]
    (-> g lgraph/digraph (lalg/bf-path start end) count dec)))

#_(part-1 sample)
#_(part-1 (slurp "resources/2022/day_12.txt"))

(defn part-2 [input]
  (let [{:keys [graph start end]} (-> input ->graph)
        possible-starts (->> graph
                             (mapcat second)
                             (filter (comp #{1} second))
                             (map first))
        g (->> graph (map (fn [[k v]] [k (keys v)])) (into {}))]
    (->> possible-starts
         (map #(-> g lgraph/digraph (lalg/bf-path % end)
                   count
                   dec))
         (sort)
         (remove neg?)
         (first))))

#_(part-2 sample)
#_(part-2 (slurp "resources/2022/day_12.txt"))
