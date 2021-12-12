(ns rpkarlsson.2021.day-12
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [rpkarlsson.utils :refer :all]))

(def parse-input-xform
  (comp
   (map #(str/split % #","))
   (mmap #(str/split % #"-"))
   (map (partial mapcat identity))))

(def input
  (with-open [f (io/reader (io/resource "2021/day_12.txt"))]
    (into [] parse-input-xform (line-seq f))))

(def cave->path
  (->> input
       (mapcat (juxt identity reverse))
       (remove (comp #{"start"} second))
       (group-by first)
       (map (fn [[k v]] [k (map second v)]))
       (into {})))

(defn get-paths
  [next-fn]
  (loop [paths []
         queue (conj clojure.lang.PersistentQueue/EMPTY ["start"])]
    (let [first-in-queue (peek queue)
          current-cave (last first-in-queue)
          next (next-fn first-in-queue)]
      (cond
        (empty? queue)
        paths

        (#{"end"} current-cave)
        (recur (conj paths first-in-queue)
               (pop queue))

        (seq next)
        (recur paths (apply conj (pop queue) next))

        :else
        (recur paths (pop queue))))))

(defn part-1
  []
  (count (get-paths
          (fn [path]
            (->> (get cave->path (last path))
                 (remove (into #{} (filter #(= (str/lower-case %) %)) path))
                 (map #(conj path %)))))))

(defn part-2
  []
  (count (get-paths
          (fn [path]
            (let [small-caves-with-many-visits
                  (->> path
                       (group-by identity)
                       (filter (fn [[k v]]
                                 (and (< 1 (count v))
                                      (= k (str/lower-case k))))))]
              (cond->> (get cave->path (last path))
                (seq small-caves-with-many-visits)
                (remove (into #{} (filter #(= (str/lower-case %) %)) path))

                :always
                (map #(conj path %))))))))

(comment
  (part-1)
  (part-2)

  ,)
