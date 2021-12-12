(ns rpkarlsson.2021.day-12
  (:require
   [clojure.string :as str]
   [rpkarlsson.utils :refer :all]
   [clojure.java.io :as io]))

(def sample
  "start-A
start-b
A-c
A-b
b-d
A-end
b-end")

(def sample-2
 "dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc")

(def sample-3
  "fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW")

(def parse-input-xform
  (comp
   (map #(str/split % #","))
   (mmap #(str/split % #"-"))
   (map (partial mapcat identity))))

(def parsed-sample
  (into [] parse-input-xform (str/split sample-3 #"\n")))

(def input
  (with-open [f (io/reader (io/resource "2021/day_12.txt"))]
    (into [] parse-input-xform (line-seq f))))

(defn get-paths
  []
  (let [cave->path (->> #_parsed-sample
                        input
                        (mapcat (juxt identity reverse))
                        (remove (comp #{"start"} second))
                        (group-by first)
                        (map (fn [[k v]] [k (map second v)]))
                        (into {}))]
    (loop [paths []
           queue '(["start"])]
      (let [first-in-queue (first queue)
            current-cave (last first-in-queue)
            next (->> (get cave->path current-cave)
                      (remove (into #{} (filter #(= (str/lower-case %) %))first-in-queue))
                      (map #(conj first-in-queue %)))]
        (cond
          (empty? queue)
          paths

          (#{"end"} current-cave)
          (recur (conj paths first-in-queue)
                 (rest queue))

          (seq next)
          (recur paths
                 (concat next (rest queue)))

          :else
          (recur paths
                 (let [return (str (second (reverse first-in-queue)))]
                   (if (= return (str/upper-case return))
                     (conj (rest queue)
                           (conj first-in-queue return))
                     (rest queue)))))))))

(defn part-1
  []
  (count (get-paths)))



(defn step-2
  []
  (let [cave->path (->> #_parsed-sample
                        input
                        (mapcat (juxt identity reverse))
                        (remove (comp #{"start"} second))
                        (group-by first)
                        (map (fn [[k v]] [k (map second v)]))
                        (into {}))]
    (loop [paths []
           queue '(["start"])]
      (let [first-in-queue (first queue)
            current-cave (last first-in-queue)
            next-paths (get cave->path current-cave)
            next (if (empty? (->> first-in-queue
                                  (frequencies)
                                  (filter (fn [[k _]] (= (str/lower-case k) k)))
                                  (filter (fn [[_ v]] (< 1 v)))))
                   (->> next-paths
                        (map #(conj first-in-queue %)))
                   (->> next-paths
                        (remove (into #{} (filter #(= (str/lower-case %) %)) first-in-queue))
                        (map #(conj first-in-queue %))))]
        (cond
          (empty? queue)
          paths

          (#{"end"} current-cave)
          (recur (conj paths first-in-queue)
                 (rest queue))

          (seq next)
          (recur paths
                 (concat next (rest queue)))

          :else
          (recur paths
                 (let [return (str (second (reverse first-in-queue)))]
                   (if (= return (str/upper-case return))
                     (conj (rest queue)
                           (conj first-in-queue return))
                     (rest queue)))))))))

(comment
  (part-1)
  (count (step-2))

  ,)
