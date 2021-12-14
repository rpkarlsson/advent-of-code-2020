(ns rpkarlsson.2021.day-14
  (:require
   [rpkarlsson.utils :refer :all]
   [clojure.string :as str]
   [clojure.java.io :as io]))

(def sample
  "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C")

(def parsed-sample
  (let [[template insertions] (str/split sample #"\n\n")]
    [(map identity template)
     (->> (str/split-lines insertions)
                     (map #(str/split % #" -> "))
                     (map (fn [[k v]]
                            [(map identity k)
                             (first v)]))

                     (into {}))]))

(def parsed-input
  (let [[template insertions] (str/split (slurp (io/resource "2021/day_14.txt")) #"\n\n")]
    [(map identity template)
     (->> (str/split-lines insertions)
          (map #(str/split % #" -> "))
          (map (fn [[k v]]
                 [(map identity k)
                  (first v)]))
          (into {}))]))

(defn solve [steps]
  (let [[template insertion-rules] parsed-input]
    (loop [taken-steps 0
           p (->> template
                  (partition-all 2 1)
                  (frequencies))]
      (if (= taken-steps steps)
        p
        (recur (inc taken-steps)
               (->> p
                    (reduce (fn [coll [k v]]
                              (let [insertion (get insertion-rules k)]
                                (if-not insertion
                                  (update coll k (fnil + 0) v)
                                  (-> coll
                                      (update [(first k) insertion] (fnil + 0) v)
                                      (update [insertion (second k)] (fnil + 0) v)))))
                            {})))))))

(defn part-1
  []
  (let [elements-by-q (->> (solve 10)
                           (reduce (fn [coll [[a b] v]]
                                     (-> coll
                                         (update a (fnil + 0) v)))
                                   {})
                           (sort-by second))]
    (- (second (last elements-by-q))
       (second (first elements-by-q)))))

(defn part-2
  []
  (let [elements-by-q (->> (solve 40)
                           (reduce (fn [coll [[a b] v]]
                                     (-> coll
                                         (update a (fnil + 0) v)))
                                   {})
                           (sort-by second))]
    (- (second (last elements-by-q))
       (second (first elements-by-q)))))
