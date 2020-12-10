(ns rpkarlsson.day-09
  (:require [rpkarlsson.util.file :as file]))

(defn any-eq
  [xs target]
  (if (empty? xs)
    target
    (let [[x & rest] xs]
      (when-not (->> rest
                     (map #(+ x %))
                     (some #(= % target)))
        (recur rest target)))))

(defn part-1
  []
  (let [preamble-n 25]
    (loop [source (file/xf-day-lines 9 (map #(Long/parseLong %)))]
      (let [preamble (take preamble-n source)
            target (nth source preamble-n)]
        (if (any-eq preamble target)
          target
          (recur (drop 1 source)))))))


(defn lazy-growing-permutations
  ([input]
   (lazy-growing-permutations input 1 nil))
  ([input n prev]
   (let [next (take n input)]
     (when-not (= prev next)
       (lazy-seq (cons next (lazy-growing-permutations input (inc n) next)))))))

(def part-2
  []
  (let [target (part-1)]
    (loop [xs (file/xf-day-lines 9 (map #(Long/parseLong %)))]
      (let [found-range (->> (lazy-growing-permutations xs)
                             (reduce (fn [_ xs]
                                       (let [sum (reduce + xs)]
                                         (cond
                                           (= target sum) (reduced xs)
                                           (< target sum) (reduced nil)
                                           :else nil)))))]
        (cond
          (empty? xs) nil
          found-range (let [r (sort found-range)]
                        (+ (first r) (last r)))
          :else
          (recur (drop 1 xs)))))))
