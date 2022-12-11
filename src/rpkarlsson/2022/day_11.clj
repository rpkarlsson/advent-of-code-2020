(ns rpkarlsson.2022.day-11
  (:require
   [clojure.string :as str]
   [clojure.math.numeric-tower :as nt]))

(def sample
  "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1")

(defn parse-op
  [s]
  (map #(cond
          (= "old" %) :old
          (#{"+" "*"} %) %
          :else (bigint %))
       (drop 4
             (-> s
                 (str/replace "Operation: new" "")
                 (str/split #" ")))))

(defn make-round-seq
  [no-of-monkeys]
  (repeat
   (range no-of-monkeys)))

(defn parse-monkey
  [s]
  (let [[monkey-str items-str op-str test-str if-true if-false] (->> s (str/split-lines))]
    {:monkey (parse-long (re-find #"\d+" monkey-str))
     :items (into (clojure.lang.PersistentQueue/EMPTY) (map parse-long (re-seq #"\d+" items-str)))
     :operation (parse-op op-str)
     :test (parse-long (re-find #"\d+" test-str))
     :if-true (parse-long (re-find #"\d+" if-true))
     :if-false (parse-long (re-find #"\d+" if-false))
     :inspections 0}))

(def str->op
  {"+" +
   "*" *})

(defn operate
  [stress xs]
  (let [[a op b] (->> xs
                      (map #(if (= :old %)
                              stress
                              %)))]
    ((str->op op) a b)))

(defn test-stress
  [test stress]
  (zero? (mod stress test)))

(defn perform-turn
  [stress-divider state round]
  (loop [state' state]
    (if (empty? (get-in state' [round :items]))
      state'
      (let [m (get state' round)
            item (peek (:items m))
            lcm (reduce nt/lcm (map :test state))
            new-stress (bigint (/ (mod (operate item (:operation m)) lcm) stress-divider))
            test (test-stress (:test m) new-stress)]
        (recur
         (-> state'
             (update-in [round :items] pop)
             (update-in [round :inspections] inc)
             (update-in [(if test (:if-true m) (:if-false m)) :items] conj new-stress)))))))

(defn perform-round
  [stress-divider state round]
  (reduce (partial perform-turn stress-divider) state round))

(defn solve
  [steps lower-stress-factor input]
  (let [state (->> (str/split input #"\n\n")
                   (mapv parse-monkey))
        rounds (take steps (make-round-seq (count state)))]
    (->> (reduce (partial perform-round lower-stress-factor) state rounds)
         (sort-by :inspections)
         (reverse)
         (take 2)
         (map :inspections)
         (apply *))))

;; Part 1
;; (solve 20 3 sample)
;; (->> "resources/2022/day_11.txt" slurp (solve 20 3))

;; Part 2
;;
;; Could not solve this one myself. Had to take to the internets.
;; Thanks Clojurians slack.
;; https://en.wikipedia.org/wiki/Least_common_multiple
;;
;; (solve 10000 1 sample)
;; (->> "resources/2022/day_11.txt" slurp (solve 10000 1))
