(ns rpkarlsson.2020.day-08
  (:require [clojure.string :as str]
            [rpkarlsson.util.file :as file]))

(defn parse-step
  [s]
  (let [f (if (= \+ (first s))
            + -)
        n (Integer/parseInt (subs s 1))]
    [f n]))

(def parse-op+step (juxt (comp keyword first) (comp parse-step second)))

(def ->op+step-xf
  (comp
   (map #(str/split % #" "))
   (map parse-op+step)))

(defn perform-steps
  [{:keys [acc prev-steps steps] :as env} next-step-index]
  (let [[op [step-f step-n]] (get steps (mod next-step-index (dec (count steps))) )]
    (cond
      (contains? prev-steps next-step-index) acc

      (#{:acc} op)
      (recur (-> env
                 (update :acc step-f step-n)
                 (update :prev-steps conj next-step-index))
             (inc next-step-index))

      (#{:nop} op)
      (recur (-> env (update :prev-steps conj next-step-index))
             (inc next-step-index))


      (#{:jmp} op)
      (recur (-> env (update :prev-steps conj next-step-index))
             (step-f next-step-index step-n)))))


(defn step-1
  []
  (let [steps (file/xf-day-lines 8 ->op+step-xf)]
    (perform-steps {:acc 0 :prev-steps #{} :steps steps} 0)))

(defn perform-steps-2
  [{:keys [acc prev-steps steps] :as env} next-step-index]
  (let [[op [step-f step-n]] (get steps  next-step-index)]
    (cond
      (contains? prev-steps next-step-index) 0

      (= next-step-index (dec (count steps))) (step-f acc step-n)

      (#{:acc} op)
      (recur (-> env
                 (update :acc step-f step-n)
                 (update :prev-steps conj next-step-index))
             (inc next-step-index))

      (#{:nop} op)
      (recur (-> env (update :prev-steps conj next-step-index))
             (inc next-step-index))


      (#{:jmp} op)
      (recur (-> env (update :prev-steps conj next-step-index))
             (step-f next-step-index step-n)))))


(defn find-index
  [index [op v]]
  (when (#{:jmp :nop} op)
    [index op v]))

(defn find-next-index
  [last-altered steps]
  (->> (drop last-altered steps)
       (map-indexed find-index)
       (remove nil?)
       (take 1)
       (first)))

(defn part-2
  []
  (let [steps (file/xf-day-lines 8 ->op+step-xf)]
    (loop [last-changed-index 0]
      (let [[index-to-change operation _] (find-next-index last-changed-index steps)
            next-index (+ last-changed-index index-to-change)
            altered-steps (update steps next-index  assoc 0 (if (= :jmp operation) :nop :jmp))
            acc (perform-steps-2 {:acc 0 :prev-steps #{} :steps altered-steps} 0)]
        (if (not (zero? acc)) (dec acc)
          (recur (inc next-index)))))))
