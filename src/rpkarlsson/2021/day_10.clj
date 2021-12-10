(ns rpkarlsson.2021.day-10
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def sample
  "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]")

(def start->end
  {\( \)
   \{ \}
   \[ \]
   \< \>})


(def char->point
  {\) 3
   \] 57
   \} 1197
   \> 25137})


(def closing (-> start->end vals set))

(def input
  (with-open [r (io/reader (io/resource "2021/day_10.txt"))]
    (->> (line-seq r)
         (mapv (partial map identity)))))

     ;; [(->> "<()}" #_"<([]){()}[{}])" #_"[({(<(())[]>[[{[]{<()<>>" (map identity))]
     ;; (map (partial map identity) (str/split sample #"\n"))
     ;; (take 1)

(->> input
     (map (fn [line]
            (loop [current-line line
                   queue '()
                   correct []]
              (let [[current] current-line
                    expected (get start->end (first queue))]
                (cond
                  (empty? current-line)
                  nil

                  (= expected current)
                  (recur (rest current-line) (pop queue) (conj correct current))

                  (and (contains? closing current)
                       (not= current (peek queue)))
                  current

                  :else
                  (recur (rest current-line) (conj queue current) (conj correct current)))))))

     (remove nil?)
     (map char->point)
     (reduce +))
