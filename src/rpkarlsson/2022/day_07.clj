(ns rpkarlsson.2022.day-07
  (:require [clojure.string :as str]))

(def sample
  "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k")

(defn run-command [state command]
  (cond
    (= "$ cd .." (and (= 7 (count command))
                      (subs command 0 7 )))
    (update state :current-path (comp vec drop-last))

    (= "$ cd" (subs command 0 4))
    (update state :current-path conj (subs command 5))

    (= "$ ls" (subs command 0 4))
    (assoc-in state [:files (:current-path state)] [])

    :else state))

(defn add-file [state [size file]]
  (update-in state [:files (:current-path state)] conj [file size]))

(defn run [state next]
  (cond
    (= \$ (first next)) (run-command state next)
    (= "dir" (subs next 0 3)) state
    :else (add-file state (str/split next #" "))))

(defn total-size
  [files path]
  (->> files
       (filter (fn [[p _]]
                 (= path (take (count path) p))))
       (remove nil?)
       (mapcat second)
       (map second)
       (map parse-long)
       (reduce +)))

(defn solve [input]
  (let [files (->> input
                    str/split-lines
                    (reduce run {:current-path [] :files {}})
                    :files)]
    (->> files
         keys
         (map (partial total-size files))
         (filter #(<= % 100000))
         (reduce +))))

#_(solve sample)
#_(-> "resources/2022/day_07.txt" slurp solve)

(defn solve-2 [input]
  (let [files (->> input
                   str/split-lines
                   (reduce run {:current-path [] :files {}})
                   :files)
        size (total-size files ["/"])
        free-size (- 70000000 size)
        size-needed 30000000]
    (->> files
         keys
         (map #(total-size files %))
         sort
         (filter #(<= size-needed (+ free-size %)))
         first)))

#_(solve-2 sample)
#_(-> "resources/2022/day_07.txt" slurp solve-2)
