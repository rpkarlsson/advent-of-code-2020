(ns rpkarlsson.2021.day-08
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def sample
  "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")

(def sample-2
  "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce")

(def digits
  [#{\a \b \c \e \f \g}
   #{\c \f}
   #{\a \c \d \e \f \g}
   #{\a \c \d \f \g}
   #{\b \c \d \f}
   #{\a \b \d \f \g}
   #{\a \b \d \e \f \g}
   #{\a \c \f}
   #{\a \b \c \d \e \f \g}
   #{\a \b \c \d \f \g}])

(def input-xf
  (comp
   (map #(str/split % #"\|"))
   (map (partial map str/trim))
   (map (partial map #(str/split % #" ")))))

(defn part-1
  []
  (let [selected-digits (into #{}  (->> (vals (select-keys digits [1 4 7 8]))
                                        (map count)))]
    (with-open [f (io/reader (io/resource "2021/day_08.txt"))]
      (->> #_(str/split sample-2 #"\n")
           (line-seq f)
           (into [] input-xf )
           (map second)
           (mapcat (partial map set))
           (map count)
           (filter selected-digits)
           (count)))))

(comment
  (let [selected-digits (into #{} (vals (select-keys digits [1 4 7 8])))]
    (->> (into [] input-xf (str/split sample-2 #"\n"))
         (map second)
         (mapcat (partial map set))
         (filter selected-digits)))
  ,)

