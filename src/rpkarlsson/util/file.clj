(ns rpkarlsson.util.file
  (:require [clojure.java.io :as io]))

(defn day [n]
  (str "day_"
       (when (< n 10)
         "0")
       n
       ".txt"))

(defn xf-day-lines
  ([n xf]
   (xf-day-lines n xf []))
  ([n xf target]
   (with-open [rdr (io/reader (io/resource (day n)))]
     (into target xf (line-seq rdr)))))
