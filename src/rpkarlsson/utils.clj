(ns rpkarlsson.utils)


(defn mmap
  ([f]
   (map (partial map f)))
  ([f coll]
   (map (partial map f) coll)))
