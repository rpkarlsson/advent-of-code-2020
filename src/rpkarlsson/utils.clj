(ns rpkarlsson.utils)


(defn mmap
  ([f]
   (map (partial map f)))
  ([f coll]
   (map (partial map f) coll)))


(defn mcatmap
  ([f]
   (mapcat (partial map f)))
  ([f coll]
   (map (partial map f) coll)))

(defn generate-coords-in-grid
  "Given a grid of size `x` and `y` generates all coords in that grid."
  [x y]
  (->> (for [x' (range 0 x)
            y' (range 0 y)]
        [x' y'])
       (map vec)
       vec))

(defn make-grid [rows columns value]
  (->> (range (* rows columns))
       (map (constantly value))
       (partition columns)
       (map vec)
       vec))
