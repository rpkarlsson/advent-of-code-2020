(ns rpkarlsson.day-12
  (:require
   [rpkarlsson.util.file :as file]))

(def instruction-char->k
  {\N :north
   \S :south
   \E :east
   \W :west
   \L :left
   \R :right
   \F :forward})

(defn parse-instruction
  [s]
  [(instruction-char->k (first s))
   (Integer/parseInt (subs s 1))])

(def heading->degrees
  {:north 0
   :south 180
   :east 90
   :west 270})

(defn turn
  [instruction state]
  (let [current-direction (:direction state)
        [turn steps] instruction
        new-direction (case turn
                        :right (mod (+ current-direction steps) 360)
                        :left (mod (- current-direction steps) 360))]
    (assoc state :direction new-direction)))

(defn move [steps state]
  (let [current-direction (:direction state)
        new-position (case current-direction
                       0 (update-in state [:position :south] - steps)
                       90 (update-in state [:position :east] + steps)
                       180 (update-in state [:position :south] + steps)
                       270 (update-in state [:position :east] - steps))]
    new-position))

(defn make-move
  [state instruction]
  (let [[next-move steps] instruction]
    (case next-move
      :forward (move steps state)
      (:right :left) (turn instruction state)
      (assoc (->> (assoc state :direction (heading->degrees next-move))
                  (move steps))
             :direction (:direction state)))))

(defn step-1
  []
  (let [instructions (file/xf-day-lines 12 (map parse-instruction))
        init-state {:position {:east 0 :south 0} :direction 90}
        end (reduce make-move init-state instructions)]
    (+ (Math/abs (-> end :position :east))
       (Math/abs (-> end :position :south)))))
