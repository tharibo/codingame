(ns Player
  (:gen-class))

; Save humans, destroy zombies!

(defn debug-value [value]
  (binding [*out* *err*]
    (println value))
  value)

(defn read-ash-pos []
  (let [x (read) y (read)]
    [x y]))

(defn read-human []
  (let [id (read) x (read) y (read)]
    {:id id :pos [x y]}))

(defn read-all-humans []
  (let [human-count (read)]
    (loop [i human-count
           humans []]
      (if (> i 0)
        (recur (dec i)
               (conj humans (read-human)))
        humans))))

(defn read-zombie []
  (let [id (read) x (read) y (read) next-x (read) next-y (read)]
    {:id id :pos [x y] :next-pos [next-x next-y]}))

(defn read-all-zombies []
  (let [zombies-count (read)]
    (loop [i zombies-count
           zombies []]
      (if (> i 0)
        (recur (dec i)
               (conj zombies (read-zombie)))
        zombies))))

(defn square [x]
  ;(reduce * (repeat x 2)))
  (Math/pow x 2))

(defn square-dist [[x1 y1] [x2 y2]]
  (+ (square (- x2 x1)) (square (- y2 y1))))

(defn sqrt-dist [[x1 y1] [x2 y2]]
  (Math/sqrt (+ (square (- x2 x1)) (square (- y2 y1)))))

(defn manhattan-dist [[x1 y1] [x2 y2]]
  (Math/abs (+ (- x2 x1) (- y2 y1))))

(def dist sqrt-dist)

(dist [5000 0] [11500 7100])

(defn find-closer-entity [start-pos entities]
  ; Entities are expected to be dictionaries with a :pos key
  (defn keep-if-shorter [closer candidate]
    (let [candidate-pos (:pos candidate)
          closer-pos (:pos closer)
          closer-dist (dist start-pos closer-pos)
          candidate-dist (dist start-pos candidate-pos)]
      (if (< candidate-dist closer-dist) candidate closer)))
  (reduce keep-if-shorter entities))

(defn find-closer-to-target-entity [entities targets]
  (defn keep-if-closer [closer candidate]
    (let [candidate-pos (:pos candidate)
          closer-pos (:pos closer)
          candidate-target (find-closer-entity candidate-pos targets)
          closer-target (find-closer-entity closer-pos targets)
          candidate-dist (dist candidate-pos (:pos candidate-target))
          closer-dist (dist closer-pos (:pos closer-target))]
      (if (< candidate-dist closer-dist) candidate closer)))
  (reduce keep-if-closer entities))

(defn compute-dists [zombies humans]
  (defn update-and-store [updated-zombies z]
    (let [target (find-closer-entity (:pos z) humans)
          dist-to-target (dist (:pos z) (:pos target))
          new-zombie (assoc z :target target :dist-to-target dist-to-target)]
      (conj updated-zombies new-zombie)))
  (reduce update-and-store [] zombies))

(defn time-to-target [dist speed] (debug-value (/ dist speed)))

(defn -main [& args]
  (while true
    (let [ash-pos (read-ash-pos)
          humans (read-all-humans)
          zombies (compute-dists (read-all-zombies) (conj humans {:id :ASH :pos ash-pos}))
          _ (debug-value zombies)
          dangerous-zombies (filter #(not= :ASH (:id (:target %))) zombies)
          __ (debug-value dangerous-zombies)
          already-dead-zombies (filter #(>= (time-to-target (:dist-to-target %) 400)
                                            (time-to-target (- (dist ash-pos (:pos (:target %))) 2000) 1000))
                                       dangerous-zombies)
          __ (debug-value already-dead-zombies)
          closer-zombie (find-closer-entity ash-pos zombies)
          closer-zombie-to-target (first (sort-by :dist-to-target already-dead-zombies))
          target-pos (or (:next-pos closer-zombie-to-target) ash-pos)]

        ; Your destination coordinates
        (println (str (first target-pos) " " (second target-pos))))))
