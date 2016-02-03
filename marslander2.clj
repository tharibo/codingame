(ns Player
  (:gen-class))

(defn debug-value [value]
  (binding [*out* *err*]
    (println value))
  value)

(def optimalSpeed -40)
(def minPower 0)
(def maxPower 4)
(defn computePower [currentPower vSpeed]
  (debug-value (min maxPower (inc currentPower)))
  (debug-value (str vSpeed optimalSpeed))
  (if (<= vSpeed optimalSpeed)
    (min maxPower (inc currentPower))
    (max minPower (dec currentPower))
  )
)

(defn computeRotation [currentRotation currentHSpeed minTargetX maxTargetX])

(defn readLand []
  (let [surfaceN (read)]
    (loop [i surfaceN
           surface []]
      (if (<= i 0)
        surface
        (let [landX (read) landY (read)]
          ; landX: X coordinate of a surface point. (0 to 6999)
          ; landY: Y coordinate of a surface point. By linking all the points together in a
          ; sequential fashion, you form the surface of Mars.
        (recur (dec i)
               (conj surface [landX landY])))))))

(defn landing-zones [surface]
  (let [grouped-by-height (reduce (fn [coll new]
                                    (let [new-height (second new)
                                          current-height (second (first (last coll)))]
                                      (if (= new-height current-height)
                                        (update-in coll [(dec (count coll))] conj new)
                                        (conj coll [new])))) [] surface)]
    (filter #(> (count %) 1) grouped-by-height)
    ;grouped-by-height
    ))

(def new [0 0])
(def coll [])
(second (first (first coll)))
(conj coll [new])

(def test-surface [[0 50] [1000 20] [1500 20] [2000 99] [3000 199] [4000 200] [6000 200]])
(landing-zones test-surface)

(defn -main [& args]
  (let [surface (debug-value (readLand))]
    (while true
      (let [X (read)
            Y (read)
            hSpeed (read)
            vSpeed (read)
            fuel (read)
            rotate (read)
            power (read)]
        ; hSpeed: the horizontal speed (in m/s), can be negative.
        ; vSpeed: the vertical speed (in m/s), can be negative.
        ; fuel: the quantity of remaining fuel in liters.
        ; rotate: the rotation angle in degrees (-90 to 90).
        ; power: the thrust power (0 to 4).

        ; (binding [*out* *err*]
        ;   (println "Debug messages..."))

        (let [newPower (computePower power vSpeed)
              newRotation 0]
          ; rotate power. rotate is the desired rotation angle. power is the desired thrust power.
          (println (str newRotation " " newPower))
        )
      ))))
