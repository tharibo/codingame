(ns Player
  (:gen-class))

(defn debug-value [value]
  (binding [*out* *err*]
    (println value))
  value)

(def MAXY 2800)
(def G -3.711)

(def optimalSpeed -35)
(def minPower 1)
(def maxPower 4)
(defn computePower [[x y :as pos] currentPower currentRotation targetRotation vSpeed minTargetX maxTargetX targetY]
  (debug-value (str vSpeed " " optimalSpeed))
  (debug-value (str x " " minTargetX " " maxTargetX))
  (let [maxPower (if (> (+ y vSpeed G) MAXY) 0 maxPower)
        minPower (if (< (+ y vSpeed G) targetY) maxPower minPower)]
    (debug-value ["minMax Power" minPower maxPower])
    (if (> (Math/abs currentRotation) 20)
      (if (= (> 0 currentRotation) (< 0 targetRotation))
        minPower
        maxPower)
      (if (or (<= vSpeed optimalSpeed)
              (or (< x minTargetX) (> x maxTargetX)))
        (min maxPower (inc currentPower))
        (if (< 5 (- vSpeed optimalSpeed))
          3
          (max minPower (dec currentPower)))))))

(defn computeRotation [[x y :as pos] currentRotation hSpeed vSpeed minTargetX maxTargetX targetY]
  (let [halfWidth (/ (- maxTargetX minTargetX) 2)
        middleX (+ minTargetX halfWidth)
        distanceToMid (- middleX x)
        inTarget? (or (> x maxTargetX) (< x minTargetX))
        ratio (max -1.0 (min 1.0 (/ distanceToMid (float halfWidth))))
        secondsToTargetY (/ (- targetY y) (if (= 0 vSpeed) 0.000001 vSpeed))
        secondsToTargetX (/ distanceToMid (if (= 0 hSpeed) 0.000001 hSpeed))
        targetHSpeed (cond (< (+ y vSpeed G) targetY) 0
                           (and (> (Math/abs hSpeed) 30) (< secondsToTargetY secondsToTargetX)) 0
                           (< x minTargetX) (* 0.05 (- minTargetX (+ x hSpeed)))
                           (> x maxTargetX) (* 0.05 (- maxTargetX (+ x hSpeed)))
                           :default 0)
        speedRatio (max -1.0 (min 1.0 (if (and (> (Math/abs hSpeed) 20) (= (> 0 targetHSpeed) (< 0 hSpeed)))
                                        (if (> 0 targetHSpeed) -1.0 1.0)
                                        (/ (- targetHSpeed hSpeed) 100.0))))
        targetAngle (cond (and inTarget? (< (+ y vSpeed G) targetY)) (* speedRatio -45.0)
                          (and (< (Math/abs hSpeed) 20) (<= (- y targetY) (- (* 4 optimalSpeed)))) 0
                          :default (* speedRatio -45.0))
        ]
    (debug-value [x minTargetX maxTargetX])
    (debug-value [y vSpeed targetY])
    (debug-value [targetHSpeed speedRatio])
    (debug-value targetAngle)
    (int targetAngle)))

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

(defn first-flat-area [surface]
  (loop [remaining-points (rest surface)
         starting-point (first surface)]
    (let [point (first remaining-points)]
      (if (= (second starting-point) (second point))
        [starting-point point]
        (recur (rest remaining-points)
               point)))))

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

        (let [
              [[x1 y1] [x2 y2]] (first-flat-area surface)
              newRotation (computeRotation [X Y] rotate hSpeed vSpeed x1 x2 y1)
              newPower (computePower [X Y] power rotate newRotation vSpeed x1 x2 y1)
              ]
          ; rotate power. rotate is the desired rotation angle. power is the desired thrust power.
          (println (str newRotation " " newPower))
        )
      ))))
