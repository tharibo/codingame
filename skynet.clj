(ns Player
  (:gen-class))

; Auto-generated code below aims at helping you parse
; the standard input according to the problem statement.

(defn computeTargetSpeed [road gap platform coordX]
    (if (<= coordX road)
        (inc gap)
        0
        )
    )

(defn -main [& args]
  (let [road (read) gap (read) platform (read)]
    ; road: the length of the road before the gap.
    ; gap: the length of the gap.
    ; platform: the length of the landing platform.
    (while true
      (let [speed (read)
            coordX (read)
            targetSpeed (computeTargetSpeed road gap platform coordX)]
        ; speed: the motorbike's speed.
        ; coordX: the position on the road of the motorbike.
        
        (binding [*out* *err*]
            (println (str coordX " " road) ))
        
        ; A single line containing one of 4 keywords: SPEED, SLOW, JUMP, WAIT.
        (cond
            (= coordX (dec road)) (println "JUMP")
            (< speed targetSpeed) (println "SPEED")
            (> speed targetSpeed) (println "SLOW")
            :else (println "WAIT"))
    ))))