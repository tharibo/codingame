(ns Player
  (:gen-class))

; Auto-generated code below aims at helping you parse
; the standard input according to the problem statement.

(defn get-mountain-to-shoot []
  (loop [i 8
         mountain-to-shoot -1
         mountain-to-shoot-height -1]
    (if-not (> i 0)
      mountain-to-shoot
      (let [mountainH (read) mountainIndex (- 8 i)]
; mountainH: represents the height of one mountain, from 9 to 0. Mountain heights are provided from left to right.
        (recur
          (dec i)
          (if (> mountainH mountain-to-shoot-height)
            mountainIndex
            mountain-to-shoot)
          (if (> mountainH mountain-to-shoot-height)
            mountainH
            mountain-to-shoot-height)
        )
      ))))

(defn -main [& args]
  (while true
    (let [spaceX (read)
          spaceY (read)
          mountain-to-shoot (get-mountain-to-shoot)]

      (binding [*out* *err*]
        (println mountain-to-shoot))
      
      ; either:  FIRE (ship is firing its phase cannons) or HOLD (ship is not firing).
      (if (= mountain-to-shoot spaceX)
            (println "FIRE")
            (println "HOLD"))
    )))