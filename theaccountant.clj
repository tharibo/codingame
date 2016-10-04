(ns Player
  (:gen-class))

; Shoot enemies before they collect all the incriminating data!
; The closer you are to an enemy, the more damage you do but don't get too close or you'll get killed.

(defn debug-value [value]
  (binding [*out* *err*]
    (println value))
  value)

(defn sqrt-dist [[x1 y1] [x2 y2]]
  (Math/sqrt (+ (square (- x2 x1)) (square (- y2 y1)))))

(def dist sqrt-dist)

(defn read-game-state []
  (let [x (read)
        y (read)
        dataCount (read)
        state {:pos [x y]}
        data (loop [i dataCount
                    data []]
               (if (> i 0)
                 (let [dataId (read)
                       dataX (read)
                       dataY (read)]
                   (recur (dec i)
                          (conj data {:id dataId :pos [dataX dataY]})))
                 data))
        enemies (let [enemyCount (read)]
                  (loop [i enemyCount
                         enemies []]
                    (if (> i 0)
                      (let [enemyId (read) enemyX (read) enemyY (read) enemyLife (read)]
                        (recur (dec i)
                               (conj enemies {:id enemyId :pos [enemyX enemyY] :life enemyLife})))
                      enemies)))]
    (assoc state :data data :enemies enemies)))



(defn -main [& args]
  (while true
    (let [state (read-game-state)]
      ; MOVE x y or SHOOT id
      (println "MOVE 8000 4500"))))
