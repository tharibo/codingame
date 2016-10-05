(ns Player
  (:gen-class))

; Shoot enemies before they collect all the incriminating data!
; The closer you are to an enemy, the more damage you do but don't get too close or you'll get killed.

(defn debug-value [value]
  (binding [*out* *err*]
    (println value))
  value)

(defn square [x]
  ;(reduce * (repeat x 2)))
  (Math/pow x 2))

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

(defn compute-all-distances [data enemies])

(defn compute-game-state [state]
  (let [wpos (:pos state)
        distances (compute-all-distances (:data state) (:enemies state))
        enemies-dist-wolf (map #(assoc % :wdist (dist wpos (:pos %))) (:enemies state))]
    (assoc state
           :distances distances
           :enemies (sort-by :wdist enemies-dist-wolf))))

(defn -main [& args]
  (while true
    (let [state (compute-game-state (read-game-state))]
      (debug-value (:data state))
      (debug-value (:enemies state))

      ; MOVE x y or SHOOT id
      (let [living-enemies (filter #(< 0 (:life %)) (:enemies state))
            target (or (first living-enemies) {:pos [0 0 ] :id 0 :life 0 :wdist 10000})
            x (first (:pos target))
            y (second (:pos target))
            id (:id target)
            d (:wdist target)]
        (if (and (> 5000 d) (< 0 (:life target)))
          (println "SHOOT" id)
          (println "MOVE " x y))))))
