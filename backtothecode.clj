(ns Player
  (:gen-class))

(def nullTarget [-1 -1])

; Auto-generated code below aims at helping you parse
; the standard input according to the problem statement.

(defn readOpponents [opponentCount]
  ;(binding [*out* *err*]
  ;  (println (str "opponentCount " opponentCount)))
  (loop [i opponentCount
         opponents []
         ]
    ;(binding [*out* *err*]
    ;  (println opponents))
    (if (<= i 0)
      opponents
      (let [opponentX (read)
            opponentY (read)
            opponentBackInTimeLeft (read)
            _ (read-line)]
        ; opponentX: X position of the opponent
        ; opponentY: Y position of the opponent
        ; opponentBackInTimeLeft: Remaining back in time of the opponent

        (recur (dec i)
               (conj opponents {:position [opponentX opponentY]
                                :backInTimeLeft opponentBackInTimeLeft}))))))

(defn readBoard []
  (loop [i 20
         board ""]
    (if (<= i 0)
      board
      (let [line (read-line)]
        ; line: One line of the map ('.' = free, '0' = you, otherwise the id of the opponent)
        ;(binding [*out* *err*]
        ;  (println (str "readBoard " i " " line)))
        (recur (dec i)
               (str board line))))))

(defn readOneGameTurn [opponentCount]
  (let [gameRound ((fn [] (let [foo (read)] (binding [*out* *err*] (println foo)) foo)))
        _ (read-line)
        position ((fn [] (let [x (read) y (read)] (binding [*out* *err*] (println x ", " y)) [x y])))
        backInTimeLeft ((fn [] (let [foo (read)] (binding [*out* *err*] (println foo)) foo)))
        __ (read-line)
        game { :gameRound gameRound
               :position position
               :backInTimeLeft backInTimeLeft
               :opponents (readOpponents opponentCount)
               :board (readBoard)}]
    ;(binding [*out* *err*]
    ;  (println "readOneGameTurn: " game))
    game))

(defn getColor [[x y] board]
  (let [color (get board (+ x (* 35 y)))]
    ;(binding [*out* *err*]
    ;  (println "color in : " x ", " y " : " color))
    color))

(defn closerPositions [position]
  "Lazy list of positions closer to given position"
  (map (fn [[x1 y1] [x2 y2]] [(+ x1 x2) (+ y1 y2)])
       (repeat position) [ [0 -1]
                           [1 0]
                           [0 1]
                           [-1 0]
                           [-1 -1]
                           [1 -1]
                           [1 1]
                           [-1 1]
                           ]))

(defn findCloserEmptyPosition [currentGame]
  )

(defn validatePosition [[x y]]
  (if (or (nil? x) (nil? y))
    nil
    [(max 0 (min 34 x))
     (max 0 (min 19 y))]))

(defn computeNextPosition2 [lastTarget currentTarget currentGame]
  (binding [*out* *err*]
    (println "computeNextPosition2!"))
  [currentTarget
   (some #(let [color (getColor % (:board currentGame))]
            (and (= \. color) %))
         (map validatePosition (closerPositions (:position currentGame))))])

(defn computeNextPosition [lastTarget currentTarget currentGame]
  (cond
   (= currentTarget nullTarget) [lastTarget [0 0]]
   ;(= (:position currentGame) currentTarget lastTarget) (:position (get (:opponents currentGame) 0))
   (= (:position currentGame) currentTarget lastTarget) [currentTarget
                                                         (some #(let [color (getColor % (:board currentGame))]
                                                                  (and (= \. color) %))
                                                               [[34 0] [0 19] [34 19]])]
   (= (:position currentGame) currentTarget) [lastTarget lastTarget]
   :default [lastTarget currentTarget]))

(defn -main [& args]
  (let [opponentCount (read)
        _ (read-line)
        firstRoundGame (readOneGameTurn opponentCount)]
    ; opponentCount: Opponent count
    ;(binding [*out* *err*]
    ;  (println "First game state was read, proceeding..." " *** " (:position firstRoundGame)))
    (loop [currentGame firstRoundGame
           [lastTarget currentTarget] (computeNextPosition (:position firstRoundGame) nullTarget currentGame)]
      ;(binding [*out* *err*]
      ;  (println currentGame))

      ; action: "x y" to move or "BACK rounds" to go back in time
      (println (clojure.string/join " " currentTarget))
      (recur (readOneGameTurn opponentCount)
             (or (#(and (get % 1) %) (computeNextPosition lastTarget currentTarget currentGame))
                 (computeNextPosition2 lastTarget currentTarget currentGame))))))
