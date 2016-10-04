(ns Player
  (:gen-class))

(defn debug-value [& values]
  (binding [*out* *err*]
    (apply println values))
  values)

; Auto-generated code below aims at helping you parse
; the standard input according to the problem statement.

(defn -main [& args]
  (let [W (read) H (read) N (read) X0 (read) Y0 (read) _ (read-line)]
    ; W: width of the building.
    ; H: height of the building.
    ; N: maximum number of turns before game over.
    (debug-value "W" W "H" H "N" N "X0" X0 "Y0" Y0)
    (loop [x X0
           y Y0
           min-x 0
           min-y 0
           max-x (dec W)
           max-y (dec H)
           jump-factors [0.5 0.5]
           history '()]
      (debug-value "x" x "y" y)
      (let [bombDir (read-line)
            y-jump-factor (second jump-factors)
            x-jump-factor (first jump-factors)
            yOffset (cond (some #{\U} bombDir) (min -1 (int (* (- min-y y) y-jump-factor)))
                          (some #{\D} bombDir) (max 1 (int (* (- max-y y) y-jump-factor)))
                          :default 0)
            xOffset (cond (some #{\L} bombDir) (min -1 (int (* (- min-x x) x-jump-factor)))
                          (some #{\R} bombDir) (max 1 (int (* (- max-x x) x-jump-factor)))
                          :default 0)
            newmin-x (if (>= xOffset 0) x min-x)
            newmin-y (if (>= yOffset 0) y min-y)
            newmax-x (if (<= xOffset 0) x max-x)
            newmax-y (if (<= yOffset 0) y max-y)
            newhistory (cons bombDir history)
            history-y (map (partial filter #{\U\D}) newhistory)
            history-x (map (partial filter #{\L\R}) newhistory)
            nb-same-dir-x (count (first (partition-by identity history-x)))
            nb-same-dir-y (count (first (partition-by identity history-y)))
            nb-opposite-dir-x (count (first (partition-by identity (partition 2 history-x))))
            nb-opposite-dir-y (count (first (partition-by identity (partition 2 history-y))))
            newjump-factor-x (if (> nb-same-dir-x nb-opposite-dir-x) (+ 0.5 (* 0.1 nb-same-dir-x)) (- 0.5 (* 0.1 nb-opposite-dir-x)))
            newjump-factor-y (if (> nb-same-dir-y nb-opposite-dir-y) (+ 0.5 (* 0.1 nb-same-dir-y)) (- 0.5 (* 0.1 nb-opposite-dir-y)))
            ]

        ; bombDir: the direction of the bombs from batman's current location (U, UR, R, DR, D, DL, L or UL)
        (debug-value "bombDir" bombDir)
        (debug-value "jump-factors" jump-factors)
        (debug-value "history-x" history-x)
        (debug-value "history-y" history-y)
        (debug-value "rect" min-x min-y max-x max-y)
        (debug-value "xOffset" xOffset "yOffset" yOffset)
        (debug-value "newrect" newmin-x newmin-y newmax-x newmax-y)

        ; the location of the next window Batman should jump to.
        (println (+ x xOffset) (+ y yOffset))
        (recur (+ x xOffset)
               (+ y yOffset)
               newmin-x
               newmin-y
               newmax-x
               newmax-y
               [newjump-factor-x newjump-factor-y]
               newhistory)))))
