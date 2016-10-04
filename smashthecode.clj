(ns Player
  (:gen-class))

(def COLMAX 6)
(def ROWMAX 12)

(defn debug-value [& values]
    (do (binding [*out* *err*]
           (apply println values))
         values))
     
(defn column [grid c]
    (reduce #(conj %1 (first (drop c %2))) [] grid))

(defn clean-column [grid c]
    (drop-while #(= \. %1) (column grid c)))

(defn clean-column-ignore-skulls [grid c]
    (drop-while #(or (= \. %1)
                     (= \0 %1))
                 (column grid c)))

(defn level [column]
    (count (drop-while #(= \. %1) column)))

(defn first-items
    ([grid] (first-items grid false))
    ([grid ignore-skulls?]
     (let [clean (if ignore-skulls? clean-column-ignore-skulls clean-column)
           columns (map (partial clean grid) (range COLMAX))]
        (map first columns))))
     
(defn find-matching-column [grid color]
    (first (keep-indexed #(if (= color %2) %1) (first-items grid))))

(defn find-matching-column2 [grid color]
    (first (keep-indexed #(if (= color %2) %1) (first-items grid true))))

(defn min-index [coll]
    (first (apply min-key second (map-indexed vector coll))))

(defn lowest-column [grid]
    (or (min-index (map level (map (partial column grid) (range COLMAX))))
        0))
    
(defn max-height [grid]
    (apply max (map level (map (partial column grid) (range COLMAX)))))
    
(defn hneighbors [grid c]
    (let [currcol (clean-column grid c)
          prevcol (clean-column grid (dec c))
          nextcol (clean-column grid (inc c))
          lvl (count currcol)
          prev-color (first (drop lvl (reverse prevcol)))
          next-color (first (drop lvl (reverse nextcol)))]
      (remove nil? [prev-color next-color])))
  
(defn find-column-with-matching-neighbor [grid color]
    (let [neighbors (map (partial hneighbors grid) (range COLMAX))
          matching-neighbors (keep-indexed #(if (some #{color} %2) %1) neighbors)]
      (first matching-neighbors)))

(defn find-groups
  ([grid] (find-groups [] [0 0]))
  ([grid groups [row col]])
  )


(defn read-next-pairs []
    (loop [i 8
           pairs []]
      (if (<= i 0)
        pairs
        (let [[colorA colorB] (clojure.string/split (read-line) #" ")]
          ; colorA: color of the first block
          ; colorB: color of the attached block
          (recur (dec i)
                 (conj pairs [(first colorA) (first colorB)]))))))

(defn read-grid []
    (loop [i ROWMAX
           rows []]
      (if (<= i 0)
          rows
            ; row: One line of the map ('.' = empty, '0' = skull block, '1' to '5' = colored block)
            (let [row (read-line)]
                (recur (dec i)
                       (conj rows row))))))

(defn -main [& args]
  (loop [c 1]
    (let [next-pairs (read-next-pairs)
          grid (read-grid)
          opponent-grid (read-grid)
          next-pair-color (first (first next-pairs))
          matching-column (find-matching-column grid next-pair-color)
          matching-column2 (find-matching-column2 grid next-pair-color)
          neighbor-matching-column (find-column-with-matching-neighbor grid next-pair-color)
          lowest-c (lowest-column grid)
          height (max-height grid)
          ]
      ;(doseq [x (map debug-value grid)])
      (debug-value height)
      (if (>= height 7)
          (println (or
                       neighbor-matching-column
                       matching-column
                       lowest-c)
                   "This is getting hot.")
          (println (or 
                       c
                       (inc (mod (dec (read-string (str next-pair-color))) 3))
                       )
                   "Vers l'infini et au-delà !"))
      (recur (inc (mod (inc c) 3))))))