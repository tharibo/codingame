(ns Player
  (:gen-class))

; from clojure.contrib.combinatorics
(defn cartesian-product
  "All the ways to take one item from each sequence"
  [& seqs]
  (let [v-original-seqs (vec seqs)
        step
        (fn step [v-seqs]
          (let [increment
                (fn [v-seqs]
                  (loop [i (dec (count v-seqs)), v-seqs v-seqs]
                    (if (= i -1) nil
                      (if-let [rst (next (v-seqs i))]
                        (assoc v-seqs i rst)
                        (recur (dec i) (assoc v-seqs i (v-original-seqs i)))))))]
            (when v-seqs
              (cons (map first v-seqs)
                    (lazy-seq (step (increment v-seqs)))))))]
    (when (every? seq seqs)
      (lazy-seq (step v-original-seqs)))))

;seed=620069462
;boxes=51

(defn debug-value [x]
  (binding [*out* *err*]
    (println x))
  x)

(defn display-board [board width]
    (let [lines (partition width board)]
      (debug-value board)
      (for [line lines]
        (debug-value line))))

(defn abs "(abs n) is the absolute value of n" [n]
  (cond
   (not (number? n)) (throw (IllegalArgumentException.
                             "abs requires a number"))
   (neg? n) (- n)
   :else n))


(defn is-digit? [c]
    (let [zero (int \0)
          nine (int \9)]
        (<= zero (int c) nine)))

(defn manhattan-dist [[x1 y1 :as p1] [x2 y2 :as p2]]
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

(def dist manhattan-dist)

;;;;;;;;;; A*
; from https://nakkaya.com/2010/06/01/path-finding-using-astar-in-clojure/

;(defn cost [curr start end]
;  (let [g (manhattan-dist start curr)
;        h (manhattan-dist curr end)
;        f (+ g h)] 
;    [f g h]))
;
;(defn edges [map width height closed [x y]]
;  (for [tx (range (- x 1) (+ x 2)) 
;        ty (range (- y 1) (+ y 2))
;        :when (and (>= tx 0)
;                   (>= ty 0)
;                   (<= tx width)
;                   (<= ty height)
;                   (not= [x y] [tx ty])
;                   (not= (nth (nth map ty) tx) 1)
;                   (not (contains? closed [tx ty])))]
;    [tx ty]))
;
;(defn path [end parent closed]
;  (reverse
;   (loop [path [end parent]
;          node (closed parent)]
;     (if (nil? node)
;       path
;       (recur (conj path node) (closed node))))))

;;;;;;;;;; A*

(defn read-entity []
  (let [entityType (read)
        owner (read)
        x (read)
        y (read)
        param1 (read)
        param2 (read)
        _ (read-line)]
    {:entity-type entityType,
     :owner owner
     :pos [x y]
     :param1 param1
     :param2 param2}))

(defn read-entities []
  (let [nb-entities (read)]
    (loop [i nb-entities
           entities []]
      (if (> i 0)
        (recur (dec i)
               (conj entities (read-entity)))
        entities))))

(defn read-board [height]
  (loop [i height
         board ""]
    (if (> i 0)
      (let [row (read-line)]
        (recur (dec i)
               (str board row)))
      board)))

(defn get-board [board [x y] width]
  (get board (+ x (* y width))))

(defn get-cells-radius [board [x y :as center] radius width height]
    (loop [[w h] [radius radius]
           cells []]
       (let [[px py :as pos] [(+ x w) (+ y h)]]
            (if (= [w h] [(- radius) (- radius)])
               cells
               (recur (if (= w (- radius))
                          [radius (dec h)]
                          [(dec w) h])
                      (if (and (>= px 0) (>= py 0) (< px width) (< py height))
                          (conj cells {:pos pos :val (get-board board pos width)})
                          cells))))))

(defn get-entities-at [entities [x y :as pos]]
  (filter #(= pos (:pos %)) entities))
                 

(defn first-box-in-radius [board origin radius width height]
  (let [cells-in-radius (get-cells-radius board origin radius width height)
        boxes-in-radius (filter #(= (:val %) \0) cells-in-radius)
        sorted-boxes (sort-by #(dist (:pos %) origin) boxes-in-radius)
        first-box (first sorted-boxes)]
    (:pos first-box)))

(defn validate-target [[x y :as target] box width height]
  (let [newx (if (< x 0) 1 (if (>= x width) (- width 2) x))
        newy (if (< y 0) 1 (if (>= y height) (- height 2) y))]
    [newx newy]))

(defn get-n-cells [board [x y :as from] direction n width height]
    (loop [i 1
           cells []]
       (let [[posx posy :as pos] (cond (= direction :RIGHT) [(+ x i) y]
                                       (= direction :LEFT) [(- x i) y]
                                       (= direction :UP) [x (- y i)]
                                       (= direction :DOWN) [x (+ y i)])]
          (if (or (> i n) (>= posx width) (>= posy height) (< posx 0) (< posy 0))
              cells
              (recur (inc i)
                     (conj cells {:pos pos :val (get-board board pos width)}))))))

(defn blows-box [cells]
  (let [first-item (first (drop-while #(= \. (:val %)) cells))]
    (if (nil? first-item)
      false
      (is-digit? (:val first-item)))))
(defn update-cell "Updates cell with entity type if they share a position" [[cell entity]]
  (if (and (= (:pos cell) (:pos entity))
           (> (:entity-type entity) 0))
    (assoc cell :val \i)
    cell))
(defn bombed-cells [cells]
  (let [bombed (take-while #(= \. (:val %)) cells)]
    bombed))

(defn nb-destroyed-for-pos ([board entities [tx ty :as target] radius width height debug]
    (let [all-cells (map #(get-n-cells board target % radius width height) [:RIGHT :LEFT :DOWN :UP])
          cells (map #(map update-cell (cartesian-product % entities)) all-cells)
          boxes (filter #(= true %) (map blows-box cells))]
      ;(debug-value ["first-item" (first (drop-while #(= \. (:val %)) down-cells))])
      (if debug (let [dentities (filter #(= [1 2] (:pos %)) entities)
                      entity (first dentities)
                      cell (first (filter #(= [1 2] (:pos %)) (first cells)))
                      ]
                  (debug-value ["cells" cells])
                  (debug-value ["cell" (nil? cell)])
                  (debug-value (map #(vector %1 %2) (first all-cells) entities))
                  (if (and entity cell) (do
                    (debug-value [entity (number? (:entity-type entity)) (> (:entity-type entity) 0)])
                    (debug-value cell)
                    ))
                  (debug-value ["boxes" boxes])))
      (count boxes)))
  ([board entities [tx ty :as target] radius width height]
          (nb-destroyed-for-pos board entities target radius width height false)))
  
(defn weighted-board [board entities width height radius player]
  (defn no-wall-box-pred [coords]
    (= \. (get-board board (:pos coords) width)))
  (defn no-bomb-pred [coords]
    (let [entities-there (get-entities-at entities (:pos coords))]
      (if (< 0 (count entities-there))
        (apply (partial not= 1) (map :entity-type entities-there))
        true)))
  (let [coordinates (for [x (range 0 width)
                          y (range 0 height)]
                      {:pos [x y]})
        valid-coordinates (filter (every-pred no-wall-box-pred no-bomb-pred) coordinates)
        nb-boxes-board (map #(into % {:nb (nb-destroyed-for-pos board entities (:pos %) radius width height)
                                      :dist (dist (:pos player) (:pos %))})
                            valid-coordinates)
        final-board (sort-by :nb > (take 3 (sort-by :dist (filter #(< 0 (:nb %)) nb-boxes-board))))]
        ;(debug-value ["plop1" (get-board board [0 2] width)])
        ;(debug-value ["plop1" (get-entities-at entities [0 2])])
        ;(debug-value ["plop1" (no-wall-box-pred {:pos [0 2]})])
        ;(debug-value ["plop1" (no-bomb-pred {:pos [0 2]})])
        ;(debug-value ["plop" (first (filter #(= [0 2] (:pos %)) nb-boxes-board))])
        ;(debug-value ["plop2" (take 5 (sort-by :dist (filter #(< 0 (:nb %)) nb-boxes-board)))])
        ;(debug-value ["final" final-board])
    final-board))

(defn -main [& args]
  (let [width (read) height (read) myId (read) _ (read-line)]
    (while true
      (let [board (read-board height)
            entities (read-entities)
            player (get entities myId)
            [boxx boxy :as box] (first-box-in-radius board (:pos player) 2 width height)
            target (if box
                     (validate-target [(inc boxx) boxy] box width height)
                     [(rand-int width) (rand-int height)])
            wboard (weighted-board board entities width height (dec (:param2 player)) player)
            target2 (:pos (first wboard))]

        ;(debug-value (nb-destroyed-for-pos board entities [0 2] 2 width height true))
        (debug-value ["wboard" wboard])
        (debug-value ["target2" target2])

        ; Write action to stdout
        (if (= target2 [nil nil])
          (println "BOMB 0 0")
          (if (= (:pos player) target2)
            (println "BOMB " (first target2) (second target2))
            (println "MOVE " (first target2) (second target2))))))))
