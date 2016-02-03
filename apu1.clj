(ns Player
  (:gen-class))

; Don't let the machines win. You are humanity's last hope...

(defn debug-value [value]
  (binding [*out* *err*]
    (println value))
  value)

(defn read-board [height]
  (loop [i 0
         nodes []]
      (if (< i height)
        (let [line (read-line)]
          ; line: width characters, each either 0 or .
          (recur (inc i)
                 (concat nodes
                         (debug-value (keep-indexed (fn [index n]
                                                      (when (= \0 n) [index i]))
                                                    line)))))
        nodes)))

(defn neighbors [nodes node-index]
  (let [node (nth nodes node-index)
        x (first node)
        y (second node)
        ;same-line (filter #(= y (second %)) nodes)
        ;same-column (filter #(= x (first %)) nodes)
        ;right-neighbor (first (filter #(> (first %) x) same-line))
        ;bottom-neighbor (first (filter #(> (first %) y) same-line))
        r-n-index (first (keep-indexed (fn [index n]
                                         (if (and (= y (second n))
                                                    (> index node-index))
                                           index)) nodes))
        b-n-index (first (keep-indexed (fn [index n]
                                         (if (and (= x (first n))
                                                    (> index node-index))
                                           index)) nodes))
        ]
    [r-n-index b-n-index]))

(defn node-as-string [node]
  (if node
    (str (first node) " " (second node))
    "-1 -1"))

(defn node-and-neighbors-string [nodes node-index]
  (let [node (nth nodes node-index)
        [r-index b-index] (neighbors nodes node-index)
        r-neighbor (or (and r-index (nth nodes r-index)) nil)
        b-neighbor (or (and b-index (nth nodes b-index)) nil)]
    (str (node-as-string node) " "
         (node-as-string r-neighbor) " "
         (node-as-string b-neighbor))))

; Three coordinates: a node, its right neighbor, its bottom neighbor
(defn -main [& args]
  (let [width (read)
        height (read)
        _ (read-line)
        nodes (debug-value (read-board height))
        ]
    (doall (map println
         (map-indexed (fn [idx itm] (node-and-neighbors-string nodes idx)) nodes)))))





;(defn get-line [board y]
;  (let [w (:width board)
;        begin (* y w)
;        end (+ begin w)]
;    (subs (:data board) begin end)))
;
;(defn get-sub-line [board y startY]
;  (drop startY (get-line board y)))
;
;(defn get-sub-col [board x startX]
;  (drop startX (get-col board x)))
;
;(defn get-col
;  ( [board x]
;    get-col board x (dec (:height board)) "")
;  ( [board x y result]
;    (if (< 0 y)
;      result
;      (recur board
;             x
;             (dec y)
;             (str (get-xy board x y)
;                  result)))))
;
;(defn get-xy [board x y]
;  (get (:data board) (+ x (* y (:width board)))))
;
;(defn find-first-neighbor [s]
;  (.indexOf s \0))
;
;(defn find-first-neighbors [board x y]
;  (let [line (get-sub-line board y)
;        col (get-sub-col board x)
;        line-neighbor (find-first-neighbor line)
;        col-neighbor (find-first-neighbor col)
;        line-neighbor-coords (if (= line-neighbor -1)
;                               [-1 -1]
;                               [(+ x line-neighbor) y])
;        col-neighbor-coords (if (= col-neighbor -1)
;                              [-1 -1]
;                              [x (+ y col-neighbor)])]
;    (list line-neighbor-coords col-neighbor-coords)))
;
;
;(defn create-board [data width height]
;  {:data data
;   :width width
;   :height height})
;
;(defn find-nodes-and-neighbors [board])
