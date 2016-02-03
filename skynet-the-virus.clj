(ns Player
  (:gen-class))

(defn debug-value [value]
  (binding [*out* *err*]
    (println value))
  value)

(defn read-links [n]
  (loop [i n
         links []]
    (if (<= i 0)
      links
      (let [N1 (read) N2 (read)]
        ; N1: N1 and N2 defines a link between these nodes
        (recur (dec i)
               (conj links [N1 N2]))))))

(defn read-exits [n]
  (loop [i n
         exits []]
    (if (<= i 0)
      exits
      (let [EI (read)]
        ; EI: the index of a gateway node
        (recur (dec i)
               (conj exits EI))))))

(defn find-targets [from links]
  (reduce #(if (= from (first %2))
             (conj %1 (second %2))
             (if (= from (second %2))
               (conj %1 (first %2))
               %1)) [] links))
(find-targets 1 [[0 1] [1 2] [2 3] [1 4] [2 4]])

(defn walk [path links]
  (let [path (seq path)
        current-node (first path)
        targets (find-targets current-node links)
        ]
    (reduce #(conj %1
                   (conj path %2))
            []
            (filter #(nil? (some #{%} path))
                    targets))))

(some #(and (= 2 (first %)) %) (walk [4] [[0 1] [1 2] [2 3] [1 4] [2 4]]))

(walk [0] [[0 1] [1 2] [2 3] [1 4] [2 4]])

(def paths [[0] [2 4]])
(def links [[0 1] [1 2] [2 3] [1 5] [2 4]])
(map #(walk % links) paths)
(apply concat (map #(walk % links) paths))
(some #(and (= 3 (first %)) %) (apply concat (map #(walk % links) paths)))
(defn walker [p] (apply concat (map #(walk % links) p)))
(walker (walker (walker paths)))

(defn find-first-path [paths target links]
  (if (zero? (count (find-targets target links)))
    nil
    (let [new-paths (apply concat (map #(walk % links) paths))
          ;_ (debug-value ["*** new-paths + paths" new-paths, paths])
          path-to-target (some #(and (= target (first %)) %) new-paths)
          equals (debug-value ["*** equals for target " target " ? " (= paths new-paths)])
          ]
      (if (and (nil? path-to-target) (not (= paths new-paths)))
        (find-first-path new-paths target links)
        (or path-to-target nil)))))

(defn find-first-paths [from targets links]
  (reduce #(let [new-path (find-first-path [[from]] %2 links)]
             (if (nil? new-path)
               %1
               (conj %1 new-path)))
          [] targets))

(find-first-paths 3 [3 4 2] links)
(sort-by count (find-first-paths 0 [3 6 2] links))
(first (sort-by count (find-first-paths 0 [3 4 2] links)))
(defn same-link? [[x1 y1] [x2 y2]]
  (or (and (= x1 x2) (= y1 y2))
      (and (= x1 y2) (= x2 y1))))


; Auto-generated code below aims at helping you parse
; the standard input according to the problem statement.

(defn -main [& args]
  (let [N (read)
        L (read)
        E (read)
        initial-links (read-links L)
        exits (read-exits E)
        ]
    ; N: the total number of nodes in the level, including the gateways
    ; L: the number of links
    ; E: the number of exit gateways
    (loop [links initial-links]
      (let [SI (read)
            _ (debug-value ["*** exits + links" exits links])
            paths (find-first-paths SI exits links)
            shorter-path (first (sort-by count paths))
            from (last shorter-path)
            to (last (butlast shorter-path))]
            ;from (first shorter-path)
            ;to (second shorter-path)]
        ; SI: The index of the node on which the Skynet agent is positioned this turn

        ; Example: 0 1 are the indices of the nodes you wish to sever the link between
        (println (str from " " to))
        (recur  (remove #(same-link? [from to] %) links))))))
