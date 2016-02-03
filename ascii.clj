(ns Solution
  (:gen-class))

; Auto-generated code below aims at helping you parse
; the standard input according to the problem statement.

(defn map-line-to-letters [s L]
  (loop [remaining-chars "ABCDEFGHIJKLMNOPQRSTUVWXYZ?"
         chars-repr s
         mapped-chars {}]
    ; (binding [*out* *err*]
    ;   (println (str "*** L=" L " count chars-repr=" (count chars-repr) " count remaining-chars=" (count remaining-chars))))
    (if (<= (count remaining-chars) 0)
      mapped-chars
      (recur
        (rest remaining-chars)
        (subs chars-repr L)
        (assoc mapped-chars (first remaining-chars) (subs chars-repr 0 L))))))

(defn read-letters [H L]
  (loop [i H
         letters []]
    (if (> i 0)
      (let [ROW (read-line)]
        ; (binding [*out* *err*]
        ;   (println i))
        (recur
          (dec i)
          (conj letters (map-line-to-letters ROW L))))
      letters)))

(defn retrieve-line-text [line T letters-repr]
  (loop [remaining-chars T
         line-of-text ""]
    (if (<= (count remaining-chars) 0)
      line-of-text
      (let [line-repr (get letters-repr line)
            letter-repr (get line-repr (first remaining-chars) (get line-repr \?))]
        (recur
          (rest remaining-chars)
          (str line-of-text letter-repr))))))

(defn -main [& args]
  (let [L (read)
        H (read)
        _ (read-line)
        T (clojure.string/upper-case (read-line))
        letters (read-letters H L)]
    
    (binding [*out* *err*]
      (println (str "*** " L " " H " " _ " " T))
      (loop [i H]
        (when (> i 0)
          (println (str "'" (retrieve-line-text (- H i) "MA" letters) "'"))
          (recur (dec i))))
    )

    (loop [i H]
      (when (> i 0)
        (println (retrieve-line-text (- H i) T letters))
        (recur (dec i))))))