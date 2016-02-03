(ns Solution
  (:gen-class))

(defn debug-value [value]
  (binding [*out* *err*]
    (println value))
  value)

(defn char-to-binary [c]
  (let [bin (Integer/toString (int (debug-value c)) 2)]
    (str
     (apply str (repeat (- 7 (count bin)) \0))
     bin)))

(defn to-binary [s]
  "Transforms a string into a string consisting of binary characters"
  (apply str (map #(debug-value (char-to-binary %)) s)))
  ;(reduce (fn [s c] (str s (debug-value (char-to-binary c))))
  ;        ""
  ;        s))

(defn chucknorris-encode-block [block]
  (str
   (if (= \1 (first block)) "0 " "00 ")
   (apply str (repeat (count block) \0))
   " "))
(Integer/toString (int \%) 2)
(defn chucknorris-encode [s]
  (apply str (map chucknorris-encode-block (partition-by identity (debug-value s)))))
(defn -main [& args]
  (let [MESSAGE (read-line)
        binary-encoded-message (to-binary (debug-value MESSAGE))
        answer (clojure.string/trim (chucknorris-encode binary-encoded-message))]

    ; (binding [*out* *err*]
    ;   (println "Debug messages..."))

    ; Write answer to stdout
    (println answer)))
