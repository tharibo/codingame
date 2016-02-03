(ns Solution
  (:gen-class))
(require '[clojure.string :as str])

(defn debug-value [value]
  (binding [*out* *err*]
    (println value))
  value)

(defn read-user-position []
  (let [LON (read)
        LAT (read)]
    [LON LAT]))

(defn read-defibrillator-data []
  (let [data (read-line)
        fields (str/split data #";")
        defibrillator-data (zipmap [:id :name :address :phone :longitude :latitude] fields)]
    defibrillator-data))

; Auto-generated code below aims at helping you parse
; the standard input according to the problem statement.

(defn -main [& args]
  (let [LON (read) LAT (read) N (read) _ (read-line)]
    (loop [i N]
      (when (> i 0)
        (let [DEFIB (read-line)]
        (recur (dec i)))))

    ; (binding [*out* *err*]
    ;   (println "Debug messages..."))

    ; Write answer to stdout
    (println "answer")))
