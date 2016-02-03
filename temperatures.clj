(ns Solution
  (:gen-class))
  ;(:require [java.lang.Math :as Math])

; Auto-generated code below aims at helping you parse
; the standard input according to the problem statement.

(defn abs [x] (max x (- 0 x)))
;(defn abs [x] (Math/abs x))

(defn closerToZero [x y]
	(cond
		(= (abs x) (abs y)) (max x y)
		(< (abs x) (abs y)) x
		:else y
		))

(defn -main [& args]
  (let [n (read) _ (read-line) temps (read-line)]
    ; n: the number of temperatures to analyse
    ; temps: the n temperatures expressed as integers ranging from -273 to 5526
    
    (binding [*out* *err*]
      (println (type (first (clojure.string/split temps #" ")))))

    (if (= (count temps) 0)
    	(println 0)
	  	(loop [closest 5526
	  		   t (map #(Integer/parseInt %) (clojure.string/split temps #" "))]
	  		(if (= (count t) 0)
	  			(println closest)
		  		(recur
		  			(closerToZero closest (first t))
		  			(rest t)
		  		)))
  	)
    
    ; Write answer to stdout
    ;(println "result")))
))